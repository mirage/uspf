module Sigs = Sigs
open Rresult
open Sigs

let src = Logs.Src.create "spf"

module Log = (val Logs.src_log src : Logs.LOG)

type ctx = Map.t

let empty = Map.empty

let with_sender sender ctx =
  match sender with
  | `HELO domain_name ->
      let domain = Colombe.Domain.Domain (Domain_name.to_strings domain_name) in
      let ctx = Map.add Map.K.helo domain ctx in
      let ctx = Map.add Map.K.domain_of_sender domain ctx in
      let ctx = Map.add Map.K.sender sender ctx in
      let ctx = Map.add Map.K.domain domain_name ctx in
      let ctx = Map.add Map.K.local (`Dot_string [ "postmaster" ]) ctx in
      (* XXX(dinosaure): see RFC 7208, 4.3, If the sender has no local-part,
       * substitute the string "postmaster" for the local-part. *)
      ctx
  | `MAILFROM { Colombe.Path.local; domain; _ } ->
      let ctx = Map.add Map.K.local local ctx in
      let ctx = Map.add Map.K.domain_of_sender domain ctx in
      let ctx = Map.add Map.K.sender sender ctx in
      let ctx =
        match domain with
        | Colombe.Domain.IPv4 _ | Colombe.Domain.IPv6 _
        | Colombe.Domain.Extension _ ->
            ctx
        | Colombe.Domain.Domain vs ->
            Map.add Map.K.domain Domain_name.(host_exn (of_strings_exn vs)) ctx
      in
      ctx

let with_ip ip ctx =
  match ip with
  | Ipaddr.V4 _ ->
      let ctx = Map.add Map.K.v `In_addr ctx in
      let ctx = Map.add Map.K.ip ip ctx in
      ctx
  | Ipaddr.V6 _ ->
      let ctx = Map.add Map.K.v `Ip6 ctx in
      let ctx = Map.add Map.K.ip ip ctx in
      ctx

module Macro = struct
  open Angstrom

  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_alphanum chr = is_alpha chr || is_digit chr

  let is_wsp = ( = ) ' '

  let toplabel =
    take_while1 is_alpha
    >>= (fun a -> take_while is_alphanum >>= fun b -> return (a ^ b))
    <|> ( take_while1 is_alphanum >>= fun a ->
          char '-' *> take_while1 (fun chr -> is_alphanum chr || chr = '-')
          >>= fun b ->
          if String.length b > 0 && b.[String.length b - 1] = '-'
          then fail "toplabel"
          else return (a ^ "-" ^ b) )

  let macro_literal =
    let satisfy = function
      | '\x21' .. '\x24' | '\x26' .. '\x7e' -> true
      | _ -> false in
    take_while1 satisfy >>| fun str -> `Literal str

  let junk_with v = advance 1 *> return v

  let macro_letter =
    peek_char >>= function
    | Some
        (('s' | 'l' | 'o' | 'd' | 'i' | 'p' | 'h' | 'c' | 'r' | 't' | 'v') as
        chr) ->
        junk_with chr
    | _ -> fail "macro-letter"

  let transformers =
    take_while is_digit >>= function
    | "" ->
        option false (char 'r' *> return true) >>= fun rev -> return (None, rev)
    | number -> (
        peek_char >>= function
        | Some 'r' -> junk_with (Some (int_of_string number), true)
        | _ -> return (Some (int_of_string number), false))

  let is_delimiter = function
    | '.' | '-' | '+' | ',' | '/' | '_' | '=' -> true
    | _ -> false

  let macro_expand =
    let macro =
      macro_letter >>= fun letter ->
      transformers >>= fun transformers ->
      take_while is_delimiter >>= fun delimiter ->
      return (`Macro (letter, transformers, delimiter)) in
    choice
      [
        string "%%" *> return `Macro_percent;
        string "%_" *> return `Macro_space;
        string "%-" *> return `Macro_encoded_space;
        string "%{" *> macro <* string "}";
      ]

  let macro_string = many (macro_expand <|> macro_literal)

  let _explain_string =
    many (macro_string <|> (take_while1 is_wsp >>| fun wsp -> [ `Space wsp ]))

  let domain_end = char '.' *> toplabel <* option '.' (char '.')

  let domain_spec = both macro_string (option None (domain_end >>| Option.some))

  type macro =
    [ `Literal of string
    | `Macro of char * (int option * bool) * string
    | `Macro_encoded_space
    | `Macro_space
    | `Macro_percent ]

  type t = macro list * string option

  type key = Key : 'a Map.key -> key

  let key_of_letter = function
    | 'i' -> Key Map.K.ip
    | 's' -> Key Map.K.sender
    | 'l' -> Key Map.K.local
    | 'd' -> Key Map.K.domain
    | 'o' -> Key Map.K.domain_of_sender
    | 'v' -> Key Map.K.v
    | 'h' -> Key Map.K.helo
    | _ -> assert false

  let keep n lst =
    let rec go acc n lst =
      if n = 0
      then acc
      else match lst with [] -> acc | x :: r -> go (x :: acc) (n - 1) r in
    go [] n (List.rev lst)

  let expand_macro hmp buf = function
    | `Literal str -> Buffer.add_string buf str
    | `Macro_encoded_space -> Buffer.add_string buf "%20"
    | `Macro_space -> Buffer.add_string buf " "
    | `Macro_percent -> Buffer.add_string buf "%"
    | `Macro (letter, (n, rev), sep) ->
        let (Key key) = key_of_letter letter in
        let pp = (Map.Key.info key).pp in
        let str = Fmt.str "%a" pp (Map.get key hmp) in
        let sep = if sep = "" then "." else sep in
        let vs = Astring.String.cuts ~sep str in
        let vs = if rev then List.rev vs else vs in
        let vs = Option.fold ~none:vs ~some:(fun n -> keep n vs) n in
        let str = String.concat "." vs in
        Buffer.add_string buf str

  let expand hmp (macro, rest) =
    let buf = Buffer.create 0x100 in
    List.iter (expand_macro hmp buf) macro ;
    match rest with
    | None -> Buffer.contents buf
    | Some str ->
        Buffer.add_string buf "." ;
        Buffer.add_string buf str ;
        Buffer.contents buf

  let expand_macro hmp t =
    try R.ok (expand hmp t)
    with Not_found ->
      Fmt.error_msg "Missing informations while expanding macro"

  let expand_string hmp str =
    match Angstrom.parse_string ~consume:All domain_spec str with
    | Ok v -> expand_macro hmp v
    | Error _ -> R.error_msgf "Invalid macro specification: %S" str
end

module Term = struct
  open Angstrom

  let is_sp = ( = ) ' '

  let is_digit = function '0' .. '9' -> true | _ -> false

  let junk_with v = advance 1 *> return v

  let junk_only_if p =
    peek_char >>= function
    | Some chr when p chr -> advance 1 *> return (Some chr)
    | _ -> return None

  let ip4_cidr_length =
    char '/' *> peek_char >>= function
    | Some '0' -> junk_with (Some 0)
    | Some ('\x31' .. '\x39' as chr) -> (
        junk_with chr >>= fun a ->
        peek_char >>= function
        | Some ('0' .. '9' as b) ->
            let tmp = Bytes.create 2 in
            Bytes.set tmp 0 a ;
            Bytes.set tmp 1 b ;
            junk_with (Some (int_of_string (Bytes.unsafe_to_string tmp)))
        | _ -> return (Some (int_of_string (String.make 1 a))))
    | _ -> return None

  let ip6_cidr_length =
    char '/' *> peek_char >>= function
    | Some '0' -> junk_with (Some 0)
    | Some '\x31' .. '\x39' -> (
        junk_only_if is_digit >>= fun chr0 ->
        junk_only_if is_digit >>= fun chr1 ->
        match (chr0, chr1) with
        | Some chr0, Some chr1 ->
            let str = Bytes.create 2 in
            Bytes.set str 0 chr0 ;
            Bytes.set str 1 chr1 ;
            return (Some (int_of_string (Bytes.unsafe_to_string str)))
        | Some chr0, _ ->
            let str = Bytes.create 1 in
            Bytes.set str 0 chr0 ;
            return (Some (int_of_string (Bytes.unsafe_to_string str)))
        | None, Some _ -> fail "ip6-cidr-length"
        | None, None -> return None)
    | _ -> return None

  let dual_cidr_length =
    option None ip4_cidr_length >>= fun ip4 ->
    option None (char '/' *> ip6_cidr_length) >>= fun ip6 -> return (ip4, ip6)

  let all = string "all" *> return `All

  let include_mechanism =
    string "include"
    *> skip_while is_sp
    *> char ':'
    *> skip_while is_sp
    *> Macro.domain_spec
    >>| fun macro -> `Include macro

  let mx =
    string "mx"
    *> option None
         (skip_while is_sp *> char ':' *> skip_while is_sp *> Macro.domain_spec
         >>| Option.some)
    >>= fun macro ->
    option (None, None) dual_cidr_length >>= fun dual_cidr_length ->
    return (`Mx (macro, dual_cidr_length))

  let a =
    char 'a'
    *> option None
         (skip_while is_sp *> char ':' *> skip_while is_sp *> Macro.domain_spec
         >>| Option.some)
    >>= fun macro ->
    option (None, None) dual_cidr_length >>= fun dual_cidr_length ->
    return (`A (macro, dual_cidr_length))

  let ptr =
    string "ptr"
    *> (skip_while is_sp *> char ':' *> skip_while is_sp *> Macro.domain_spec
       >>| Option.some)
    >>= fun macro -> return (`Ptr macro)

  let qnum =
    string "25" *> (satisfy @@ function '\x30' .. '\x35' -> true | _ -> false)
    >>= (fun a -> return ("25" ^ String.make 1 a))
    <|> ( char '2'
        *> both
             (satisfy @@ function '\x30' .. '\x34' -> true | _ -> false)
             (satisfy is_digit)
        >>= fun (a, b) ->
          let tmp = Bytes.create 3 in
          Bytes.set tmp 0 '2' ;
          Bytes.set tmp 1 a ;
          Bytes.set tmp 2 b ;
          return (Bytes.unsafe_to_string tmp) )
    <|> ( char '1' *> both (satisfy is_digit) (satisfy is_digit)
        >>= fun (a, b) ->
          let tmp = Bytes.create 3 in
          Bytes.set tmp 0 '1' ;
          Bytes.set tmp 1 a ;
          Bytes.set tmp 2 b ;
          return (Bytes.unsafe_to_string tmp) )
    <|> ( both
            (satisfy @@ function '\x31' .. '\x39' -> true | _ -> false)
            (satisfy is_digit)
        >>= fun (a, b) ->
          let tmp = Bytes.create 2 in
          Bytes.set tmp 0 a ;
          Bytes.set tmp 1 b ;
          return (Bytes.unsafe_to_string tmp) )
    <|> (satisfy is_digit >>| String.make 1)

  let ip6_network =
    available >>= fun len ->
    Unsafe.peek len Bigstringaf.substring >>= fun str ->
    let idx = ref 0 in
    try
      let v6 = Ipaddr.V6.of_string_raw str idx in
      advance !idx *> return v6
    with _ -> fail "ip6-network"

  let ip4_network =
    qnum <* char '.' >>= fun a ->
    qnum <* char '.' >>= fun b ->
    qnum <* char '.' >>= fun c ->
    qnum >>= fun d -> return (a, b, c, d)

  let ip4 =
    string "ip4"
    *> skip_while is_sp
    *> char ':'
    *> skip_while is_sp
    *> ip4_network
    >>= fun (a, b, c, d) ->
    option None ip4_cidr_length >>= fun cidr ->
    let str =
      Fmt.str "%s.%s.%s.%s%a" a b c d
        Fmt.(option ~none:(const string "/0") (prefix (const string "/") int))
        cidr in
    return (`V4 (Ipaddr.V4.Prefix.of_string_exn str))

  let ip6 =
    string "ip6"
    *> skip_while is_sp
    *> char ':'
    *> skip_while is_sp
    *> ip6_network
    >>= fun v6 ->
    option None ip6_cidr_length >>= fun cidr ->
    let cidr = Option.value ~default:0 cidr in
    return (`V6 (Ipaddr.V6.Prefix.make cidr v6))

  let exists =
    string "exists"
    *> skip_while is_sp
    *> char ':'
    *> skip_while is_sp
    *> Macro.domain_spec
    >>= fun macro -> return (`Exists macro)

  let redirect =
    string "redirect"
    *> skip_while is_sp
    *> char '='
    *> skip_while is_sp
    *> Macro.domain_spec
    >>| fun macro -> `Redirect macro

  let explanation =
    string "exp"
    *> skip_while is_sp
    *> char '='
    *> skip_while is_sp
    *> Macro.domain_spec
    >>| fun macro -> `Explanation macro

  let unknown_modifier =
    let name =
      peek_char >>= function
      | Some ('a' .. 'z' | 'A' .. 'Z') -> (
          take_while1 @@ function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
          | _ -> false)
      | _ -> fail "name" in
    name >>= fun name ->
    skip_while is_sp *> char '=' *> skip_while is_sp *> Macro.macro_string
    >>= fun str -> return (`Unknown (name, str))

  let mechanism =
    all <|> include_mechanism <|> a <|> mx <|> ptr <|> ip4 <|> ip6 <|> exists

  let modifier = redirect <|> explanation <|> unknown_modifier

  let qualifier =
    peek_char >>= function
    | Some (('+' | '-' | '?' | '~') as chr) -> junk_with (Some chr)
    | _ -> return None

  let directive =
    qualifier >>= fun qualifier ->
    mechanism >>= fun mechanism -> return (`Directive (qualifier, mechanism))

  let terms = many (take_while1 is_sp *> (directive <|> modifier))

  let record = string "v=spf1" *> terms <* skip_while is_sp

  type t =
    [ `Directive of
      char option
      * [ `A of Macro.t option * (int option * int option)
        | `All
        | `Exists of Macro.t
        | `Include of Macro.t
        | `Mx of Macro.t option * (int option * int option)
        | `Ptr of Macro.t option
        | `V4 of Ipaddr.V4.Prefix.t
        | `V6 of Ipaddr.V6.Prefix.t ]
    | `Explanation of Macro.t
    | `Redirect of Macro.t
    | `Unknown of string * Macro.macro list ]
    list

  let parse_record str =
    match Angstrom.parse_string ~consume:All record str with
    | Ok (v : t) -> Ok v
    | Error _ -> R.error_msgf "Invalid SPF record: %S" str
end

type mechanism =
  | A of string option * int option * int option
  | All
  | Exists of string
  | Include of string
  | Mx of string option * int option * int option
  | Ptr of string option
  | V4 of Ipaddr.V4.Prefix.t
  | V6 of Ipaddr.V6.Prefix.t

let pp_cidr ppf = function None -> () | Some v -> Fmt.pf ppf "/%d" v

let pp_dual_cidr ppf = function
  | v4, (Some _ as v6) -> Fmt.pf ppf "%a/%a" pp_cidr v4 pp_cidr v6
  | v4, None -> Fmt.pf ppf "%a" pp_cidr v4

let pp_mechanism ppf = function
  | A (v, cidr_v4, cidr_v6) ->
      Fmt.pf ppf "a:%a%a" Fmt.(option string) v pp_dual_cidr (cidr_v4, cidr_v6)
  | All -> Fmt.string ppf "all"
  | Exists v -> Fmt.pf ppf "exists:%s" v
  | Include v -> Fmt.pf ppf "include:%s" v
  | Mx (v, cidr_v4, cidr_v6) ->
      Fmt.pf ppf "mx:%a%a" Fmt.(option string) v pp_dual_cidr (cidr_v4, cidr_v6)
  | Ptr (Some v) -> Fmt.pf ppf "ptr:%s" v
  | Ptr None -> ()
  | V4 ipv4 -> Fmt.pf ppf "ip4:%a" Ipaddr.V4.Prefix.pp ipv4
  | V6 ipv6 -> Fmt.pf ppf "ip6:%a" Ipaddr.V6.Prefix.pp ipv6

type quantifier = Pass | Fail | Softfail | Neutral

let pp_quantifier ppf = function
  | Pass -> ()
  | Fail -> Fmt.string ppf "-"
  | Softfail -> Fmt.string ppf "~"
  | Neutral -> Fmt.string ppf "?"

let quantifier_of_letter = function
  | Some '+' | None -> Pass
  | Some '-' -> Fail
  | Some '~' -> Softfail
  | Some '?' -> Neutral
  | _ -> invalid_arg "quantifier_of_letter"

type modifier = Explanation of string | Redirect of string

let pp_modifier ppf = function
  | Explanation v -> Fmt.pf ppf "exp=%s" v
  | Redirect v -> Fmt.pf ppf "redirect=%s" v

type record = {
  mechanisms : (quantifier * mechanism) list;
  modifiers : modifier Lazy.t list;
}

let pp ppf { mechanisms; modifiers } =
  Fmt.pf ppf "%a"
    Fmt.(list ~sep:(always " ") (pair ~sep:nop pp_quantifier pp_mechanism))
    mechanisms ;
  match modifiers with
  | [] -> ()
  | _ :: _ ->
      let modifiers = List.map Lazy.force modifiers in
      Fmt.pf ppf " %a" Fmt.(list ~sep:(always " ") pp_modifier) modifiers

let ( <.> ) f g x = f (g x)

let fold ctx acc = function
  | `Directive (quantifier, `A (macro, (cidr_v4, cidr_v6))) ->
      let macro =
        Option.map (Result.to_option <.> Macro.expand_macro ctx) macro in
      let macro = Option.join macro in
      let mechanism =
        (quantifier_of_letter quantifier, A (macro, cidr_v4, cidr_v6)) in
      { acc with mechanisms = mechanism :: acc.mechanisms }
  | `Directive (quantifier, `All) ->
      {
        acc with
        mechanisms = (quantifier_of_letter quantifier, All) :: acc.mechanisms;
      }
  | `Directive (quantifier, `Exists macro) -> (
      let quantifier = quantifier_of_letter quantifier in
      match Macro.expand_macro ctx macro with
      | Ok macro ->
          { acc with mechanisms = (quantifier, Exists macro) :: acc.mechanisms }
      | Error _ -> acc
      (* TODO *))
  | `Directive (quantifier, `Include macro) -> (
      let quantifier = quantifier_of_letter quantifier in
      match Macro.expand_macro ctx macro with
      | Ok macro ->
          {
            acc with
            mechanisms = (quantifier, Include macro) :: acc.mechanisms;
          }
      | Error _ -> acc
      (* TODO *))
  | `Directive (quantifier, `Mx (macro, (cidr_v4, cidr_v6))) ->
      let macro =
        Option.map (Result.to_option <.> Macro.expand_macro ctx) macro in
      let macro = Option.join macro in
      let mechanism =
        (quantifier_of_letter quantifier, Mx (macro, cidr_v4, cidr_v6)) in
      { acc with mechanisms = mechanism :: acc.mechanisms }
  | `Directive (quantifier, `Ptr macro) ->
      let quantifier = quantifier_of_letter quantifier in
      let macro =
        Option.map (Result.to_option <.> Macro.expand_macro ctx) macro in
      let macro = Option.join macro in
      { acc with mechanisms = (quantifier, Ptr macro) :: acc.mechanisms }
  | `Directive (quantifier, `V4 ipv4) ->
      let quantifier = quantifier_of_letter quantifier in
      { acc with mechanisms = (quantifier, V4 ipv4) :: acc.mechanisms }
  | `Directive (quantifier, `V6 ipv6) ->
      let quantifier = quantifier_of_letter quantifier in
      { acc with mechanisms = (quantifier, V6 ipv6) :: acc.mechanisms }
  | `Explanation macro ->
      let modifier =
        Lazy.from_fun @@ fun () ->
        Explanation (R.failwith_error_msg (Macro.expand_macro ctx macro)) in
      { acc with modifiers = modifier :: acc.modifiers }
  | `Redirect macro ->
      let modifier =
        Lazy.from_fun @@ fun () ->
        Redirect (R.failwith_error_msg (Macro.expand_macro ctx macro)) in
      { acc with modifiers = modifier :: acc.modifiers }
  | `Unknown _ -> acc

let rec select_spf1 = function
  | [] -> R.error_msgf "No SPF record found"
  | x :: r when String.length x >= 6 ->
      if String.sub x 0 6 = "v=spf1" then R.ok x else select_spf1 r
  | _ :: r -> select_spf1 r

let get_records :
    type dns t.
    ctx ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    ( ([ `None | `Permerror | `Record of record ], [> `Msg of string ]) result,
      t )
    io =
 fun ctx { bind; return } dns (module DNS) ->
  let ( >>= ) = bind in
  match Map.find Map.K.domain ctx with
  | None -> return (R.error_msgf "Missing domain-name into the given context")
  | Some domain_name -> (
      DNS.getaddrinfo dns `TXT domain_name >>= function
      | Error (`Msg err) ->
          Log.warn (fun m -> m "Got an error with <getaddrinfo>: %s." err) ;
          return (Ok `None)
      | Ok txts ->
      match R.(select_spf1 txts >>= Term.parse_record) with
      | Error (`Msg err) ->
          Log.err (fun m ->
              m "Invalid SPF record: %a: %s." Fmt.(Dump.list string) txts err) ;
          return (Ok `Permerror)
      | Ok terms ->
          let record =
            List.fold_left (fold ctx) { mechanisms = []; modifiers = [] } terms
          in
          return
            (R.ok
               (`Record
                 {
                   mechanisms = List.rev record.mechanisms;
                   modifiers = List.rev record.modifiers;
                 })))
