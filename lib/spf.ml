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
    try Domain_name.of_string (expand hmp t)
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
  | A of [ `raw ] Domain_name.t option * int option * int option
  | All
  | Exists of [ `raw ] Domain_name.t
  | Include of [ `raw ] Domain_name.t
  | Mx of [ `raw ] Domain_name.t option * int option * int option
  | Ptr of [ `raw ] Domain_name.t option
  | V4 of Ipaddr.V4.Prefix.t
  | V6 of Ipaddr.V6.Prefix.t

let pp_cidr ppf = function None -> () | Some v -> Fmt.pf ppf "/%d" v

let pp_dual_cidr ppf = function
  | v4, (Some _ as v6) -> Fmt.pf ppf "%a/%a" pp_cidr v4 pp_cidr v6
  | v4, None -> Fmt.pf ppf "%a" pp_cidr v4

let pp_mechanism ppf = function
  | A (v, cidr_v4, cidr_v6) ->
      Fmt.pf ppf "a:%a%a"
        Fmt.(option Domain_name.pp)
        v pp_dual_cidr (cidr_v4, cidr_v6)
  | All -> Fmt.string ppf "all"
  | Exists v -> Fmt.pf ppf "exists:%a" Domain_name.pp v
  | Include v -> Fmt.pf ppf "include:%a" Domain_name.pp v
  | Mx (v, cidr_v4, cidr_v6) ->
      Fmt.pf ppf "mx:%a%a"
        Fmt.(option Domain_name.pp)
        v pp_dual_cidr (cidr_v4, cidr_v6)
  | Ptr (Some v) -> Fmt.pf ppf "ptr:%a" Domain_name.pp v
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
      let macro = Option.map (R.to_option <.> Macro.expand_macro ctx) macro in
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
      let macro = Option.map (R.to_option <.> Macro.expand_macro ctx) macro in
      let macro = Option.join macro in
      let mechanism =
        (quantifier_of_letter quantifier, Mx (macro, cidr_v4, cidr_v6)) in
      { acc with mechanisms = mechanism :: acc.mechanisms }
  | `Directive (quantifier, `Ptr macro) ->
      let quantifier = quantifier_of_letter quantifier in
      let macro = Option.map (R.to_option <.> Macro.expand_macro ctx) macro in
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
        Explanation
          (Domain_name.to_string
          @@ R.failwith_error_msg (Macro.expand_macro ctx macro)) in
      { acc with modifiers = modifier :: acc.modifiers }
  | `Redirect macro ->
      let modifier =
        Lazy.from_fun @@ fun () ->
        Redirect
          (Domain_name.to_string
          @@ R.failwith_error_msg (Macro.expand_macro ctx macro)) in
      { acc with modifiers = modifier :: acc.modifiers }
  | `Unknown _ -> acc

let rec select_spf1 = function
  | [] -> Error `None
  | x :: r when String.length x >= 6 ->
      if String.sub x 0 6 = "v=spf1" then R.ok x else select_spf1 r
  | _ :: r -> select_spf1 r

type res =
  [ `None | `Neutral | `Pass | `Fail | `Softfail | `Temperror | `Permerror ]

let pp_res ppf = function
  | `None -> Fmt.pf ppf "none"
  | `Neutral -> Fmt.pf ppf "neutral"
  | `Pass -> Fmt.pf ppf "pass"
  | `Fail -> Fmt.pf ppf "fail"
  | `Softfail -> Fmt.pf ppf "softfail"
  | `Temperror -> Fmt.pf ppf "temperror"
  | `Permerror -> Fmt.pf ppf "permerror"

let record :
    type dns t.
    ctx:ctx ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    (([ res | `Record of record ], [> `Msg of string ]) result, t) io =
 fun ~ctx { bind; return } dns (module DNS) ->
  let ( >>= ) = bind in
  match Map.find Map.K.domain ctx with
  | None -> return (R.error_msgf "Missing domain-name into the given context")
  | Some domain_name -> (
      DNS.getrrecord dns Dns.Rr_map.Txt domain_name >>= function
      | Error (`No_domain _ | `No_data _) -> return (Ok `None)
      | Error (`Msg err) ->
          Log.err (fun m -> m "Got an error while requesting DNS: %s." err) ;
          return (Ok `Temperror)
      | Ok (_, txts) ->
      match
        R.(select_spf1 (Dns.Rr_map.Txt_set.elements txts) >>= Term.parse_record)
      with
      | Error `None -> return (Ok `None) (* XXX(dinosaure): see RFC 7208, 4.5 *)
      | Error (`Msg err) ->
          Log.err (fun m ->
              m "Invalid SPF record: %a: %s."
                Fmt.(Dump.list string)
                (Dns.Rr_map.Txt_set.elements txts)
                err) ;
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

let of_quantifier q match' =
  match (q, match') with
  | Pass, true -> `Pass
  | Fail, true -> `Fail
  | Softfail, true -> `Softfail
  | Neutral, true -> `Neutral
  | _ -> `Continue

let ipv4_with_cidr cidr v4 =
  Option.fold
    ~none:(Ipaddr.V4.Prefix.of_addr v4)
    ~some:(fun v -> Ipaddr.V4.Prefix.make v v4)
    cidr

let ipv6_with_cidr cidr v6 =
  Option.fold
    ~none:(Ipaddr.V6.Prefix.of_addr v6)
    ~some:(fun v -> Ipaddr.V6.Prefix.make v v6)
    cidr

let ipaddrs_of_mx :
    type t dns.
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    Dns.Mx.t ->
    int option * int option ->
    ((Ipaddr.Prefix.t list, [> `Temperror ]) result, t) io =
 fun { return; bind } dns (module DNS) { Dns.Mx.mail_exchange; _ }
     (cidr_v4, cidr_v6) ->
  let ( >>= ) = bind in
  DNS.getrrecord dns Dns.Rr_map.A mail_exchange >>= function
  | Ok (_, v4s) ->
      let v4s = Dns.Rr_map.Ipv4_set.elements v4s in
      let v4s = List.map (ipv4_with_cidr cidr_v4) v4s in
      return (Ok (List.map (fun v -> Ipaddr.V4 v) v4s))
  | Error _ -> (
      (* XXX(dinosaure): care about [`Msg _] and return [`Temperror]? *)
      DNS.getrrecord dns Dns.Rr_map.Aaaa mail_exchange
      >>= function
      | Ok (_, v6s) ->
          let v6s = Dns.Rr_map.Ipv6_set.elements v6s in
          let v6s = List.map (ipv6_with_cidr cidr_v6) v6s in
          return (Ok (List.map (fun v -> Ipaddr.V6 v) v6s))
      | Error (`Msg _) -> return (Error `Temperror)
      | Error (`No_domain _ | `No_data _) -> return (Ok []))

let rec mx_mechanism :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    quantifier ->
    'a Domain_name.t ->
    int option * int option ->
    ([ `Continue | res ], t) io =
 fun ~ctx ~limit ({ bind; return } as state) dns (module DNS) q domain_name
     dual_cidr ->
  let ( >>= ) = bind in
  DNS.getrrecord dns Dns.Rr_map.Mx domain_name >>= function
  | Error (`Msg _) -> return `Temperror
  | Error (`No_data _ | `No_domain _) (* RCODE:3 *) -> return `Continue
  | Ok (_, mxs) ->
      go ~limit state dns
        (module DNS)
        q (Map.get Map.K.ip ctx)
        (Dns.Rr_map.Mx_set.elements mxs)
        dual_cidr

and go :
    type t dns.
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    quantifier ->
    Ipaddr.t ->
    Dns.Mx.t list ->
    int option * int option ->
    ([ `Continue | res ], t) io =
 fun ~limit ({ bind; return } as state) dns (module DNS) q expected mxs
     dual_cidr ->
  let ( >>= ) = bind in
  if limit >= 10
  then return `Permerror
  else
    match mxs with
    | [] -> return `Continue
    | mx :: mxs -> (
        ipaddrs_of_mx state dns (module DNS) mx dual_cidr >>= function
        | Error `Temperror -> return `Temperror
        | Ok vs ->
        match of_quantifier q (List.exists (Ipaddr.Prefix.mem expected) vs) with
        | `Continue ->
            go ~limit:(succ limit) state dns
              (module DNS)
              q expected mxs dual_cidr
        | #res as res -> return res)

let a_mechanism :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    quantifier ->
    'a Domain_name.t ->
    int option * int option ->
    ([ `Continue | res ], t) io =
 fun ~ctx ~limit:_ { bind; return } dns (module DNS) q domain_name
     (cidr_v4, cidr_v6) ->
  let ( >>= ) = bind in
  DNS.getrrecord dns Dns.Rr_map.A domain_name >>= function
  | Ok (_, v4s) ->
      let v4s = Dns.Rr_map.Ipv4_set.elements v4s in
      let v4s = List.map (ipv4_with_cidr cidr_v4) v4s in
      let expected = Map.get Map.K.ip ctx in
      return
        (of_quantifier q
           (List.exists
              (Ipaddr.Prefix.mem expected)
              (List.map (fun v -> Ipaddr.V4 v) v4s)))
  | Error _ -> (
      DNS.getrrecord dns Dns.Rr_map.Aaaa domain_name >>= function
      | Ok (_, v6s) ->
          let v6s = Dns.Rr_map.Ipv6_set.elements v6s in
          let v6s = List.map (ipv6_with_cidr cidr_v6) v6s in
          let expected = Map.get Map.K.ip ctx in
          return
            (of_quantifier q
               (List.exists
                  (Ipaddr.Prefix.mem expected)
                  (List.map (fun v -> Ipaddr.V6 v) v6s)))
      | Error (`Msg _) -> return `Temperror
      | Error (`No_domain _ | `No_data _) -> return `Continue)

let exists_mechanism :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    quantifier ->
    'a Domain_name.t ->
    ([ `Continue | res ], t) io =
 fun ~ctx:_ ~limit:_ { bind; return } dns (module DNS) q domain_name ->
  let ( >>= ) = bind in
  DNS.getrrecord dns Dns.Rr_map.A domain_name >>= function
  | Ok _ -> return (of_quantifier q true)
  | Error _ -> (
      DNS.getrrecord dns Dns.Rr_map.Aaaa domain_name >>= function
      | Ok _ -> return (of_quantifier q true)
      | Error (`Msg _) -> return `Temperror
      | Error (`No_domain _ | `No_data _) -> return `Continue)

let rec include_mechanism :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    quantifier ->
    'a Domain_name.t ->
    ([ `Continue | res ], t) io =
 fun ~ctx ~limit ({ bind; return } as state) dns (module DNS) q domain_name ->
  let ( >>= ) = bind in
  let domain_name = Domain_name.host_exn domain_name in
  (* TODO(dinosaure): may be we don't need to sanitize the given domain-name. *)
  let ctx = Map.add Map.K.domain domain_name ctx in
  DNS.getrrecord dns Dns.Rr_map.Txt domain_name >>= function
  | Error (`Msg _) -> return `Temperror
  | Error (`No_domain _ | `No_data _) -> return `Permerror
  | Ok (_, txts) ->
  match
    R.(select_spf1 (Dns.Rr_map.Txt_set.elements txts) >>= Term.parse_record)
  with
  | Error `None | Error (`Msg _) -> return `Permerror
  | Ok terms -> (
      let record =
        List.fold_left (fold ctx) { mechanisms = []; modifiers = [] } terms
      in
      check ~ctx ~limit:(succ limit) state dns (module DNS) record >>= function
      | `Permerror | `None -> return `Permerror
      | `Temperror -> return `Temperror
      | `Pass -> return (of_quantifier q true)
      | `Fail | `Softfail | `Neutral -> return (of_quantifier q false))

and apply :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    quantifier * mechanism ->
    ([ `Continue | res ], t) io =
 fun ~ctx ~limit ({ return; _ } as state) dns (module DNS) (q, mechanism) ->
  match mechanism with
  | All -> return (of_quantifier q true)
  | A (Some domain_name, cidr_ipv4, cidr_ipv6) ->
      Log.debug (fun m ->
          m "Apply A mechanism with %a." Domain_name.pp domain_name) ;
      a_mechanism ~ctx ~limit state dns
        (module DNS)
        q domain_name (cidr_ipv4, cidr_ipv6)
  | Mx (Some domain_name, cidr_ipv4, cidr_ipv6) ->
      Log.debug (fun m ->
          m "Apply MX mechanism with %a." Domain_name.pp domain_name) ;
      mx_mechanism ~ctx ~limit state dns
        (module DNS)
        q domain_name (cidr_ipv4, cidr_ipv6)
  | Include domain_name ->
      Log.debug (fun m ->
          m "Apply INCLUDE mechanism with %a." Domain_name.pp domain_name) ;
      include_mechanism ~ctx ~limit state dns (module DNS) q domain_name
  | V4 v4 ->
      Log.debug (fun m ->
          m "Apply IPv4 mechanism with %a." Ipaddr.V4.Prefix.pp v4) ;
      return
        (of_quantifier q
           (Ipaddr.Prefix.mem (Map.get Map.K.ip ctx) (Ipaddr.V4 v4)))
  | V6 v6 ->
      Log.debug (fun m ->
          m "Apply IPv6 mechanism with %a." Ipaddr.V6.Prefix.pp v6) ;
      return
        (of_quantifier q
           (Ipaddr.Prefix.mem (Map.get Map.K.ip ctx) (Ipaddr.V6 v6)))
  | A (None, _, _) | Mx (None, _, _) -> return `Continue
  | Exists domain_name ->
      exists_mechanism ~ctx ~limit state dns (module DNS) q domain_name
  | Ptr _ -> assert false

and check :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    record ->
    (res, t) io =
 fun ~ctx ~limit state dns (module DNS) record ->
  go ~ctx ~limit state dns (module DNS) record.mechanisms

and go :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    (quantifier * mechanism) list ->
    (res, t) io =
 fun ~ctx ~limit ({ bind; return } as state) dns (module DNS) -> function
  | [] -> assert false (* TODO *)
  | (q, mechanism) :: r when limit < 10 -> (
      let ( >>= ) = bind in
      apply ~ctx ~limit state dns (module DNS) (q, mechanism) >>= function
      | `Continue -> go ~ctx ~limit:(succ limit) state dns (module DNS) r
      | #res as res -> return res)
  | _ -> return `Permerror

let check :
    type t dns.
    ctx:ctx ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    [ res | `Record of record ] ->
    (res, t) io =
 fun ~ctx ({ return; _ } as state) dns (module DNS) -> function
  | #res as res -> return res
  | `Record record -> check ~ctx ~limit:0 state dns (module DNS) record
