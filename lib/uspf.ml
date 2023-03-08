module Sigs = Sigs
open Sigs

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let error_msg msg = Error (`Msg msg)
let failwith_error_msg = function Ok v -> v | Error (`Msg err) -> failwith err
let ( >>| ) x f = Result.map f x
let ( <.> ) f g x = f (g x)
let src = Logs.Src.create "spf"

module Log = (val Logs.src_log src : Logs.LOG)

type ctx = Map.t

let empty = Map.empty

let with_sender sender ctx =
  match sender with
  | `HELO (domain_name : [ `raw ] Domain_name.t) ->
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
        match (domain, Map.find Map.K.domain ctx) with
        | Colombe.Domain.Domain vs, None ->
            (* XXX(dinosaure): assume that an [HELO] should be already done with
             * the same domain-name. *)
            Map.add Map.K.domain (Domain_name.of_strings_exn vs) ctx
        | Colombe.Domain.Domain vs, Some vs' ->
            let vs = Domain_name.of_strings_exn vs in
            if not (Domain_name.equal vs vs')
            then Map.add Map.K.domain vs ctx
            else ctx
        | _ -> ctx in
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

let colombe_domain_to_domain_name = function
  | Colombe.Domain.Domain lst -> Domain_name.of_strings lst
  | v -> error_msgf "Invalid domain-name: %a" Colombe.Domain.pp v

let domain ctx =
  match
    ( Map.find Map.K.helo ctx,
      Map.find Map.K.domain_of_sender ctx,
      Map.find Map.K.domain ctx,
      Map.find Map.K.sender ctx )
  with
  | _, _, Some v, _ | _, _, _, Some (`HELO v) -> Some v
  | Some v, None, None, None
  | _, Some v, None, None
  | _, _, _, Some (`MAILFROM { Colombe.Path.domain = v; _ }) ->
      Result.to_option (colombe_domain_to_domain_name v)
  | None, None, None, None -> None

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

  let pp ppf (ms, domain) =
    let pp_macro ppf = function
      | `Literal v -> Fmt.pf ppf "%s" v
      | `Macro (letter, (transformers, true), delimiter) ->
          Fmt.pf ppf "%%{%c%ar%s}" letter
            Fmt.(option int)
            transformers delimiter
      | `Macro (letter, (transformers, false), delimiter) ->
          Fmt.pf ppf "%%{%c%a%s}" letter Fmt.(option int) transformers delimiter
      | `Macro_encoded_space -> Fmt.pf ppf "%%-"
      | `Macro_space -> Fmt.pf ppf "%%_"
      | `Macro_percent -> Fmt.pf ppf "%%%%" in
    Fmt.pf ppf "%a" (Fmt.list ~sep:Fmt.nop pp_macro) ms ;
    match domain with Some domain -> Fmt.pf ppf ".%s" domain | None -> ()

  let to_string = Fmt.to_to_string pp

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

  (* Copyright (c) 2016 The astring programmers *)

  let add ~empty s ~start ~stop acc =
    if start = stop
    then if not empty then acc else "" :: acc
    else String.sub s start (stop - start) :: acc

  let cuts ~empty ~sep s =
    let sep_len = String.length sep in
    if sep_len = 0 then invalid_arg "cuts: the separator is empty" ;
    let s_len = String.length s in
    let max_sep_idx = sep_len - 1 in
    let max_s_idx = s_len - sep_len in
    let rec check_sep start i k acc =
      if k > max_sep_idx
      then
        let new_start = i + sep_len in
        scan new_start new_start (add ~empty s ~start ~stop:i acc)
      else if s.[i + k] = sep.[k]
      then check_sep start i (k + 1) acc
      else scan start (i + 1) acc
    and scan start i acc =
      if i > max_s_idx
      then
        if start = 0
        then if (not empty) && s_len = 0 then [] else [ s ]
        else List.rev (add ~empty s ~start ~stop:s_len acc)
      else if s.[i] = sep.[0]
      then check_sep start i 1 acc
      else scan start (i + 1) acc in
    scan 0 0 []

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
        let vs = cuts ~sep ~empty:true str in
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
    | Error _ -> error_msgf "Invalid macro specification: %S" str
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
    *> option None
         (skip_while is_sp *> char ':' *> skip_while is_sp *> Macro.domain_spec
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
        Fmt.(option ~none:(const string "/32") (const string "/" ++ int))
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
    let cidr = Option.value ~default:128 cidr in
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

  let pp ppf ts =
    let pp_qualifier = Fmt.(option char) in
    let pp_cidr ppf = function
      | Some v4, Some v6 -> Fmt.pf ppf "/%d//%d" v4 v6
      | Some v4, None -> Fmt.pf ppf "/%d" v4
      | None, Some v6 -> Fmt.pf ppf "//%d" v6
      | None, None -> () in
    let pp ppf = function
      | `Directive (qualifier, `A (Some macro, cidr)) ->
          Fmt.pf ppf "%aa:%a%a" pp_qualifier qualifier Macro.pp macro pp_cidr
            cidr
      | `Directive (qualifier, `A (None, cidr)) ->
          Fmt.pf ppf "%aa%a" pp_qualifier qualifier pp_cidr cidr
      | `Directive (qualifier, `All) ->
          Fmt.pf ppf "%aall" pp_qualifier qualifier
      | `Directive (qualifier, `Exists v) ->
          Fmt.pf ppf "%aexists:%a" pp_qualifier qualifier Macro.pp v
      | `Directive (qualifier, `Include v) ->
          Fmt.pf ppf "%ainclude:%a" pp_qualifier qualifier Macro.pp v
      | `Directive (qualifier, `Mx (Some macro, cidr)) ->
          Fmt.pf ppf "%amx:%a%a" pp_qualifier qualifier Macro.pp macro pp_cidr
            cidr
      | `Directive (qualifier, `Mx (None, cidr)) ->
          Fmt.pf ppf "%amx%a" pp_qualifier qualifier pp_cidr cidr
      | `Directive (qualifier, `Ptr (Some macro)) ->
          Fmt.pf ppf "%aptr:%a" pp_qualifier qualifier Macro.pp macro
      | `Directive (qualifier, `Ptr None) ->
          Fmt.pf ppf "%aptr" pp_qualifier qualifier
      | `Directive (qualifier, `V4 v4) ->
          Fmt.pf ppf "%aip4:%a" pp_qualifier qualifier Ipaddr.V4.Prefix.pp v4
      | `Directive (qualifier, `V6 v6) ->
          Fmt.pf ppf "%aip6:%a" pp_qualifier qualifier Ipaddr.V6.Prefix.pp v6
      | `Explanation macro -> Fmt.pf ppf "exp=%a" Macro.pp macro
      | `Redirect macro -> Fmt.pf ppf "redirect=%a" Macro.pp macro
      | `Unknown (identifier, ms) ->
          Fmt.pf ppf "%s=%a" identifier Macro.pp (ms, None) in
    Fmt.pf ppf "v=spf1 %a" Fmt.(list ~sep:(any " ") pp) ts

  let to_string = Fmt.to_to_string pp
  let equal = ( = )

  let parse_record str =
    match Angstrom.parse_string ~consume:All record str with
    | Ok (v : t) -> Ok v
    | Error _ -> error_msgf "Invalid SPF record: %S" str
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

let a ?cidr_v4 ?cidr_v6 domain_name = A (Some domain_name, cidr_v4, cidr_v6)
let all = All
let exists domain_name = Exists domain_name
let inc domain_name = Include domain_name
(* TODO(dinosaure): currently, [mechanism] is a **result** of the macro
   expansion - the user can not specify by this way its own macro, he can
   specify only a domain-name. We must provide something else than the
   [mechanism] type which accepts macro. *)

let mx ?cidr_v4 ?cidr_v6 domain_name = Mx (Some domain_name, cidr_v4, cidr_v6)
let v4 v = V4 v
let v6 v = V6 v
let pp_cidr ppf = function None -> () | Some v -> Fmt.pf ppf "/%d" v

let pp_dual_cidr ppf = function
  | v4, (Some _ as v6) -> Fmt.pf ppf "%a/%a" pp_cidr v4 pp_cidr v6
  | v4, None -> Fmt.pf ppf "%a" pp_cidr v4

let pp_mechanism ppf = function
  | A (v, cidr_v4, cidr_v6) ->
      Fmt.pf ppf "a%a%a"
        Fmt.(option (any ":" ++ Domain_name.pp))
        v pp_dual_cidr (cidr_v4, cidr_v6)
  | All -> Fmt.string ppf "all"
  | Exists v -> Fmt.pf ppf "exists:%a" Domain_name.pp v
  | Include v -> Fmt.pf ppf "include:%a" Domain_name.pp v
  | Mx (v, cidr_v4, cidr_v6) ->
      Fmt.pf ppf "mx%a%a"
        Fmt.(option (any ":" ++ Domain_name.pp))
        v pp_dual_cidr (cidr_v4, cidr_v6)
  | Ptr (Some v) -> Fmt.pf ppf "ptr:%a" Domain_name.pp v
  | Ptr None -> ()
  | V4 ipv4 -> Fmt.pf ppf "ip4:%a" Ipaddr.V4.Prefix.pp ipv4
  | V6 ipv6 -> Fmt.pf ppf "ip6:%a" Ipaddr.V6.Prefix.pp ipv6

type qualifier = Pass | Fail | Softfail | Neutral

let pass m = (Pass, m)
let fail m = (Fail, m)
let softfail m = (Softfail, m)
let neutral m = (Neutral, m)

let pp_qualifier ppf = function
  | Pass -> ()
  | Fail -> Fmt.string ppf "-"
  | Softfail -> Fmt.string ppf "~"
  | Neutral -> Fmt.string ppf "?"

let qualifier_of_letter = function
  | Some '+' | None -> Pass
  | Some '-' -> Fail
  | Some '~' -> Softfail
  | Some '?' -> Neutral
  | _ -> invalid_arg "qualifier_of_letter"

type modifier = Explanation of string | Redirect of string

let pp_modifier ppf = function
  | Explanation v -> Fmt.pf ppf "exp=%s" v
  | Redirect v -> Fmt.pf ppf "redirect=%s" v

type record = {
  mechanisms : (qualifier * mechanism) list;
  modifiers : modifier Lazy.t list;
}

let record_equal : record -> record -> bool =
 fun a b ->
  (* TODO(dinosaure): replace [Stdlib.compare]. *)
  let ma = List.sort Stdlib.compare a.mechanisms in
  let mb = List.sort Stdlib.compare b.mechanisms in
  try List.for_all2 ( = ) ma mb with _ -> false

let concat sep lst =
  let _, lst = List.partition (( = ) "") lst in
  let len = List.fold_right (( + ) <.> String.length) lst 0 in
  match lst with
  | [] -> ""
  | [ x ] -> x
  | x :: r ->
      let sep_len = String.length sep in
      let res = Bytes.create (len + (List.length r * sep_len)) in
      Bytes.blit_string x 0 res 0 (String.length x) ;
      let pos = ref (String.length x) in
      let blit x =
        Bytes.blit_string sep 0 res !pos sep_len ;
        Bytes.blit_string x 0 res (!pos + sep_len) (String.length x) ;
        pos := !pos + sep_len + String.length x in
      List.iter blit r ;
      Bytes.unsafe_to_string res

let record_to_string { mechanisms; modifiers } =
  let mechanism_to_string (q, m) =
    Fmt.str "%a%a" pp_qualifier q pp_mechanism m in
  let modifier_to_string m = Fmt.to_to_string pp_modifier m in
  let mechanisms = List.map mechanism_to_string mechanisms in
  let modifiers = List.map (modifier_to_string <.> Lazy.force) modifiers in
  concat " " [ concat " " mechanisms; concat " " modifiers ]

let record mechanisms modifiers =
  { mechanisms; modifiers = List.map Lazy.from_val modifiers }

let pp ppf { mechanisms; modifiers } =
  Fmt.pf ppf "%a"
    Fmt.(list ~sep:(any " ") (pair ~sep:nop pp_qualifier pp_mechanism))
    mechanisms ;
  match modifiers with
  | [] -> ()
  | _ :: _ ->
      let modifiers = List.map Lazy.force modifiers in
      Fmt.pf ppf " %a" Fmt.(list ~sep:(any " ") pp_modifier) modifiers

let fold ctx acc = function
  | `Directive (qualifier, `A (macro, (cidr_v4, cidr_v6))) ->
      let macro =
        Option.map (Result.to_option <.> Macro.expand_macro ctx) macro in
      let macro = Option.join macro in
      let mechanism =
        (qualifier_of_letter qualifier, A (macro, cidr_v4, cidr_v6)) in
      { acc with mechanisms = mechanism :: acc.mechanisms }
  | `Directive (qualifier, `All) ->
      {
        acc with
        mechanisms = (qualifier_of_letter qualifier, All) :: acc.mechanisms;
      }
  | `Directive (qualifier, `Exists macro) -> (
      let qualifier = qualifier_of_letter qualifier in
      match Macro.expand_macro ctx macro with
      | Ok macro ->
          { acc with mechanisms = (qualifier, Exists macro) :: acc.mechanisms }
      | Error _ -> acc
      (* TODO *))
  | `Directive (qualifier, `Include macro) -> (
      let qualifier = qualifier_of_letter qualifier in
      match Macro.expand_macro ctx macro with
      | Ok macro ->
          { acc with mechanisms = (qualifier, Include macro) :: acc.mechanisms }
      | Error _ -> acc
      (* TODO *))
  | `Directive (qualifier, `Mx (macro, (cidr_v4, cidr_v6))) ->
      let macro =
        Option.map (Result.to_option <.> Macro.expand_macro ctx) macro in
      let macro = Option.join macro in
      let mechanism =
        (qualifier_of_letter qualifier, Mx (macro, cidr_v4, cidr_v6)) in
      { acc with mechanisms = mechanism :: acc.mechanisms }
  | `Directive (qualifier, `Ptr macro) ->
      let qualifier = qualifier_of_letter qualifier in
      let macro =
        Option.map (Result.to_option <.> Macro.expand_macro ctx) macro in
      let macro = Option.join macro in
      { acc with mechanisms = (qualifier, Ptr macro) :: acc.mechanisms }
  | `Directive (qualifier, `V4 ipv4) ->
      let qualifier = qualifier_of_letter qualifier in
      { acc with mechanisms = (qualifier, V4 ipv4) :: acc.mechanisms }
  | `Directive (qualifier, `V6 ipv6) ->
      let qualifier = qualifier_of_letter qualifier in
      { acc with mechanisms = (qualifier, V6 ipv6) :: acc.mechanisms }
  | `Explanation macro ->
      let modifier =
        Lazy.from_fun @@ fun () ->
        Explanation
          (Domain_name.to_string
          @@ failwith_error_msg (Macro.expand_macro ctx macro)) in
      { acc with modifiers = modifier :: acc.modifiers }
  | `Redirect macro ->
      let modifier =
        Lazy.from_fun @@ fun () ->
        Redirect
          (Domain_name.to_string
          @@ failwith_error_msg (Macro.expand_macro ctx macro)) in
      { acc with modifiers = modifier :: acc.modifiers }
  | `Unknown _ -> acc

let record_of_string ~ctx str =
  Term.parse_record str >>| fun terms ->
  List.fold_left (fold ctx) { mechanisms = []; modifiers = [] } terms

let rec select_spf1 = function
  | [] -> Error `None
  | x :: r when String.length x >= 6 ->
      if String.sub x 0 6 = "v=spf1" then Ok x else select_spf1 r
  | _ :: r -> select_spf1 r

type res =
  [ `None
  | `Neutral
  | `Pass of mechanism
  | `Fail
  | `Softfail
  | `Temperror
  | `Permerror ]

let pp_res ppf = function
  | `None -> Fmt.pf ppf "none"
  | `Neutral -> Fmt.pf ppf "neutral"
  | `Pass _ -> Fmt.pf ppf "pass"
  | `Fail -> Fmt.pf ppf "fail"
  | `Softfail -> Fmt.pf ppf "softfail"
  | `Temperror -> Fmt.pf ppf "temperror"
  | `Permerror -> Fmt.pf ppf "permerror"

let get :
    type dns t.
    ctx:ctx ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    (([ res | `Record of record ], [> `Msg of string ]) result, t) io =
 fun ~ctx { bind; return } dns (module DNS) ->
  let ( >>= ) = bind in
  match Map.find Map.K.domain ctx with
  | None -> return (error_msgf "Missing domain-name into the given context")
  | Some domain_name -> (
      DNS.getrrecord dns Dns.Rr_map.Txt domain_name >>= function
      | Error (`No_domain _ | `No_data _) -> return (Ok `None)
      | Error (`Msg err) ->
          Log.err (fun m -> m "Got an error while requesting DNS: %s." err) ;
          return (Ok `Temperror)
      | Ok (_, txts) ->
      match
        let ( >>= ) x f = Result.bind x f in
        select_spf1 (Dns.Rr_map.Txt_set.elements txts) >>= Term.parse_record
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
            (Ok
               (`Record
                 {
                   mechanisms = List.rev record.mechanisms;
                   modifiers = List.rev record.modifiers;
                 })))

let of_qualifier ~mechanism q match' =
  match (q, match') with
  | Pass, true -> `Pass mechanism
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
      let v4s = Ipaddr.V4.Set.elements v4s in
      let v4s = List.map (ipv4_with_cidr cidr_v4) v4s in
      return (Ok (List.map (fun v -> Ipaddr.V4 v) v4s))
  | Error _ -> (
      (* XXX(dinosaure): care about [`Msg _] and return [`Temperror]? *)
      DNS.getrrecord dns Dns.Rr_map.Aaaa mail_exchange
      >>= function
      | Ok (_, v6s) ->
          let v6s = Ipaddr.V6.Set.elements v6s in
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
    qualifier ->
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
        domain_name dual_cidr

and go :
    type t dns.
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    qualifier ->
    Ipaddr.t ->
    Dns.Mx.t list ->
    [ `raw ] Domain_name.t ->
    int option * int option ->
    ([ `Continue | res ], t) io =
 fun ~limit ({ bind; return } as state) dns (module DNS) q expected mxs
     domain_name ((cidr_v4, cidr_v6) as dual_cidr) ->
  let ( >>= ) = bind in
  if limit >= 10
  then return `Permerror
  else
    let mechanism = Mx (Some domain_name, cidr_v4, cidr_v6) in
    match mxs with
    | [] -> return `Continue
    | mx :: mxs -> (
        ipaddrs_of_mx state dns (module DNS) mx dual_cidr >>= function
        | Error `Temperror -> return `Temperror
        | Ok vs ->
        match
          of_qualifier ~mechanism q
            (List.exists (Ipaddr.Prefix.mem expected) vs)
        with
        | `Continue ->
            go ~limit:(succ limit) state dns
              (module DNS)
              q expected mxs domain_name dual_cidr
        | #res as res -> return res)

let a_mechanism :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    qualifier ->
    'a Domain_name.t ->
    int option * int option ->
    ([ `Continue | res ], t) io =
 fun ~ctx ~limit:_ { bind; return } dns (module DNS) q domain_name
     (cidr_v4, cidr_v6) ->
  let ( >>= ) = bind in
  let mechanism = A (Some domain_name, cidr_v4, cidr_v6) in
  DNS.getrrecord dns Dns.Rr_map.A domain_name >>= function
  | Ok (_, v4s) ->
      let v4s = Ipaddr.V4.Set.elements v4s in
      let v4s = List.map (ipv4_with_cidr cidr_v4) v4s in
      let expected = Map.get Map.K.ip ctx in
      return
        (of_qualifier ~mechanism q
           (List.exists
              (Ipaddr.Prefix.mem expected)
              (List.map (fun v -> Ipaddr.V4 v) v4s)))
  | Error _ -> (
      DNS.getrrecord dns Dns.Rr_map.Aaaa domain_name >>= function
      | Ok (_, v6s) ->
          let v6s = Ipaddr.V6.Set.elements v6s in
          let v6s = List.map (ipv6_with_cidr cidr_v6) v6s in
          let expected = Map.get Map.K.ip ctx in
          return
            (of_qualifier ~mechanism q
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
    qualifier ->
    'a Domain_name.t ->
    ([ `Continue | res ], t) io =
 fun ~ctx:_ ~limit:_ { bind; return } dns (module DNS) q domain_name ->
  let ( >>= ) = bind in
  let mechanism = Exists domain_name in
  DNS.getrrecord dns Dns.Rr_map.A domain_name >>= function
  | Ok _ -> return (of_qualifier ~mechanism q true)
  | Error _ -> (
      DNS.getrrecord dns Dns.Rr_map.Aaaa domain_name >>= function
      | Ok _ -> return (of_qualifier ~mechanism q true)
      | Error (`Msg _) -> return `Temperror
      | Error (`No_domain _ | `No_data _) -> return `Continue)

let rec include_mechanism :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    qualifier ->
    'a Domain_name.t ->
    ([ `Continue | res ], t) io =
 fun ~ctx ~limit ({ bind; return } as state) dns (module DNS) q domain_name ->
  let ( >>= ) = bind in
  let ctx = Map.add Map.K.domain domain_name ctx in
  DNS.getrrecord dns Dns.Rr_map.Txt domain_name >>= function
  | Error (`Msg _) -> return `Temperror
  | Error (`No_domain _ | `No_data _) -> return `Permerror
  | Ok (_, txts) ->
  match
    let ( >>= ) x f = Result.bind x f in
    select_spf1 (Dns.Rr_map.Txt_set.elements txts) >>= Term.parse_record
  with
  | Error `None | Error (`Msg _) -> return `Permerror
  | Ok terms -> (
      let record =
        List.fold_left (fold ctx) { mechanisms = []; modifiers = [] } terms
      in
      let record =
        {
          mechanisms = List.rev record.mechanisms;
          modifiers = List.rev record.modifiers;
        } in
      Log.debug (fun m -> m "Compute new record: %a." pp record) ;
      check ~ctx ~limit:(succ limit) state dns (module DNS) record >>= function
      | `Permerror | `None -> return `Permerror
      | `Temperror -> return `Temperror
      | `Pass mechanism -> return (of_qualifier ~mechanism q true)
      | `Fail | `Softfail | `Neutral -> return `Continue)

and apply :
    type t dns.
    ctx:ctx ->
    limit:int ->
    t state ->
    dns ->
    (module DNS with type t = dns and type backend = t) ->
    qualifier * mechanism ->
    ([ `Continue | res ], t) io =
 fun ~ctx ~limit ({ return; _ } as state) dns (module DNS) (q, mechanism) ->
  match mechanism with
  | All -> return (of_qualifier ~mechanism q true)
  | A (Some domain_name, cidr_ipv4, cidr_ipv6) ->
      Log.debug (fun m ->
          m "Apply A mechanism with %a." Domain_name.pp domain_name) ;
      a_mechanism ~ctx ~limit state dns
        (module DNS)
        q domain_name (cidr_ipv4, cidr_ipv6)
  | A (None, cidr_ipv4, cidr_ipv6) ->
      let domain_name = Map.get Map.K.domain ctx in
      Log.debug (fun m ->
          m "Apply A mechanism with no domain, using %a." Domain_name.pp
            domain_name) ;
      a_mechanism ~ctx ~limit state dns
        (module DNS)
        q domain_name (cidr_ipv4, cidr_ipv6)
  | Mx (Some domain_name, cidr_ipv4, cidr_ipv6) ->
      Log.debug (fun m ->
          m "Apply MX mechanism with %a." Domain_name.pp domain_name) ;
      mx_mechanism ~ctx ~limit state dns
        (module DNS)
        q domain_name (cidr_ipv4, cidr_ipv6)
  | Mx (None, cidr_ipv4, cidr_ipv6) ->
      let domain_name = Map.get Map.K.domain ctx in
      Log.debug (fun m ->
          m "Apply MX mechanism with no domain, using %a." Domain_name.pp
            domain_name) ;
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
        (of_qualifier ~mechanism q
           (Ipaddr.Prefix.mem (Map.get Map.K.ip ctx) (Ipaddr.V4 v4)))
  | V6 v6 ->
      Log.debug (fun m ->
          m "Apply IPv6 mechanism with %a." Ipaddr.V6.Prefix.pp v6) ;
      return
        (of_qualifier ~mechanism q
           (Ipaddr.Prefix.mem (Map.get Map.K.ip ctx) (Ipaddr.V6 v6)))
  | Exists domain_name ->
      exists_mechanism ~ctx ~limit state dns (module DNS) q domain_name
  | Ptr _ -> return `Continue
(* See RFC 7802, Appendix B. *)

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
    (qualifier * mechanism) list ->
    (res, t) io =
 fun ~ctx ~limit ({ bind; return } as state) dns (module DNS) -> function
  | [] -> return `Neutral
  | (q, mechanism) :: r when limit < 10 -> (
      let ( >>= ) = bind in
      apply ~ctx ~limit state dns (module DNS) (q, mechanism) >>= function
      | `Continue -> go ~ctx ~limit state dns (module DNS) r
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

module Encoder = struct
  open Prettym

  let res ppf = function
    | `None -> string ppf "none"
    | `Neutral -> string ppf "neutral"
    | `Pass _ -> string ppf "pass"
    | `Fail -> string ppf "fail"
    | `Softfail -> string ppf "softfail"
    | `Temperror -> string ppf "temperror"
    | `Permerror -> string ppf "permerror"

  let to_safe_string pp v = Fmt.str "%a" pp v (* TODO *)

  let kv :
      type a v. name:string -> a Map.key -> ?pp:a Fmt.t -> Map.t -> (v, v) fmt =
   fun ~name key ?pp ctx ->
    match Map.find key ctx with
    | None -> [ cut ]
    | Some v ->
        let pp =
          match pp with Some pp -> pp | None -> (Map.Key.info key).Map.pp in
        [
          spaces 1;
          string $ name;
          cut;
          char $ '=';
          cut;
          string $ to_safe_string pp v;
          cut;
          char $ ';';
        ]

  let ( ^^ ) a b = concat a b

  let pp_identity ppf = function
    | `MAILFROM _ -> Fmt.string ppf "mailfrom"
    | `HELO _ -> Fmt.string ppf "helo"

  let sender ppf v = eval ppf [ !!string ] (Fmt.to_to_string Map.pp_path v)

  let domain_name ppf = function
    | `Addr (Emile.IPv4 v) -> eval ppf [ !!string ] (Ipaddr.V4.to_string v)
    | `Addr (Emile.IPv6 v) -> eval ppf [ !!string ] (Ipaddr.V6.to_string v)
    | `Addr (Emile.Ext (k, v)) ->
        eval ppf [ char $ '['; !!string; char $ ':'; !!string; char $ ']' ] k v
    | `Domain vs ->
        let sep = ((fun ppf () -> string ppf "."), ()) in
        eval ppf [ !!(list ~sep string) ] vs
    | `Literal v -> eval ppf [ char $ '['; !!string; char $ ']' ] v

  let ipaddr ppf v = eval ppf [ !!string ] (Ipaddr.to_string v)

  let comment ~ctx ?receiver ppf (v : res) =
    match (receiver, Map.get Map.K.sender ctx, v) with
    | None, _, _ -> ppf
    | Some receiver, `MAILFROM p, `Pass _ ->
        eval ppf
          [
            spaces 1;
            char $ '(';
            !!domain_name;
            char $ ':';
            spaces 1;
            string $ "domain";
            spaces 1;
            string $ "of";
            spaces 1;
            !!sender;
            spaces 1;
            string $ "designates";
            spaces 1;
            !!ipaddr;
            spaces 1;
            string $ "as";
            spaces 1;
            string $ "permitted";
            spaces 1;
            string $ "sender";
            char $ ')';
          ]
          receiver p (Map.get Map.K.ip ctx)
    | Some receiver, `MAILFROM p, _ ->
        eval ppf
          [
            spaces 1;
            char $ '(';
            !!domain_name;
            char $ ':';
            spaces 1;
            string $ "domain";
            spaces 1;
            string $ "of";
            spaces 1;
            !!sender;
            spaces 1;
            string $ "does";
            spaces 1;
            string $ "not";
            spaces 1;
            string $ "designates";
            spaces 1;
            !!ipaddr;
            spaces 1;
            string $ "as";
            spaces 1;
            string $ "permitted";
            spaces 1;
            string $ "sender";
            char $ ')';
          ]
          receiver p (Map.get Map.K.ip ctx)
    | Some _, `HELO _, _ -> ppf

  let field ~ctx ?receiver ppf v =
    eval ppf
      ([ tbox 1; !!res; !!(comment ~ctx ?receiver) ]
      ^^ kv ~name:"client-ip" Map.K.ip ~pp:Ipaddr.pp ctx
      ^^ kv ~name:"envelope-from" Map.K.sender ctx
      ^^ kv ~name:"helo" Map.K.helo ctx
      ^^ kv ~name:"identity" Map.K.sender ~pp:pp_identity ctx
      ^^ (match receiver with
         | None -> [ cut ]
         | Some receiver ->
             [
               spaces 1;
               string $ "receiver";
               cut;
               char $ '=';
               cut;
               string $ Fmt.to_to_string Emile.pp_domain receiver;
               cut;
               char $ ';';
             ])
      ^^ (match v with
         | `Pass m ->
             [
               spaces 1;
               string $ "mechanism";
               cut;
               char $ '=';
               cut;
               string $ to_safe_string pp_mechanism m;
               cut;
               char $ ';';
             ]
         | _ -> [ cut ])
      ^^ [ close; new_line ])
      v v
end

let field_received_spf = Mrmime.Field_name.v "Received-SPF"

type newline = LF | CRLF

let to_field :
    ctx:ctx ->
    ?receiver:Emile.domain ->
    res ->
    Mrmime.Field_name.t * Unstrctrd.t =
 fun ~ctx ?receiver res ->
  let v = Prettym.to_string (Encoder.field ~ctx ?receiver) res in
  let _, v =
    match Unstrctrd.of_string v with Ok v -> v | Error _ -> assert false in
  (field_received_spf, v)

module Decoder = struct
  open Angstrom

  let is_alpha = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
  let is_space = ( = ) ' '

  let result =
    choice
      [
        (string "pass" >>| fun _ -> `Pass);
        (string "fail" >>| fun _ -> `Fail);
        (string "softfail" >>| fun _ -> `Softfail);
        (string "neutral" >>| fun _ -> `Neutral);
        (string "none" >>| fun _ -> `None);
        (string "temperror" >>| fun _ -> `Temperror);
        (string "permerror" >>| fun _ -> `Permerror);
      ]

  let key =
    let name =
      satisfy is_alpha >>= fun x ->
      (take_while @@ function
       | 'a' .. 'z' | 'A' .. 'Z' -> true
       | '0' .. '9' -> true
       | '-' | '_' | '.' -> true
       | _ -> false)
      >>= fun r -> return (String.make 1 x ^ r) in
    choice
      [
        string "client-ip";
        string "envelope-from";
        string "helo";
        string "problem";
        string "receiver";
        string "identity";
        string "mechanism";
        name;
      ]

  (* XXX(dinosaure): [dot-atom] and [quoted-string] are specified by RFC 5322
   * and they are implemented by [emile]/[mrmime]. We took them in this case. *)

  let dot_atom = Emile.Parser.dot_atom >>| String.concat "."
  let quoted_string = Emile.Parser.quoted_string

  let key_value_pair =
    key >>= fun key ->
    skip_while is_space
    *> char '='
    *> skip_while is_space
    *> (dot_atom <|> quoted_string)
    >>= fun value -> return (key, value)

  let key_value_list =
    key_value_pair >>= fun x ->
    many
      (skip_while is_space *> char ';' *> skip_while is_space *> key_value_pair)
    >>= fun r ->
    option () (skip_while is_space *> char ';' *> skip_while is_space)
    >>= fun () -> return (x :: r)

  let ipaddr =
    Term.ip6_network
    >>| (fun v -> Ipaddr.V6 v)
    <|> ( Term.ip4_network >>| fun (a, b, c, d) ->
          Ipaddr.V4 (Ipaddr.V4.of_string_exn (Fmt.str "%s.%s.%s.%s" a b c d)) )

  (* XXX(dinosaure): this part is not specified but it can give to us informations such as 
   * the domain of the [receiver], the mailbox of the [sender] (it should be the same as [From])
   * and the IP address of the [sender]. *)

  let good_comment =
    char '(' *> skip_while is_space *> Emile.Parser.domain >>= fun receiver ->
    skip_while is_space
    *> char ':'
    *> skip_while is_space
    *> string "domain"
    *> skip_while is_space
    *> string "of"
    *> skip_while is_space
    *> Emile.Parser.addr_spec
    >>= fun sender ->
    skip_while is_space *> string "designates" *> skip_while is_space *> ipaddr
    >>= fun ip ->
    skip_while is_space
    *> string "as"
    *> skip_while is_space
    *> string "permitted"
    *> skip_while is_space
    *> string "sender"
    *> skip_while is_space
    *> char ')'
    *> return (receiver, sender, ip)

  let bad_comment =
    char '(' *> skip_while is_space *> Emile.Parser.domain >>= fun receiver ->
    skip_while is_space
    *> char ':'
    *> skip_while is_space
    *> string "domain"
    *> skip_while is_space
    *> string "of"
    *> skip_while is_space
    *> Emile.Parser.addr_spec
    >>= fun sender ->
    skip_while is_space
    *> string "does"
    *> skip_while is_space
    *> string "not"
    *> skip_while is_space
    *> string "designate"
    *> skip_while is_space
    *> ipaddr
    >>= fun ip ->
    skip_while is_space
    *> string "as"
    *> skip_while is_space
    *> string "permitted"
    *> skip_while is_space
    *> string "sender"
    *> skip_while is_space
    *> char ')'
    *> return (receiver, sender, ip)

  let comment = good_comment <|> bad_comment

  let header_field =
    skip_while is_space *> result >>= fun res ->
    option None (skip_while is_space *> comment >>| Option.some)
    >>= fun comment ->
    option [] (skip_while is_space *> key_value_list) >>= fun kvs ->
    return (res, comment, kvs)

  let parse_received_spf_field_value unstrctrd =
    let str = Unstrctrd.(to_utf_8_string (fold_fws unstrctrd)) in
    match Angstrom.parse_string ~consume:Prefix header_field str with
    | Ok v -> Ok v
    | Error _ -> error_msgf "Invalid Received-SPF value: %S" str
end

let sub_string_and_replace_newline chunk len =
  let count = ref 0 in
  String.iter
    (function '\n' -> incr count | _ -> ())
    (Bytes.sub_string chunk 0 len) ;
  let plus = !count in
  let pos = ref 0 in
  let res = Bytes.create (len + plus) in
  for i = 0 to len - 1 do
    match Bytes.unsafe_get chunk i with
    | '\n' ->
        Bytes.unsafe_set res !pos '\r' ;
        Bytes.unsafe_set res (!pos + 1) '\n' ;
        pos := !pos + 2
    | chr ->
        Bytes.unsafe_set res !pos chr ;
        incr pos
  done ;
  Bytes.unsafe_to_string res

let sanitize_input newline chunk len =
  match newline with
  | CRLF -> Bytes.sub_string chunk 0 len
  | LF -> sub_string_and_replace_newline chunk len

type extracted = spf list

and spf = {
  result :
    [ `None | `Neutral | `Pass | `Fail | `Softfail | `Temperror | `Permerror ];
  receiver : Emile.domain option;
  sender : Emile.mailbox option;
  ip : Ipaddr.t option;
  ctx : ctx;
}

let pp_result ppf = function
  | `None -> Fmt.string ppf "none"
  | `Neutral -> Fmt.string ppf "neutral"
  | `Pass -> Fmt.string ppf "pass"
  | `Fail -> Fmt.string ppf "fail"
  | `Softfail -> Fmt.string ppf "softfail"
  | `Temperror -> Fmt.string ppf "temperror"
  | `Permerror -> Fmt.string ppf "permerror"

let pp_spf ppf spf =
  Fmt.pf ppf
    "{ @[<hov>result= %a;@ receiver= @[<hov>%a@];@ sender= @[<hov>%a@];@ ip= \
     @[<hov>%a@];@ ctx= #ctx;@] }"
    pp_result spf.result
    Fmt.(Dump.option Emile.pp_domain)
    spf.receiver
    Fmt.(Dump.option Emile.pp_mailbox)
    spf.sender
    Fmt.(Dump.option Ipaddr.pp)
    spf.ip

let to_unstrctrd unstructured =
  let fold acc = function #Unstrctrd.elt as elt -> elt :: acc | _ -> acc in
  let unstrctrd = List.fold_left fold [] unstructured in
  match Unstrctrd.of_list (List.rev unstrctrd) with
  | Ok v -> v
  | _ -> assert false

let ctx_of_kvs kvs =
  let identity =
    match List.assoc_opt "identity" kvs with
    | Some "mailfrom" -> Some `MAILFROM
    | Some "helo" -> Some `HELO
    | _ -> None in
  let receiver =
    Option.bind
      (List.assoc_opt "receiver" kvs)
      (Result.to_option <.> Colombe.Domain.of_string) in
  let fold ctx = function
    | "client-ip", v -> (
        match Ipaddr.of_string v with
        | Ok (Ipaddr.V4 _ as v) ->
            Map.add Map.K.ip v (Map.add Map.K.v `In_addr ctx)
        | Ok (Ipaddr.V6 _ as v) -> Map.add Map.K.ip v (Map.add Map.K.v `Ip6 ctx)
        | _ -> ctx)
    | "helo", v -> (
        match (Colombe.Domain.of_string v, Domain_name.of_string v) with
        | Ok v0, Ok v1 -> Map.add Map.K.helo v0 (Map.add Map.K.domain v1 ctx)
        | Ok v, Error _ -> Map.add Map.K.helo v ctx
        | Error _, Ok v -> Map.add Map.K.domain v ctx
        | _ -> ctx)
    | "envelope-from", v -> (
        match (Colombe.Path.of_string (Fmt.str "<%s>" v), identity) with
        | Ok { Colombe.Path.local; domain; _ }, Some `HELO ->
            let ctx = Map.add Map.K.local local ctx in
            let ctx = Map.add Map.K.domain_of_sender domain ctx in
            let ctx =
              match (domain, Map.find Map.K.domain ctx) with
              | Colombe.Domain.Domain vs, None ->
                  let v = Domain_name.of_strings_exn vs in
                  Map.add Map.K.sender (`HELO v) ctx |> Map.add Map.K.domain v
              | Colombe.Domain.Domain vs, Some _ ->
                  let v = Domain_name.of_strings_exn vs in
                  Map.add Map.K.sender (`HELO v) ctx
              | _ -> ctx in
            ctx
        | Ok ({ Colombe.Path.local; domain; _ } as v), (Some `MAILFROM | None)
          ->
            let ctx = Map.add Map.K.sender (`MAILFROM v) ctx in
            let ctx = Map.add Map.K.local local ctx in
            let ctx = Map.add Map.K.domain_of_sender domain ctx in
            let ctx =
              match (domain, Map.find Map.K.domain ctx) with
              | Colombe.Domain.Domain vs, None ->
                  Map.add Map.K.domain (Domain_name.of_strings_exn vs) ctx
              | _ -> ctx in
            ctx
        | Error _, _ -> ctx)
    | _ -> ctx in
  (identity, receiver, List.fold_left fold Map.empty kvs)

let colombe_domain_to_emile_domain = function
  | Colombe.Domain.IPv4 v -> `Addr (Emile.IPv4 v)
  | Colombe.Domain.IPv6 v -> `Addr (Emile.IPv6 v)
  | Colombe.Domain.Domain vs -> `Domain vs
  | Colombe.Domain.Extension (k, v) -> `Addr (Emile.Ext (k, v))

let to_mailbox { Colombe.Path.local; domain; _ } =
  let local =
    match local with
    | `Dot_string vs -> List.map (fun v -> `Atom v) vs
    | `String v -> [ `String v ] in
  let domain = colombe_domain_to_emile_domain domain in
  { Emile.local; domain = (domain, []); name = None }

let to_spf = function
  | result, Some ((receiver' : Emile.domain), sender', ip'), kvs ->
      let p' = failwith_error_msg (Colombe_emile.to_path sender') in
      let identity, receiver, ctx = ctx_of_kvs kvs in
      let receiver =
        Option.value ~default:receiver'
          (Option.map colombe_domain_to_emile_domain receiver) in
      let ctx, ip =
        match Map.find Map.K.ip ctx with
        | Some ip -> (ctx, ip)
        | None -> (Map.add Map.K.ip ip' ctx, ip') in
      let ctx, sender =
        match (Map.find Map.K.sender ctx, identity) with
        | Some (`HELO _), Some `MAILFROM ->
            assert false (* XXX(dinosaure): I'm correct? *)
        | Some (`HELO _), (Some `HELO | None) -> (ctx, to_mailbox p')
        | Some (`MAILFROM p), _ -> (ctx, to_mailbox p)
        | None, (Some `MAILFROM | None) ->
            let { Colombe.Path.local; domain; _ } = p' in
            let ctx = Map.add Map.K.sender (`MAILFROM p') ctx in
            let ctx = Map.add Map.K.local local ctx in
            let ctx = Map.add Map.K.domain_of_sender domain ctx in
            let ctx =
              match (domain, Map.find Map.K.domain ctx) with
              | Colombe.Domain.Domain vs, None ->
                  Map.add Map.K.domain (Domain_name.of_strings_exn vs) ctx
              | _ -> ctx in
            (ctx, to_mailbox p')
        | None, Some `HELO ->
            let { Colombe.Path.local; domain; _ } = p' in
            let ctx = Map.add Map.K.local local ctx in
            let ctx = Map.add Map.K.domain_of_sender domain ctx in
            let ctx =
              match (domain, Map.find Map.K.domain ctx) with
              | Colombe.Domain.Domain vs, None ->
                  let v = Domain_name.of_strings_exn vs in
                  Map.add Map.K.sender (`HELO v) ctx |> Map.add Map.K.domain v
              | Colombe.Domain.Domain vs, Some _ ->
                  let v = Domain_name.of_strings_exn vs in
                  Map.add Map.K.sender (`HELO v) ctx
              | _ -> ctx in
            (ctx, to_mailbox p') in
      {
        result;
        receiver = Some receiver;
        sender = Some sender;
        ip = Some ip;
        ctx;
      }
  | result, None, kvs ->
      let _identity, receiver, ctx = ctx_of_kvs kvs in
      let receiver = Option.map colombe_domain_to_emile_domain receiver in
      let sender =
        match Map.find Map.K.sender ctx with
        | Some (`MAILFROM p) -> Some (to_mailbox p)
        | _ -> None in
      let ip = Map.find Map.K.ip ctx in
      { result; receiver; sender; ip; ctx }

let p =
  let open Mrmime in
  let unstructured = Field.(Witness Unstructured) in
  let open Field_name in
  Map.empty
  |> Map.add date unstructured
  |> Map.add from unstructured
  |> Map.add sender unstructured
  |> Map.add reply_to unstructured
  |> Map.add (v "To") unstructured
  |> Map.add cc unstructured
  |> Map.add bcc unstructured
  |> Map.add subject unstructured
  |> Map.add message_id unstructured
  |> Map.add comments unstructured
  |> Map.add content_type unstructured
  |> Map.add content_encoding unstructured

let parse_received_spf_field_value unstrctrd =
  match Decoder.parse_received_spf_field_value unstrctrd with
  | Ok v -> Ok (to_spf v)
  | Error _ as err -> err

let extract_received_spf :
    type flow t.
    ?newline:newline ->
    flow ->
    t state ->
    (module FLOW with type flow = flow and type backend = t) ->
    ((extracted, [> `Msg of string ]) result, t) io =
 fun ?(newline = LF) flow { bind; return } (module Flow) ->
  let open Mrmime in
  let ( >>= ) = bind in
  let chunk = 0x1000 in
  let raw = Bytes.create chunk in
  let decoder = Hd.decoder p in
  let rec go acc =
    match Hd.decode decoder with
    | `Field field -> (
        let (Field.Field (field_name, w, v)) = Location.prj field in
        match (Field_name.equal field_name field_received_spf, w) with
        | true, Field.Unstructured -> (
            let v = to_unstrctrd v in
            match parse_received_spf_field_value v with
            | Ok v -> go (v :: acc)
            | Error (`Msg err) ->
                Log.warn (fun m -> m "Ignore Received-SPF value: %s." err) ;
                go acc)
        | _ -> go acc)
    | `Malformed _err ->
        Log.err (fun m -> m "The given email is malformed.") ;
        return (error_msg "Invalid email")
    | `End _rest -> return (Ok (List.rev acc))
    | `Await ->
        Flow.input flow raw 0 (Bytes.length raw) >>= fun len ->
        let raw = sanitize_input newline raw len in
        Hd.src decoder raw 0 (String.length raw) ;
        go acc in
  go []
