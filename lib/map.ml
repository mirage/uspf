type 'a tag = { name: string; pp: 'a Fmt.t; equal: 'a -> 'a -> bool }

module Info = struct
  type 'a t = 'a tag = { name: string; pp: 'a Fmt.t; equal: 'a -> 'a -> bool }
end

include Hmap.Make (Info)

let pp_local ppf = function
  | `String x -> Fmt.(quote string) ppf x
  | `Dot_string l -> Fmt.(list ~sep:(const string ".") string) ppf l

let pp_path ppf { Colombe.Path.local; domain; _ } =
  Fmt.pf ppf "%a@%a" pp_local local Colombe.Domain.pp domain
(* XXX(dinosaure): SPF does not follow RFC 5321 when we want to print
 * a path. It shows the path a simple mailbox. *)

module K = struct
  let ip : Ipaddr.t key =
    let equal a b = Ipaddr.compare a b = 0 in
    let pp ppf = function
      | Ipaddr.V4 _ as v -> Ipaddr.pp ppf v
      | Ipaddr.V6 v6 ->
          let a, b, c, d, e, f, g, h = Ipaddr.V6.to_int16 v6 in
          Fmt.pf ppf
            "%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x.%x"
            (a lsr 12)
            ((a lsr 8) land 0xf)
            ((a lsr 4) land 0xf)
            (a land 0xf) (b lsr 12)
            ((b lsr 8) land 0xf)
            ((b lsr 4) land 0xf)
            (b land 0xf) (c lsr 12)
            ((c lsr 8) land 0xf)
            ((c lsr 4) land 0xf)
            (c land 0xf) (d lsr 12)
            ((d lsr 8) land 0xf)
            ((d lsr 4) land 0xf)
            (d land 0xf) (e lsr 12)
            ((e lsr 8) land 0xf)
            ((e lsr 4) land 0xf)
            (e land 0xf) (f lsr 12)
            ((f lsr 8) land 0xf)
            ((f lsr 4) land 0xf)
            (f land 0xf) (g lsr 12)
            ((g lsr 8) land 0xf)
            ((g lsr 4) land 0xf)
            (g land 0xf) (h lsr 12)
            ((h lsr 8) land 0xf)
            ((h lsr 4) land 0xf)
            (h land 0xf) in
    Key.create { name= "<ip>"; pp; equal }

  let domain : [ `raw ] Domain_name.t key =
    let pp = Domain_name.pp in
    let equal = Domain_name.equal in
    Key.create { name= "<domain>"; pp; equal }

  let sender :
      [ `HELO of [ `raw ] Domain_name.t | `MAILFROM of Colombe.Path.t ] key =
    let pp ppf = function
      | `HELO v -> Domain_name.pp ppf v
      | `MAILFROM v -> pp_path ppf v in
    let equal a b =
      match (a, b) with
      | `HELO a, `HELO b -> Domain_name.equal a b
      | `MAILFROM a, `MAILFROM b -> Colombe.Path.equal a b
      | _ -> false in
    Key.create { name= "<sender>"; pp; equal }

  let local : [ `String of string | `Dot_string of string list ] key =
    let pp ppf = function
      | `String v -> Fmt.string ppf v
      | `Dot_string vs -> Fmt.(list ~sep:(const string ".") string) ppf vs in
    let equal a b =
      match (a, b) with
      | `String a, `String b -> String.equal a b
      | `Dot_string a, `Dot_string b -> begin
          try List.for_all2 String.equal a b with _ -> false
        end
      | _ -> false in
    Key.create { name= "local-part"; pp; equal }

  let domain_of_sender : Colombe.Domain.t key =
    let pp = Colombe.Domain.pp in
    let equal = Colombe.Domain.equal in
    Key.create { name= "domain-of-sender"; pp; equal }

  let v : [ `In_addr | `Ip6 ] key =
    let pp ppf = function
      | `In_addr -> Fmt.string ppf "in-addr"
      | `Ip6 -> Fmt.string ppf "ip6" in
    let equal a b =
      match (a, b) with
      | `In_addr, `In_addr -> true
      | `Ip6, `Ip6 -> true
      | _ -> false in
    Key.create { name= "v"; pp; equal }

  let helo : Colombe.Domain.t key =
    let pp = Colombe.Domain.pp in
    let equal = Colombe.Domain.equal in
    Key.create { name= "helo"; pp; equal }

  let origin : [ `HELO | `MAILFROM ] key =
    let pp ppf = function
      | `HELO -> Fmt.string ppf "HELO"
      | `MAILFROM -> Fmt.string ppf "MAILFROM" in
    let equal a b =
      match (a, b) with
      | `HELO, `HELO -> true
      | `MAILFROM, `MAILFROM -> true
      | _ -> false in
    Key.create { name= "origin"; pp; equal }
end
