type 'a tag = { name : string; pp : 'a Fmt.t }

module Info = struct
  type 'a t = 'a tag = { name : string; pp : 'a Fmt.t }
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
    Key.create { name = "<ip>"; pp }

  let domain : [ `raw ] Domain_name.t key =
    Key.create { name = "<domain>"; pp = Domain_name.pp }

  let sender :
      [ `HELO of [ `raw ] Domain_name.t | `MAILFROM of Colombe.Path.t ] key =
    let pp ppf = function
      | `HELO v -> Domain_name.pp ppf v
      | `MAILFROM v -> pp_path ppf v in
    Key.create { name = "<sender>"; pp }

  let local : [ `String of string | `Dot_string of string list ] key =
    let pp ppf = function
      | `String v -> Fmt.string ppf v
      | `Dot_string vs -> Fmt.(list ~sep:(const string ".") string) ppf vs in
    Key.create { name = "local-part"; pp }

  let domain_of_sender : Colombe.Domain.t key =
    Key.create { name = "domain-of-sender"; pp = Colombe.Domain.pp }

  let v : [ `In_addr | `Ip6 ] key =
    let pp ppf = function
      | `In_addr -> Fmt.string ppf "in-addr"
      | `Ip6 -> Fmt.string ppf "ip6" in
    Key.create { name = "v"; pp }

  let helo : Colombe.Domain.t key =
    Key.create { name = "helo"; pp = Colombe.Domain.pp }
end
