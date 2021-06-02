module type FUNCTOR = sig
  type 'a t
end

type (+'a, 't) io

type 't state = {
  bind : 'a 'b. ('a, 't) io -> ('a -> ('b, 't) io) -> ('b, 't) io;
  return : 'a. 'a -> ('a, 't) io;
}

module type X = sig
  type 'a s

  type t

  external inj : 'a s -> ('a, t) io = "%identity"

  external prj : ('a, t) io -> 'a s = "%identity"
end

module Common = struct
  type t

  external inj : 'a -> 'b = "%identity"

  external prj : 'a -> 'b = "%identity"
end

module Make (T : FUNCTOR) = struct
  type 'a s = 'a T.t

  include Common
end

module type FLOW = sig
  type backend

  type flow

  val input : flow -> bytes -> int -> int -> (int, backend) io
end

module type DNS = sig
  type backend

  type t

  type error =
    [ `Msg of string
    | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
    | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

  val getrrecord :
    t ->
    'v Dns.Rr_map.rr ->
    'a Domain_name.t ->
    (('v, [> error ]) result, backend) io
end
