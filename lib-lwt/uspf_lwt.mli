(** LWT layer of uSPF.

    uSPF is a standalone library which requires some specialisations like the
    {i scheduler} and the DNS stack used to get DNS record. This module
    specialises specifically the scheduler with LWT. It lets the user to choose
    the DNS implementation {i via} a first-class module (see
    {!module-type:DNS}).

    {b NOTE}: This layer is currently incomplete and it uses mostly use to get
    the DNS record and do some verifications on it - but it does not actually do
    (and can not) the SPF check (as the Unix layer, see {!module:Uspf_unix}). *)

module type DNS = sig
  type t

  type error =
    [ `Msg of string
    | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
    | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

  val getrrecord :
    t -> 'r Dns.Rr_map.key -> _ Domain_name.t -> ('r, [> error ]) result Lwt.t
end

val get :
  domain:_ Domain_name.t ->
  'dns ->
  (module DNS with type t = 'dns) ->
  (Uspf.Term.t, [> `Not_found | `Invalid_SPF_record ]) result Lwt.t
