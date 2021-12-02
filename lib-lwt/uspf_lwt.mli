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
