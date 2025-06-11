(** LWT layer of uSPF.

    uSPF is a standalone library which requires some specialisations like the
    {i scheduler} and the DNS stack used to get DNS record. This module
    specialises specifically the scheduler with LWT. It lets the user to choose
    the DNS implementation {i via} a first-class module (see
    {!module-type:DNS}).

    {b NOTE}: This layer is currently incomplete and it uses mostly use to get
    the DNS record and do some verifications on it - but it does not actually do
    (and can not) the SPF check (as the Unix layer, see {!module:Uspf_unix}). *)

val get_and_check : Dns_client_lwt.t -> Uspf.ctx -> Uspf.Result.t option Lwt.t

val get :
     Dns_client_lwt.t
  -> Uspf.ctx
  -> (Uspf.Term.t, [> `Msg of string ]) result Lwt.t
