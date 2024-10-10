module Make (Dns_client : Dns_client_mirage.S) : sig
  val get :
    ctx:Uspf.ctx ->
    Dns_client.t ->
    ([ Uspf.res | `Record of Uspf.record ], [> `Msg of string ]) result Lwt.t

  val check :
    ctx:Uspf.ctx ->
    Dns_client.t ->
    [ Uspf.res | `Record of Uspf.record ] ->
    Uspf.res Lwt.t
end
