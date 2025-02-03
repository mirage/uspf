module Make (Dns_client : Dns_client_mirage.S) : sig
  val get_and_check : Dns_client.t -> Uspf.ctx -> Uspf.Result.t option Lwt.t
end
