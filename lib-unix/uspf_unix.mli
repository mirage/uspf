(** Unix layer of uSPF.

    uSPF is a standalone library which requires some specialisations like the
    {i scheduler} and the DNS stack used to get DNS records. This module
    specialises uSPF with the module [Unix] and [ocaml-dns]. {!val:check} does
    the SPF verification from a given {!type:Uspf.ctx} and
    {!val:extract_received_spf} extracts the [Received-SPF] field from a given
    email.

    For more details about uSPF and how to use it, please take a look on
    {!module:Uspf}. *)

val get_and_check : Dns_client_unix.t -> Uspf.ctx -> Uspf.Result.t option

val extract_received_spf :
     ?newline:[ `LF | `CRLF ]
  -> in_channel
  -> (Uspf.Extract.field list, [> `Msg of string ]) result
