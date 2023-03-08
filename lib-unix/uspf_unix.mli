(** Unix layer of uSPF.

    uSPF is a standalone library which requires some specialisations like the
    {i scheduler} and the DNS stack used to get DNS records. This module
    specialises uSPF with the module [Unix] and [ocaml-dns]. {!val:check} does
    the SPF verification from a given {!type:Uspf.ctx} and
    {!val:extract_received_spf} extracts the [Received-SPF] field from a given
    email.

    For more details about uSPF and how to use it, please take a look on
    {!module:Uspf}. *)

val check :
  ?nameservers:Dns.proto * (Ipaddr.t * int) list ->
  timeout:int64 ->
  Uspf.ctx ->
  (Uspf.res, [> `Msg of string ]) result

val extract_received_spf :
  ?newline:Uspf.newline ->
  in_channel ->
  (Uspf.extracted, [> `Msg of string ]) result
