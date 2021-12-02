val check :
  ?nameservers:Dns.proto * (Ipaddr.t * int) list ->
  timeout:int64 ->
  Uspf.ctx ->
  (Uspf.res, [> `Msg of string ]) result

val extract_received_spf :
  ?newline:Uspf.newline ->
  in_channel ->
  (Uspf.extracted, [> `Msg of string ]) result
