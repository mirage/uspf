val check :
  ?nameservers:Dns.proto * (Ipaddr.t * int) list ->
  timeout:int64 ->
  Spf.ctx ->
  (Spf.res, [> `Msg of string ]) result

val extract_received_spf :
  ?newline:Spf.newline ->
  in_channel ->
  (Spf.extracted, [> `Msg of string ]) result
