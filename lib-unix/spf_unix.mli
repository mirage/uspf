val check :
  ?nameserver:[ `TCP | `UDP ] * (Ipaddr.t * int) ->
  timeout:int64 ->
  Spf.ctx ->
  (Spf.res, [> `Msg of string ]) result
