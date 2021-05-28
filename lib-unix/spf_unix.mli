val get_records :
  Spf.ctx ->
  ([ `None | `Permerror | `Record of Spf.record ], [> `Msg of string ]) result
