let eval : type a. dns:Dns_client_unix.t -> a Uspf.t -> Uspf.Result.t option =
 fun ~dns t ->
  let rec go : type a. a Uspf.t -> a = function
    | Request (domain_name, record, fn) ->
        let resp = Dns_client_unix.get_resource_record dns record domain_name in
        go (fn resp)
    | Return v -> v
    | Tries fns -> List.iter (fun fn -> go (fn ())) fns
    | Map (x, fn) -> fn (go x)
    | Choose_on c ->
    try go (c.fn ())
    with Uspf.Result result ->
      let none _ = Uspf.terminate result in
      let some = Fun.id in
      let fn =
        match result with
        | `None -> Option.fold ~none ~some c.none
        | `Neutral -> Option.fold ~none ~some c.neutral
        | `Fail -> Option.fold ~none ~some c.fail
        | `Softfail -> Option.fold ~none ~some c.softfail
        | `Temperror -> Option.fold ~none ~some c.temperror
        | `Permerror -> Option.fold ~none ~some c.permerror
        | `Pass m -> begin
            fun () -> match c.pass with Some pass -> pass m | None -> none ()
          end in
      go (fn ()) in
  match go t with exception Uspf.Result result -> Some result | _ -> None

let get_and_check dns ctx = eval ~dns (Uspf.get_and_check ctx)

let extract_received_spf ?(newline = `LF) ic =
  let buf = Bytes.create 0x7ff in
  let rec go extract =
    match Uspf.Extract.extract extract with
    | `Fields fields -> Ok fields
    | `Malformed _ -> Error (`Msg "Invalid email")
    | `Await extract ->
    match input ic buf 0 (Bytes.length buf) with
    | 0 -> go (Uspf.Extract.src extract "" 0 0)
    | len when newline = `CRLF ->
        go (Uspf.Extract.src extract (Bytes.sub_string buf 0 len) 0 len)
    | len ->
        let str = Bytes.sub_string buf 0 len in
        let str = String.split_on_char '\n' str in
        let str = String.concat "\r\n" str in
        let len = String.length str in
        go (Uspf.Extract.src extract str 0 len) in
  go (Uspf.Extract.extractor ())
