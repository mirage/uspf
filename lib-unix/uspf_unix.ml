let eval : type a. dns:Dns_client_unix.t -> a Uspf.t -> Uspf.Result.t option =
 fun ~dns t ->
  let exception Result of Uspf.Result.t in
  let rec go : type a. a Uspf.t -> a = function
    | Request (domain_name, record) ->
        Dns_client_unix.get_resource_record dns record domain_name
    | Return v -> v
    | Terminate result -> raise (Result result)
    | Bind (x, fn) -> go (fn (go x))
    | Choose_on
        { none; neutral; pass; fail; softfail; temperror; permerror; fn } ->
    match go (fn ()) with
    | v -> v
    | exception Result result ->
        let reraise _ = raise (Result result) in
        let fn =
          match result with
          | `None -> Option.fold ~none:reraise ~some:Fun.id none
          | `Neutral -> Option.fold ~none:reraise ~some:Fun.id neutral
          | `Fail -> Option.fold ~none:reraise ~some:Fun.id fail
          | `Softfail -> Option.fold ~none:reraise ~some:Fun.id softfail
          | `Temperror -> Option.fold ~none:reraise ~some:Fun.id temperror
          | `Permerror -> Option.fold ~none:reraise ~some:Fun.id permerror
          | `Pass m ->
              let fn () =
                match pass with Some pass -> pass m | None -> reraise () in
              fn in
        go (fn ()) in
  match go t with exception Result result -> Some result | _ -> None

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
