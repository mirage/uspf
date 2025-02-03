open Lwt.Infix

external reraise : exn -> 'a = "%reraise"

module Make (Dns_client : Dns_client_mirage.S) = struct
  let eval : type a. dns:Dns_client.t -> a Uspf.t -> Uspf.Result.t option Lwt.t
      =
   fun ~dns t ->
    let exception Result of Uspf.Result.t in
    let rec go : type a. a Uspf.t -> a Lwt.t = function
      | Request (domain_name, record) ->
          Dns_client.get_resource_record dns record domain_name
      | Return v -> Lwt.return v
      | Terminate result -> raise (Result result)
      | Bind (x, fn) -> go x >>= fun x -> go (fn x)
      | Choose_on
          {
            none= fnone
          ; neutral
          ; pass
          ; fail
          ; softfail
          ; temperror
          ; permerror
          ; fn
          } -> (
          Lwt.catch (fun () -> go (fn ())) @@ function
          | Result result ->
              let none _ = reraise (Result result) in
              let fn =
                match result with
                | `None -> Option.fold ~none ~some:Fun.id fnone
                | `Neutral -> Option.fold ~none ~some:Fun.id neutral
                | `Fail -> Option.fold ~none ~some:Fun.id fail
                | `Softfail -> Option.fold ~none ~some:Fun.id softfail
                | `Temperror -> Option.fold ~none ~some:Fun.id temperror
                | `Permerror -> Option.fold ~none ~some:Fun.id permerror
                | `Pass m ->
                    let fn () =
                      match pass with Some pass -> pass m | None -> none ()
                    in
                    fn in
              go (fn ())
          | exn -> reraise exn) in
    Lwt.catch (fun () -> go t >>= fun _ -> Lwt.return_none) @@ function
    | Result result -> Lwt.return_some result
    | _exn -> Lwt.return_none

  let get_and_check dns ctx = eval ~dns (Uspf.get_and_check ctx)
end
