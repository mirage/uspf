open Lwt.Infix

external reraise : exn -> 'a = "%reraise"

let ( % ) f g = fun x -> f (g x)

module Make (Dns_client : Dns_client_mirage.S) = struct
  let eval : type a. dns:Dns_client.t -> a Uspf.t -> Uspf.Result.t option Lwt.t
      =
   fun ~dns t ->
    let rec go : type a. a Uspf.t -> a Lwt.t = function
      | Request (domain_name, record, fn) ->
          Dns_client.get_resource_record dns record domain_name >>= fun resp ->
          go (fn resp)
      | Return v -> Lwt.return v
      | Tries lst -> Lwt_list.iter_p (fun fn -> go (fn ())) lst
      | Map (x, fn) -> go x >|= fn
      | Choose_on c -> begin
          Lwt.catch (go % c.fn) @@ function
          | Uspf.Result result ->
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
                    fun () ->
                      match c.pass with Some pass -> pass m | None -> none ()
                  end in
              go (fn ())
          | exn -> reraise exn
        end in
    let fn () = go t >>= fun _ -> Lwt.return_none in
    Lwt.catch fn @@ function
    | Uspf.Result result -> Lwt.return_some result
    | _exn -> Lwt.return_none

  let get_and_check dns ctx = eval ~dns (Uspf.get_and_check ctx)
end
