module Lwt_scheduler = Spf.Sigs.Make (struct
  type 'a t = 'a Lwt.t
end)

let _state =
  let open Spf.Sigs in
  let open Lwt_scheduler in
  let open Lwt.Infix in
  {
    bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
    return = (fun x -> inj (Lwt.return x));
  }

module type DNS = sig
  type t

  type error =
    [ `Msg of string
    | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
    | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

  val getrrecord :
    t -> 'r Dns.Rr_map.key -> _ Domain_name.t -> ('r, [> error ]) result Lwt.t
end

let get :
    type dns.
    domain:_ Domain_name.t -> dns -> (module DNS with type t = dns) -> _ =
 fun ~domain:domain_name dns (module DNS) ->
  let open Rresult in
  let open Lwt.Infix in
  DNS.getrrecord dns Dns.Rr_map.Txt domain_name >>= function
  | Error (`Msg _) -> Lwt.return_error `Not_found
  | Error (`No_domain _ | `No_data _) -> Lwt.return_error `Not_found
  | Ok (_, txts) ->
  match
    R.(
      Spf.select_spf1 (Dns.Rr_map.Txt_set.elements txts)
      >>= Spf.Term.parse_record)
  with
  | Ok terms -> Lwt.return_ok terms
  | Error `None -> Lwt.return_error `Not_found
  | Error (`Msg _) -> Lwt.return_error `Invalid_SPF_record
