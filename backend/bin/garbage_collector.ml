open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend
open Libcommon

(*
 * For initial testing:
 * 43e58c86-2580-45e8-a569-b02740ec189b is paulshen-animalcrossing
 * *)
let usage () : unit =
  Format.printf
    "Usage: %s [count|delete] <limit> [all|<canvas_id>]\n\nIf given 'all', it will iterate across all canvases; if given a specific canvas_id,\nit will only gc that canvas\n"
    Sys.argv.(0) ;
  exit 1


let () =
  (* parse args *)
  let (action, action_arg, limit, canvas_arg)
        : Stored_event.trim_events_action * string * int * string =
    match Sys.argv with
    | [|_argv0; action_arg; limit; canvas_arg|] ->
        let action : Stored_event.trim_events_action =
          match action_arg with
          | "count" ->
              Count
          | "delete" ->
              Delete
          | _ ->
              usage () ;
              Exception.internal "Can't happen, unreachable code"
        in
        let limit = limit |> int_of_string in
        (action, action_arg, limit, canvas_arg)
    | _ ->
        usage () ;
        Exception.internal "Can't happen, unreachable code"
  in
  (* Outer span - one per script invocation *)
  Telemetry.with_root "garbage_collector" (fun span ->
      (* Get all canvas ids *)
      let canvas_ids_and_names : (Uuidm.t * string) list =
        ( match canvas_arg with
        | "all" ->
            Telemetry.with_span span "get_canvases_for_gc" (fun span ->
                let canvas_ids : (string * string) list =
                  Db.fetch
                    ~name:"canvases"
                    "SELECT id, name FROM canvases"
                    ~params:[]
                  (* List.map over all the rows; then List.hd_exn each row, it's is a single field, canvases.id *)
                  |> List.map ~f:(function
                         | [id; name] ->
                             (id, name)
                         | _ ->
                             Exception.internal "Wrong shape")
                in
                Telemetry.Span.set_attr
                  span
                  "canvas_count"
                  (`Int (List.length canvas_ids)) ;
                canvas_ids)
        | canvas_arg ->
            let canvas_name =
              Libbackend.Canvas.name_for_id
                (canvas_arg |> Uuidm.of_string |> Tc.Option.value_exn)
            in
            [(canvas_arg, canvas_name)] )
        |> List.map ~f:(fun (cid, canvas_name) ->
               (cid |> Uuidm.of_string |> Tc.Option.value_exn, canvas_name))
      in
      Telemetry.Span.set_attr
        span
        "canvas_count"
        (`Int (List.length canvas_ids_and_names)) ;
      (* Map over canvases *)
      let row_count =
        canvas_ids_and_names
        |> List.map ~f:(fun (canvas_id, canvas_name) ->
               Stored_event.trim_events_for_canvas
                 ~span
                 ~action
                 canvas_id
                 canvas_name
                 limit)
        (* Sum the row_count returned for each canvas and put it in this outer
         * span *)
        |> Tc.List.sum
      in
      Telemetry.Span.set_attr span "row_count" (`Int row_count))
  |> ignore