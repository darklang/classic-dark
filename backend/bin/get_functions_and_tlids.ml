open Core_kernel
module Util = Libexecution.Util
module Account = Libbackend.Account
open Libbackend
open Libexecution
module RTT = Types.RuntimeT
open Libcommon
open Types
open RTT.HandlerT

let flatmap ~(f : 'a -> 'b list) : 'a list -> 'b list =
  List.fold ~init:[] ~f:(fun acc e -> f e @ acc)


let rec fnnames_of_expr (expr : RTT.expr) : RTT.fnname list =
  match expr with
  | Partial _ | Blank _ ->
      []
  | Filled (_, nexpr) ->
    ( match nexpr with
    | If (expr1, expr2, expr3) ->
        [expr1; expr2; expr3] |> flatmap ~f:fnnames_of_expr
    | Thread exprs ->
        exprs |> List.fold ~init:[] ~f:(fun acc e -> acc @ fnnames_of_expr e)
    | FnCall (fnname, exprs) ->
        fnname :: (exprs |> flatmap ~f:fnnames_of_expr)
    | Variable _ ->
        []
    | Let (_, expr1, expr2) ->
        [expr1; expr2] |> flatmap ~f:fnnames_of_expr
    | Lambda (_, expr) ->
        fnnames_of_expr expr
    | Value _ ->
        []
    | FieldAccess (expr, _) ->
        fnnames_of_expr expr
    | ObjectLiteral pairs ->
        pairs
        |> List.map ~f:(fun (key, expr) -> expr)
        |> flatmap ~f:fnnames_of_expr
    | ListLiteral exprs ->
        exprs |> flatmap ~f:fnnames_of_expr
    | FeatureFlag (_, expr1, expr2, expr3) ->
        [expr1; expr2; expr3] |> flatmap ~f:fnnames_of_expr
    | FnCallSendToRail (fnname, exprs) ->
        fnname :: (exprs |> flatmap ~f:fnnames_of_expr)
    | Match (expr, matches) ->
        fnnames_of_expr expr
        @ ( matches
          |> List.map ~f:(fun (pattern, expr) -> expr)
          |> flatmap ~f:fnnames_of_expr )
    | Constructor (_, exprs) ->
        exprs |> flatmap ~f:fnnames_of_expr
    | FluidPartial (_, expr) ->
        fnnames_of_expr expr
    | FluidRightPartial (_, expr) ->
        fnnames_of_expr expr
    | FluidLeftPartial (_, expr) ->
        fnnames_of_expr expr )


let usage () =
  Format.printf
    "Usage: %s <fnNames...>\n  Where <fnNames> is a space-separated list of functions to look for"
    Sys.argv.(0) ;
  exit 1


let prompt str =
  print_string str ;
  Out_channel.flush Out_channel.stdout ;
  match In_channel.input_line In_channel.stdin with None -> "" | Some s -> s


type fn =
  { host : string
  ; handler : string
  ; tlid : string
  ; fnname : RTT.fnname }

let pairs_of_fn (fn : fn) : (string * string) list =
  [ ("host", fn.host)
  ; ("handler", fn.handler)
  ; ("tlid", fn.tlid)
  ; ("fnname", fn.fnname) ]

type reason =
  (* first string is handler/fn name *)
  StringLiteral of string * string
  (* handler/userfn *)
  | BearerToken of string
  (* handler/userfn *)
  | BasicAuth of string
  (* handler/userfn *)
  | HttpclientCall of string
  (* handler/userfn *)
  | Crypto of string
  (* userfn *)
  | FnCallOnlyLiteral of string

let show_reason reason =
  match reason with
  | StringLiteral (tlname, literal) -> "String literal > 12 (" ^ literal ^ ") in handler/fn: " ^ tlname
  | BearerToken tlname -> "Httpclient::bearerToken/Httpclient::bearerToken_v1 used in handler/fn: " ^ tlname
  | BasicAuth tlname -> "Httpclient::basicAuth/Httpclient::basicAuth_v1 used in handler/fn: " ^ tlname
  | HttpclientCall tlname -> "Httpclient::{get/post/put/delete/patch}_v{1,2,3,4,5} used in handler/fn: " ^ tlname
  | Crypto tlname -> "Password::hash/Crypto::sha256/Crypto::sha384/Crypto:sha256hmac/Crypto::sha1hmac used in handler/fn: " ^ tlname
  | FnCallOnlyLiteral fnname -> "Function named: " ^ fnname ^ " contains only a literal string"

let show_reasons reasons =
  reasons
  |> List.map ~f:show_reason
  |> Tc.String.join ~sep:", "
  |> fun str -> "{" ^ str ^ "}"

let process_canvas (canvas : RTT.expr Canvas.canvas ref) : (reason list) option =
  let handler_name (handler : RuntimeT.expr handler) =
    let spec = handler.spec in
    String.concat
      ( [spec.module_; spec.name; spec.modifier]
      |> List.map ~f:(function Filled (_, s) -> s | Partial _ | Blank _ -> "")
      )
      ~sep:"-"
  in
  let handlers =
    !(canvas : RuntimeT.expr Canvas.canvas ref).handlers
    |> IDMap.data
    |> List.filter_map ~f:Toplevel.as_handler
    |> List.map ~f:(fun h -> (handler_name h, h.ast))
  in
  let functions =
    !(canvas : RuntimeT.expr Canvas.canvas ref).user_functions
    |> IDMap.data
    |> List.filter_map ~f:(fun (uf : RuntimeT.expr RuntimeT.user_fn) ->
        uf.metadata.name
        |> (fun bo -> match bo with Filled (_, n) -> Some n | _ -> None)
        |> Option.map ~f:(fun name -> (name, uf.ast)))
  in
  let tls = handlers @ functions in
  let strip_version fnname =
    fnname
    |> String.split ~on:'_'
    |> List.hd_exn
  in
  let fnCallReasons =
    tls
    |> List.fold ~init:[] ~f:(fun acc (name, ast) ->
        let fnCalls = ast |> fnnames_of_expr |> List.map ~f:strip_version in
        Log.infO "fncalls" ~data:(Log.dump fnCalls);
        if Tc.List.member ~value:"HttpClient::bearerToken" fnCalls
        then (BearerToken name) :: acc
        else if Tc.List.member ~value:"HttpClient::basicAuth" fnCalls
        then (BasicAuth name) :: acc
        (* else if Tc.List.member ~value:"HttpClient::get" fnCalls || Tc.List.member ~value:"HttpClient::post" fnCalls || Tc.List.member ~value:"HttpClient::put" fnCalls  || Tc.List.member ~value:"HttpClient::delete" fnCalls || Tc.List.member ~value:"HttpClient::patch" fnCalls *)
        (* then (HttpclientCall name) :: acc *)
        else if Tc.List.member ~value:"Password::hash" fnCalls || Tc.List.member ~value:"Password::check" fnCalls || Tc.List.member ~value:"JWT::signAndEncode" fnCalls || Tc.List.member ~value:"JWT::signAndEncodeWithHeaders" fnCalls  || Tc.List.member ~value:"JWT::verifyAndExtract" fnCalls || Tc.List.member ~value:"Crypto::sha256" fnCalls || Tc.List.member ~value:"Crypto::sha384" fnCalls || Tc.List.member ~value:"Crypto::sha256hmac" fnCalls || Tc.List.member ~value:"Crypto::sha1hmac" fnCalls
        then (Crypto name) :: acc
        else acc)
  in
  let reasons = fnCallReasons in
  if List.length reasons = 0
  then None
  else Some reasons

(*
let () =
  Libs.init [] ;
  ignore (Libs.FnMap.keys !Libs.static_fns |> List.map ~f:(fun s -> Log.infO s)) ;
  ()
  *)

let filterFnsNotInStaticFns (fn : fn) =
  let (realfn : RuntimeT.expr RuntimeT.fn option) =
    Libs.FnMap.find !Libs.static_fns fn.fnname
  in
  match realfn with Some _ -> false | None -> true


let isDeprecated (fn : fn) =
  let (realfn : RuntimeT.expr RuntimeT.fn option) =
    Libs.FnMap.find !Libs.static_fns fn.fnname
  in
  match realfn with Some realfn -> realfn.deprecated | None -> false


let filterFunction _ = true

let () =
  Libs.init [] ;
  ignore
    (let hosts = Serialize.current_hosts () in
     hosts
     |> List.filter_map ~f:(fun host ->
            let canvas =
              try
               Some ( Canvas.load_all host []
                |> Result.map_error ~f:(String.concat ~sep:", ")
                |> Prelude.Result.ok_or_internal_exception "Canvas load error"
                )
              with
              | Pageable.PageableExn e ->
                  Log.erroR
                    "Can't load canvas"
                    ~params:[("host", host); ("exn", Exception.exn_to_string e)] ;
                  None
              | Exception.DarkException _ as e ->
                  Log.erroR
                    "DarkException"
                    ~params:[("host", host); ("exn", Exception.exn_to_string e)] ;
                  None
            in
            let filter =
            (* Dark employees past/present, plus ops/sample *)
              ["fee1aaed-a853-4c85-b67b-8af4ff003021"; "535ad764-7a71-495f-8d6c-c043e6b3a824"; "7fbde0b3-c327-321d-019e-6e487715bbbc"; "28d46d3a-0f5e-311f-d954-2d83cecfd5fe"; "a19ac901-9e8b-3f66-3967-a7f27a57bdb3"; "02681802-2dd5-4f66-b381-03fecafe0750"; "1886e61a-c05b-4d54-976a-1c03ac3e66ff"; "7ffc1bd6-4076-40e5-bdc2-58a5213f8136"; "15e53a1f-064e-4732-ae71-76a212e819ac"; "bfbee04d-8bd8-4d8e-80f3-ba8e90fcaf87"; "5673de6d-821e-4365-8079-ffdf22ee1b7f"]
              |> List.map ~f:(fun uuid -> Option.value_exn (Uuidm.of_string uuid))
            in
            canvas
            |> Option.bind ~f:(fun (canvas : RTT.expr Canvas.canvas ref) ->
             if Tc.List.member ~value:(!canvas.owner) filter then None else Some canvas)
            |> Option.bind ~f:process_canvas
            |> Option.map ~f:(fun reasons -> (host, reasons)))
      |> List.iter ~f:(fun (host, reasons) ->
          Log.infO "Hit" ~params:[("canvas", host); ("reasons", show_reasons reasons)]));
  ()
