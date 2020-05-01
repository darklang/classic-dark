open Core_kernel
module Util = Libexecution.Util
module Account = Libbackend.Account
open Libbackend
open Libexecution
module RTT = Types.RuntimeT
open Libcommon
open Types
open RTT.HandlerT
module FluidExpression = Libshared.FluidExpression

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

let bearerTokenParams (expr : RTT.expr) : FluidExpression.t list =
  let fluidExpr : FluidExpression.t = Fluid.toFluidExpr expr in
  let params = ref [] in
  let processor fe =
    ( match fe with
    | FluidExpression.EFnCall (_, name, ps, _) ->
        if name = "HttpClient::bearerToken" || name = "HttpClient::bearerToken_v1"
        then
          params := ps :: !params
        else
          ()
    | _ ->
        () ) ;
    fe
  in
  fluidExpr |> FluidExpression.postTraversal ~f:processor |> ignore ;
  (!params) |> List.concat |> List.sort ~compare:FluidExpression.compare

let strings_of_expr (expr : RTT.expr) : string list =
  let fluidExpr : FluidExpression.t = Fluid.toFluidExpr expr in
  let strings = ref [] in
  let processor fe =
    ( match fe with
    | FluidExpression.EString (_, str) ->
        strings := str :: !strings
    | _ ->
        () ) ;
    fe
  in
  fluidExpr |> FluidExpression.postTraversal ~f:processor |> ignore ;
  (!strings) |> List.map ~f:String.lowercase |> List.sort ~compare:compare_string

type reason =
  (* first string is handler/fn name *)
  StringLiteral of string * string
  (* handler/userfn *)
  | BearerToken of string * string
  (* handler/userfn *)
  | BasicAuth of string * string
  (* handler/userfn *)
  | HttpclientCall of string * string
  (* handler/userfn *)
  | Crypto of string * string
  | API of string * string
  | Password of string * string

let show_reason host reason =
  let reason_to_link host typ id =
    "https://darklang.com/a/" ^ host ^ "#" ^ typ ^ "=" ^ id
  in
  match reason with
  | StringLiteral (typ, id) -> reason_to_link host typ id
  | BearerToken (typ, id) -> reason_to_link host typ id
  | BasicAuth (typ, id) -> reason_to_link host typ id
  | HttpclientCall (typ, id) -> reason_to_link host typ id
  | Crypto (typ, id) -> reason_to_link host typ id
  | API (typ, id) -> reason_to_link host typ id
  | Password (typ, id) -> reason_to_link host typ id

let show_reasons host reasons =
  reasons
  |> List.map ~f:(show_reason host)
  |> Tc.String.join ~sep:","

let process_canvas (canvas : RTT.expr Canvas.canvas ref) : (reason list) option =
  let handlers =
    !(canvas : RuntimeT.expr Canvas.canvas ref).handlers
    |> IDMap.to_alist
    |> List.filter_map ~f:(fun (tlid, tl) ->
        tl |> Toplevel.as_handler |> Option.map ~f:(fun h -> ("handler", tlid, h.ast)))
  in
  let functions =
    !(canvas : RuntimeT.expr Canvas.canvas ref).user_functions
    |> IDMap.to_alist
    |> List.map ~f:(fun (tlid, (uf : RuntimeT.expr RuntimeT.user_fn)) -> ("fn", tlid, uf.ast))
  in
  let tls = handlers @ functions in
  let strip_version fnname =
    fnname
    |> String.split ~on:'_'
    |> List.hd_exn
  in
  let shouldFilterStoreReport (c : RTT.expr Canvas.canvas) (id : string) (ast : RTT.expr) =
    (* id of Worker::storeReport handler *)
    if id ="681287691"
    then
      let knownBearerTokenParams = [FluidExpression.EFnCall (id_of_string "794305869", "sampleAirtableKey", [], FluidExpression.NoRail)] in
      let _origknownSampleKeyStrings = ["note, this key is solely for the sample and is not used in any places. the sample is regularly reviewed."; "keyAqo8LWU2tImZF4"] in
      let knownSampleKeyStrings = ["note, this key is solely for the sample and is not used in any places. the sample is regularly reviewed."; "keyaqo8lwu2timzf4"] |> List.sort ~compare:compare_string
      in
      let sampleKeyStringsMatch, stringsss =
        match IDMap.find c.user_functions (id_of_int 646253428) with
        | Some kfn ->
            ((strings_of_expr (kfn.ast)) = knownSampleKeyStrings, (strings_of_expr (kfn.ast)))
        | None ->
            (true, [])
      in
      let list_eq a b =
        List.zip a b
        |> Option.map ~f:(fun zipped -> List.for_all ~f:(fun (ax, bx) -> FluidExpression.testEqualIgnoringIds ax bx) zipped)
        |> Option.value ~default:false
      in
      let bearerTokenParamsMatch = list_eq (bearerTokenParams ast) knownBearerTokenParams in
      Log.infO "shouldFilter" ~params:[("host", c.host); ("btpm", string_of_bool bearerTokenParamsMatch); ("sksm", string_of_bool sampleKeyStringsMatch)];
      if bearerTokenParamsMatch && sampleKeyStringsMatch
      then true
      else
        begin
          Log.infO "bearer" ~params:[("orig", Log.dump knownBearerTokenParams); ("new", Log.dump (bearerTokenParams ast))];
          Log.infO "sk" ~params:[("orig", Log.dump knownSampleKeyStrings); ("new", Log.dump (stringsss))];
          false
        end
    else
      false
  in
  let fnCallReasons =
    tls
    |> List.fold ~init:[] ~f:(fun acc (typ, id, ast) ->
        let id = Int63.to_string id in
        let fnCalls = ast |> fnnames_of_expr |> List.map ~f:strip_version in
        Log.infO "fncalls" ~data:(Log.dump fnCalls);
        if shouldFilterStoreReport !canvas id ast
        then acc
        else
          if Tc.List.member ~value:"HttpClient::bearerToken" fnCalls
          then (BearerToken (typ, id)) :: acc
          else if Tc.List.member ~value:"HttpClient::basicAuth" fnCalls
          then (BasicAuth (typ, id)) :: acc
          (* else if Tc.List.member ~value:"HttpClient::get" fnCalls || Tc.List.member ~value:"HttpClient::post" fnCalls || Tc.List.member ~value:"HttpClient::put" fnCalls  || Tc.List.member ~value:"HttpClient::delete" fnCalls || Tc.List.member ~value:"HttpClient::patch" fnCalls *)
          (* then (HttpclientCall name) :: acc *)
          (* if Tc.List.member ~value:"Password::hash" fnCalls || Tc.List.member ~value:"Password::check" fnCalls *)
          (* then (Password (typ, id)) :: acc *)
          else if Tc.List.member ~value:"JWT::signAndEncode" fnCalls || Tc.List.member ~value:"JWT::signAndEncodeWithHeaders" fnCalls  || Tc.List.member ~value:"JWT::verifyAndExtract" fnCalls || Tc.List.member ~value:"Crypto::sha256" fnCalls || Tc.List.member ~value:"Crypto::sha384" fnCalls || Tc.List.member ~value:"Crypto::sha256hmac" fnCalls || Tc.List.member ~value:"Crypto::sha1hmac" fnCalls
          then (Crypto (typ, id)) :: acc
          else if List.length (List.filter ~f:(fun fn -> String.is_prefix ~prefix:"dark/" fn) fnCalls) <> 0 || Tc.List.member ~value:"Twilio::sendText" fnCalls
          then (API (typ, id)) :: acc
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
          print_endline (host ^ "," ^ (show_reasons host reasons))));
  ()
