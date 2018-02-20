open Core

open Types.RuntimeT
open Types.SpecTypes
module RT = Runtime

type spec_types = { input : dark_type Types.or_blank
                  ; output : dark_type Types.or_blank
                  } [@@deriving eq, show, yojson, sexp, bin_io]


type spec = { module_ : string Types.or_blank [@key "module"]
            ; name : string Types.or_blank
            ; modifier : string Types.or_blank
            ; types : spec_types
            } [@@deriving eq, show, yojson, sexp, bin_io]

type handler = { tlid: Types.tlid
               ; ast: Ast.ast
               ; spec : spec
               } [@@deriving eq, show, yojson, sexp, bin_io]

let url_for (h: handler) : string option =
  match h.spec.module_, h.spec.name with
  | Filled (_, module_), Filled (_, name) when String.lowercase module_ = "http" ->
    Some name
  | _ -> None

let url_for_exn (h: handler) : string =
  match (url_for h) with
  | Some s -> s
  | None ->
    Exception.internal
      "Called url_for_exn on a toplevel without a `url` param"

let modifier_for (h: handler) : string option =
  match h.spec.module_, h.spec.modifier with
  | Filled (_, module_), Filled (_, modifier) when String.lowercase module_ = "http" ->
    Some modifier
  | _ -> None

let modifier_for_exn (h: handler) : string =
  match (modifier_for h) with
  | Some s -> s
  | None ->
    Exception.internal
      "Called modifier_for_exn on a toplevel without a `modifier` param"

let default_env (h: handler) : dval_map =
  let init = DvalMap.empty in
  match url_for h with
  | Some n ->
    List.fold_left
      ~init
      ~f:(fun acc v ->
          DvalMap.set ~key:v ~data:DNull acc)
      (Http.route_variables n)
  | None -> init

let with_defaults (h: handler) (env: Ast.symtable) : Ast.symtable =
  Util.merge_left env (default_env h)

let execute (h: handler) (env: Ast.symtable) : dval =
  Ast.execute (with_defaults h env) h.ast

let execute_for_analysis (h: handler) (env: Ast.symtable) :
    (dval * Ast.dval_store * Ast.sym_store) =
  let traced_symbols = Ast.symbolic_execute (with_defaults h env) h.ast in
  let (ast_value, traced_values) = Ast.execute_saving_intermediates env h.ast in
  (ast_value, traced_values, traced_symbols)
