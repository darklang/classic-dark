/// Collection of helpful "shortcut" functions to create Dark values quickly
module TestUtils.RTShortcuts

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser

let eStdFnVal (module_ : string) (function_ : string) (version : int) : Expr =
  EFQFnValue(
    gid (),
    FQFnNameParser.stdlibFqName module_ function_ version |> PT2RT.FQFnName.toRT
  )

let eUserFnVal (function_ : string) : Expr =
  EFQFnValue(gid (), FQFnNameParser.userFqName function_ |> PT2RT.FQFnName.toRT)


let eFn'
  (module_ : string)
  (function_ : string)
  (version : int)
  (args : List<Expr>)
  (ster : SendToRail)
  : Expr =
  EApply(gid (), (eStdFnVal module_ function_ version), args, NotInPipe, ster)

let eFn
  (module_ : string)
  (function_ : string)
  (version : int)
  (args : List<Expr>)
  : Expr =
  eFn' module_ function_ version args NoRail

let eFnRail
  (module_ : string)
  (function_ : string)
  (version : int)
  (args : List<Expr>)
  : Expr =
  eFn' module_ function_ version args Rail

let eApply'
  (fnVal : Expr)
  (args : List<Expr>)
  (isInPipe : IsInPipe)
  (ster : SendToRail)
  : Expr =
  EApply(gid (), fnVal, args, isInPipe, ster)

let eApply (fnVal : Expr) (args : List<Expr>) : Expr =
  eApply' fnVal args NotInPipe NoRail

let ePipeApply (fnVal : Expr) (args : List<Expr>) : Expr =
  eApply' fnVal args (InPipe(gid ())) NoRail

let eStr (str : string) : Expr = EString(gid (), str)

let eInt (i : int) : Expr = EInteger(gid (), int64 i)

let eBlank () : Expr = EBlank(gid ())

let eBool (b : bool) : Expr = EBool(gid (), b)

let eFloat (sign : Sign) (whole : string) (fraction : string) : Expr =
  EFloat(gid (), makeFloat sign whole fraction)

let eNull () : Expr = ENull(gid ())

let eList (elems : Expr list) : Expr = EList(gid (), elems)

let eVar (name : string) : Expr = EVariable(gid (), name)

let eFieldAccess (expr : Expr) (fieldName : string) : Expr =
  EFieldAccess(gid (), expr, fieldName)

let eLambda (varNames : string list) (body : Expr) : Expr =
  ELambda(gid (), List.map (fun name -> (gid (), name)) varNames, body)

let eConstructor (name : string) (args : Expr list) : Expr =
  EConstructor(gid (), name, args)
