module Tests.StdLib

// Misc tests of Stdlib (both LibBackend and LibExecution) that could not be
// tested via LibExecution.tests

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module Exe = LibExecution.Execution
module FQFnNameParser = TestUtils.FQFnNameParser

open TestUtils.TestUtils

let hardToRepresentTests =
  let execute
    ((fn, args) : PT.FQFnName.StdlibFnName * List<RT.Dval>)
    (expected : RT.Dval)
    : Task<bool> =
    task {
      // evaluate the fn call against both backends
      let! meta = initializeTestCanvas (Randomized "ExecutePureFunction")
      let args = List.mapi (fun i arg -> ($"v{i}", arg)) args
      let fnArgList = List.map (fun (name, _) -> PT.EVariable(gid (), name)) args

      let ast = PT.EFnCall(gid (), PT.FQFnName.Stdlib fn, fnArgList, PT.NoRail)

      let symtable = Map.ofList args

      let! state = executionStateFor meta Map.empty Map.empty
      let! actual =
        LibExecution.Execution.executeExpr state symtable (PT2RT.Expr.toRT ast)
      return Expect.dvalEquality actual expected
    }

  let fnName mod_ function_ version =
    FQFnNameParser.stdlibFnName mod_ function_ version

  // These are hard to represent in .tests files, usually because of FakeDval behaviour
  testMany2Task
    "hardToRepresent"
    execute
    [ (fnName "List" "fold" 0,
       [ RT.DList [ RT.DBool true; RT.DErrorRail(RT.DInt 0L) ]

         RT.DList []

         RT.DFnVal(
           RT.Lambda { parameters = []; symtable = Map.empty; body = RT.EBlank 1UL }
         ) ]),
      (RT.DError(RT.SourceNone, "Expected 0 arguments, got 2")),
      true

      (fnName "Result" "fromOption" 0,
       [ RT.DOption(
           Some(
             RT.DFnVal(
               RT.Lambda
                 { parameters = []
                   symtable = Map.empty
                   body = RT.EFloat(84932785UL, -9.223372037e+18) }
             )
           )
         )
         RT.DStr "s" ]),
      (RT.DResult(
        Ok(
          RT.DFnVal(
            RT.Lambda
              { parameters = []
                symtable = Map.empty
                body = RT.EFloat(84932785UL, -9.223372037e+18) }
          )
        )
      )),
      true

      (fnName "Result" "fromOption" 0,
       [ RT.DOption(
           Some(
             RT.DFnVal(
               RT.Lambda
                 { parameters = []
                   symtable = Map.empty
                   body =
                     RT.EMatch(
                       gid (),
                       RT.ENull(gid ()),
                       [ (RT.MPFloat(gid (), -9.223372037e+18), RT.ENull(gid ())) ]
                     ) }
             )
           )
         )
         RT.DStr "s" ]),

      RT.DResult(
        Ok(
          RT.DFnVal(
            RT.Lambda
              { parameters = []
                symtable = Map.empty
                body =
                  RT.EMatch(
                    gid (),
                    RT.ENull(gid ()),
                    [ (RT.MPFloat(gid (), -9.223372037e+18), RT.ENull(gid ())) ]
                  ) }
          )
        )
      ),
      true ]

let oldFunctionsAreDeprecated =
  test "old functions are deprecated" {
    let counts = ref Map.empty

    let fns = libraries.Force().stdlib |> Map.values

    fns
    |> List.iter (fun fn ->
      let key = RT.FQFnName.StdlibFnName.toString { fn.name with version = 0 }

      if fn.deprecated = RT.NotDeprecated then
        counts.Value <-
          Map.update
            key
            (fun count -> count |> Option.defaultValue 0 |> (+) 1 |> Some)
            counts.Value

      ())

    Map.iter
      (fun name count ->
        Expect.equal count 1 $"{name} has more than one undeprecated function")
      counts.Value
  }

let intInfixMatch =
  test "int infix functions match" {
    let actual = LibExecution.Errors.intInfixFns
    let expected =
      LibExecutionStdLib.StdLib.infixFnMapping
      |> Map.filterWithIndex (fun name _ -> name.module_ = "Int")
      |> Map.values
      |> List.map (fun (name, _) -> name.function_)
      |> Set

    Expect.equal actual expected "We didn't miss any infix functions"
  }

let tests =
  testList
    "stdlib"
    [ hardToRepresentTests; oldFunctionsAreDeprecated; intInfixMatch ]
