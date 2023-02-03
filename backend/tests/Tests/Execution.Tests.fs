module Tests.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth

open TestUtils.TestUtils
open LibExecution.RuntimeTypes
open TestUtils.RTShortcuts

module Exe = LibExecution.Execution
module RuntimeTypesAst = LibExecution.RuntimeTypesAst

module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module FSharpToExpr = TestUtils.FSharpToExpr

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let parse = FSharpToExpr.parseRTExpr

let executionStateForPreview
  (name : string)
  (dbs : Map<string, DB.T>)
  (fns : Map<string, UserFunction.T>)
  : Task<AT.AnalysisResults * ExecutionState> =
  task {
    let! meta = createTestCanvas (Randomized name)
    let! state = executionStateFor meta dbs fns
    let results, traceFn = Exe.traceDvals ()

    let state =
      { state with
          tracing =
            { state.tracing with traceDval = traceFn; realOrPreview = Preview } }
    return (results, state)
  }

let execSaveDvals
  (canvasName : string)
  (dbs : List<DB.T>)
  (userFns : List<UserFunction.T>)
  (ast : Expr)
  : Task<AT.AnalysisResults> =
  task {
    let fns = userFns |> List.map (fun fn -> fn.name, fn) |> Map.ofList
    let dbs = dbs |> List.map (fun db -> db.name, db) |> Map.ofList
    let! (results, state) = executionStateForPreview canvasName dbs fns

    let inputVars = Map.empty
    let! _result = Exe.executeExpr state inputVars ast

    return results
  }


let testExecFunctionTLIDs : Test =
  testTask "test that exec function returns the right tlids in the trace" {
    let! meta = initializeTestCanvas (Randomized "exec-function-tlids")
    let name = "testFunction"
    let fn = testUserFn name [] (PT.EInteger(gid (), 5)) |> PT2RT.UserFunction.toRT
    let fns = Map.ofList [ (name, fn) ]
    let! state = executionStateFor meta Map.empty fns

    let tlids, traceFn = Exe.traceTLIDs ()

    let state =
      { state with
          tracing =
            { state.tracing with traceTLID = traceFn; realOrPreview = Preview } }

    let! value = Exe.executeFunction state (gid ()) (FQFnName.User name) []

    Expect.equal (HashSet.toList tlids) [ fn.tlid ] "tlid of function is traced"
    Expect.equal value (DInt 5L) "sanity check"
  }


let testErrorRailUsedInAnalysis : Test =
  testTask
    "When a function isn't available on the client, but has analysis data, we need to make sure we process the errorrail functions correctly" {
    let! meta = createTestCanvas (Randomized "testErrorRailsUsedInAnalysis")
    let! state = executionStateFor meta Map.empty Map.empty

    let loadTraceResults _ _ =
      Some(DOption(Some(DInt 12345L)), NodaTime.Instant.now ())

    let state =
      { state with
          tracing =
            { state.tracing with
                loadFnResult = loadTraceResults
                realOrPreview = Preview } }

    let inputVars = Map.empty
    let ast = eFnRail "" "fake_test_fn" 0 [ eInt 4; eInt 5 ]

    let! result = Exe.executeExpr state inputVars ast

    Expect.equal result (DInt 12345L) "is on the error rail"
  }

let testOtherDbQueryFunctionsHaveAnalysis : Test =
  testTask
    "The SQL compiler inserts analysis results, but I forgot to support DB:queryOne and friends." {
    let varID = gid ()

    let (db : DB.T) =
      { tlid = gid (); name = "MyDB"; version = 0; cols = [ "age", TInt ] }

    let ast =
      eFn
        "DB"
        "queryOne"
        4
        [ eVar "MyDB"
          eLambda [ "value" ] (eFieldAccess (EVariable(varID, "value")) "age") ]

    let! (results, state) =
      executionStateForPreview "test" (Map [ "MyDB", db ]) Map.empty

    let state =
      { state with libraries = { state.libraries with stdlib = Map.empty } }

    let! _value = Exe.executeExpr state Map.empty ast

    Expect.equal
      (Dictionary.get varID results)
      (Some(AT.ExecutedResult(DObj(Map.ofList [ "age", DIncomplete SourceNone ]))))
      "Has an age field"
  }


let testListLiterals : Test =
  testTask "Blank in a list evaluates to Incomplete" {
    let id = gid ()
    let ast = eList [ eInt 1; EBlank id ]
    let! (results : AT.AnalysisResults) =
      execSaveDvals "blank is incomplete" [] [] ast

    return
      match Dictionary.get id results with
      | Some (AT.ExecutedResult (DIncomplete _)) -> Expect.isTrue true ""
      | _ -> Expect.isTrue false ""
  }


let testRecursionInEditor : Test =
  testTask "execution avoids recursion in editor" {
    let callerID = gid ()
    let skippedCallerID = gid ()

    let fnExpr =
      PT.EIf(
        gid (),

        // condition
        PT.EFnCall(
          gid (),
          PTParser.FQFnName.stdlibFqName "" "<" 0,
          [ PT.EVariable(gid (), "i"); PT.EInteger(gid (), 1) ],
          PT.NoRail
        ),

        // 'then' expression
        PT.EInteger(gid (), 0),

        // 'else' expression
        // calls self ("recurse") resulting in recursion
        PT.EFnCall(
          skippedCallerID,
          PTParser.FQFnName.userFqName "recurse",
          [ PT.EInteger(gid (), 2) ],
          PT.NoRail
        )
      )

    let recurse = testUserFn "recurse" [ "i" ] fnExpr |> PT2RT.UserFunction.toRT
    let ast = EApply(callerID, eUserFnVal "recurse", [ eInt 0 ], NotInPipe, NoRail)
    let! results = execSaveDvals "recursion in editor" [] [ recurse ] ast

    Expect.equal
      (Dictionary.get callerID results)
      (Some(AT.ExecutedResult(DInt 0L)))
      "result is there as expected"

    Expect.equal
      (Dictionary.get skippedCallerID results)
      (Some(
        AT.NonExecutedResult(DIncomplete(SourceID(recurse.tlid, skippedCallerID)))
      ))
      "result is incomplete for other path"
  }

let testIfPreview : Test =
  let f cond =
    task {
      let ifID = gid ()
      let thenID = gid ()
      let elseID = gid ()
      let ast = EIf(ifID, cond, EString(thenID, "then"), EString(elseID, "else"))
      let! results = execSaveDvals "if-preview" [] [] ast

      return
        (Dictionary.get ifID results
         |> Exception.unwrapOptionInternal "cannot find ifID" [],
         Dictionary.get thenID results
         |> Exception.unwrapOptionInternal "cannot find thenID" [],
         Dictionary.get elseID results
         |> Exception.unwrapOptionInternal "cannot find elseID" [])
    }

  // Using the first test below for illustration,
  //
  // First we pass in a condition to be evaluated:
  // - `eBool false`
  //
  // The 3-tuple that follows is used to check three things:
  //
  // - the first part is "what does the if/then expression evaluate to?"
  //   If the condition is 'truthy', then the expression will return "then"
  //   Otherwise it willll turn "else"
  //
  // - the other two parts correspond to the `then` and `else` branches of the if condition.
  //   if the first is an `ExecutedResult` and the second is a `NonExecutedResult`,
  //   then the 'then' condition was evaluated but not the 'else' condition.

  testManyTask
    "if-then expression previews correctly"
    f
    [ (eBool false,
       (AT.ExecutedResult(DStr "else"),
        AT.NonExecutedResult(DStr "then"),
        AT.ExecutedResult(DStr "else")))
      (eNull (),
       (AT.ExecutedResult(DStr "else"),
        AT.NonExecutedResult(DStr "then"),
        AT.ExecutedResult(DStr "else")))
      // fakevals
      (eFn "Test" "errorRailValue" 0 [ eConstructor "Nothing" [] ],
       (AT.ExecutedResult(DErrorRail(DOption None)),
        AT.NonExecutedResult(DStr "then"),
        AT.NonExecutedResult(DStr "else")))
      (EBlank 999UL,
       (AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.NonExecutedResult(DStr "then"),
        AT.NonExecutedResult(DStr "else")))
      // others are true
      (eBool true,
       (AT.ExecutedResult(DStr "then"),
        AT.ExecutedResult(DStr "then"),
        AT.NonExecutedResult(DStr "else")))
      (eInt 5,
       (AT.ExecutedResult(DStr "then"),
        AT.ExecutedResult(DStr "then"),
        AT.NonExecutedResult(DStr "else")))
      (eStr "test",
       (AT.ExecutedResult(DStr "then"),
        AT.ExecutedResult(DStr "then"),
        AT.NonExecutedResult(DStr "else"))) ]

let testOrPreview : Test =
  let orID = gid ()
  let f (arg1, arg2) =
    task {
      let ast = EOr(orID, arg1, arg2)
      let! results = execSaveDvals "or-preview" [] [] ast

      return
        (Dictionary.get (Expr.toID (arg1)) results
         |> Exception.unwrapOptionInternal "cannot find arg1 id" [],
         Dictionary.get (Expr.toID (arg2)) results
         |> Exception.unwrapOptionInternal "cannot find arg2 id" [],
         Dictionary.get orID results
         |> Exception.unwrapOptionInternal "cannot find overall id" [])
    }

  testManyTask
    "or expression previews correctly"
    f
    // bools
    [ ((eBool false, eBool false),
       (AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool false)))
      ((eBool false, eBool true),
       (AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool true)))
      ((eBool true, eBool false),
       (AT.ExecutedResult(DBool true),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DBool true)))
      ((eBool true, eBool true),
       (AT.ExecutedResult(DBool true),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DBool true)))
      // fakevals
      ((eBool false, EBlank 999UL),
       (AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL)))))
      ((eBool true, EBlank 999UL),
       (AT.ExecutedResult(DBool true),
        AT.NonExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.ExecutedResult(DBool true)))
      ((EBlank 999UL, eBool false),
       (AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL)))))
      ((EBlank 999UL, eBool true),
       (AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL)))))
      // strings
      ((eBool false, eStr "test"),
       (AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DStr "test"),
        AT.ExecutedResult(DError(SourceID(7UL, orID), "|| only supports Booleans"))))
      ((eBool true, eStr "test"),
       (AT.ExecutedResult(DBool true),
        AT.NonExecutedResult(DStr "test"),
        AT.ExecutedResult(DBool true)))
      ((EString(999UL, "test"), eBool false),
       (AT.ExecutedResult(DStr "test"),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DError(SourceID(7UL, orID), "|| only supports Booleans"))))
      ((EString(999UL, "test"), eBool true),
       (AT.ExecutedResult(DStr "test"),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DError(SourceID(7UL, orID), "|| only supports Booleans")))) ]

let testAndPreview : Test =
  let andID = gid ()
  let f (arg1, arg2) =
    task {
      let ast = EAnd(andID, arg1, arg2)
      let! results = execSaveDvals "and-preview" [] [] ast

      return
        (Dictionary.get (Expr.toID arg1) results
         |> Exception.unwrapOptionInternal "cannot find arg1 id" [],
         Dictionary.get (Expr.toID arg2) results
         |> Exception.unwrapOptionInternal "cannot find arg2 id" [],
         Dictionary.get andID results
         |> Exception.unwrapOptionInternal "cannot find overall id" [])
    }

  testManyTask
    "and expression previews correctly"
    f
    // bools
    [ ((eBool false, eBool false),
       (AT.ExecutedResult(DBool false),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DBool false)))
      ((eBool false, eBool true),
       (AT.ExecutedResult(DBool false),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DBool false)))
      ((eBool true, eBool false),
       (AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool false)))
      ((eBool true, eBool true),
       (AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool true)))
      // fakevals
      ((eBool false, EBlank 999UL),
       (AT.ExecutedResult(DBool false),
        AT.NonExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.ExecutedResult(DBool false)))
      ((eBool true, EBlank 999UL),
       (AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL)))))
      ((EBlank 999UL, eBool false),
       (AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL)))))
      ((EBlank 999UL, eBool true),
       (AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL))),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DIncomplete(SourceID(7UL, 999UL)))))
      // strings
      ((eBool false, eStr "test"),
       (AT.ExecutedResult(DBool false),
        AT.NonExecutedResult(DStr "test"),
        AT.ExecutedResult(DBool false)))
      ((eBool true, eStr "test"),
       (AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DStr "test"),
        AT.ExecutedResult(DError(SourceID(7UL, andID), "&& only supports Booleans"))))
      ((EString(999UL, "test"), eBool false),
       (AT.ExecutedResult(DStr "test"),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DError(SourceID(7UL, andID), "&& only supports Booleans"))))
      ((EString(999UL, "test"), eBool true),
       (AT.ExecutedResult(DStr "test"),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DError(SourceID(7UL, andID), "&& only supports Booleans")))) ]



let testFeatureFlagPreview : Test =
  let f cond =
    task {
      let ffID = gid ()
      let oldID = gid ()
      let newID = gid ()
      let ast =
        EFeatureFlag(ffID, cond, EString(oldID, "old"), EString(newID, "new"))
      let! results = execSaveDvals "ff-preview" [] [] ast

      return
        (Dictionary.get ffID results
         |> Exception.unwrapOptionInternal "missing ffID" [ "ffid", ffID ],
         Dictionary.get oldID results
         |> Exception.unwrapOptionInternal "missing oldID" [ "oldID", oldID ],
         Dictionary.get newID results
         |> Exception.unwrapOptionInternal "missing newID" [ "newID", newID ])
    }

  // see notes in above `testIfPreview` regarding how these tests work

  testManyTask
    "feature flag expression previews correctly"
    f
    [ (eBool true,
       (AT.ExecutedResult(DStr "new"),
        AT.NonExecutedResult(DStr "old"),
        AT.ExecutedResult(DStr "new")))
      // everything else should be old
      (eBool false,
       (AT.ExecutedResult(DStr "old"),
        AT.ExecutedResult(DStr "old"),
        AT.NonExecutedResult(DStr "new")))
      (eFn "Test" "errorRailValue" 0 [ eConstructor "Nothing" [] ],
       (AT.ExecutedResult(DStr "old"),
        AT.ExecutedResult(DStr "old"),
        AT.NonExecutedResult(DStr "new")))
      (eBlank (),
       (AT.ExecutedResult(DStr "old"),
        AT.ExecutedResult(DStr "old"),
        AT.NonExecutedResult(DStr "new")))
      (eInt 5,
       (AT.ExecutedResult(DStr "old"),
        AT.ExecutedResult(DStr "old"),
        AT.NonExecutedResult(DStr "new")))
      (eStr "test",
       (AT.ExecutedResult(DStr "old"),
        AT.ExecutedResult(DStr "old"),
        AT.NonExecutedResult(DStr "new")))
      (eNull (),
       (AT.ExecutedResult(DStr "old"),
        AT.ExecutedResult(DStr "old"),
        AT.NonExecutedResult(DStr "new"))) ]

let testLambdaPreview : Test =
  let lID = gid ()
  let p1ID = gid ()
  let p2ID = gid ()
  let f body =
    task {
      let ast = ELambda(lID, [ (p1ID, ""); (p2ID, "var") ], body)
      let! results = execSaveDvals "lambda-preview" [] [] ast
      return results |> Dictionary.toList |> Map
    }
  testManyTask
    "lambda preview"
    f
    [ (EString(65UL, "body")),
      (Map.fromList [ (lID,
                       AT.ExecutedResult(
                         DFnVal(
                           Lambda(
                             { parameters = [ (p2ID, "var") ]
                               symtable = Map.empty
                               body = EString(65UL, "body") }
                           )
                         )
                       ))
                      (p1ID, AT.NonExecutedResult(DIncomplete(SourceID(7UL, p1ID))))
                      (p2ID, AT.NonExecutedResult(DIncomplete(SourceID(7UL, p2ID))))
                      (65UL, AT.NonExecutedResult(DStr "body")) ]) ]


/// Test the results that are returned when we're "previewing" (i.e. Analysis)
///
/// "Preview" evals involve more tracing, including of unmatched patterns. In
/// this test, we set up a `match` expr with many patterns and RHSs. Each test
/// supplies an `expr` to match against, and a list of expected traces to look
/// for - up to one of the traces will be an Executed result, and the others
/// should all be NonExecutedResults.
let testMatchPreview : Test =
  let matchId = gid ()
  let pIntId, intRhsId = gid (), gid ()
  let pFloatId, floatRhsId = gid (), gid ()
  let pBoolId, boolRhsId = gid (), gid ()
  let pStrId, strRhsId = gid (), gid ()
  let pNullId, nullRhsId = gid (), gid ()
  let pBlankId, blankRhsId = gid (), gid ()
  let pOkBlankOkId, pOkBlankBlankId, okBlankRhsId = gid (), gid (), gid ()

  let (pOkVarOkId,
       pOkVarVarId,
       okVarRhsId,
       binopFnValId,
       okVarRhsVarId,
       okVarRhsStrId) =
    gid (), gid (), gid (), gid (), gid (), gid ()

  let pNothingId, nothingRhsId = gid (), gid ()
  let pVarId, varRhsId = gid (), gid ()

  let patternsToMatchAgainst =
    [ // | 5 -> 17
      (MPInteger(pIntId, 5L), EInteger(intRhsId, 17L))

      // | 5.6 -> "float"
      (MPFloat(pFloatId, 5.6), EString(floatRhsId, "float"))

      // | false -> "bool"
      (MPBool(pBoolId, false), EString(boolRhsId, "bool"))

      // | "myStr" -> "str"
      (MPString(pStrId, "myStr"), EString(strRhsId, "str"))

      // | null -> "null"
      (MPNull(pNullId), EString(nullRhsId, "null"))

      // | _ -> "blank" (should never been matched)
      (MPBlank(pBlankId), EString(blankRhsId, "blank"))

      // | Ok _ -> "ok blank"
      (MPConstructor(pOkBlankOkId, "Ok", [ MPBlank pOkBlankBlankId ]),
       EString(okBlankRhsId, "ok blank"))

      // | Ok x -> "ok: " ++ x
      (MPConstructor(pOkVarOkId, "Ok", [ MPVariable(pOkVarVarId, "x") ]),
       EApply(
         okVarRhsId,
         EFQFnValue(
           binopFnValId,
           PTParser.FQFnName.stdlibFqName "" "++" 0 |> PT2RT.FQFnName.toRT
         ),
         [ EString(okVarRhsStrId, "ok: "); EVariable(okVarRhsVarId, "x") ],
         NotInPipe,
         NoRail
       ))

      // | None -> "constructor nothing"
      (MPConstructor(pNothingId, "Nothing", []),
       EString(nothingRhsId, "constructor nothing"))

      // | name -> name
      // (everything should match this, except for 'fake' dvals such as errors)
      (MPVariable(pVarId, "name"), EVariable(varRhsId, "name")) ]

  let getSubExprIds (arg : Expr) =
    let mutable argIDs = []
    arg
    |> RuntimeTypesAst.postTraversal (fun e ->
      argIDs <- (Expr.toID e) :: argIDs
      e)
    |> ignore<Expr>
    argIDs

  // Checks that the 'expected' list of results are all as expected.
  // Then, checks that any patterns not explicitly called out are present in
  // the results, as NotExecutedResults, ensuring that all patterns are
  // accounted for.
  // - `arg` is the expr we're matching against
  // - `expected` is a list of explicitly called-out results to verify,
  //   including at most one 'matched' pattern (ExecutedResult)
  //   - the `string` in `expected` is the 'msg' to report in case of failure
  let t
    (msg : string)
    (arg : Expr)
    (expected : List<id * string * AT.ExecutionResult>)
    =
    testTask msg {
      let ast = EMatch(matchId, arg, patternsToMatchAgainst)

      let! results = execSaveDvals "match-preview" [] [] ast

      // check expected values are there
      List.iter
        (fun (id, name, value) ->
          Expect.equal
            (Dictionary.get id results)
            (Some value)
            $"{msg}: {id}, {name}")
        expected

      // Check that all patterns not included in 'expected' were evaluated,
      // and are NotExecutedResults

      // we expect _some_ result for all of these exprs
      let expectedIDs =
        (matchId :: getSubExprIds arg) @ List.map Tuple3.first expected |> Set

      // ensure we don't have more expected results than actual results
      Expect.isGreaterThan results.Count (Set.count expectedIDs) "sanity check"

      let resultsUnaccountedFor =
        Set.ofSeq (Dictionary.keys results)
        |> Set.difference expectedIDs
        |> Seq.map (fun id -> id, Dictionary.get id results)

      resultsUnaccountedFor
      |> Seq.iter (fun (id, result) ->
        match result with
        | Some (AT.ExecutedResult dv) ->
          Expect.isTrue
            false
            $"{msg}: found unexpected execution result ({id}: {dv})"
        | None -> Expect.isTrue false "missing value"
        | Some (AT.NonExecutedResult _) -> ())
    }

  // helpers
  let er x = AT.ExecutedResult x
  let ner x = AT.NonExecutedResult x
  let inc iid = DIncomplete(SourceID(id 7, iid))

  testList
    "test match evaluation"
    [ t
        "int match"
        (eInt 5)
        [ (pIntId, "matching pat", er (DInt 5L))
          (intRhsId, "matching rhs", er (DInt 17L))

          (pVarId, "2nd matching pat", ner (DInt 5L))
          (varRhsId, "2nd matching rhs", ner (DInt 5L)) ]

      t
        "non match"
        (eInt 6)
        [ (pIntId, "non matching pat", ner (DInt 5L))
          (intRhsId, "non matching rhs", ner (DInt 17L))

          (pFloatId, "float pat", ner (DFloat 5.6))
          (floatRhsId, "float rhs", ner (DStr "float"))

          (pBoolId, "bool pat", ner (DBool false))
          (boolRhsId, "bool rhs", ner (DStr "bool"))

          (pNullId, "null pat", ner DNull)
          (nullRhsId, "null rhs", ner (DStr "null"))

          (pOkVarOkId, "ok var pat ok", ner (inc pOkVarOkId))
          (pOkVarVarId, "ok var pat var", ner (inc pOkVarVarId))
          (okVarRhsId, "ok var pat rhs", ner (inc okVarRhsVarId))
          (okVarRhsVarId, "ok var rhs var", ner (inc okVarRhsVarId))
          (okVarRhsStrId, "ok var rhs str", ner (DStr "ok: "))

          (pNothingId, "nothing pat", ner (DOption None))
          (nothingRhsId, "nothing pat rhs", ner (DStr "constructor nothing"))

          (pOkBlankOkId, "ok blank pat ok", ner (inc pOkBlankOkId))
          (pOkBlankBlankId, "ok blank pat blank", ner (inc pOkBlankBlankId))
          (okBlankRhsId, "ok blank rhs", ner (DStr "ok blank"))

          (pVarId, "catch all pat", er (DInt 6L))
          (varRhsId, "catch all rhs", er (DInt 6L)) ]

      t
        "float"
        (eFloat Positive "5" "6")
        [ (pFloatId, "pat", er (DFloat 5.6))
          (floatRhsId, "rhs", er (DStr "float")) ]

      t
        "bool"
        (eBool false)
        [ (pBoolId, "pat", er (DBool false)); (boolRhsId, "rhs", er (DStr "bool")) ]

      t
        "null"
        (eNull ())
        [ (pNullId, "pat", er DNull); (nullRhsId, "rhs", er (DStr "null")) ]

      t
        "ok: y"
        (eConstructor "Ok" [ eStr "y" ])
        [ (pOkBlankOkId, "ok pat 1", ner (inc pOkBlankOkId))
          (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
          (okBlankRhsId, "rhs", ner (DStr "ok blank"))
          (pOkVarOkId, "ok pat 2", er (DResult(Ok(DStr "y"))))

          (binopFnValId,
           "fnval",
           er (
             DFnVal(
               FnName(
                 PTParser.FQFnName.stdlibFqName "" "++" 0 |> PT2RT.FQFnName.toRT
               )
             )
           ))
          (pOkVarVarId, "var pat", er (DStr "y"))
          (okVarRhsId, "rhs", er (DStr "ok: y"))
          (okVarRhsVarId, "rhs", er (DStr "y"))
          (okVarRhsStrId, "str", er (DStr "ok: ")) ]

      t
        "ok: blank"
        (eConstructor "Ok" [ EBlank(gid ()) ])
        [ (pOkBlankOkId, "blank pat", ner (inc pOkBlankOkId))
          (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
          (okBlankRhsId, "blank rhs", ner (DStr "ok blank"))
          (pOkVarOkId, "ok pat", ner (inc pOkVarOkId))
          (pOkVarVarId, "var pat", ner (inc pOkVarVarId))
          (okVarRhsId, "rhs", ner (inc okVarRhsVarId))
          (okVarRhsVarId, "rhs var", ner (inc okVarRhsVarId))
          (okVarRhsStrId, "str", ner (DStr "ok: ")) ]

      t
        "nothing"
        (eConstructor "Nothing" [])
        [ (pNothingId, "ok pat", er (DOption None))
          (nothingRhsId, "rhs", er (DStr "constructor nothing")) ]

      // TODO: test constructor around a literal
      // TODO: constructor around a variable
      // TODO: constructor around a constructor around a value
      ]

let tests =
  testList
    "ExecutionUnitTests"
    [ testListLiterals
      testRecursionInEditor
      testIfPreview
      testOrPreview
      testAndPreview
      testLambdaPreview
      testFeatureFlagPreview
      testMatchPreview
      testExecFunctionTLIDs
      testErrorRailUsedInAnalysis
      testOtherDbQueryFunctionsHaveAnalysis ]
