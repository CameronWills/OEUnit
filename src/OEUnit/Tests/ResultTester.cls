ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.*.
USING OEUnit.Runners.*.
USING Progress.Lang.*.

CLASS OEUnit.Tests.ResultTester:
  
  DEFINE PUBLIC PROPERTY Runner AS OEUnitRunner NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Results AS TestClassResult NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Tcase AS Object NO-UNDO GET. SET.
  
  CONSTRUCTOR ResultTester():
  END METHOD.
  
  CONSTRUCTOR ResultTester(INPUT test AS Object):
    RunTest(test).
  END METHOD.
  
  DESTRUCTOR ResultTester():
    DeleteProperties().
  END DESTRUCTOR.
  
  METHOD PUBLIC VOID RunTest(INPUT test AS Object):
    DeleteProperties().
    Tcase = test.
    Runner = NEW OEUnitRunner().
    runner:RunTest(Tcase).
    Results = runner:Results.
  END METHOD.
  
  METHOD PUBLIC TestMethodResult GetMethodResult(INPUT methodName AS CHARACTER):
    RETURN CAST(results:GetResultByName(methodName), TestMethodResult).
  END METHOD.
  
  METHOD PUBLIC VOID DeleteProperties():
    DELETE OBJECT Tcase NO-ERROR.
    DELETE OBJECT Runner NO-ERROR.
  END METHOD.
  
END CLASS.