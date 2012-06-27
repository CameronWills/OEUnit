ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.
USING OEUnit.Assertion.AssertString.
USING OEUnit.Runners.OEUnitRunner.
USING OEUnit.Runner.*.
USING OEUnit.Reflection.*.
USING OEUnit.Runner.Manipulation.IFilter.
USING OEUnit.Tests.Runner.Manipulation.*.

CLASS OEUnit.Tests.Runner.Manipulation.FilterTester IMPLEMENTS IFilter:

  DEFINE PRIVATE VARIABLE shouldRun AS CHARACTER NO-UNDO.
  
  @Test.
  METHOD PUBLIC VOID Filtered():
    
    DEFINE VARIABLE runner AS OEUnitRunner NO-UNDO.
    DEFINE VARIABLE results AS TestClassResult NO-UNDO.
    DEFINE VARIABLE test AS FilterTest NO-UNDO.
    DEFINE VARIABLE methodResult AS TestMethodResult NO-UNDO.
    
    runner = NEW OEUnitRunner().
    test = NEW FilterTest().
    runner:Filter = THIS-OBJECT.
    runner:RunTest(test).
    results = runner:Results.

    Assert:AreEqual(results:GetStatus(), TestResult:StatusPassed).
    Assert:AreEqual(results:ResultCount, 3).
    Assert:AreEqual(ENTRY(1,shouldRun), "OEUnit.Tests.Runner.Manipulation.FilterTest").
    Assert:AreEqual(ENTRY(2,shouldRun), "Test1").
    Assert:AreEqual(ENTRY(3,shouldRun), "Test2").
    Assert:AreEqual(ENTRY(4,shouldRun), "Test3").
    
    methodResult = CAST(results:GetResultByName("Test1"), TestMethodResult).
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusPassed).  

    methodResult = CAST(results:GetResultByName("Test2"), TestMethodResult).
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusIgnored).

    methodResult = CAST(results:GetResultByName("Test3"), TestMethodResult).
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusPassed).                  
    
    DELETE OBJECT test NO-ERROR.
    DELETE OBJECT runner NO-ERROR.
  END METHOD. 
 
  METHOD PUBLIC LOGICAL ShouldRun(INPUT inf AS StatementInfo):
    shouldRun = shouldRun + inf:NAME + ",".
    RETURN (inf:Name <> "Test2").
  END METHOD.
 
END CLASS.