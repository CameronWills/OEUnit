ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.
USING OEUnit.Runners.OEUnitRunner.
USING OEUnit.Runner.*.
USING OEUnit.Reflection.*.
USING OEUnit.Runner.Notification.*.
USING OEUnit.Tests.Runner.Notification.*.

CLASS OEUnit.Tests.Runner.Notification.NotifierTester INHERITS RunListener:

  DEFINE PRIVATE VARIABLE runningTest AS CHARACTER NO-UNDO.
  DEFINE PRIVATE VARIABLE testFinished AS LOGICAL NO-UNDO INIT TRUE.
  
  @Test.
  METHOD PUBLIC VOID Notified():
    
    DEFINE VARIABLE runner AS OEUnitRunner NO-UNDO.
    DEFINE VARIABLE results AS TestClassResult NO-UNDO.
    DEFINE VARIABLE notifier AS RunNotifier NO-UNDO.
    DEFINE VARIABLE test AS NotifierTest NO-UNDO.
    
    runner = NEW OEUnitRunner().
    notifier = NEW RunNotifier().
    notifier:AddRunListener(THIS-OBJECT).
    runner:Notifier = notifier.
    test = NEW NotifierTest().
    runner:RunTest(test).
    results = runner:Results.

    Assert:AreEqual(results:GetStatus(), TestResult:StatusPassed).
    Assert:IsTrue(testFinished).
    
    DELETE OBJECT test NO-ERROR.
    DELETE OBJECT runner NO-ERROR.
  END METHOD. 
 
  METHOD PUBLIC OVERRIDE VOID TestRunStarted(INPUT inf AS ClassInfo):
    Assert:AreEqual(inf:Name, "OEUnit.Tests.Runner.Notification.NotifierTest").
  END METHOD.
  
  METHOD PUBLIC OVERRIDE VOID TestStarted(INPUT inf AS StatementInfo):
    ASSIGN
      runningTest = inf:Name
      testFinished = FALSE.
  END METHOD.

  METHOD PUBLIC OVERRIDE VOID TestIgnored(INPUT res AS TestResult):
    Assert:AreEqual(res:GetName(), "Ignore").
  END METHOD.
  
  METHOD PUBLIC OVERRIDE VOID TestFinished(INPUT res AS TestResult):
    Assert:IsFalse(testFinished).
    Assert:AreEqual(runningTest, res:GetName()).
    Assert:AreEqual(res:GetStatus(), TestResult:StatusPassed).
    ASSIGN 
      testFinished = TRUE.
  END METHOD.
  
  METHOD PUBLIC OVERRIDE VOID TestRunFinished(INPUT classResult AS TestClassResult):
    Assert:AreEqual(classResult:GetName(), "OEUnit.Tests.Runner.Notification.NotifierTest").
    Assert:AreEqual(classResult:GetStatus(), TestResult:StatusPassed).
    Assert:IsTrue(testFinished).
  END METHOD.  
 
END CLASS.