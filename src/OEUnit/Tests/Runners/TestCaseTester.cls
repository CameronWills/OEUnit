ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.
USING OEUnit.Assertion.AssertString.
USING OEUnit.Runner.*.
USING OEUnit.Tests.Runners.*.

CLASS OEUnit.Tests.Runners.TestCaseTester INHERITS OEUnit.Tests.ResultTester:

  DEFINE VARIABLE methodResult AS TestmethodResult NO-UNDO.
  DEFINE VARIABLE errorCase AS TestCase NO-UNDO.
  
  @After.
  METHOD PUBLIC VOID DeleteErrorInstance():
    DELETE OBJECT errorCase NO-ERROR.   
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID ErrorInBefore():
    errorCase = NEW TestCase("Before,Test").
    RunTest(errorCase).
    methodResult = GetmethodResult("Test").
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusError).
    Assert:AreEqual(methodResult:GetErrors():SIZE,1).
    Assert:IsTrue(errorCase:After1Called).
    Assert:IsTrue(errorCase:After2Called).
    Assert:IsFalse(errorCase:TestCalled).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ErrorInBeforeClass():
    errorCase = NEW TestCase("BeforeClass,Test").
    RunTest(errorCase).
    methodResult = GetmethodResult("Test").
    Assert:IsNull(methodResult).
    Assert:IsTrue(results:GetErrors():SIZE = 1).
    Assert:AreEqual(results:GetStatus(), TestResult:StatusError).
    Assert:IsTrue(errorCase:BeforeClassCalled).
    Assert:IsFalse(errorCase:TestCalled).
    Assert:IsTrue(errorCase:AfterClass1Called).
    Assert:IsTrue(errorCase:AfterClass2Called).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ErrorInTest():
    errorCase = NEW TestCase("Test").
    RunTest(errorCase).
    methodResult = GetmethodResult("Test").
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusError).
    Assert:AreEqual(methodResult:GetErrors():SIZE,1).
    Assert:IsTrue(errorCase:After1Called).
    Assert:IsTrue(errorCase:After2Called).
    Assert:IsTrue(errorCase:BeforeCalled).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ErrorsInAfters():
    errorCase = NEW TestCase("After").
    RunTest(errorCase).
    methodResult = GetmethodResult("Test").
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusError).
    Assert:AreEqual(methodResult:GetErrors():SIZE,2).
    Assert:IsTrue(errorCase:BeforeCalled).
    Assert:IsTrue(errorCase:TestCalled).
    Assert:IsTrue(errorCase:After1Called).
    Assert:IsTrue(errorCase:After2Called).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ErrorsInAfterClasses():
    errorCase = NEW TestCase("AfterClass").
    RunTest(errorCase).
    methodResult = GetmethodResult("Test").
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusPassed).
    Assert:AreEqual(methodResult:GetErrors():SIZE,0).
    Assert:AreEqual(results:GetErrors():SIZE,2).
    Assert:IsTrue(errorCase:TestCalled).
    Assert:IsTrue(errorCase:After1Called).
    Assert:IsTrue(errorCase:After2Called).
    Assert:IsTrue(errorCase:AfterClass1Called).
    Assert:IsTrue(errorCase:AfterClass2Called).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ErrorInBeforeTestAndAfter():
    errorCase = NEW TestCase("Before,Test,After").
    RunTest(errorCase).
    methodResult = GetmethodResult("Test").
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusError).
    Assert:AreEqual(methodResult:GetErrors():SIZE,3).
    Assert:IsTrue(errorCase:BeforeCalled).
    Assert:IsFalse(errorCase:TestCalled).
    Assert:IsTrue(errorCase:After1Called).
    Assert:IsTrue(errorCase:After2Called).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID InheritedTestCase():
    DEFINE VARIABLE inheritedCase AS InheritedTestCase NO-UNDO.
    inheritedCase = NEW InheritedTestCase().
    RunTest(inheritedCase).
    methodResult = GetmethodResult("Test").
    Assert:AreEqual(methodResult:GetStatus(), TestResult:StatusPassed).
    
    Assert:IsTrue(inheritedCase:BeforeClassCalled).
    Assert:IsTrue(inheritedCase:SecondBeforeClassCalled).
    Assert:IsTrue(inheritedCase:WasRunBefore("BeforeClass", "SecondBeforeClass")).
    Assert:IsFalse(inheritedCase:WasRunBefore("SecondBeforeClass", "BeforeClass")).
    
    Assert:IsTrue(inheritedCase:BeforeCalled).
    Assert:IsTrue(inheritedCase:SecondBeforeCalled).
    Assert:IsTrue(inheritedCase:WasRunBefore("Before", "SecondBefore")).
    
    Assert:IsFalse(inheritedCase:TestCalled).
    Assert:IsTrue(inheritedCase:OverridedTestCalled).
    Assert:IsTrue(inheritedCase:SecondTestCalled).
    
    Assert:IsTrue(inheritedCase:After1Called).
    Assert:IsTrue(inheritedCase:After2Called).
    Assert:IsTrue(inheritedCase:SecondAfterCalled).
    Assert:IsTrue(inheritedCase:WasRunBefore("SecondAfter", "After1")).
    Assert:IsTrue(inheritedCase:WasRunBefore("SecondAfter", "After2")).

    Assert:IsTrue(inheritedCase:AfterClass1Called).
    Assert:IsTrue(inheritedCase:AfterClass2Called).
    Assert:IsTrue(inheritedCase:SecondAfterClassCalled).
    Assert:IsTrue(inheritedCase:WasRunBefore("SecondAfterClass", "AfterClass1")).
    Assert:IsTrue(inheritedCase:WasRunBefore("SecondAfterClass", "AfterClass2")).    
    
  END METHOD.

  @Test.
  METHOD PUBLIC VOID IgnoredCase():
    RunTest(NEW IgnoredCase()).
    Assert:IsNotNull(Results).
    Assert:AreEqual(Results:ResultCount, 0).
  END METHOD.  

  @Test.
  METHOD PUBLIC VOID IgnoredSuite():
    RunTest(NEW IgnoredSuite()).
    Assert:IsNotNull(Results).
    Assert:AreEqual(Results:ResultCount, 0).
  END METHOD.  
  
END CLASS.