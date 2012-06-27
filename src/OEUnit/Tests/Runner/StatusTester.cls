ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.
USING OEUnit.Runner.*.
USING OEUnit.Tests.Runner.StatusTest.

CLASS OEUnit.Tests.Runner.StatusTester INHERITS OEUnit.Tests.ResultTester:

  CONSTRUCTOR StatusTester():
    SUPER (NEW StatusTest()).
  END CONSTRUCTOR.

  @Test.
  METHOD PUBLIC VOID Passes():
   Assert:AreEqual(CAST(GetMethodResult("Passes"),TestMethodResult):GetStatus(), TestResult:StatusPassed).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID PassesStatic():
   Assert:AreEqual(CAST(GetMethodResult("PassesStatic"),TestMethodResult):GetStatus(), TestResult:StatusPassed).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID PassesWithExpected():
   Assert:AreEqual(CAST(GetMethodResult("PassesWithExpected"),TestMethodResult):GetStatus(), TestResult:StatusPassed).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID Fails():
   Assert:AreEqual(CAST(GetMethodResult("Fails"),TestMethodResult):GetStatus(), TestResult:StatusFailed).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID Ignored():
   Assert:AreEqual(CAST(GetMethodResult("Ignored"),TestMethodResult):GetStatus(), TestResult:StatusIgnored).
  END METHOD.    

  @Test.
  METHOD PUBLIC VOID Errored():
   Assert:AreEqual(CAST(GetMethodResult("Errored"),TestMethodResult):GetStatus(), TestResult:StatusError).
  END METHOD.    
  
 
END CLASS.