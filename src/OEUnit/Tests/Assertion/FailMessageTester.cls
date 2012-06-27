ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.*.
USING OEUnit.Runner.*.
USING OEUnit.Tests.Assertion.*.

CLASS OEUnit.Tests.Assertion.FailMessageTester INHERITS OEUnit.Tests.ResultTester:

  CONSTRUCTOR FailMessageTester():
    SUPER (NEW FailMessageTest()).
  END CONSTRUCTOR.
  
  @Test.
  METHOD PUBLIC VOID FailNoMessage():
    Assert:AreEqual(CAST(GetMethodResult("FailNoMessage"), TestMethodResult):GetMessage(), "Expecting: 14 but found: 12").
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID FailWithMessage():
    Assert:AreEqual(CAST(GetMethodResult("FailWithMessage"), TestMethodResult):GetMessage(), "Specific Fail Message").
  END METHOD.   

  @Test.
  METHOD PUBLIC VOID ReturnError():
    Assert:AreEqual(CAST(GetMethodResult("ReturnError"), TestMethodResult):GetMessage(), "Return Error Message").
  END METHOD.   
  
  @Test.
  METHOD PUBLIC VOID AppErrorReturned():
    Assert:AreEqual(CAST(GetMethodResult("AppErrorReturned"), TestMethodResult):GetMessage(), "Returned AppError Message").
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID AppErrorMessage():
    Assert:AreEqual(CAST(GetMethodResult("AppErrorMessage"), TestMethodResult):GetMessage(), "Error AppError Message").    
  END METHOD.
  
  
    
END CLASS.