ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.

CLASS OEUnit.Tests.Assertion.FailMessageTest :

  @Test.
  METHOD PUBLIC VOID FailNoMessage():
    Assert:AreEqual(12,14).
  END METHOD. 
  
  @Test.
  METHOD PUBLIC VOID FailWithMessage():
    Assert:AreEqual(12,14, "Specific Fail Message").
  END METHOD.
 
  @Test.
  METHOD PUBLIC VOID ReturnError():
    RETURN ERROR "Return Error Message". 
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID AppErrorReturned():
    RETURN ERROR NEW Progress.Lang.AppError("Returned AppError Message"). 
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID AppErrorMessage():
    RETURN ERROR NEW Progress.Lang.AppError("Error AppError Message",0). 
  END METHOD.
 
END CLASS.