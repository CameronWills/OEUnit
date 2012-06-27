ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.

CLASS OEUnit.Tests.Runner.StatusTest:
  
  @Test.
  METHOD PUBLIC VOID Passes():
  END METHOD.

  @Test.
  METHOD PUBLIC STATIC VOID PassesStatic():
  END METHOD.

  @Test(expected="Progress.Lang.AppError").
  METHOD PUBLIC VOID PassesWithExpected():
    RETURN ERROR NEW Progress.Lang.AppError().
  END METHOD. 
  
  @Test.
  METHOD PUBLIC VOID Fails():
    Assert:Fail().
  END METHOD.

  @Test.
  @Ignore.
  METHOD PUBLIC VOID Ignored():
  END METHOD.

  @Test.
  METHOD PUBLIC VOID Errored():
    RETURN ERROR NEW Progress.Lang.AppError("User Raised Error",0).
  END METHOD.

END CLASS.