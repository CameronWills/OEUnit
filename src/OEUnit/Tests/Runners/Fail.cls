ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.

CLASS OEUnit.Tests.Runners.Fail:
  
  @Test.
  METHOD PUBLIC VOID Failed():
    Assert:Fail().
  END METHOD. 
  
END CLASS.