ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.

@Ignore.
CLASS OEUnit.Tests.Runners.IgnoredCase:
  
  @Test. /* Will not be run as case has Ignore annotation */
  METHOD PUBLIC VOID DoesntGetCalled():
    Assert:Fail().
  END METHOD. 
  
END CLASS.