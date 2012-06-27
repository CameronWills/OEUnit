ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.*.

CLASS OEUnit.Tests.Runners.BlockingCodeTest:

  @Test.
  METHOD PUBLIC VOID BlockUsingUpdate():
    DEFINE VARIABLE someVar AS CHARACTER NO-UNDO.
    MESSAGE "Enter anything" UPDATE someVar.
  END METHOD. 
 
END CLASS.