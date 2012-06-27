ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Tests.Reflection.MethodInfoTest:

  @Test(expected="Stop").
  METHOD PUBLIC VOID Stop():
    STOP.
  END METHOD.

  @Test(expected="Quit").
  METHOD PUBLIC VOID Quit():
    QUIT.
  END METHOD.

  @Test(expected="SysError").
  METHOD PUBLIC VOID MethodWithParams(INPUT param1 AS INTEGER):
  END METHOD.

END CLASS.
  