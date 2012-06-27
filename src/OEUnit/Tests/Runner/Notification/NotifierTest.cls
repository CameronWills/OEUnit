ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Tests.Runner.Notification.NotifierTest:

  @Test.
  METHOD PUBLIC VOID Pass1():
  END METHOD. 
  
  @Test.
  METHOD PUBLIC VOID Pass2():
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID Pass3():
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID Pass4():
  END METHOD.

  @Test.
  @Ignore.
  METHOD PUBLIC VOID Ignore():
  END METHOD.
  
END CLASS.