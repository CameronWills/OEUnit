ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Tests.Reflection.InfoBuilderTest.

CLASS OEUnit.Tests.Reflection.InfoBuilderInheritsTest INHERITS InfoBuilderTest:
  
  @Annotation11.
  METHOD PUBLIC OVERRIDE VOID OverrideMethod():
  END METHOD.

  @Annotation12.
  METHOD PUBLIC VOID AnotherMethod():
  END METHOD.

END CLASS.
  