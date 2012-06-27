ROUTINE-LEVEL ON ERROR UNDO, THROW.

@Ignore.
CLASS OEUnit.Tests.Reflection.InfoBuilderTest:
  
  @Skipped.
  CONSTRUCTOR InfoBuilderTest():
  END CONSTRUCTOR. 
  
  @Annotation1.
  @Annotation2.
  METHOD PUBLIC VOID SkipAnnotations():
  END METHOD.
  
  @Skipped.
  DEFINE PUBLIC PROPERTY someProperty AS INTEGER NO-UNDO GET. SET.
  
  @Annotation1.
  @Annotation2.
  METHOD PUBLIC VOID SkipAnnotations1():
  END METHOD.
  
  @Skipped.
  DEFINE PUBLIC VARIABLE someVariable AS INTEGER NO-UNDO.
  
  @Annotation1.
  @Annotation2.
  METHOD PUBLIC VOID SkipAnnotations2():
  END METHOD.

  @Skipped.
  DESTRUCTOR InfoBuilderTest():
  END DESTRUCTOR. 
  
  @Annotation1.
  @Annotation2.
  METHOD PUBLIC VOID SkipAnnotations3():
  END METHOD.

  @Skipped.
  FUNCTION someFunction RETURNS VOID IN SESSION:FIRST-CHILD.
  
  @Annotation1.
  @Annotation2.
  METHOD PUBLIC VOID SkipAnnotations4():
  END METHOD.

  @Skipped.
  DEFINE PUBLIC EVENT someEvent VOID (). 
  
  @Annotation1.
  @Annotation2.
  METHOD PUBLIC VOID SkipAnnotations5():
  END METHOD.

  @Annotation6.
  @Annotation7.
  METHOD PUBLIC VOID OverrideMethod():
  END METHOD.

  @Annotation8.
  @Annotation9.
  METHOD PUBLIC STATIC VOID StaticMethod():
  END METHOD.

  /* Methods in include files should also be picked up */
  {OEUnit/Tests/Reflection/InfoBuilderTest.i}

END CLASS.
  