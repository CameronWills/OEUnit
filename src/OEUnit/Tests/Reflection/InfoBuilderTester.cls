ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.
USING OEUnit.Annotations.*.
USING OEUnit.Reflection.*.
USING OEUnit.Tests.Reflection.*.

CLASS OEUnit.Tests.Reflection.InfoBuilderTester:
  
  DEFINE VARIABLE classInf AS ClassInfo NO-UNDO.
  DEFINE VARIABLE classObj AS Progress.Lang.Object NO-UNDO.
  DEFINE VARIABLE methodInf AS MethodInfo NO-UNDO. 
  
  @BeforeClass.
  METHOD PUBLIC VOID GetClassInfo():
    classObj = NEW InfoBuilderInheritsTest().
    classInf = InfoBuilder:BuildClassInfo(classObj).
    FINALLY:
      DELETE OBJECT classObj NO-ERROR.
    END FINALLY.
  END METHOD.
  
  @AfterClass.
  METHOD PUBLIC VOID DeleteInfo():
  	DELETE OBJECT classInf NO-ERROR.
  END METHOD. 

  @Test.
  METHOD PUBLIC VOID SkipStatements():
    VerifyAnnotationsSkipped(classInf:GetMethodNamed("SkipAnnotations")).
    VerifyAnnotationsSkipped(classInf:GetMethodNamed("SkipAnnotations1")).
    VerifyAnnotationsSkipped(classInf:GetMethodNamed("SkipAnnotations2")).
    VerifyAnnotationsSkipped(classInf:GetMethodNamed("SkipAnnotations3")).
    VerifyAnnotationsSkipped(classInf:GetMethodNamed("SkipAnnotations4")).
    VerifyAnnotationsSkipped(classInf:GetMethodNamed("SkipAnnotations5")).
  END METHOD.
  
  METHOD PRIVATE VOID VerifyAnnotationsSkipped(INPUT inf AS MethodInfo):
    Assert:AreEqual(inf:Annotations:Size, 2).
    Assert:IsTrue(inf:HasAnnotationOfType("Annotation1")).
    Assert:IsTrue(inf:HasAnnotationOfType("Annotation2")).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID OverridenMethod():
    methodInf = classInf:GetMethodNamed("OverrideMethod").
    Assert:AreEqual(methodInf:Annotations:Size, 1).
    Assert:IsTrue(methodInf:HasAnnotationOfType("Annotation11")).
    Assert:IsFalse(methodInf:IsStatic).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID StaticMethod():
    methodInf = classInf:GetMethodNamed("StaticMethod").
    Assert:AreEqual(methodInf:Annotations:Size, 2).
    Assert:IsTrue(methodInf:HasAnnotationOfType("Annotation8")).
    Assert:IsTrue(methodInf:HasAnnotationOfType("Annotation9")).
    Assert:IsTrue(methodInf:IsStatic).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID IncludeMethod():
    methodInf = classInf:GetMethodNamed("IncludeMethod").
    Assert:AreEqual(methodInf:Annotations:Size, 1).
    Assert:IsTrue(methodInf:HasAnnotationOfType("Annotation10")).
    Assert:IsFalse(methodInf:IsStatic).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID AnotherMethod():
    methodInf = classInf:GetMethodNamed("AnotherMethod").
    Assert:AreEqual(methodInf:Annotations:Size, 1).
    Assert:IsTrue(methodInf:HasAnnotationOfType("Annotation12")).
    Assert:IsFalse(methodInf:IsStatic).
  END METHOD.
  
END CLASS.
  