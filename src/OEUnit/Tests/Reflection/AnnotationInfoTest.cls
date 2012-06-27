ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.
USING OEUnit.Reflection.AnnotationInfo.

CLASS OEUnit.Tests.Reflection.AnnotationInfoTest:
  
  DEFINE VARIABLE annotation AS AnnotationInfo NO-UNDO.
  
  @After.
  METHOD PUBLIC VOID DeleteAnnotation():
    DELETE OBJECT annotation NO-ERROR.
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID ParseAnnotationNoAttribute():
    annotation = NEW AnnotationInfo("Annotation").
    Assert:AreEqual(annotation:AnnotationType, "Annotation").
    Assert:AreEqual(annotation:AttributeName, "").
    Assert:AreEqual(annotation:AttributeValue, "").
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ParseAnnotationWithAttribute():
    annotation = NEW AnnotationInfo("Annotation(Attribute=Value)").
    Assert:AreEqual(annotation:AnnotationType, "Annotation").
    Assert:AreEqual(annotation:AttributeName, "Attribute").
    Assert:AreEqual(annotation:AttributeValue, "Value").
  END METHOD.  
  
  @Test.
  METHOD PUBLIC VOID ParseAnnotationComplexAttribute():
    annotation = NEW AnnotationInfo("Annotation(a=5+1=6)").
    Assert:AreEqual(annotation:AnnotationType, "Annotation").
    Assert:AreEqual(annotation:AttributeName, "a").
    Assert:AreEqual(annotation:AttributeValue, "5+1=6").
  END METHOD.  
  
END CLASS.