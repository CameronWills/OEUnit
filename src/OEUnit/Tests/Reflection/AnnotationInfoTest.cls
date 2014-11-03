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
    Assert:AreEqual(annotation:Attributes:Size, 1).
    Assert:IsTrue(annotation:HasAttribute("Attribute")).
    Assert:AreEqual(annotation:GetAttribute("Attribute"), "Value").
  END METHOD.  
  
  @Test.
  METHOD PUBLIC VOID ParseAnnotationComplexAttribute():
    annotation = NEW AnnotationInfo("Annotation(a=5+1=6)").
    Assert:AreEqual(annotation:AnnotationType, "Annotation").
    Assert:AreEqual(annotation:AttributeName, "a").
    Assert:AreEqual(annotation:AttributeValue, "5+1=6").
    Assert:AreEqual(annotation:Attributes:Size, 1).
    Assert:IsTrue(annotation:HasAttribute("a")).
    Assert:AreEqual(annotation:GetAttribute("a"), "5+1=6").
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID ParseAnnotationMultipleAttributes():
    annotation = NEW AnnotationInfo("Annotation(Attribute1=Value1,Attribute2=Value2,Attribute3=Value3)").
    Assert:AreEqual(annotation:Attributes:Size, 3).
    Assert:IsTrue(annotation:HasAttribute("Attribute1")).
    Assert:IsTrue(annotation:HasAttribute("Attribute2")).
    Assert:IsTrue(annotation:HasAttribute("Attribute3")).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID AnnotationHasAttribute():
    annotation = NEW AnnotationInfo("Annotation(Attribute=Value)").
    Assert:IsTrue(annotation:HasAttribute("Attribute")).
    Assert:IsFalse(annotation:HasAttribute("DoesNotExist")).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID AnnotationGetAttribute():
    annotation = NEW AnnotationInfo("Annotation(Attribute1=Value1,Attribute2=Value2,Attribute3=Value3)").
    Assert:AreEqual(annotation:GetAttribute("Attribute1"), "Value1").
    Assert:AreEqual(annotation:GetAttribute("Attribute2"), "Value2").
    Assert:AreEqual(annotation:GetAttribute("Attribute3"), "Value3").
    Assert:IsNull(annotation:GetAttribute("Attribute4")).
  END METHOD.
  
END CLASS.