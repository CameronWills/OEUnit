/*------------------------------------------------------------------------------
  File        : AnnotationInfo.cls
  Package     : OEUnit.Reflection
  Description : Represents a single ABL Annotation (e.g. @Test) appearing in a 
                class definition. For Example:
                  @Test.
                  @AfterClass.

  Author      : Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Reflection.AnnotationInfo:
  
  /*----------------------------------------------------------------------------
    Parts of the annotation. Defined by the syntax below: 
      AnnotationType(AttributeName=AttributeValue)
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY AnnotationType AS CHARACTER NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AttributeName AS CHARACTER NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AttributeValue AS CHARACTER NO-UNDO GET. PRIVATE SET.
  
  /*----------------------------------------------------------------------------
    Constructor. Accepts the annotation string. Some input examples:
      Test
      Test(expected=Progress.Lang.Error)
      Ignore
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR PUBLIC AnnotationInfo(INPUT annotationString AS CHARACTER):
    
    DEFINE VARIABLE attribute AS CHARACTER NO-UNDO.
    ASSIGN
      AnnotationType = ENTRY(1, annotationString, "(")
      attribute = TRIM(SUBSTRING(annotationString, LENGTH(AnnotationType) + 1),"()")
      AttributeName = ENTRY(1, attribute, "=")
      AttributeValue = SUBSTRING(attribute, LENGTH(AttributeName) + 2).
  END CONSTRUCTOR.

END CLASS.