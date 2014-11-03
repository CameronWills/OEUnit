/*------------------------------------------------------------------------------
  File        : AnnotationInfo.cls
  Package     : OEUnit.Reflection
  Description : Represents a single ABL Annotation (e.g. @Test) appearing in a 
                class definition. For Example:
                  @Test.
                  @AfterClass.

------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Util.NamedList.

CLASS OEUnit.Reflection.AnnotationInfo:
  
  /*----------------------------------------------------------------------------
    Parts of the annotation. Defined by the syntax below: 
      AnnotationType(AttributeName=AttributeValue)
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY AnnotationType AS CHARACTER NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AttributeName AS CHARACTER NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AttributeValue AS CHARACTER NO-UNDO GET. PRIVATE SET.
  
  /*----------------------------------------------------------------------------
    Annotation attributes. Defined by the syntax below: 
      AnnotationType(AttributeName=AttributeValue,AttributeName=AttributeValue)
  ----------------------------------------------------------------------------*/ 
  DEFINE PUBLIC PROPERTY Attributes AS NamedList NO-UNDO
    GET.
    PROTECTED SET. 
  
  /*----------------------------------------------------------------------------
    Constructor. Accepts the annotation string. Some input examples:
      Test
      Test(expected=Progress.Lang.Error)
      Test(expected=Progress.Lang.Error,dataProvider=GenerateTestMatrix)
      Ignore
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR PUBLIC AnnotationInfo(INPUT annotationString AS CHARACTER):
    
    DEFINE VARIABLE attribute AS CHARACTER NO-UNDO.
    ASSIGN
      AnnotationType = ENTRY(1, annotationString, "(")
      attribute = TRIM(SUBSTRING(annotationString, LENGTH(AnnotationType) + 1),"()")
      AttributeName = ENTRY(1, attribute, "=")
      AttributeValue = SUBSTRING(attribute, LENGTH(AttributeName) + 2).
      
    Attributes = NEW NamedList().
    IF attribute NE "" AND attribute NE ? THEN ParseAttributes(attribute).
  END CONSTRUCTOR.
  
  DESTRUCTOR AnnotationInfo():
    DELETE OBJECT Attributes NO-ERROR.
  END DESTRUCTOR.
  
  METHOD PROTECTED VOID ParseAttributes(INPUT attribs AS CHARACTER):
      
      DEFINE VARIABLE startPos    AS INTEGER   NO-UNDO INITIAL 1.
      DEFINE VARIABLE endPos      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE attribName  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE attribValue AS CHARACTER NO-UNDO.
      
      attribLoop:
      DO WHILE startPos < LENGTH(attribs):
          endPos     = INDEX(attribs,"=", startPos).
          IF(endPos = 0) THEN LEAVE attribLoop.
          attribName = TRIM(SUBSTRING(attribs, startPos, endPos  - startPos)).
          startPos   = endPos + 1.
          endPos = INDEX(attribs, ",", startPos).
          IF(endPos = 0) THEN endPos = LENGTH(attribs) + 1.
          attribValue = TRIM(SUBSTRING(attribs, startPos, endPos - startPos)). 
          startPos = endPos + 1.
          Attributes:Add(attribName, attribValue). 
      END.
  END METHOD.
  
  METHOD PUBLIC LOGICAL HasAttribute(INPUT attribName AS CHARACTER):
      RETURN Attributes:HasValue(INPUT attribName).
  END METHOD.
  
  METHOD PUBLIC CHARACTER GetAttribute(INPUT attribName AS CHARACTER):
      IF HasAttribute(attribName) = TRUE THEN RETURN Attributes:Get(attribName).
      ELSE RETURN ?.
  END METHOD.

END CLASS.