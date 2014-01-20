/*------------------------------------------------------------------------------
  File        :   StatementInfo.cls
  Package     :   OEUnit.Reflection
  Description :   Represents a single ABL statement appearing in a class 
                  definition (e.g. a CLASS or METHOD statement). Stores 
                  the name and list of annotations. Annotations must precede 
                  the statement to be included.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.AnnotationInfo.
USING OEUnit.Util.List.

CLASS OEUnit.Reflection.StatementInfo ABSTRACT:
  
  /*----------------------------------------------------------------------------
    Name given in the statement
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET. 

  /*----------------------------------------------------------------------------
    Annotations preceding the statement
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY Annotations AS List NO-UNDO
    GET.
    SET. 

  /*----------------------------------------------------------------------------
    Accepts the name given in the statement 
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR PROTECTED StatementInfo(INPUT nam AS CHARACTER):
    Name = nam.
  END CONSTRUCTOR.

  /*----------------------------------------------------------------------------
    Accepts the name given in the statement and the list of annotations 
    preceding it. 
  ----------------------------------------------------------------------------*/   
  CONSTRUCTOR PROTECTED StatementInfo(INPUT nam AS CHARACTER, INPUT annots AS List):
    ASSIGN
      Name = nam
      Annotations = annots.
  END CONSTRUCTOR.
  
  DESTRUCTOR StatementInfo():
    DELETE OBJECT Annotations NO-ERROR.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Returns true if Annotations contains an Annotation object of the given type. 
    Otherwise returns false.
  ----------------------------------------------------------------------------*/ 
  METHOD PUBLIC LOGICAL HasAnnotationOfType(INPUT annotationType AS CHARACTER):
    IF VALID-OBJECT(Annotations) THEN DO: 
      Annotations:Reset().
      DO WHILE Annotations:MoveNext():
        IF CAST(Annotations:Current, AnnotationInfo):AnnotationType = annotationType THEN
          RETURN TRUE.
      END.
    END.
    RETURN FALSE.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the first AnnotationInfo object found having the given type. Returns ? 
    if no matching Annotation is found.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC AnnotationInfo GetAnnotationOfType(INPUT annotationType AS CHARACTER):
    DEFINE VARIABLE annotation AS AnnotationInfo NO-UNDO.
    
    IF VALID-OBJECT(Annotations) THEN DO:    
      Annotations:Reset().
      DO WHILE Annotations:MoveNext():
        annotation = CAST(Annotations:Current, AnnotationInfo).
        IF annotation:AnnotationType = annotationType THEN RETURN annotation.
      END.
    END.
    RETURN ?.
  END METHOD.
 
END CLASS.