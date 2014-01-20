/*------------------------------------------------------------------------------
  File        :   ClassInfo.cls
  Package     :   OEUnit.Reflection
  Description :   Stores information about an ABL class definition (typically a 
                  test case or test suite class).
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.*.
USING OEUnit.Util.List.
USING Progress.Lang.Object.

CLASS OEUnit.Reflection.ClassInfo INHERITS StatementInfo:
  
  /*----------------------------------------------------------------------------
    A list of the methods defined in the class.
  ----------------------------------------------------------------------------*/
  DEFINE PROTECTED VARIABLE methods AS List NO-UNDO.
  
  /*----------------------------------------------------------------------------
    An instance of the class being represented.
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY ClassInstance AS Object NO-UNDO GET. PRIVATE SET.
  
  /*----------------------------------------------------------------------------
    Constructor. Accepts the class type name and an instance of the class being 
    represented.
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR ClassInfo(INPUT className AS CHARACTER, INPUT instance AS Object):
    SUPER(className).
    ASSIGN 
      classInstance = instance
      methods = NEW List(TRUE).
  END CONSTRUCTOR.

  DESTRUCTOR ClassInfo():
    DELETE OBJECT methods NO-ERROR.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Add a method to the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID AddMethod(INPUT inf AS MethodInfo):
    methods:Add(inf).
  END METHOD.

  /*----------------------------------------------------------------------------
    Get the method at the given position in the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC MethodInfo GetMethod(INPUT indx AS INTEGER):
    RETURN CAST(methods:Get(indx), MethodInfo).
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the first method having the given name. Returns ? if not found.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC MethodInfo GetMethodNamed(INPUT methodName AS CHARACTER):
    DEFINE VARIABLE inf AS MethodInfo NO-UNDO.
    methods:Reset().
    DO WHILE methods:MoveNext():
      inf = CAST(methods:Current, MethodInfo).
      IF inf:Name = methodName THEN
        RETURN inf.
    END.
    RETURN ?.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns a list of those methods that have an annotation of the given type.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC List GetAnnotatedMethods(INPUT annotationType AS CHARACTER):
    DEFINE VARIABLE annotatedMethods AS List NO-UNDO.
    annotatedMethods = NEW List(FALSE).
    methods:Reset().
    DO WHILE methods:MoveNext():
      IF CAST(methods:Current, MethodInfo):HasAnnotationOfType(annotationType) THEN
        annotatedMethods:Add(methods:Current).
    END.
    RETURN annotatedMethods.
  END METHOD.
  
END CLASS.  