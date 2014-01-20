/*------------------------------------------------------------------------------
  File        :   MethodInfo.cls
  Package     :   OEUnit.Reflection
  Description :   Stores information about a METHOD statement appearing in a class 
                  definition (e.g. a test method).
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.*.
USING OEUnit.Util.*.

CLASS OEUnit.Reflection.MethodInfo INHERITS StatementInfo:

  /*----------------------------------------------------------------------------
    Is the method static. 
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY IsStatic AS LOGICAL NO-UNDO GET. PRIVATE SET.

  /*----------------------------------------------------------------------------
    Class where this method is defined.
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY ParentInfo AS ClassInfo NO-UNDO GET. PRIVATE SET.
  
  /*----------------------------------------------------------------------------
    Constructor. Accepts the method name, a list of annotations preceding the 
    method, the parent class and whether the method is static.
  ----------------------------------------------------------------------------*/  
  CONSTRUCTOR MethodInfo(INPUT methodName AS CHARACTER,
                         INPUT annots AS List,
                         INPUT parentInf AS ClassInfo,
                         INPUT staticMethod AS LOGICAL):
    SUPER(methodName, annots).
    ASSIGN
      IsStatic = staticMethod
      ParentInfo = parentInf.
  END CONSTRUCTOR.
 
  /*----------------------------------------------------------------------------
    Invoke the method being represented. The method being called must be a 
    public method accepting no parameters. A StopConditionError will be thrown 
    if the method cannot be found.
  ----------------------------------------------------------------------------*/ 
  METHOD PUBLIC VOID Invoke():
    DO ON ERROR UNDO, THROW
       ON QUIT UNDO, RETURN ERROR NEW QuitConditionError("Quit condition occured") 
       ON STOP UNDO, LEAVE:
      IF IsStatic THEN
        DYNAMIC-INVOKE(parentInfo:Name, Name).
      ELSE
        DYNAMIC-INVOKE(parentInfo:ClassInstance, Name).
      
      RETURN.  
    END.
    
    /* HACK: To accomodate bug in OpenEdge runtime, crashes when returning custom 
             error instances on stop conditions */
    RETURN ERROR NEW StopConditionError("Stop condition occured").
    
  END METHOD.
 
END CLASS.

