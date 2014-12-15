/*------------------------------------------------------------------------------
  File        :   MethodInfo.cls
  Package     :   OEUnit.Reflection
  Description :   Stores information about a METHOD statement appearing in a class 
                  definition (e.g. a test method).
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.*.
USING OEUnit.Util.*.
USING OEUnit.Runners.*.
USING OEUnit.Data.*.

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
 
  /*----------------------------------------------------------------------------
    Invoke the method being represented, with a parameter list. The method being
    called must be a public method accepting the same parameters as are listed
    in params. A StopConditionError will be thrown if the method cannot be found.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Invoke(INPUT params AS Progress.Lang.ParameterList):
    DEFINE VARIABLE classInf AS Progress.Lang.Class NO-UNDO.
    DO ON ERROR UNDO, THROW
       ON QUIT UNDO, RETURN ERROR NEW QuitConditionError("Quit condition occured")
       ON STOP UNDO, LEAVE:
      classInf = Progress.Lang.Class:GetClass(parentInfo:Name).
      IF IsStatic THEN
        classInf:Invoke(Name, params).
      ELSE
        classInf:Invoke(parentInfo:ClassInstance, Name, params).

      RETURN.
    END.

    /* HACK: To accomodate bug in OpenEdge runtime, crashes when returning custom
             error instances on stop conditions */
    RETURN ERROR NEW StopConditionError("Stop condition occured").
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns another MethodInfo object which holds the information regarding the
    data provider method for this MethodInfo.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC MethodInfo GetDataProviderInfo():

    DEFINE VARIABLE annotation             AS AnnotationInfo NO-UNDO.
    DEFINE VARIABLE dataProviderAnnotation AS AnnotationInfo NO-UNDO.
    DEFINE VARIABLE dataProviders          AS List           NO-UNDO.
    DEFINE VARIABLE dataProviderName       AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE dataProvider           AS MethodInfo     NO-UNDO.
    DEFINE VARIABLE result                 AS LOGICAL        NO-UNDO INITIAL FALSE.

    /* If this is a @Test, with a "DataProvider=" attribute, then attempt to
     * find the data provider method
     */
    annotation = GetAnnotationOfType(OEUnitAnnotations:Test).

    IF annotation NE ? AND annotation:HasAttribute("dataProvider") THEN
    DO:
      dataProviderName = annotation:Attributes:Get(OEUnitAnnotations:TestDataProviderAttribute).
      dataProviders = parentInfo:GetAnnotatedMethods(OEUnitAnnotations:DataProvider).

      /* Find first data provider with that name attribute or method name */
      dataProviders:MoveFirst().
      dataProviderLoop:
      DO WHILE dataProviders:CURRENT NE ?:
        dataProvider = CAST(dataProviders:CURRENT,"MethodInfo").
        dataProviderAnnotation = dataProvider:GetAnnotationOfType(OEUnitAnnotations:DataProvider).
        IF (dataProviderAnnotation:HasAttribute("name") = TRUE) THEN
        DO:
          IF dataProviderName = dataProviderAnnotation:Attributes:Get(OEUnitAnnotations:DataProviderNameAttribute) THEN
            LEAVE dataProviderLoop.
        END.
        ELSE IF dataProvider:Name = dataProviderName THEN LEAVE dataProviderLoop.
        ELSE dataProvider = ?.
        dataProviders:MoveNext().
      END.
    END.

    RETURN dataProvider.
  END METHOD.

  /*----------------------------------------------------------------------------
    Determines whether this method has a @Test annotation with a DataProvider
    attribute, and a method with a matching @DataProvider attribute exists.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL RequiresDataProvider():
    RETURN (GetDataProviderInfo() NE ?).
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns a DataProvider object from the invocation of a DataProvider method.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC DataProvider GetDataProvider():
    DEFINE VARIABLE dataProvMethod AS MethodInfo NO-UNDO.
    DEFINE VARIABLE dataProvider   AS DataProvider NO-UNDO.
    DO ON ERROR UNDO, THROW
       ON QUIT UNDO, RETURN ERROR NEW QuitConditionError("Quit condition occured")
       ON STOP UNDO, LEAVE:
      dataProvMethod = GetDataProviderInfo().
      IF dataProvMethod NE ? AND VALID-OBJECT(dataProvMethod) THEN
      DO:
          IF dataProvMethod:IsStatic THEN
            dataProvider = DYNAMIC-INVOKE(dataProvMethod:ParentInfo:Name, dataProvMethod:Name).
          ELSE
            dataProvider = DYNAMIC-INVOKE(dataProvMethod:ParentInfo:ClassInstance, dataProvMethod:Name).
      END.
      
      RETURN dataProvider.
    END.

    /* HACK: To accomodate bug in OpenEdge runtime, crashes when returning custom
             error instances on stop conditions */
    RETURN ERROR NEW StopConditionError("Stop condition occured").
  END METHOD.

END CLASS.

