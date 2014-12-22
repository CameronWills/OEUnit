/*------------------------------------------------------------------------------
  File        :   OEUnitRunner.cls
  Package     :   OEUnit.Runners
  Description :   The standard OEUnit test runner. By default, this runner will 
                  recursively run all child tests contained in the given test
                  object. To control which child tests are run, enlist a custom 
                  OEUnit.Runner.Manipulation.IFilter object or annotate tests 
                  with the @Ignore annotation. To monitor a test run, enlist an 
                  instance of OEUnit.Runner.Notification.RunNotifier. Custom 
                  runners should inherit from this class.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW. 

USING OEUnit.Assertion.AssertionFailedError.
USING OEUnit.Reflection.*.
USING OEUnit.Runner.*.
USING OEUnit.Runners.OEUnitAnnotations.
USING OEUnit.Util.*.
USING OEUnit.Data.*.
USING Progress.Lang.*.

CLASS OEUnit.Runners.OEUnitRunner INHERITS BaseRunner:

  
  /*----------------------------------------------------------------------------
    The current test being run and its result.
  ----------------------------------------------------------------------------*/   
  DEFINE PROTECTED VARIABLE currentTest AS Object NO-UNDO.
  DEFINE PROTECTED VARIABLE currentInfo AS ClassInfo NO-UNDO.
  DEFINE PROTECTED VARIABLE currentResult AS TestClassResult NO-UNDO. 

  /*----------------------------------------------------------------------------
    The primary entry point for running a test class. Runs the test and stores 
    the results.
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC OVERRIDE VOID RunTest(INPUT test AS Object):
    SetCurrentTest(test).
    Results = currentResult.
    NotifyOfRunStart(currentInfo).
    RunTestClass(?).
    FINALLY:
      NotifyOfRunFinish(Results).    		
    END FINALLY.
  END METHOD.

  /*----------------------------------------------------------------------------
    Runs the current test class - which may be a test suite or test case. The 
    enlisted Filter ultimatley determines whether to run the test class or not. 
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID RunTestClass(INPUT parentResult AS TestClassResult):
    
    IF VALID-OBJECT(parentResult) THEN
      parentResult:AddResult(currentResult).
    
    IF ShouldRun(currentInfo) THEN DO ON ERROR UNDO, THROW:
      IF TYPE-OF(currentTest, TestSuite) THEN
        RunTestSuite().
      ELSE
        RunTestCase().
    END.
    ELSE DO:
      currentResult:SetStatus(TestResult:StatusIgnored).     
      NotifyOfTestIgnored(currentResult).
    END.
    
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Runs all child tests in the current test suite.
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID RunTestSuite():

    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE test AS Object NO-UNDO.
    DEFINE VARIABLE suite AS TestSuite NO-UNDO.
    DEFINE VARIABLE suiteResult AS TestClassResult NO-UNDO.
    
    ASSIGN
      suite = CAST(currentTest,TestSuite)
      suiteResult = currentResult.
    
    DO i = 1 TO suite:TestCount ON ERROR UNDO, THROW:
      test = suite:GetTest(i).
      SetCurrentTest(test).
      RunTestClass(suiteResult).
    END.

  END METHOD.
  
  /*----------------------------------------------------------------------------
    Runs all test methods in the current test case.
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED VOID RunTestCase():
    RunBefores(currentInfo:GetAnnotatedMethods(OEUnitAnnotations:BeforeClass)).
    RunTestMethods(currentInfo:GetAnnotatedMethods(OEUnitAnnotations:Test)).
    CATCH err AS Progress.Lang.Error :
      currentResult:AddError(err).  		
    END CATCH.
    FINALLY:
      RunAfters(currentInfo:GetAnnotatedMethods(OEUnitAnnotations:AfterClass), currentResult).        		
    END FINALLY.
  END METHOD. 

  /*----------------------------------------------------------------------------
    Sets the current test to the given object. The class info is constructed for
    the object. 
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID SetCurrentTest(INPUT test AS OBJECT):
    ASSIGN 
      currentTest = test
      currentInfo = InfoBuilder:BuildClassInfo(test) 
      currentResult = NEW TestClassResult(currentInfo).
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns true if the given test should be run as decided by the Filter.
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED OVERRIDE LOGICAL ShouldRun(INPUT inf AS StatementInfo):
    IF inf:HasAnnotationOfType(OEUnitAnnotations:Ignore) THEN
      RETURN FALSE.
    RETURN SUPER:ShouldRun(inf).
  END METHOD.

  /*----------------------------------------------------------------------------
    Invokes each of the methods in the given list in reverse order. Any 
    Progress.Lang.Errors are thrown to the caller.
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID RunBefores(INPUT methods AS List):
    methods:Reset().
    DO WHILE methods:MovePrevious() ON ERROR UNDO, THROW:
      CAST(methods:Current, MethodInfo):Invoke().
    END.
    FINALLY:
     DELETE OBJECT methods NO-ERROR.   		
    END FINALLY.
  END METHOD.
    
  /*----------------------------------------------------------------------------
    Invokes each of the methods in the given list. Any Progress.Lang.Errors 
    thrown while running the methods are stored in the given test result.
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID RunAfters(INPUT methods AS List, INPUT res AS TestResult):
    methods:Reset().
    DO WHILE methods:MoveNext():
      DO ON ERROR UNDO, THROW:
        CAST(methods:Current, MethodInfo):Invoke().
        CATCH err AS Progress.Lang.Error :
          res:AddError(err).      		
        END CATCH.
      END.
    END.
    FINALLY:
     DELETE OBJECT methods NO-ERROR.   		
    END FINALLY.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Run all of the test methods in the given list.
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID RunTestMethods(INPUT testMethods AS List):
    DEFINE VARIABLE meth AS MethodInfo NO-UNDO.
    testMethods:Reset().
    DO WHILE testMethods:MoveNext():
      meth = CAST(testMethods:Current, MethodInfo).
      IF meth:RequiresDataProvider() THEN
        RunTestMethodWithDataProvider(meth).
      ELSE
      RunTestMethod(meth).
    END.        
    FINALLY:
      DELETE OBJECT testMethods NO-ERROR.   		
    END FINALLY.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Run a single test method multiple times using the given DataProvider.
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID RunTestMethodWithDataProvider(INPUT testMethod AS MethodInfo):
      
    DEFINE VARIABLE dataProvider AS DataProvider NO-UNDO.
    DEFINE VARIABLE moveResult   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE counter      AS INTEGER NO-UNDO INITIAL 0.
    DEFINE VARIABLE methodResult AS TestMethodResult NO-UNDO.
    DEFINE VARIABLE methodName   AS CHARACTER NO-UNDO.
    dataProvider = testMethod:GetDataProvider().
    IF NOT VALID-OBJECT(dataProvider) OR dataProvider = ? THEN
      RETURN ERROR NEW DataProviderError("Invalid DataProvider object returned from method.").
    methodName = testMethod:NAME.
    moveResult = dataProvider:MoveFirst().
    DO WHILE moveResult EQ TRUE:
      counter = counter + 1.
      RunTestMethod(testMethod,dataProvider:GetParameterList()).
      moveResult = dataProvider:MoveNext().
    END.
  END METHOD.

  /*----------------------------------------------------------------------------
    Run a single test method and store it's result. Any Progress.Lang.Errors 
    thrown during the run are caught and stored in the result. 
  ----------------------------------------------------------------------------*/
   METHOD PROTECTED VOID RunTestMethod(INPUT testMethod AS MethodInfo):
     RunTestMethod(INPUT testMethod, INPUT ?).
   END METHOD.
   
   /*----------------------------------------------------------------------------
    Run a single test method with the given parameters and store it's result. If
    a test fixture is required, it will be extracted and a transaction started.
    Any Progress.Lang.Errors thrown during the run are caught and stored in the
    result. 
  ----------------------------------------------------------------------------*/
   METHOD PROTECTED VOID RunTestMethod(INPUT testMethod AS MethodInfo, INPUT paramList AS Progress.Lang.ParameterList):
       
       IF testMethod:RequiresFixture() THEN
       DO:
           OEUnitFixtureTransaction:
           DO TRANSACTION
            ON ERROR UNDO, THROW:
              RunTestMethod(INPUT testMethod, testMethod:GetFixture(), INPUT paramList).
              UNDO OEUnitFixtureTransaction, LEAVE OEUnitFixtureTransaction.
           END.
       END.
       ELSE 
           RunTestMethod(INPUT testMethod, INPUT ?, INPUT paramList).
   END METHOD.
   
   /*----------------------------------------------------------------------------
    Run a single test method with the given fixure object and parameters and 
    store it's result. If a fixture is provided, no transaction is started in
    this method. Any Progress.Lang.Errors thrown during the run are caught and 
    stored in the result. 
  ----------------------------------------------------------------------------*/
   METHOD PROTECTED VOID RunTestMethod(INPUT testMethod AS MethodInfo, INPUT testFixture AS Fixture, INPUT paramList AS Progress.Lang.ParameterList):

    DEFINE VARIABLE methodResult AS TestMethodResult NO-UNDO.
    methodResult = NEW TestMethodResult(testMethod).
    currentResult:AddResult(methodResult).
    IF ShouldRun(testMethod) THEN DO ON ERROR UNDO, THROW:
      NotifyOfTestStart(testMethod).
      IF testFixture NE ? AND VALID-OBJECT(testFixture) AND TRANSACTION THEN testFixture:CreateData().
      ETIME(TRUE).
      RunBefores(currentInfo:GetAnnotatedMethods(OEUnitAnnotations:Before)).
      ETIME(TRUE).
      CallTestMethod(testMethod,paramList).
      methodResult:SetStatus(TestResult:StatusPassed).
      CATCH err AS Progress.Lang.Error :
        methodResult:AddError(err).
      END CATCH.
      FINALLY:
        methodResult:SetDuration(ETIME).
        RunAfters(currentInfo:GetAnnotatedMethods(OEUnitAnnotations:After), methodResult).
        NotifyOfTestFinish(methodResult).
      END FINALLY.
    END.
    ELSE DO:
      methodResult:SetStatus(TestResult:StatusIgnored).
      NotifyOfTestIgnored(methodResult).
    END.
  END METHOD.

  /*----------------------------------------------------------------------------
    Invoke the given test method. If an unexpected Progress.Lang.Error is thrown 
    during the test, the error is re-thrown to the caller. 
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID CallTestMethod(INPUT testMethod AS MethodInfo):
      CallTestMethod(testMethod, ?).
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Invoke the given test method with the given parameter list. If an unexpected 
    Progress.Lang.Error is thrown during the test, the error is re-thrown to the 
    caller. 
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID CallTestMethod(INPUT testMethod AS MethodInfo, INPUT paramList AS Progress.Lang.ParameterList):

    DEFINE VARIABLE testAnnotation AS AnnotationInfo NO-UNDO.
        
    testAnnotation = testMethod:GetAnnotationOfType(OEUnitAnnotations:Test).
    IF paramList NE ? AND VALID-OBJECT(paramList) THEN   
        testMethod:Invoke(paramList).
    ELSE
        testMethod:Invoke().
    IF testAnnotation:HasAttribute(OEUnitAnnotations:TestExpectedAttribute) THEN
      RETURN ERROR NEW AssertionFailedError("Expecting error: " + testAnnotation:GetAttribute(OEUnitAnnotations:TestExpectedAttribute)).
    
    CATCH assertionFailed AS AssertionFailedError:
      RETURN ERROR assertionFailed.
    END CATCH.
    
    CATCH err AS Progress.Lang.Error:
      IF testAnnotation:HasAttribute(OEUnitAnnotations:TestExpectedAttribute) THEN DO:
        IF IsTypeOf(err, GetExpectedErrorType(testAnnotation:GetAttribute(OEUnitAnnotations:TestExpectedAttribute))) THEN DO:
          DELETE OBJECT err NO-ERROR.
        END.
        ELSE
          RETURN ERROR err.
      END.
      ELSE 
        RETURN ERROR err.
    END CATCH.
  END METHOD.


  /*----------------------------------------------------------------------------
    Expands the given expected error type if it is a recognised abbreviation.
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED STATIC CHARACTER GetExpectedErrorType(INPUT expectedError AS CHARACTER):
    
    /* Add the package name for recognised error types */
    CASE expectedError:
      WHEN "Error"           
        OR WHEN "AppError"
        OR WHEN "ProError"
        OR WHEN "SoapFaultError"          
        OR WHEN "SysError" THEN
        RETURN "Progress.Lang." + expectedError.
      WHEN "Stop" THEN
        RETURN "OEUnit.Reflection.StopConditionError".
      WHEN "Quit" THEN
        RETURN "OEUnit.Reflection.QuitConditionError".
    END CASE.
    RETURN expectedError.    
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns true if the given object can be cast to the given type. Otherwise 
    returns false.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC LOGICAL IsTypeOf(INPUT obj AS Progress.Lang.Object, 
                                        INPUT castType AS CHARACTER):

    IF obj:GetClass():TypeName = castType THEN RETURN TRUE.
    DEFINE VARIABLE castObject AS Progress.Lang.Object NO-UNDO.
    castObject = DYNAMIC-CAST(obj, castType) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      RETURN FALSE.
    RETURN TRUE.
  END METHOD.

END CLASS.