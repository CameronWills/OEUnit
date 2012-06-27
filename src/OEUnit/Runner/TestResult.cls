/*------------------------------------------------------------------------------
  File        :   TestResult.cls
  Package     :   OEUnit.Runner
  Description :   Base Test Result class. Represents the result of running a 
                  single test. Inherited by TestClassResult and TestMethodResult.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.AssertionFailedError.
USING OEUnit.Reflection.StatementInfo.
USING OEUnit.Util.*.

CLASS OEUnit.Runner.TestResult ABSTRACT:
  
  /*----------------------------------------------------------------------------
    Test Status Constants
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC STATIC PROPERTY StatusIgnored    AS INTEGER NO-UNDO INIT 1 GET.
  DEFINE PUBLIC STATIC PROPERTY StatusNoTests    AS INTEGER NO-UNDO INIT 2 GET.
  DEFINE PUBLIC STATIC PROPERTY StatusPassed     AS INTEGER NO-UNDO INIT 3 GET.
  DEFINE PUBLIC STATIC PROPERTY StatusNotStarted AS INTEGER NO-UNDO INIT 4 GET.
  DEFINE PUBLIC STATIC PROPERTY StatusFailed     AS INTEGER NO-UNDO INIT 5 GET.
  DEFINE PUBLIC STATIC PROPERTY StatusError      AS INTEGER NO-UNDO INIT 6 GET.

  /*----------------------------------------------------------------------------
    Status of the test method. See 'Test Status Constants' in TestResult. 
  ----------------------------------------------------------------------------*/   
  DEFINE PROTECTED VARIABLE currentStatus AS INTEGER NO-UNDO. 
  
  /*----------------------------------------------------------------------------
    Matching info object for the test.
  ----------------------------------------------------------------------------*/
  DEFINE PROTECTED VARIABLE testInfo AS StatementInfo NO-UNDO. 

  /*----------------------------------------------------------------------------
    The error objects caught if/when Progress.Lang.Errors are thrown while 
    running the test.
  ----------------------------------------------------------------------------*/   
  DEFINE PROTECTED VARIABLE errors AS List NO-UNDO.

  /*----------------------------------------------------------------------------
    A Message describing the test failure/error. Populated when the test fails 
    or throws an error.
  ----------------------------------------------------------------------------*/  
  DEFINE PROTECTED VARIABLE failMessage AS CHARACTER NO-UNDO.

  /*----------------------------------------------------------------------------
    Constructor. Accepts the matching info object for this result.
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR PROTECTED TestResult(INPUT testInf AS StatementInfo):
    TestInfo = testInf.
    Errors = NEW List(TRUE).
  END CONSTRUCTOR.
  
  DESTRUCTOR TestResult():
    DELETE OBJECT Errors NO-ERROR.
    DELETE OBJECT TestInfo NO-ERROR.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Add an error to the list of errors  
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC CHARACTER AddError(INPUT err AS Progress.Lang.Error):
    Errors:Add(err).
    IF TYPE-OF(err, AssertionFailedError) 
      AND currentStatus <= StatusFailed THEN
        currentStatus = StatusFailed.
    ELSE
      currentStatus = StatusError.
    IF Errors:SIZE > 1 THEN 
      failMessage = " -- Multiple failures/errors occured --".
    ELSE
      failMessage = ErrorMessages:GetErrorMessage(err).
      
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the matching info object for the test.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC StatementInfo GetTestInfo():
    RETURN testInfo.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the message describing the test failure/error.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC CHARACTER GetMessage():
    RETURN failMessage.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the list of errors caught while running the test.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC List GetErrors():
    RETURN errors.
  END METHOD.


  /*----------------------------------------------------------------------------
    Returns the name of the Test.  
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC CHARACTER GetName():
    RETURN testInfo:Name.
  END METHOD.

  /*----------------------------------------------------------------------------
    Abstract. Return the amount of time taken to run the test. Overridden in 
    subclasses.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC ABSTRACT INT64 GetDuration().
  
  /*----------------------------------------------------------------------------
    Returns 1 if the given testStatus matches the current status. Otherwise 
    returns 0.
  ----------------------------------------------------------------------------*/ 
  METHOD PUBLIC INTEGER CountTestsWithStatus(INPUT testStatus AS INTEGER):
    IF currentStatus = testStatus OR testStatus = ? THEN
      RETURN 1.
    RETURN 0.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Returns the test status. 
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC INTEGER GetStatus():
    RETURN currentStatus.
  END METHOD.

  /*----------------------------------------------------------------------------
    Sets the current status
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC INTEGER SetStatus(INPUT testStatus AS INTEGER):
    currentStatus = testStatus.
  END METHOD.
    
  /*----------------------------------------------------------------------------
    Returns the current test status as a string.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC CHARACTER GetStatusAsString():
    RETURN GetStatusAsString(GetStatus()).              
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the given test status as a string.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC CHARACTER GetStatusAsString(INPUT testStatus AS INTEGER):
        
    CASE testStatus:
      WHEN StatusNotStarted THEN RETURN "Not Started".
      WHEN StatusNoTests THEN RETURN "No Tests".
      WHEN StatusPassed THEN RETURN "Passed".
      WHEN StatusIgnored THEN RETURN "Ignored".
      WHEN StatusFailed THEN RETURN "Failed".
      WHEN StatusError THEN RETURN "Error".
    END CASE.
    RETURN ?.
      
  END METHOD.
  
END CLASS.