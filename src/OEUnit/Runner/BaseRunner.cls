/*------------------------------------------------------------------------------
  File        :   BaseRunner.cls
  Package     :   OEUnit.Runner
  Description :   Base class for a test runner. Custom runners should inherit 
                  from OEUnit.Runners.OEUnitRunner instead of this class. 
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.*.
USING OEUnit.Reflection.*.
USING OEUnit.Runner.Manipulation.IFilter.
USING OEUnit.Runner.Notification.RunNotifier.
USING Progress.Lang.Object.

CLASS OEUnit.Runner.BaseRunner ABSTRACT:

  /*----------------------------------------------------------------------------
    Events fired while running tests
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC EVENT RunStarted VOID (INPUT inf AS ClassInfo).
  DEFINE PUBLIC EVENT RunFinished VOID (INPUT res AS TestClassResult).
  DEFINE PUBLIC EVENT TestIgnored VOID (INPUT res AS TestResult).
  DEFINE PUBLIC EVENT TestStarted VOID (INPUT inf AS StatementInfo).
  DEFINE PUBLIC EVENT TestFinished VOID (INPUT res AS TestResult).

  /*----------------------------------------------------------------------------
    Results from the test run.
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY Results AS TestClassResult NO-UNDO GET. PROTECTED SET.
  
  /*----------------------------------------------------------------------------
    Notifier to notify run listeners of the events occuring during the test run.
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY Notifier AS RunNotifier NO-UNDO 
    SET(INPUT val AS RunNotifier): 
      IF VALID-OBJECT(val) THEN DO:
        THIS-OBJECT:RunStarted:SUBSCRIBE(val:FireTestRunStarted).
        THIS-OBJECT:RunFinished:SUBSCRIBE(val:FireTestRunFinished).
        THIS-OBJECT:TestIgnored:SUBSCRIBE(val:FireTestIgnored).
        THIS-OBJECT:TestStarted:SUBSCRIBE(val:FireTestStarted).
        THIS-OBJECT:TestFinished:SUBSCRIBE(val:FireTestFinished).
      END.
    END SET.
  
  /*----------------------------------------------------------------------------
    Filter to ignore test classes and methods that you dont want to run 
  ----------------------------------------------------------------------------*/  
  DEFINE PUBLIC PROPERTY Filter AS IFilter NO-UNDO GET. SET.


  DESTRUCTOR BaseRunner():
    DELETE OBJECT Results NO-ERROR.
    DELETE OBJECT Filter NO-ERROR.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    The primary entry point for running a test class. Runs the test and stores 
    the results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC ABSTRACT VOID RunTest(INPUT test AS Object).
  
  /*----------------------------------------------------------------------------
    Returns true if the given test should be run as decided by the Filter.
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED LOGICAL ShouldRun(INPUT inf AS StatementInfo):
    IF VALID-OBJECT(Filter) THEN   
      RETURN Filter:ShouldRun(inf).
    RETURN TRUE.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Notifies listeners that the test run is about to start 
  ----------------------------------------------------------------------------*/   
  METHOD PROTECTED VOID NotifyOfRunStart(INPUT inf AS ClassInfo):
    RunStarted:Publish(inf).
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Notifies listeners that the test run has finished 
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID NotifyOfRunFinish(INPUT res AS TestClassResult):
    RunFinished:Publish(res).
  END METHOD.

  /*----------------------------------------------------------------------------
    Notifies listeners that a test class/method was ignored  
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID NotifyOfTestIgnored(INPUT res AS TestResult):
    TestIgnored:Publish(res).
  END METHOD.

  /*----------------------------------------------------------------------------
    Notifies listeners that a test class/method is about to start 
  ----------------------------------------------------------------------------*/
  METHOD PROTECTED VOID NotifyOfTestStart(INPUT inf AS StatementInfo):
    TestStarted:Publish(inf).
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Notifies listeners that a test class/method has finished  
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED VOID NotifyOfTestFinish(INPUT res AS TestResult):
    TestFinished:Publish(res).
  END METHOD.
  
  
END CLASS.