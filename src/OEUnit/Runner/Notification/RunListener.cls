/*------------------------------------------------------------------------------
  File        :   RunListener.cls
  Package     :   OEUnit.Runner.Notification
  Description :   Base class for a run listener - to observe events of a test run.
                  Useful for monitoring the progress of a test run, and
                  responding to events. RunListeners are registered with an
                  instance of OEUnit.Runner.Notification.RunNotifier. 
                  For example, a run listener that will display a message box
                  when a test fails:
                    
                    CLASS FailureListener INHERITS OEUnit.Runner.Notification.RunListener:
                      
                        METHOD PUBLIC OVERRIDE VOID TestFinished(INPUT res AS TestResult):
                          IF res:GetStatus() = TestResult:StatusFailed THEN
                            MESSAGE res:GetName() VIEW-AS ALERT-BOX.
                        END METHOD.
                        
                    END CLASS.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.*.
USING OEUnit.Runner.*.

CLASS OEUnit.Runner.Notification.RunListener ABSTRACT:
  
  /*----------------------------------------------------------------------------
    Called when a test run is about to start.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID TestRunStarted(INPUT inf AS ClassInfo):
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Called when a test run finishes.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC VOID TestRunFinished(INPUT res AS TestClassResult):
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Called when a test class/method is about to start.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID TestStarted(INPUT inf AS StatementInfo):
  END METHOD.

  /*----------------------------------------------------------------------------
    Called when a test class/method finishes.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC VOID TestFinished(INPUT res AS TestResult):
  END METHOD.

  /*----------------------------------------------------------------------------
    Called when a test class/method is ignored.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC VOID TestIgnored(INPUT res AS TestResult):
  END METHOD.
   
END CLASS.