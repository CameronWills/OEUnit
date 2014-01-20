/*------------------------------------------------------------------------------
  File        :   RunNotifier.cls
  Package     :   OEUnit.Runner.Notification
  Description :   Stores a list of RunListener objects. Each stored RunListener 
                  object will be notified of the events occuring during a test 
                  run. To enlist a RunNotifier for a test run, set it as the 
                  Notifer in the runner. For Example:
                    
                    runner = NEW OEUnit.Runners.OEUnitRunner().
                    runner:Notifier = NEW RunNotifier(). 
                    runner:Notifier:AddRunListener(NEW MyRunListener1).
                    runner:Notifier:AddRunListener(NEW MyRunListener2).

------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.*.
USING OEUnit.Runner.*.
USING OEUnit.Runner.Notification.RunListener.
USING OEUnit.Util.List.

CLASS OEUnit.Runner.Notification.RunNotifier:
  
  /*----------------------------------------------------------------------------
    A list of RunListener objects. 
  ----------------------------------------------------------------------------*/
  DEFINE PRIVATE VARIABLE listeners AS List NO-UNDO.

  CONSTRUCTOR RunNotifier():
    listeners = NEW List(TRUE).
  END CONSTRUCTOR.

  DESTRUCTOR RunNotifier():
    DELETE OBJECT listeners.
  END DESTRUCTOR.
  
  /*----------------------------------------------------------------------------
    Add a listener to the list.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC FINAL VOID AddRunListener(INPUT listener AS RunListener):
    listeners:Reset().
    DO WHILE listeners:MoveNext():
      IF CAST(listeners:Current, RunListener) = listener THEN        RETURN.
    END.
    listeners:Add(listener).
  END.

  /*----------------------------------------------------------------------------
    Remove the given listener from the list.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC FINAL VOID RemoveRunListener(INPUT listener AS RunListener):
    listeners:Reset().
    DO WHILE listeners:MoveNext():
      IF CAST(listeners:Current, RunListener) = listener THEN
        listeners:RemoveCurrent(FALSE).
        RETURN.
    END.
  END METHOD.

  /*----------------------------------------------------------------------------
    Notify all listeners that the test run is about to start.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID FireTestRunStarted(INPUT inf AS ClassInfo):
    listeners:Reset().
    DO WHILE listeners:MoveNext():
      CAST(listeners:Current, RunListener):TestRunStarted(inf).
    END.
  END METHOD. 

  /*----------------------------------------------------------------------------
    Notify all listeners that the test run has finished.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID FireTestRunFinished(INPUT res AS TestClassResult):
    listeners:Reset().
    DO WHILE listeners:MoveNext():
      CAST(listeners:Current, RunListener):TestRunFinished(res).
    END.
  END METHOD. 

  /*----------------------------------------------------------------------------
    Notify all listeners that the test class/method was ignored.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID FireTestIgnored(INPUT res AS TestResult):
    listeners:Reset().
    DO WHILE listeners:MoveNext():
      CAST(listeners:Current, RunListener):TestIgnored(res).
    END.
  END METHOD. 

  /*----------------------------------------------------------------------------
    Notify all listeners that the test class/method is about to start.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID FireTestStarted(INPUT inf AS StatementInfo):
    listeners:Reset().
    DO WHILE listeners:MoveNext():
      CAST(listeners:Current, RunListener):TestStarted(inf).
    END.
  END METHOD. 

  /*----------------------------------------------------------------------------
    Notify all listeners that the test class/method has finished.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID FireTestFinished(INPUT res AS TestResult):
    listeners:Reset().
    DO WHILE listeners:MoveNext():
      CAST(listeners:Current, RunListener):TestFinished(res).
    END.
  END METHOD.

END CLASS.