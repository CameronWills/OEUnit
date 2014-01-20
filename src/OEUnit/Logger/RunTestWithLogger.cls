/*------------------------------------------------------------------------------
  File        :   RunTestWithLogger.cls
  Package     :   OEUnit.Logger
  Description :   Helper methods for running tests and logging the results. 
                  Typically called from the constructor of a test case or test 
                  suite. This is a modified copy of OEUnit.Runners.RunTest, as
                  the STATIC approached used in the interactive implementation
                  prohibits OO extension to enable logging to a file instead
                  of displaying the results.
  Author      :   Jamie Townsend
  Revisions   :   1.0 - November, 2010
                  Initial Implementation.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Logger.IResultsLogger.
USING OEUnit.Logger.TestExecutor.
USING OEUnit.Runner.BaseRunner.
USING OEUnit.Runner.Manipulation.IFilter.
USING OEUnit.Runner.Notification.RunNotifier.
USING OEUnit.Runner.TestResult.
USING OEUnit.Runners.OEUnitRunner.

USING Progress.Lang.Object.

CLASS OEUnit.Logger.RunTestWithLogger:
  
  DEFINE PRIVATE STATIC VARIABLE resultsLogger    AS IResultsLogger NO-UNDO.
  
  /*----------------------------------------------------------------------------
    Run test with the default runner and display the results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID WithDefault(INPUT test AS Object):
    WithDefault(test, ?, ?).
  END METHOD.

  /*----------------------------------------------------------------------------
    Run test with the default runner, enlisting the given notifier and filter. 
    And display the results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID WithDefault(INPUT test AS Object,
                                        INPUT notifier AS RunNotifier, 
                                        INPUT filter AS IFilter): 
    
    DEFINE VARIABLE runner AS OEUnitRunner NO-UNDO.
    ASSIGN
      runner = NEW OEUnitRunner()
      runner:Filter = filter
      runner:Notifier = notifier.
      
    WithRunner(test, runner).
    
    FINALLY:
      DELETE OBJECT runner NO-ERROR.
    END FINALLY.
  END METHOD.

  /*----------------------------------------------------------------------------
    Run test with the given runner and display the results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID WithRunner(INPUT test AS Object, INPUT runner AS BaseRunner):

    runner:RunTest(test).
    resultsLogger:WriteResults(runner:Results).

    IF runner:Results:GetStatus() <> TestResult:StatusPassed THEN
        TestExecutor:hasErrors = TRUE.
    
  END METHOD.

  /*----------------------------------------------------------------------------
    Added method - run test logging to the customResultsLogger.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID WithDefault(INPUT customResultsLogger AS IResultsLogger,
      INPUT test AS Object, INPUT notifier AS RunNotifier, INPUT filter AS IFilter):
  
    resultsLogger = customResultsLogger.
    
    WithDefault (INPUT test, INPUT notifier, INPUT filter).
            
  END METHOD. 
  
  
END CLASS.