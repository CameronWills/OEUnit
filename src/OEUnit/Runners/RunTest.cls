/*------------------------------------------------------------------------------
  File        :   RunTest.cls
  Package     :   OEUnit.Runners
  Description :   Helper methods for running tests and displaying the results. 
                  Typically called from the constructor of a test case or test 
                  suite. OpenEdge Architect provides the ability to 'Run' a class 
                  - simply creates an instance of the class and then deletes 
                  the instance. You can place code inside the constructor of a 
                  test case or test suite to run itself. For example:
                    
                    ROUTINE-LEVEL ON ERROR UNDO, THROW.
                    CLASS MyTestCase:
                    
                      CONSTRUCTOR MyTestCase():
                        OEUnit.Runners.RunTest:WithDefault(THIS-OBJECT).
                      END CONSTRUCTOR.
                    
                      @Test.
                      METHOD PUBLIC VOID MyFirstTest(): 
                        ...
                      END METHOD.
                        
                    END CLASS.

------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.BaseRunner.
USING OEUnit.Runner.Manipulation.IFilter.
USING OEUnit.Runner.Notification.RunNotifier.
USING OEUnit.Runners.OEUnitRunner.
USING OEUnit.UI.ResultsPresenter.
USING Progress.Lang.Object.

CLASS OEUnit.Runners.RunTest:
  
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
    IF VALID-OBJECT(runner:Results) THEN
      ResultsPresenter:DisplayResults(runner:Results).
  END METHOD.
  
END CLASS.