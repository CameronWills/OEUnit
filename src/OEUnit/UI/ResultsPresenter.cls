/*------------------------------------------------------------------------------
  File        :   ResultsPresenter.cls
  Package     :   OEUnit.UI
  Description :   A gateway between the user interface and the test framework. 
                  Passes the test results to an IResultsView and provides a 
                  number of methods for the UI to interact with the testing 
                  framework.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.AppError.
USING OEUnit.Runner.TestClassResult.
USING OEUnit.Runners.RunTest.
USING OEUnit.Runners.Manipulation.MethodFilter.
USING OEUnit.UI.*.
USING OEUnit.Util.*.

CLASS OEUnit.UI.ResultsPresenter:
  
  DEFINE PRIVATE STATIC VARIABLE resultsView AS IResultsView NO-UNDO.
  
  /*----------------------------------------------------------------------------
    Reruns the given test class (must be the full class type-name). Optionally 
    specify the name of a test method name to run that method only. 
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID RerunTest(INPUT classType AS CHARACTER, INPUT methodName AS CHARACTER):
    
    DEFINE VARIABLE test AS Progress.Lang.Object NO-UNDO.
    DEFINE VARIABLE filter AS MethodFilter NO-UNDO.
    
    test = Instance:FromType(classType).
    IF methodName <> "" AND methodName <> ? THEN
      filter = NEW MethodFilter(methodName).
    RunTest:WithDefault(test, ?, filter).
      
    FINALLY:
      DELETE OBJECT test NO-ERROR.
      DELETE OBJECT filter NO-ERROR.
    END FINALLY.
  END METHOD.
 
  /*----------------------------------------------------------------------------
    Run the given file as a test. Must be a valid class file (.cls).
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC STATIC VOID RunAsTest(INPUT classFile AS CHARACTER):
        
    DEFINE VARIABLE test AS Progress.Lang.Object NO-UNDO.
    test = Instance:FromFile(classFile).
    
    RunTest:WithDefault(test, ?, ?).
    
    FINALLY:
      DELETE OBJECT test NO-ERROR.
    END FINALLY.
  END METHOD.
 
  /*----------------------------------------------------------------------------
    Displays the given test results on screen.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID DisplayResults(INPUT results AS TestClassResult):
    IF NOT(VALID-OBJECT(resultsView)) THEN
      resultsView = ResultsViewFactory:GetResultsView().
    resultsView:DisplayResults(results).
  END METHOD.
  
END CLASS.