/*------------------------------------------------------------------------------
  File        :   ResultsWindowView.cls
  Package     :   OEUnit.UI
  Description :   Results view for windows based user-interfaces (GUI). Displays
                  results in a window that can be docked in OpenEdge Architect.
  Author      :   Cameron Wills    
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.*.
USING OEUnit.UI.*.
USING OEUnit.Util.*.

CLASS OEUnit.UI.ResultsWindowView IMPLEMENTS IResultsView:

  /*----------------------------------------------------------------------------
    Test Result and Error Temp-Tables
  ----------------------------------------------------------------------------*/  
  {OEUnit/UI/ttTestResult.i &access = "STATIC"}
  {OEUnit/UI/ttError.i &access = "STATIC"}

  /*----------------------------------------------------------------------------
    Total number of records in the ttTestResult temp-table.
  ----------------------------------------------------------------------------*/  
  DEFINE PRIVATE STATIC VARIABLE testCount AS INTEGER NO-UNDO.
  
  /*----------------------------------------------------------------------------
    Handle to the persistently run ResultsWindow procedure. 
  ----------------------------------------------------------------------------*/
  DEFINE PRIVATE STATIC VARIABLE resultWindow AS HANDLE NO-UNDO.
  DEFINE PRIVATE STATIC PROPERTY ResultsWindow AS HANDLE NO-UNDO 
    GET:
      IF NOT(VALID-HANDLE(resultWindow)) THEN
        RUN OEUnit/UI/ResultsWindow.w PERSISTENT SET resultWindow.
      RETURN resultWindow.      
    END GET.

  /*----------------------------------------------------------------------------
    Reruns the given test class (must be the full class type-name). Optionally 
    specify the name of a test method name to run that method only.
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC STATIC VOID RerunTest(INPUT className AS CHARACTER, INPUT methodName AS CHARACTER):
    DisplayRunning(className).
    ResultsPresenter:RerunTest(className, methodName).
    CATCH e AS Progress.Lang.Error :
      DisplayError(className, e).
      DELETE OBJECT e NO-ERROR. 
    END CATCH.
  END METHOD.

  /*----------------------------------------------------------------------------
    Run the given file as a test. Must be a valid class file (.cls).
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC STATIC VOID RunAsTest(INPUT classFileName AS CHARACTER):
    DisplayRunning(classFileName).
    ResultsPresenter:RunAsTest(classFileName).
    CATCH e AS Progress.Lang.Error :
      DisplayError(classFileName, e).
      DELETE OBJECT e NO-ERROR. 
    END CATCH.
  END METHOD.

  /*----------------------------------------------------------------------------
    Display the results window and indicate that testing is in progress
  ----------------------------------------------------------------------------*/   
  METHOD PRIVATE STATIC LOGICAL DisplayRunning(INPUT className AS CHARACTER):
    EMPTY TEMP-TABLE ttTestResult.
    EMPTY TEMP-TABLE ttError.
    CREATE ttTestResult.
    ASSIGN
      ttTestResult.Name = className
      ttTestResult.DisplayName = className
      ttTestResult.TestId = 1
      ttTestResult.ResultStatus = ?
      ttTestResult.ResultStatusString = "RUNNING"
      ttTestResult.ResultMessage = ""
      ttTestResult.Duration = ?
      ttTestResult.ErrorCount = 0
      ttTestResult.IsClass = TRUE
      ttTestResult.HasErrors = FALSE.
    RUN setTestResults IN ResultsWindow (INPUT TABLE ttTestResult BIND, 
                    INPUT TABLE ttError BIND, 1, 0, 0, 0, 0).
  END METHOD. 

  /*----------------------------------------------------------------------------
    Display the given error in the results window.
  ----------------------------------------------------------------------------*/   
  METHOD PRIVATE STATIC LOGICAL DisplayError(INPUT className AS CHARACTER, INPUT err AS Progress.Lang.Error):
    EMPTY TEMP-TABLE ttTestResult.
    EMPTY TEMP-TABLE ttError.
    CREATE ttTestResult.
    ASSIGN
      ttTestResult.Name = className
      ttTestResult.DisplayName = className
      ttTestResult.TestId = 1
      ttTestResult.ResultStatus = TestResult:StatusError
      ttTestResult.ResultStatusString = CAPS(TestResult:GetStatusAsString(TestResult:StatusError))
      ttTestResult.ResultMessage = Errors:GetMessage(err)
      ttTestResult.ErrorCount = 1
      ttTestResult.IsClass = TRUE
      ttTestResult.HasErrors = TRUE.
    AddError(err, 1).
    RUN setTestResults IN ResultsWindow (INPUT TABLE ttTestResult BIND, 
                    INPUT TABLE ttError BIND, 1, 0, 0, 1, 0).
  END METHOD. 

  /*----------------------------------------------------------------------------
    Displays the given test results in the results window.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID DisplayResults(INPUT results AS TestClassResult):
    testCount = 0.
    EMPTY TEMP-TABLE ttTestResult.
    EMPTY TEMP-TABLE ttError.
    AddResult(results,"",0).
    RUN setTestResults IN ResultsWindow (INPUT TABLE ttTestResult BIND,
      INPUT TABLE ttError BIND, testCount,
      results:CountTestsWithStatus(TestResult:StatusPassed), 
      results:CountTestsWithStatus(TestResult:StatusFailed), 
      results:CountTestsWithStatus(TestResult:StatusError), 
      results:CountTestsWithStatus(TestResult:StatusIgnored)).
  END METHOD.

  /*----------------------------------------------------------------------------
    Add the given test result to the ttTestResult and ttError tables.
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE VOID AddResult(INPUT res AS TestResult, INPUT parentClass AS CHARACTER, 
                                INPUT depth AS INTEGER):

    DEFINE VARIABLE testName AS CHARACTER NO-UNDO.
    CREATE ttTestResult.
    ASSIGN testCount = testCount + 1
           testName = res:GetName()
           ttTestResult.TestId = testCount
           ttTestResult.Name = testName
           ttTestResult.DisplayName = FILL("       ",depth) + testName
           ttTestResult.Duration = res:GetDuration()
           ttTestResult.ParentClass = parentClass
           ttTestResult.ErrorCount = res:GetErrors():Size
           ttTestResult.ResultMessage = res:GetMessage()
           ttTestResult.ResultStatus = res:GetStatus()
           ttTestResult.ResultStatusString = CAPS(res:GetStatusAsString()).           
           
    AddErrors(res:GetErrors()).
    
    IF TYPE-OF(res, TestClassResult) THEN DO:
      DEFINE VARIABLE classResult AS TestClassResult NO-UNDO.
      ASSIGN
        classResult = CAST(res, TestClassResult)
        ttTestResult.IsClass = TRUE
        ttTestResult.HasPasses = (res:CountTestsWithStatus(TestResult:StatusPassed) > 0)
        ttTestResult.HasFailures = (res:CountTestsWithStatus(TestResult:StatusFailed) > 0)
        ttTestResult.HasErrors = ((res:CountTestsWithStatus(TestResult:StatusError) > 0) 
                                   OR (classResult:GetStatus() = TestResult:StatusError)).
        ttTestResult.HasIgnored = ((res:CountTestsWithStatus(TestResult:StatusIgnored) > 0) 
                                   OR (classResult:GetStatus() = TestResult:StatusIgnored)).                                   
      IF classResult:GetMessage() = "" AND classResult:GetStatus() > TestResult:StatusNoTests THEN 
        ttTestResult.ResultStatusString = "".
      
      /* Add child results */
      DEFINE VARIABLE i AS INTEGER NO-UNDO. 
      DO i = 1 TO classResult:resultCount:
        AddResult(classResult:GetResult(i), testName, depth + 1).
      END.
    END.
    ELSE ttTestResult.IsClass = FALSE.

  END METHOD.

  /*----------------------------------------------------------------------------
    Add each of the Progress.Lang.Errors in the given list to the ttError table.
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE STATIC VOID AddErrors(INPUT errors AS List):
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DO i = 1 TO errors:Size:
      AddError(CAST(errors:Get(i), Progress.Lang.Error), i).
    END.

  END METHOD.
  
  /*----------------------------------------------------------------------------
    Add the given error to ttError.
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE STATIC VOID AddError(INPUT err AS Progress.Lang.Error, INPUT errorId AS INTEGER):

    CREATE ttError.
    ASSIGN ttError.TestId = ttTestResult.TestId
           ttError.ErrorId = errorId 
           ttError.ErrorMessage = Errors:GetMessage(err)
           ttError.CallStack = err:GetClass():TypeName + " at" + CHR(13) 
             + err:CallStack.
    
  END METHOD.
 
END CLASS.