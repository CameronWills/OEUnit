/*------------------------------------------------------------------------------
  File        :   TestClassResult.cls
  Package     :   OEUnit.Runner
  Description :   Represents the test results for a test case or test suite. 
                  Contains a list of child test results. The child results may
                  themselves contain more child test results, reflecting the 
                  tree-like structure of the tests.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestResult.
USING OEUnit.Reflection.ClassInfo.
USING OEUnit.Util.List.

CLASS OEUnit.Runner.TestClassResult INHERITS TestResult:
  
  /*----------------------------------------------------------------------------
    A list of child test results
  ----------------------------------------------------------------------------*/
  DEFINE PROTECTED VARIABLE results AS List NO-UNDO.
  
  /*----------------------------------------------------------------------------
    The number of child test results.
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY ResultCount AS INTEGER NO-UNDO
    GET():
      RETURN results:Size.
    END GET.
     
  /*----------------------------------------------------------------------------
    Constructor. Accepts the matching ClassInfo object.
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR TestClassResult(INPUT testInf AS ClassInfo):
    SUPER(testInf).
    ASSIGN
      results = NEW List(TRUE)
      currentStatus = StatusNoTests.
  END CONSTRUCTOR.

  DESTRUCTOR TestClassResult():
    DELETE OBJECT results NO-ERROR.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Add a child test result object to the results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID AddResult(INPUT res AS TestResult):
    results:Add(res).
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the test result at the given index from the child results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC TestResult GetResult(INPUT indx AS INTEGER):
    RETURN CAST(results:Get(indx), TestResult).
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the first test result having the given name from the child results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC TestResult GetResultByName(INPUT testName AS CHARACTER):
    DEFINE VARIABLE res AS TestResult NO-UNDO.
    results:Reset().
    DO WHILE results:MoveNext():
      res = CAST(results:Current, TestResult).
      IF res:GetName() = TestName THEN
        RETURN res.
    END.
    RETURN ?.
  END METHOD.

  /*----------------------------------------------------------------------------
    Return the most significant test-status from the child results.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC OVERRIDE INTEGER GetStatus():

    DEFINE VARIABLE statusOfResults AS INTEGER NO-UNDO.
    DEFINE VARIABLE statusOfCurrentResult AS INTEGER NO-UNDO.
    statusOfResults = currentStatus.
    
    results:Reset().
    DO WHILE results:MoveNext():
      statusOfCurrentResult = CAST(results:Current, TestResult):GetStatus().
      IF statusOfCurrentResult > statusOfResults THEN
        statusOfResults = statusOfCurrentResult.
    END.
    RETURN statusOfResults.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Returns the total time taken (milliseconds) to run the test class.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC OVERRIDE INT64 GetDuration():
    DEFINE VARIABLE durationOfResults AS INT64 NO-UNDO INIT 0.
    
    results:Reset().
    DO WHILE results:MoveNext():
      durationOfResults = durationOfResults + CAST(results:Current, TestResult):GetDuration().
    END.
    RETURN durationOfResults. 
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Returns the total number of test methods known to this object.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC OVERRIDE INTEGER CountTests():
    DEFINE VARIABLE testCount AS INTEGER NO-UNDO INIT 0.
    
    results:Reset().
    DO WHILE results:MoveNext():
      testCount = testCount + CAST(results:Current, TestResult):CountTests().
    END.
    RETURN testCount. 
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Returns the total number of test methods that have the given status.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC OVERRIDE INTEGER CountTestsWithStatus(INPUT testStatus AS INTEGER):
    DEFINE VARIABLE testCount AS INTEGER NO-UNDO INIT 0.
    
    results:Reset().
    DO WHILE results:MoveNext():
      testCount = testCount + CAST(results:Current, TestResult):CountTestsWithStatus(testStatus).
    END.
    RETURN testCount. 
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns a string representing the state of this result and it's child results.     
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC OVERRIDE CHARACTER ToString():
    DEFINE VARIABLE state AS CHARACTER NO-UNDO.
    
    state = "|--"  + GetName() + "(" + STRING(GetDuration()) + ")"
        + " :: " + GetStatusAsString() + CHR(13).
        
    results:Reset().
    DO WHILE results:MoveNext():
      state = state + CAST(results:Current, TestResult):ToString().
    END.
    RETURN REPLACE(state, CHR(13), CHR(13) + "   ") + CHR(13).
  END METHOD.
  
END CLASS.  