/*------------------------------------------------------------------------------
  File        :   TestMethodResult.cls
  Package     :   OEUnit.Runner
  Description :   Represents the test result for single test method. A method
                  needs to be annotated with @Test to run as a test method.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestResult.
USING OEUnit.Reflection.MethodInfo.

CLASS OEUnit.Runner.TestMethodResult INHERITS TestResult:
  
  /*----------------------------------------------------------------------------
    Time taken, in milliseconds, for the test to run. 
  ----------------------------------------------------------------------------*/  
  DEFINE PRIVATE VARIABLE testDuration AS INT64 NO-UNDO INIT 0.
    
  /*----------------------------------------------------------------------------
    Constructor. Accepts the matching MethodInfo object.
  ----------------------------------------------------------------------------*/  
  CONSTRUCTOR TestMethodResult(INPUT testInf AS MethodInfo):
    SUPER(testInf).
    currentStatus = StatusNotStarted.
  END CONSTRUCTOR.

  /*----------------------------------------------------------------------------
    Returns the duration.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC INT64 SetDuration(INPUT duration AS INT64):
    testDuration = duration.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the duration.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC OVERRIDE INT64 GetDuration():
    RETURN testDuration.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Return a string representing the state of this result.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC OVERRIDE CHARACTER ToString():
    RETURN "|--" + GetName()
        + "(" + STRING(testDuration) + ")" + " :: " + GetStatusAsString()
        + (IF failMessage <> "" THEN ": " + STRING(failMessage) ELSE "") + CHR(13).
  END METHOD.
  
END CLASS.
  