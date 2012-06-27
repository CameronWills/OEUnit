/*------------------------------------------------------------------------------
  File        :   MethodFilter.cls
  Package     :   OEUnit.Runners.Manipulation   
  Description :   Filter used to run a single test method in a test case. All 
                  other test methods are ignored.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.*.
USING OEUnit.Runner.Manipulation.IFilter.

CLASS OEUnit.Runners.Manipulation.MethodFilter IMPLEMENTS IFilter:
  
  DEFINE PRIVATE VARIABLE methodName AS CHARACTER NO-UNDO.

  /*----------------------------------------------------------------------------
    Constructor. Accepts the name of the method to be run.   
  ----------------------------------------------------------------------------*/   
  CONSTRUCTOR MethodFilter(INPUT methodToRun AS CHARACTER):
    methodName = methodToRun.
  END CONSTRUCTOR.
  
  /*----------------------------------------------------------------------------
    Returns true if the given test should be run   
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC LOGICAL ShouldRun(INPUT inf AS StatementInfo):
    IF TYPE-OF(inf, MethodInfo) THEN
      RETURN (inf:Name EQ methodName).
    RETURN TRUE.
  END METHOD.
  
END CLASS.