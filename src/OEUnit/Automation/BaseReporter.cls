/*------------------------------------------------------------------------------
  File        :   BaseReporter.cls
  Package     :   OEUnit.Automation
  Description :   Base Reporter Class. Custom reporters should inherit from this
                  class.
------------------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.*.

CLASS OEUnit.Automation.BaseReporter ABSTRACT: 
  
  /*----------------------------------------------------------------------------
    Reports the test results in the provided TestClassResult object
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC ABSTRACT VOID Report(INPUT results AS TestClassResult).
   
END CLASS.