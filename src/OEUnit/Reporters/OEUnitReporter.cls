/*------------------------------------------------------------------------------
  File        :   OEUnitReporter.cls
  Package     :   OEUnit.Reporters
  Description :   Base Reporter Class. Custom reporters should inherit from this
                  class.
  Author      :   Mark Abbott
------------------------------------------------------------------------------*/

USING OEUnit.Runner.*.
USING Progress.Lang.*.

CLASS OEUnit.Reporters.OEUnitReporter ABSTRACT: 

  /*------------------------------------------------------------------------------
    Base class constructor  																	  
  ------------------------------------------------------------------------------*/
  CONSTRUCTOR PUBLIC OEUnitReporter( ):
      SUPER().
	
  END CONSTRUCTOR.
  
  /*----------------------------------------------------------------------------
    Reports the test results in the provided TestClassResult object
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC ABSTRACT VOID Report(INPUT results AS OEUnit.Runner.TestClassResult).
   
END CLASS.