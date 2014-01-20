/*------------------------------------------------------------------------------
  File        :   IFilter.cls
  Package     :   OEUnit.Runner.Manipulation   
  Description :   Objects implementing the IFilter interface can be used by
                  a OEUnit.Runner.BaseRunner to decide which test suites, cases 
                  and methods are run. Tests that are 'filtered' out are ignored
                  and no result/s are stored for them. A typical use of a filter
                  may be if you only wanted to run a single test method in a 
                  test case. 
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.StatementInfo.

INTERFACE OEUnit.Runner.Manipulation.IFilter:
  
  /*----------------------------------------------------------------------------
    Returns true if the given test should be run   
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC LOGICAL ShouldRun(INPUT inf AS StatementInfo).
  
END INTERFACE.