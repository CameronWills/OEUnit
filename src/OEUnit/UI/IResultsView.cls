/*------------------------------------------------------------------------------
  File        :   IResultsView.cls
  Package     :   OEUnit.UI
  Description :   Classes implementing the IResultView interface can be used by
                  the ResultsPresenter to display the results of a test run to 
                  the user.
  Author      :   Cameron Wills                  
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestClassResult.

INTERFACE OEUnit.UI.IResultsView:
  
  /*----------------------------------------------------------------------------
    Displays the given test results to the user.
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC VOID DisplayResults(INPUT results AS TestClassResult).
  
END INTERFACE.