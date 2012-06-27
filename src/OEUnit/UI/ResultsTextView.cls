/*------------------------------------------------------------------------------
  File        :   ResultsTextView.cls
  Package     :   OEUnit.UI
  Description :   Results view for character based user-interfaces (TTY). More
                  work to be done on this... 
  Author      :   Cameron Wills    
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.*.
USING OEUnit.UI.*.

CLASS OEUnit.UI.ResultsTextView IMPLEMENTS IResultsView:
  
  /*----------------------------------------------------------------------------
    Displays the given test results on screen.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID DisplayResults(INPUT results AS TestClassResult):
    MESSAGE results:ToString() VIEW-AS ALERT-BOX.
  END METHOD.
  
END CLASS.