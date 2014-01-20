/*------------------------------------------------------------------------------
  File        :   ResultsViewFactory.cls
  Package     :   OEUnit.UI
  Description :   Factory for the different Results Views.  
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.UI.*.

CLASS OEUnit.UI.ResultsViewFactory:
  
  /*----------------------------------------------------------------------------
    Factory method for Results Views. Creates and returns an IResultsView 
    matching the current sessions display-type (e.g. GUI, TTY).
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC STATIC IResultsView GetResultsView():
    IF SESSION:DISPLAY-TYPE = "GUI" THEN
      RETURN NEW ResultsWindowView().
    ELSE 
      RETURN NEW ResultsTextView().
  END METHOD.
  
END CLASS.