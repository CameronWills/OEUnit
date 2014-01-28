/*------------------------------------------------------------------------------
  File        :   RerunTest.p
  Package     :   OEUnit.UI
  Description :   Rerun the last run test class or test suite. This file should 
                  be run persistently from within OpenEdge Architect.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.UI.ResultsWindowView.

ResultsWindowView:RerunTest().
