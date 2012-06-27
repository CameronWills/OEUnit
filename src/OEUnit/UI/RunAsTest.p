/*------------------------------------------------------------------------------
  File        :   RunAsTest.p
  Package     :   OEUnit.UI
  Description :   Run the given class file as a test. This file should only be 
                  run persistently from within OpenEdge Architect.
  Author      :   Cameron Wills    
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.UI.ResultsWindowView.

DEFINE INPUT PARAMETER ipClassFile AS CHARACTER NO-UNDO.

ipClassFile = TRIM(ipClassFile, " ").
IF ipClassFile <> "" AND ipClassFile <> ? THEN
  ResultsWindowView:RunAsTest(ipClassFile).
