/*------------------------------------------------------------------------------
  File        :   OEAResultsLogger.p
  Package     :   OEUnit.Logger
  Description :   Wrapper of ResultsLogger.p to enable it to be easily invoked
                  from OpenEdge Architect.  The OutputDirectory must be passed
                  in as the only entry in the "Paramters" field in the
                  Customization Editor.
                  
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER ipParam AS CHARACTER NO-UNDO.

DEFINE VARIABLE OutputDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE TestLocation AS CHARACTER NO-UNDO.

ASSIGN
  OutputDirectory = ENTRY(1, ipParam, CHR(3))
  TestLocation    = ENTRY(2, ipParam, CHR(3)) 
  .

DEFINE VARIABLE hasErrors AS LOGICAL NO-UNDO.

RUN OEUnit/Automation/Pct/RunTests(INPUT OutputDirectory, INPUT TestLocation, OUTPUT hasErrors).

