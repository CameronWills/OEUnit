/*------------------------------------------------------------------------------
  File        :   OEAResultsLogger.p
  Package     :   OEUnit.Logger
  Description :   Wrapper of ResultsLogger.p to enable it to be easily invoked
                  from OpenEdge Architect.  The OutputDirectory must be passed
                  in as the only entry in the "Paramters" field in the
                  Customization Editor.
  Author      :   Jamie Townsend
  Revisions   :   1.0 - November, 2010
                  Initial Implementation.
                  
------------------------------------------------------------------------------*/

USING OEUnit.Logger.TestExecutor.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER ipParam AS CHARACTER NO-UNDO.

DEFINE VARIABLE OutputDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE TestLocation AS CHARACTER NO-UNDO.

ASSIGN
  OutputDirectory = ENTRY(1, ipParam, CHR(3))
  TestLocation    = ENTRY(2, ipParam, CHR(3)) 
  .

TestExecutor:RunAsTest(INPUT OutputDirectory, INPUT TestLocation).
