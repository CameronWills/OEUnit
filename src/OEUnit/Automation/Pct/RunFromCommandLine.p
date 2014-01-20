/*------------------------------------------------------------------------------
  File        :   RunFromCommandLine.p
  Package     :   OEUnit.Logger
  Description :   Wrapper of RunTests.p to enable it to be easily invoked
                  from the command line. The OutputDirectory and TestLocation
                  must be passed into the session using a comma separated
                  value containing the OutputDirectory and TestLocation:
                    -param "<OutputDirectory>,<TestLocation>"

------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE OutputDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE TestLocation    AS CHARACTER NO-UNDO.

IF NUM-ENTRIES(SESSION:PARAMETER) <> 2 THEN DO:
  MESSAGE 'The OutputDirectory and TestLocation must be passed into the session '
    + 'using a comma separated value containing the OutputDirectory and '
    + 'TestLocation: -param "<OutputDirectory>,<TestLocation>"'.
  RETURN ERROR.
END.

ASSIGN
  OutputDirectory = ENTRY(1, SESSION:PARAMETER)
  TestLocation    = ENTRY(2, SESSION:PARAMETER)
  .

DEFINE VARIABLE hasErrors AS LOGICAL NO-UNDO.
RUN OEUnit/Automation/Pct/RunTests(INPUT OutputDirectory, INPUT TestLocation, OUTPUT hasErrors).

IF hasErrors THEN
  RETURN "1".
ELSE
  RETURN "0".