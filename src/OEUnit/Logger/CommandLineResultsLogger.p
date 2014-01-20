/*------------------------------------------------------------------------------
  File        :   CommandLineResultsLogger.p
  Package     :   OEUnit.Logger
  Description :   Wrapper of ResultsLogger.p to enable it to be easily invoked
                  from the command line.  The OutputDirectory and TestLocation
                  must be passed into the session using a comma separated
                  value containing the OutputDirectory and TestLocation:
                    -param "<OutputDirectory>,<TestLocation>"
  Author      :   Jamie Townsend
  Revisions   :   1.0 - November, 2010
                  Initial Implementation.
                  
------------------------------------------------------------------------------*/

USING OEUnit.Logger.TestExecutor.

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

TestExecutor:RunAsTest(INPUT OutputDirectory, INPUT TestLocation).

IF TestExecutor:hasErrors THEN
  RETURN "1".
ELSE
  RETURN "0".