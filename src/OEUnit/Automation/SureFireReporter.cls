/*------------------------------------------------------------------------------
  File        :   SurefireReporter.cls
  Package     :   OEUnit.Automation
  Description :   Used for writing test results to a file in a Surefire-like
                  format. One log file will be created for each test class.
 Known Issues :
    - the output directory must already exist
    - XML returned by currentResult:GetMessage() should be escaped
    - Implementation should not create XML directly, rather use the provided
      ABL XML APIs. 
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.*.
USING OEUnit.Runner.TestClassResult.
USING OEUnit.Automation.BaseReporter.
USING OEUnit.Util.List.

USING Progress.Lang.Error.

CLASS OEUnit.Automation.SureFireReporter INHERITS BaseReporter:
  
  DEFINE STREAM sLOG.
  
  DEFINE VARIABLE outputDirectory AS CHARACTER NO-UNDO INITIAL ".".
  
  CONSTRUCTOR PUBLIC SureFireReporter(INPUT outputDirectoryName AS CHARACTER):
    outputDirectory = outputDirectoryName.  
  END CONSTRUCTOR.


  /*----------------------------------------------------------------------------
    Output the given test results to the appropriate log file.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC OVERRIDE VOID Report(INPUT results AS TestClassResult):
    DEFINE VARIABLE hXML AS HANDLE NO-UNDO.
    CREATE X-DOCUMENT hXML.
    hXML:LOAD("Longchar", ToXml(results), FALSE).
    hXML:SAVE("FILE", SUBSTITUTE("&1&2&3.xml",outputDirectory,
            IF OPSYS = "WIN32" THEN "\" ELSE "/",
            results:GetName())).  
  END METHOD.
  
    /*----------------------------------------------------------------------------
    Returns a string containing a surefire-like xml representation of the state of
    this result and it's child results.
  ----------------------------------------------------------------------------*/
  METHOD PRIVATE LONGCHAR ToXml(INPUT results AS TestClassResult):
    DEFINE VARIABLE outputXml       AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE resultNum       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE currentResult   AS TestResult NO-UNDO.
    DEFINE VARIABLE errorList       AS List       NO-UNDO.
    DEFINE VARIABLE errorNum        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE errorStack      AS CHARACTER  NO-UNDO.

    outputXml = outputXml
      + SUBSTITUTE('<testsuite errors="&1" skipped="&2" tests="&3" time="&4" '
            + 'failures="&5" name="&6">~n',
          STRING(results:CountTestsWithStatus(TestResult:StatusError)),
          STRING(results:CountTestsWithStatus(TestResult:StatusIgnored)),
          STRING(results:ResultCount),
          STRING(results:GetDuration()),
          STRING(results:CountTestsWithStatus(TestResult:StatusFailed)),
          results:GetName()).

    DO resultNum = 1 TO results:ResultCount:
      currentResult = results:GetResult(resultNum).

      /* don't close the tag - wait to see if there was a failure */
      outputXml = outputXml + 
        SUBSTITUTE('  <testcase classname="&1" time="&2" name="&3"',
          results:GetName(),
          STRING(currentResult:GetDuration()),
          currentResult:GetName()).

      /* check for errors */      
      IF currentResult:GetErrors():Size > 0 THEN DO:
        outputXml = outputXml
          + SUBSTITUTE('>~n    <failure'
              /* + ' type="??"' */
              + ' message="&1">~n',
            currentResult:GetMessage()).

        /* loop through the list of errors */  
        errorList = currentResult:GetErrors().
        DO errorNum = 1 TO errorList:Size:
            
          /* capture the stack trace */
          errorStack = CAST(errorList:Get(errorNum), Error):CallStack.
          
          IF errorStack = ? THEN DO:
            errorStack = "~n ...stack trace disabled. Please start the session"
              + " with the parameter -errorstack to enable stack traces".
          END.

          outputXml = outputXml
            + SUBSTITUTE('&1 at &2',
                currentResult:GetMessage(),
                errorStack).
        END.

        /* close the testcase tag */
        outputXml = outputXml
          + '~n    </failure>~n  </testcase>~n'.
      END.
      /* check to see if the test was ignored */      
      ELSE IF currentResult:GetStatus() = TestResult:StatusIgnored THEN DO:
        outputXml = outputXml
          + SUBSTITUTE('>~n    <ignore/>~n  </testcase>~n').
      END.
      ELSE DO:
        /* no errors - close the testcase tag */
        outputXml = outputXml
          + '/>~n'.
      END.
      
    END.

    /* close the testsuite tag */
    outputXml = outputXml + '</testsuite>~n'.

    RETURN outputXml.    
  END METHOD.
    
END CLASS.