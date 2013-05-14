/*------------------------------------------------------------------------------
  File        :   ResultsSurefireLogger.cls
  Package     :   OEUnit.Logger
  Description :   Used for writing test results to a file in a Surefire-like
                  format. One log file will be created for each test class.
  Author      :   Jamie Townsend
  Revisions   :   1.0 - November, 2010
                  Initial Implementation.
                  1.4 - May, 2013 Arek Jaworski
                  Integrated with OEUnit 1.3. Use longchar and x-document instead of character and put statement.
                  
  Known Issues :
    - the output directory must already exist
    - XML returned by currentResult:GetMessage() should be escaped
    - Implementation should not create XML directly, rather use the provided
      ABL XML APIs. 
------------------------------------------------------------------------------*/

routine-level on error undo, throw.

using OEUnit.Runner.TestClassResult.
using OEUnit.Runner.TestResult.
using OEUnit.Logger.IResultsLogger.
using OEUnit.Util.List.

using Progress.Lang.Error.

class OEUnit.Logger.ResultsSurefireLogger implements IResultsLogger:
  
  define stream sLOG.
  
  define variable outputDirectory as character no-undo initial ".".

  /*----------------------------------------------------------------------------
    Sets the directory location where log files will be written.
  ----------------------------------------------------------------------------*/   
  method public void SetOutpuDirectory(input outputDirectoryName as character):
    outputDirectory = outputDirectoryName.
  end method.
  

  /*----------------------------------------------------------------------------
    Output the given test results to the appropriate log file.
  ----------------------------------------------------------------------------*/
  method public void WriteResults(input results as TestClassResult):
    define variable hXML as handle no-undo.
    create x-document hXML.
    hXML:load("Longchar", ToXml(results), false).
    hXML:save("FILE", substitute("&1&2&3.xml",outputDirectory,
            if opsys = "WIN32" then "\" else "/",
            results:GetName())).  
  end method.
  
    /*----------------------------------------------------------------------------
    Returns a string containing a surefire-like xml representation of the state of
    this result and it's child results.
  ----------------------------------------------------------------------------*/
  method public longchar ToXml(input results as TestClassResult):
    define variable outputXml       as longchar   no-undo.
    define variable resultNum       as integer    no-undo.
    define variable currentResult   as TestResult no-undo.
    define variable errorList       as List       no-undo.
    define variable errorNum        as integer    no-undo.
    define variable errorStack      as character  no-undo.

    outputXml = outputXml
      + SUBSTITUTE('<testsuite errors="&1" skipped="&2" tests="&3" time="&4" '
            + 'failures="&5" name="&6">~n',
          string(results:CountTestsWithStatus(TestResult:StatusError)),
          string(results:CountTestsWithStatus(TestResult:StatusIgnored)),
          string(results:ResultCount),
          string(results:GetDuration()),
          string(results:CountTestsWithStatus(TestResult:StatusFailed)),
          results:GetName()).

    do resultNum = 1 to results:ResultCount:
      currentResult = results:GetResult(resultNum).

      /* don't close the tag - wait to see if there was a failure */
      outputXml = outputXml + 
        SUBSTITUTE('  <testcase classname="&1" time="&2" name="&3"',
          results:GetName(),
          string(currentResult:GetDuration()),
          currentResult:GetName()).

      /* check for errors */      
      if currentResult:GetErrors():Size > 0 then do:
        outputXml = outputXml
          + SUBSTITUTE('>~n    <failure'
              /* + ' type="??"' */
              + ' message="&1">~n',
            currentResult:GetMessage()).

        /* loop through the list of errors */  
        errorList = currentResult:GetErrors().
        do errorNum = 1 to errorList:Size:
            
          /* capture the stack trace */
          errorStack = cast(errorList:Get(errorNum), Error):CallStack.
          
          if errorStack = ? then do:
            errorStack = "~n ...stack trace disabled. Please start the session"
              + " with the parameter -errorstack to enable stack traces".
          end.

          outputXml = outputXml
            + SUBSTITUTE('&1 at &2',
                currentResult:GetMessage(),
                errorStack).
        end.

        /* close the testcase tag */
        outputXml = outputXml
          + '~n    </failure>~n  </testcase>~n'.
      end.
      /* check to see if the test was ignored */      
      else if currentResult:GetStatus() = TestResult:StatusIgnored then do:
        outputXml = outputXml
          + SUBSTITUTE('>~n    <ignore/>~n  </testcase>~n').
      end.
      else do:
        /* no errors - close the testcase tag */
        outputXml = outputXml
          + '/>~n'.
      end.
      
    end.

    /* close the testsuite tag */
    outputXml = outputXml + '</testsuite>~n'.

    return outputXml.    
  end method.
    
end class.