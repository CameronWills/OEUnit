/*------------------------------------------------------------------------------
  File        :   CSVReporter.cls
  Package     :   OEUnit.Automation
  Description :   CSV Output Reporter Class.
  Author      :   Mark Abbott
------------------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING OEUnit.Automation.FileReporter.
USING OEUnit.Runner.*.

CLASS OEUnit.Automation.CSVReporter INHERITS FileReporter:
    
  /*----------------------------------------------------------------------------
    Stream where file output will be sent.
  ----------------------------------------------------------------------------*/    
  DEFINE PRIVATE STREAM OutputStream.
    
  /*----------------------------------------------------------------------------
    Name of the current Test Suite being output.
  ----------------------------------------------------------------------------*/
  DEFINE PROTECTED PROPERTY SuiteName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

  /*----------------------------------------------------------------------------
    Class constructor. fName is path and file name for output XML file.
  ----------------------------------------------------------------------------*/    
  CONSTRUCTOR PUBLIC CSVReporter(INPUT fName AS CHARACTER):
      SUPER (fName).
  END CONSTRUCTOR.

  /*----------------------------------------------------------------------------
    Reports TestClassResult in CSV format
  ----------------------------------------------------------------------------*/  
  METHOD OVERRIDE PUBLIC VOID Report(INPUT results AS OEUnit.Runner.TestClassResult):
      
      ASSIGN SuiteName = Results:GetName().
      
      OUTPUT STREAM OutputStream TO VALUE(FileName) UNBUFFERED.
      
      PUT STREAM OutputStream UNFORMATTED
          '"Test Suite","Test Class","Test Name","Test Duration","Test Status","Message"'
          SKIP.
      
      ReportTestClassResult(INPUT results).
      
      OUTPUT STREAM OutputStream CLOSE.
  END METHOD.

  /*----------------------------------------------------------------------------
    Reports results for a TestClassResult object
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED VOID ReportTestClassResult(INPUT results AS TestClassResult):
      
      DEFINE VARIABLE i AS INTEGER NO-UNDO.

      /* Loop through results held, and call correct procedure to output data */
      DO i = 1 TO results:ResultCount:
          /* Based on the class, report the result */
          IF TYPE-OF(results:GetResult(INPUT i), "OEUnit.Runner.TestClassResult") THEN
          DO:
              ReportTestClassResult(INPUT CAST(results:GetResult(INPUT i), "OEUnit.Runner.TestClassResult")).
          END.
          ELSE
          IF TYPE-OF(results:GetResult(INPUT i), "OEUnit.Runner.TestMethodResult") THEN
          DO:
              ReportTestMethodResult(INPUT CAST(results:GetResult(INPUT i), "OEUnit.Runner.TestMethodResult"),
                                     INPUT results).
          END.
      END.
      
    END METHOD.
  
  /*----------------------------------------------------------------------------
    Reports results for a TestMethodResult object
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED VOID ReportTestMethodResult(INPUT results AS TestMethodResult, INPUT parent AS TestClassResult):
      
      PUT STREAM OutputStream UNFORMATTED
          '"' 
          SuiteName '","'
          parent:GetTestInfo():NAME '","'
          results:GetName() '","'
          TRIM(STRING(results:GetDuration() / 1000,">>>>>>>>>>>>9.999<<")) '","'
          results:GetStatusAsString() '","'
          results:GetMessage() '"'
          SKIP.
      
  END METHOD.

END CLASS.