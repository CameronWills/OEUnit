/*------------------------------------------------------------------------------
  File        :   ttTestResult.i
  Package     :   OEUnit.UI
  Description :   Temp-table to house all test results from a test run. Used 
                  by ResultsWindow to display results in a browse widget.
------------------------------------------------------------------------------*/

DEFINE {&access} TEMP-TABLE ttTestResult {&reference}
  FIELD TestId AS INTEGER
  FIELD Name AS CHARACTER
  FIELD DisplayName AS CHARACTER FORMAT "x(256)"
  FIELD ResultStatus AS INTEGER
  FIELD ResultStatusString AS CHARACTER FORMAT "x(15)"
  FIELD Duration AS INT64 FORMAT ">>>,>>>,>>9"
  FIELD ResultMessage AS CHARACTER FORMAT "x(300)"
  FIELD ErrorCount AS INTEGER
  FIELD ParentClass AS CHARACTER
  FIELD IsClass AS LOGICAL
  FIELD HasFailures AS LOGICAL
  FIELD HasPasses AS LOGICAL
  FIELD HasErrors AS LOGICAL
  FIELD HasIgnored AS LOGICAL
  FIELD DisplayInBrowser AS LOGICAL INIT TRUE
    INDEX inTestId AS PRIMARY UNIQUE TestId.
    
    