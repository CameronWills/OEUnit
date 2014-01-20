 /*------------------------------------------------------------------------------
  File        :   JUnitReporter.cls
  Package     :   OEUnit.Automation
  Description :   JUnit XML Format Reporter Class.
------------------------------------------------------------------------------*/

USING OEUnit.Automation.FileReporter.
USING OEUnit.Runner.*.

CLASS OEUnit.Automation.JUnitReporter INHERITS FileReporter:
	
  /*----------------------------------------------------------------------------
    Class constructor. fName is path and file name for output XML file.
  ----------------------------------------------------------------------------*/  
  CONSTRUCTOR PUBLIC JUnitReporter(INPUT fName AS CHARACTER):
	SUPER(fName).
  END CONSTRUCTOR.

  /*----------------------------------------------------------------------------
    Reports TestClassResult with JUnit output
  ----------------------------------------------------------------------------*/  
  METHOD OVERRIDE PUBLIC VOID Report(INPUT results AS OEUnit.Runner.TestClassResult):
      
      DEFINE VARIABLE hSaxWriter AS HANDLE NO-UNDO.
      
      CREATE SAX-WRITER hSaxWriter.
      hSaxWriter:FORMATTED = TRUE.
      
      hSaxWriter:SET-OUTPUT-DESTINATION("FILE", FileName).
      hSaxWriter:START-DOCUMENT().
      
      ReportTestClassResult(INPUT hSaxWriter, INPUT results).
      
      hSaxWriter:END-DOCUMENT().

  END METHOD.

  /*----------------------------------------------------------------------------
    Reports results for a TestClassResult object
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED VOID ReportTestClassResult(INPUT hSaxWriter AS HANDLE, INPUT results AS TestClassResult):
      
      DEFINE VARIABLE HasChildSuites AS LOGICAL NO-UNDO INITIAL FALSE.
      DEFINE VARIABLE i              AS INTEGER NO-UNDO.
      
      /* Determine if there are any child TestClassResult items - if so, this
       * needs to be output as a "testsuites" element, not a "testsuite".
       */
      ChildLoop:
      DO i = 1 TO results:ResultCount:
          IF TYPE-OF(results:GetResult(INPUT i), "OEUnit.Runner.TestClassResult") THEN
          DO:
              ASSIGN HasChildSuites = TRUE.
              LEAVE ChildLoop.
          END.
      END.
      
      IF HasChildSuites THEN
	      hSaxWriter:START-ELEMENT("testsuites").
	  ELSE
	      hSaxWriter:START-ELEMENT("testsuite").
	      
      hSaxWriter:INSERT-ATTRIBUTE("errors",    STRING(results:CountTestsWithStatus(TestResult:StatusError))).
      hSaxWriter:INSERT-ATTRIBUTE("failures",  STRING(results:CountTestsWithStatus(TestResult:StatusFailed))).
      hSaxWriter:INSERT-ATTRIBUTE("name",      results:GetName()).
      hSaxWriter:INSERT-ATTRIBUTE("tests",     STRING(results:CountTests())).
      hSaxWriter:INSERT-ATTRIBUTE("time",      TRIM(STRING(results:GetDuration() / 1000,">>>>>>>>>>>>9.999<<"))).
      
      /* These attributes should only be ouput on a "testsuite", and not on "testsuites" */
      IF NOT HasChildSuites THEN
      DO:
          hSaxWriter:INSERT-ATTRIBUTE("skipped", STRING(results:CountTestsWithStatus(TestResult:StatusIgnored))).
          /* The following attributes are not implementable at this point in time, but are available in the
           * JUnit XML format. Uncomment, and implement when possible.
           */
          /* hSaxWriter:INSERT-ATTRIBUTE("id",        ""). */
          /* hSaxWriter:INSERT-ATTRIBUTE("package",   ""). */
          /* hSaxWriter:INSERT-ATTRIBUTE("hostname",  ""). */
          /* hSaxWriter:INSERT-ATTRIBUTE("timestamp", ""). */
      END.

      /* Loop through results held, and call correct procedure to output data */
      DO i = 1 TO results:ResultCount:
          /* Based on the class, report the result */
          IF TYPE-OF(results:GetResult(INPUT i), "OEUnit.Runner.TestClassResult") THEN
          DO:
              ReportTestClassResult(INPUT hSaxWriter, INPUT CAST(results:GetResult(INPUT i), "OEUnit.Runner.TestClassResult")).
          END.
          ELSE
          IF TYPE-OF(results:GetResult(INPUT i), "OEUnit.Runner.TestMethodResult") THEN
          DO:
              ReportTestMethodResult(INPUT hSaxWriter, 
                                      INPUT CAST(results:GetResult(INPUT i), "OEUnit.Runner.TestMethodResult"),
                                      INPUT results).
          END.
      END.
      
      IF HasChildSuites THEN
          hSaxWriter:END-ELEMENT("testsuites").
      ELSE
          hSaxWriter:END-ELEMENT("testsuite").

    END METHOD.
  
  /*----------------------------------------------------------------------------
    Reports results for a TestMethodResult object
  ----------------------------------------------------------------------------*/  
  METHOD PROTECTED VOID ReportTestMethodResult(INPUT hSaxWriter AS HANDLE, INPUT results AS TestMethodResult, INPUT parent AS TestClassResult):
      
      hSaxWriter:START-ELEMENT("testcase").
      
      hSaxWriter:INSERT-ATTRIBUTE("classname", parent:GetTestInfo():NAME).
      hSaxWriter:INSERT-ATTRIBUTE("name", results:GetName()).
      hSaxWriter:INSERT-ATTRIBUTE("time", TRIM(STRING(results:GetDuration() / 1000,">>>>>>>>>>>>9.999<<"))).
      
      CASE results:GetStatus():
          
          WHEN TestResult:StatusError THEN
          DO:
               hSaxWriter:WRITE-EMPTY-ELEMENT("error").
               hSaxWriter:INSERT-ATTRIBUTE("message", results:GetMessage()).
          END.
          
          WHEN TestResult:StatusFailed THEN
          DO:
               hSaxWriter:WRITE-EMPTY-ELEMENT("failure").
               hSaxWriter:INSERT-ATTRIBUTE("message", results:GetMessage()).
          END.
          
          WHEN TestResult:StatusIgnored THEN
          DO:
              hSaxWriter:WRITE-EMPTY-ELEMENT("skipped").
          END.
          
      END CASE.
      
      hSaxWriter:END-ELEMENT("testcase").
      
  END METHOD.

END CLASS.