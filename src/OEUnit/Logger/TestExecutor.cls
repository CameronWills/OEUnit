/*------------------------------------------------------------------------------
  File        :   TestExecutor.cls
  Package     :   OEUnit.Logger
  Description :   A gateway between the test framework and the ResultsLogger. 
  Author      :   Jamie Townsend
  Revisions   :   1.0 - November, 2010
                  Initial Implementation.
                  1.4 - May, 2013 Arek Jaworski
                  Integrated with OEUnit 1.3. Replaced ErrorMessages with Errors.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.AppError.
USING OEUnit.Logger.IResultsLogger.
USING OEUnit.Logger.ResultsLoggerFactory.
USING OEUnit.Logger.RunTestWithLogger.
USING OEUnit.Runners.Manipulation.MethodFilter.
USING OEUnit.Util.Errors.
USING OEUnit.Util.List.

CLASS OEUnit.Logger.TestExecutor:

  DEFINE PUBLIC STATIC VARIABLE hasErrors AS LOGICAL NO-UNDO INIT FALSE.

  /*----------------------------------------------------------------------------
    Runs the given test class (must be the full class type-name). Optionally 
    specify the name of a test method name to run that method only.
  ----------------------------------------------------------------------------*/
  METHOD PRIVATE STATIC VOID RunTest(INPUT resultsLogger AS IResultsLogger, 
      INPUT className AS CHARACTER, INPUT methodName AS CHARACTER):
    
    DEFINE VARIABLE test AS Progress.Lang.Object NO-UNDO.
    DEFINE VARIABLE filter AS MethodFilter NO-UNDO.
    
    test = DYNAMIC-NEW (className) () NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      RETURN ERROR NEW AppError("Failed to create instance of '" + className 
        + "'.~r~n" + Errors:GetMessage(INPUT ERROR-STATUS:HANDLE),0).
    IF methodName <> "" AND methodName <> ? THEN
      filter = NEW MethodFilter(methodName).
      
    RunTestWithLogger:WithDefault(resultsLogger, test, ?, filter).
      
    FINALLY:
      DELETE OBJECT test NO-ERROR.
      DELETE OBJECT filter NO-ERROR.
    END FINALLY.
  END METHOD.

  /*----------------------------------------------------------------------------
    Run the test(s).  If testLocation contains a class file, that file will be
    executed as a test case.  If testLocation contains a directory, it will be
    searched for test classes to execute. 
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID RunAsTest(
      INPUT outputDirectory AS CHARACTER,
      INPUT testLocation AS CHARACTER,
      INPUT methodName AS CHARACTER
        ):

    DEFINE VARIABLE classFiles       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE classFileCounter AS INTEGER NO-UNDO.
    DEFINE VARIABLE classFileName    AS CHARACTER NO-UNDO.

    /* if the testLocation is a file, assume it's a .cls reference and use it */
    FILE-INFO:FILE-NAME = testLocation.
    IF SUBSTRING(FILE-INFO:FILE-TYPE, 1, 1) = "F" THEN DO:
      classFiles = testLocation.
    END.
    ELSE DO:
      /* the testLocation is a directory, create a list of test classes */
      RUN OEUnit\Util\FindClassFiles.p (INPUT testLocation, OUTPUT classFiles).
    END.
    
    REPEAT classFileCounter = 1 TO NUM-ENTRIES(classFiles):
      classFilename = ENTRY(classFileCounter, classFiles).
      MESSAGE "Executing Test:" classFilename.
      RunClassAsTest(outputDirectory, classFilename, methodName).
    END.

  END METHOD.

  METHOD PROTECTED STATIC VOID RunClassAsTest (
      INPUT outputDirectory AS CHARACTER,
      INPUT classFileName AS CHARACTER,
      INPUT methodName AS CHARACTER
        ):
            
    DEFINE VARIABLE resultsLogger AS IResultsLogger NO-UNDO.

    /* configure the results logger */    
    resultsLogger = ResultsLoggerFactory:GetResultsLogger().
    resultsLogger:SetOutpuDirectory(outputDirectory).

    /* compile the class */
    IF NOT(classFileName MATCHES "*.cls") THEN
      RETURN ERROR NEW AppError("Not a valid test. Must be a class (.cls).",0).
    
    Errors:Initalize().
    COMPILE VALUE(classFileName) SAVE = FALSE NO-ERROR.
    IF COMPILER:ERROR THEN
      RETURN ERROR NEW AppError("Failed to compile '" + classFileName
        + "'.~r~n" + Errors:GetMessage(INPUT COMPILER:HANDLE),0).
    
    RunTest(resultsLogger, COMPILER:CLASS-TYPE, methodName).
    
  END METHOD.

  METHOD PUBLIC STATIC VOID RunAsTest(
      INPUT outputDirectory AS CHARACTER,
      INPUT testLocation AS CHARACTER
        ):
    RunAsTest(outputDirectory, testLocation, "").
  END METHOD.

END CLASS.