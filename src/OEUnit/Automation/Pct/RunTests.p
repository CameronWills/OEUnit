/*------------------------------------------------------------------------------
  File        :   RunTests.p
  Package     :   OEUnit.Automation.Pct
  Description :   Run test classes in the given testLocation. If testLocation 
                  value contains a class file, that file will be executed as a 
                  test case. If testLocation contains a directory, it will be
                  searched for test classes to execute. 
  ------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.Object.
USING OEUnit.Automation.SureFireReporter.
USING OEUnit.Runner.TestResult.
USING OEUnit.Runners.OEUnitRunner.
USING OEUnit.Util.Instance.

DEFINE INPUT PARAMETER outputDirectory AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER testLocation AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER hasErrors AS LOGICAL NO-UNDO. 

/* if the testLocation is a file, assume it's a .cls reference and use it */
DEFINE TEMP-TABLE ttClassFiles NO-UNDO
    FIELD classFile AS CHARACTER.
     
DEFINE BUFFER b_ClassFile FOR ttClassFiles.

FILE-INFO:FILE-NAME = testLocation.
IF SUBSTRING(FILE-INFO:FILE-TYPE, 1, 1) = "F" THEN DO:
    CREATE b_ClassFile.
    b_ClassFile.classFile = testLocation.
END.
ELSE DO:
  /* the testLocation is a directory, create a list of test classes */
  RUN FindClassFiles(INPUT testLocation).
END.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE classFile AS CHARACTER NO-UNDO.

FOR EACH b_ClassFile:
  RUN RunClassAsTest(b_ClassFile.classFile).
END.


  /*----------------------------------------------------------------------------
    Run a single test class and record the results using the SureFire xml
    format.
  ----------------------------------------------------------------------------*/ 
PROCEDURE RunClassAsTest PRIVATE:
  DEFINE INPUT PARAMETER classFile AS CHARACTER NO-UNDO.
      
  DEFINE VARIABLE test AS Object NO-UNDO.
  test = Instance:FromFile(classFile).
  
  DEFINE VARIABLE runner AS OEUnitRunner NO-UNDO.
  runner = NEW OEUnitRunner().

  /* Run your test case or suite */
  runner:RunTest(test).
  IF runner:Results:GetStatus() <> TestResult:StatusPassed THEN
    hasErrors = TRUE.

  /* Log the results */
  DEFINE VARIABLE reporter AS SureFireReporter NO-UNDO.
  reporter = NEW SureFireReporter(outputDirectory).
  reporter:Report(runner:Results).

  FINALLY:
    DELETE OBJECT test NO-ERROR.
    DELETE OBJECT runner NO-ERROR.
    DELETE OBJECT reporter NO-ERROR.	
  END FINALLY.
    
END PROCEDURE.


  /*----------------------------------------------------------------------------
    Searches recursively for class files in a given path. Full filenames are
    returned seperated by a star(*)
  ----------------------------------------------------------------------------*/
PROCEDURE FindClassFiles PRIVATE:
  DEFINE INPUT PARAMETER path AS CHARACTER NO-UNDO.

  DEFINE VARIABLE directoryEntry AS CHARACTER NO-UNDO.
  
  INPUT FROM OS-DIR (path).
  REPEAT:

    IMPORT directoryEntry.
    FILE-INFO:FILE-NAME = path
      + (IF OPSYS = "WIN32" THEN "\" ELSE "/")
      + directoryEntry.

    CASE SUBSTRING(FILE-INFO:FILE-TYPE, 1, 1):
      WHEN "F" THEN DO:
        IF directoryEntry MATCHES ("Test*.cls") THEN DO:            
            CREATE b_ClassFile.
            b_ClassFile.classFile = FILE-INFO:FULL-PATHNAME.
        END.
      END.
      WHEN "D" THEN DO:
        IF directoryEntry <> ".." AND directoryEntry <> "." THEN DO:
          RUN FindClassFiles(INPUT FILE-INFO:FULL-PATHNAME).          
        END.
      END.
    END CASE.
  END.

END PROCEDURE.
