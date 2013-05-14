/*------------------------------------------------------------------------------
  File        : FindClassFiles.p
  Package     : OEUnit.Util
  Description : Searches for class files in a given location
  Parameters  : INPUT searchLocation - directory to search from
                OUTPUT classFiles - a comma separated list of classes found
  Author      : Jamie Townsend
  Revisions   : 1.0 - July 2010
                Initial Implementation
                  
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER searchLocation AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER classFiles      AS CHARACTER NO-UNDO.

DEFINE STREAM dirlist.
            
DEFINE VARIABLE childClassFiles   AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE directoryEntry    AS CHARACTER NO-UNDO.

INPUT STREAM dirlist FROM OS-DIR (searchLocation).
REPEAT:
  IMPORT STREAM dirlist directoryEntry.
  FILE-INFO:FILE-NAME = searchLocation
    + (IF OPSYS = "WIN32" THEN "\" ELSE "/")
    + directoryEntry.

  CASE SUBSTRING(FILE-INFO:FILE-TYPE, 1, 1):
      WHEN "F" THEN DO:
          IF directoryEntry MATCHES ("*.cls") THEN
            classFiles = classFiles
              + (IF classFiles <> "" THEN "," ELSE "")
              + FILE-INFO:FULL-PATHNAME.
        END.
      WHEN "D" THEN DO:
          IF directoryEntry <> ".." AND directoryEntry <> "." THEN DO:
            RUN OEUnit\Util\FindClassFiles.p (INPUT FILE-INFO:FULL-PATHNAME,
                OUTPUT childClassFiles).
            IF childClassFiles <> "" THEN DO:
              classFiles = classFiles
                + (IF classFiles <> "" THEN "," ELSE "")
                + childClassFiles.
            END.
          END.
        END.
  END CASE.
END.

INPUT STREAM dirlist CLOSE.
        
