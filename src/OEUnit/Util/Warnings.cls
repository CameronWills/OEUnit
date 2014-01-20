/*------------------------------------------------------------------------------
  File        : Warnings.cls
  Package     : OEUnit.Util
  Description : Helper methods for suppressing warning messages during COMPILE. 
------------------------------------------------------------------------------*/

CLASS OEUnit.Util.Warnings:

  /*----------------------------------------------------------------------------
    Copy of the existing suppressed codes. 
  ----------------------------------------------------------------------------*/
  DEFINE PRIVATE STATIC VARIABLE Suppressed AS CHARACTER NO-UNDO.

  /*----------------------------------------------------------------------------
    Is currently suppressed. 
  ----------------------------------------------------------------------------*/
  DEFINE PRIVATE STATIC VARIABLE IsSuppressed AS LOGICAL NO-UNDO INIT FALSE.

  /*----------------------------------------------------------------------------
    Warning message codes to suppress.
  ----------------------------------------------------------------------------*/
  DEFINE PRIVATE STATIC PROPERTY SuppressCodes AS CHARACTER NO-UNDO 
    INIT "6430," 
    GET.
 
  /*----------------------------------------------------------------------------
    Suppress warning codes    
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID Suppress():
  
    IF NOT IsSuppressed THEN
      ASSIGN
        Suppressed = SESSION:SUPPRESS-WARNINGS-LIST
        SESSION:SUPPRESS-WARNINGS-LIST = SuppressCodes + Suppressed
        IsSuppressed = TRUE.

  END METHOD.


  /*----------------------------------------------------------------------------
    Unsupress codes and restore prior suppressed warning list
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC VOID Restore():
    
    IF IsSuppressed THEN
      ASSIGN
        SESSION:SUPPRESS-WARNINGS-LIST = Suppressed
        IsSuppressed = FALSE.
        
  END METHOD.

END CLASS.