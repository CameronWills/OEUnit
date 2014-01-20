/*------------------------------------------------------------------------------
  File        :   ttError.i
  Package     :   OEUnit.UI
  Description :   Temp-table to house the errors that occured during a test 
                  run.
------------------------------------------------------------------------------*/

DEFINE {&access} TEMP-TABLE ttError {&reference}
  FIELD TestId AS INTEGER
  FIELD ErrorId AS INTEGER
  FIELD ErrorMessage AS CHARACTER
  FIELD CallStack AS CHARACTER
    INDEX indTestId AS PRIMARY UNIQUE TestId ErrorId.
    
    