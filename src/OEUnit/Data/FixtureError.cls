/*------------------------------------------------------------------------------
  File        :   FixtureError.cls
  Package     :   OEUnit.Data
  Description :   The exception thrown when Fixture encounters a problem.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Data.FixtureError INHERITS Progress.Lang.AppError: 

  /*----------------------------------------------------------------------------
    Constructor. Accepts an error message.
  ----------------------------------------------------------------------------*/    
  CONSTRUCTOR PUBLIC FixtureError(INPUT errorMessage AS CHARACTER):
    SUPER(errorMessage, 0).
  END CONSTRUCTOR.

END CLASS.