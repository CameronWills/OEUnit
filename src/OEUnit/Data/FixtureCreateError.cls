/*------------------------------------------------------------------------------
  File        :   FixtureCreateError.cls
  Package     :   OEUnit.Data
  Description :   The exception thrown when Fixture encounters a problem.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Data.FixtureCreateError INHERITS Progress.Lang.AppError: 

  /*----------------------------------------------------------------------------
    Constructor. Accepts an error message.
  ----------------------------------------------------------------------------*/    
  CONSTRUCTOR PUBLIC FixtureCreateError(INPUT errorMessage AS CHARACTER):
    SUPER(SUBSTITUTE("Fixture Error: &1",errorMessage), 0).
  END CONSTRUCTOR.

END CLASS.