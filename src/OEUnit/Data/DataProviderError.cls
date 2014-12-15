/*------------------------------------------------------------------------------
  File        :   DataProvider.cls
  Package     :   OEUnit.Data
  Description :   The exception thrown when DataProvider setup is not complete.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Data.DataProviderError INHERITS Progress.Lang.AppError: 

  /*----------------------------------------------------------------------------
    Constructor. Accepts an error message.
  ----------------------------------------------------------------------------*/	
  CONSTRUCTOR PUBLIC DataProviderError(INPUT errorMessage AS CHARACTER):
    SUPER(errorMessage, 0).
  END CONSTRUCTOR.

END CLASS.