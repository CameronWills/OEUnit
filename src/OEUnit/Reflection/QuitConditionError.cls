/*------------------------------------------------------------------------------
  File        :   QuitConditionError.cls
  Package     :   OEUnit.Reflection
  Description :   The exception thrown when a quit condition occurs while running
                  MethodInfo:Invoke().
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Reflection.QuitConditionError INHERITS Progress.Lang.AppError:

  /*----------------------------------------------------------------------------
    Constructor. Accepts an error message.
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR PUBLIC QuitConditionError(INPUT errorMessage AS CHARACTER):
    SUPER(errorMessage, 0).
  END CONSTRUCTOR.

END CLASS.