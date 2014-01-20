/*------------------------------------------------------------------------------
  File        :   StopConditionError.cls
  Package     :   OEUnit.Reflection
  Description :   The exception thrown when a quit condition occurs while running
                  MethodInfo:Invoke().
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Reflection.StopConditionError INHERITS Progress.Lang.AppError:

  /*----------------------------------------------------------------------------
    Constructor. Accepts an error message.
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR PUBLIC StopConditionError(INPUT errorMessage AS CHARACTER):
    SUPER(errorMessage, 0).
  END CONSTRUCTOR.

END CLASS.