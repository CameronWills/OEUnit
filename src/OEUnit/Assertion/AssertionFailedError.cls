/*------------------------------------------------------------------------------
  File        :   AssertionFailedError.cls
  Package     :   OEUnit.Assertion
  Description :   The exception thrown when an assertion fails.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Assertion.AssertionFailedError INHERITS Progress.Lang.AppError:

  CONSTRUCTOR PUBLIC AssertionFailedError(INPUT failMessage AS CHARACTER):
    SUPER(failMessage, 0).
  END CONSTRUCTOR.

END CLASS.