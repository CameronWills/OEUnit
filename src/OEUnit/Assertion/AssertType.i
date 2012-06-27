/*------------------------------------------------------------------------------
  File        :   AssertType.i
  Package     :   OEUnit.Assertion
  Description :   Assertion methods for use in test methods. The {&DataType} 
                  argument defines the ABL data type for the assertion methods.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

/* Set encapsulating character for CHARACTER and LONGCHAR data-types. */
&IF "{&DataType}" = "CHARACTER" OR "{&DataType}" = "LONGCHAR" &THEN
  &SCOPED-DEFINE EncapChar '
&ENDIF

  /*----------------------------------------------------------------------------
    Asserts that expected equals actual. If not, then throws an 
    AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreEqual
    (INPUT actual AS {&DataType}, INPUT expected AS {&DataType}):
    
    HandleAssert(INPUT (expected = actual),  
      "{&EncapChar}" + STRING(expected) + "{&EncapChar}",
      "{&EncapChar}" + STRING(actual) + "{&EncapChar}", TRUE).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that expected equals actual. If not, then throws an 
    AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreEqual
    (INPUT actual AS {&DataType}, INPUT expected AS {&DataType},
     INPUT failMessage AS CHARACTER):
    
    HandleAssert(INPUT (expected = actual),  
      "{&EncapChar}" + STRING(expected) + "{&EncapChar}",
      "{&EncapChar}" + STRING(actual) + "{&EncapChar}", TRUE, failMessage).

  END METHOD.
  
  /*----------------------------------------------------------------------------
    Asserts that notExpected does not equal actual. If equal, then throws an 
    AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreNotEqual
    (INPUT actual AS {&DataType}, INPUT notExpected AS {&DataType}):
    
    HandleAssert(INPUT (notExpected <> actual),  
      "{&EncapChar}" + STRING(notExpected) + "{&EncapChar}",
      "{&EncapChar}" + STRING(actual) + "{&EncapChar}", FALSE).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that notExpected does not equal actual. If equal, then throws an 
    AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/    
  METHOD STATIC VOID AreNotEqual
    (INPUT actual AS {&DataType}, INPUT notExpected AS {&DataType},
     INPUT failMessage AS CHARACTER):
    
    HandleAssert(INPUT (notExpected <> actual),  
      "{&EncapChar}" + STRING(notExpected) + "{&EncapChar}",
      "{&EncapChar}" + STRING(actual) + "{&EncapChar}", FALSE, failMessage).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that val is null. If not then then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/   
  METHOD STATIC VOID IsNull(INPUT val AS {&DataType}):
    HandleAssert((val = ?), "?",  "{&EncapChar}" + STRING(val) + "{&EncapChar}", TRUE).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that val is null. If not then then throws an AssertionFailedError with
    the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNull(INPUT val AS {&DataType}, INPUT failMessage AS CHARACTER):
    HandleAssert((val = ?), "?",  "{&EncapChar}" + STRING(val) + "{&EncapChar}", TRUE, failMessage).
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Asserts that val is not null. If null then then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNotNull(INPUT val AS {&DataType}):
    HandleAssert((val <> ?), "{&DataType}",  "{&EncapChar}" + STRING(val) + "{&EncapChar}", TRUE).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that val is not null. If null then then throws an AssertionFailedError 
    with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNotNull(INPUT val AS {&DataType}, INPUT failMessage AS CHARACTER):
    HandleAssert((val <> ?), "{&DataType}",  "{&EncapChar}" + STRING(val) + "{&EncapChar}", TRUE, failMessage).
  END METHOD.
  