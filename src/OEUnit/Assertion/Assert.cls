/*------------------------------------------------------------------------------
  File        :   Assert.cls
  Package     :   OEUnit.Assertion
  Description :   A set of assertion methods for use in test methods. If an 
                  assertion fails, an AssertionFailedError is thrown. Messages 
                  are only recorded when an assertion fails.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.AssertionFailedError.

CLASS OEUnit.Assertion.Assert: 

  /*----------------------------------------------------------------------------
    Assertion methods for standard ABL data-types
  ----------------------------------------------------------------------------*/
  {OEUnit/Assertion/AssertType.i &DataType="CHARACTER" }    
  {OEUnit/Assertion/AssertType.i &DataType="COM-HANDLE" }               
  {OEUnit/Assertion/AssertType.i &DataType="DATE" }                     
  {OEUnit/Assertion/AssertType.i &DataType="DATETIME" }                 
  {OEUnit/Assertion/AssertType.i &DataType="DATETIME-TZ" }              
  {OEUnit/Assertion/AssertType.i &DataType="DECIMAL" }      
  {OEUnit/Assertion/AssertType.i &DataType="HANDLE" }
  {OEUnit/Assertion/AssertType.i &DataType="INT64" }        
  {OEUnit/Assertion/AssertType.i &DataType="INTEGER" }      
  {OEUnit/Assertion/AssertType.i &DataType="LOGICAL" }      
  {OEUnit/Assertion/AssertType.i &DataType="LONGCHAR" }
  {OEUnit/Assertion/AssertType.i &DataType="MEMPTR" }       
  {OEUnit/Assertion/AssertType.i &DataType="RAW" }          
  {OEUnit/Assertion/AssertType.i &DataType="RECID" }        
  {OEUnit/Assertion/AssertType.i &DataType="ROWID" }     

  /*----------------------------------------------------------------------------
    Asserts that condition is true. If not then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsTrue(INPUT condition AS LOGICAL):
    HandleAssert(condition, "yes", STRING(condition), TRUE).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that condition is true. If not then throws an AssertionFailedError 
    with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsTrue(INPUT condition AS LOGICAL, INPUT failMessage AS CHARACTER):
    HandleAssert(condition, "yes", STRING(condition), TRUE, failMessage).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that condition is false. If not then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsFalse(INPUT condition AS LOGICAL):
    HandleAssert(NOT(condition), "no", STRING(condition), TRUE).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that condition is false. If not then throws an AssertionFailedError 
    with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsFalse(INPUT condition AS LOGICAL, INPUT failMessage AS CHARACTER):
    HandleAssert(NOT(condition), "no", STRING(condition), TRUE, failMessage).
  END METHOD.

  /*----------------------------------------------------------------------------
    Fails the test, throwing an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID Fail():
    Fail(?).
  END METHOD.  

  /*----------------------------------------------------------------------------
    Fails the test, throwing an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID Fail(INPUT failMessage AS CHARACTER):
    IF failMessage = ? THEN
      failMessage = "Fail() method of Assert class was called".
    HandleAssert(FALSE, failMessage). 
  END METHOD.  
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and expected are equal. Not necessarily the same.
    If not, then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreEqual
    (INPUT actual AS Progress.Lang.Object, INPUT expected AS Progress.Lang.Object):
    
    HandleAssert(INPUT expected:Equals(actual),  
      STRING(expected),
      STRING(actual), TRUE).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and expected are equal. Not necessarily the same.
    If not, then throws an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreEqual
    (INPUT actual AS Progress.Lang.Object, INPUT expected AS Progress.Lang.Object,
     INPUT failMessage AS CHARACTER):
    
    HandleAssert(INPUT expected:Equals(actual),  
      STRING(expected),
      STRING(actual), TRUE, failMessage).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and notExpected are not equal. If equal, then 
    throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreNotEqual
    (INPUT actual AS Progress.Lang.Object, INPUT notExpected AS Progress.Lang.Object):
    
    HandleAssert(INPUT NOT(notExpected:Equals(actual)),  
      STRING(notExpected),
      STRING(actual), FALSE).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and notExpected are not equal. If equal, then 
    throws an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreNotEqual
    (INPUT actual AS Progress.Lang.Object, INPUT notExpected AS Progress.Lang.Object,
     INPUT failMessage AS CHARACTER):
    
    HandleAssert(INPUT NOT(notExpected:Equals(actual)),  
      STRING(notExpected),
      STRING(actual), FALSE, failMessage).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and expected are the same object. If not, then 
    throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreSame
    (INPUT actual AS Progress.Lang.Object, INPUT expected AS Progress.Lang.Object):
    
    HandleAssert(INPUT (expected = actual),  
      STRING(expected),
      STRING(actual), TRUE).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and expected are the same object. If not, then 
    throws an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreSame
    (INPUT actual AS Progress.Lang.Object, INPUT expected AS Progress.Lang.Object,
     INPUT failMessage AS CHARACTER):
    
    HandleAssert(INPUT (expected = actual),  
      STRING(expected),
      STRING(actual), TRUE, failMessage).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and notExpected are not the same object. If they
    are then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreNotSame
    (INPUT actual AS Progress.Lang.Object, INPUT notExpected AS Progress.Lang.Object):
    
    HandleAssert(INPUT (notExpected <> actual),  
      STRING(notExpected),
      STRING(actual), FALSE).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that objects actual and notExpected are not the same object. If they
    are then throws an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID AreNotSame
    (INPUT actual AS Progress.Lang.Object, INPUT notExpected AS Progress.Lang.Object,
     INPUT failMessage AS CHARACTER):
    
    HandleAssert(INPUT (notExpected <> actual),  
      STRING(notExpected),
      STRING(actual), FALSE, failMessage).

  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that obj is null. If not then then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/      
  METHOD STATIC VOID IsNull(INPUT obj AS Progress.Lang.Object):
    HandleAssert(NOT(VALID-OBJECT(obj)), "?",  STRING(obj), TRUE).
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Asserts that obj is null. If not then then throws an AssertionFailedError 
    with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNull(INPUT obj AS Progress.Lang.Object, INPUT failMessage AS CHARACTER):
    HandleAssert(NOT(VALID-OBJECT(obj)), "?",  STRING(obj), TRUE, failMessage).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that obj is not null. If null then throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNotNull(INPUT obj AS Progress.Lang.Object):
    HandleAssert(VALID-OBJECT(obj), "object",  STRING(obj), TRUE).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that obj is not null. If null then throws an AssertionFailedError
    with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNotNull(INPUT obj AS Progress.Lang.Object, INPUT failMessage AS CHARACTER):
    HandleAssert(VALID-OBJECT(obj), "object",  STRING(obj), TRUE, failMessage).
  END METHOD.

  /*----------------------------------------------------------------------------
    If success is false, then throws an AssertionFailedError.  
  ----------------------------------------------------------------------------*/
  METHOD PRIVATE STATIC VOID HandleAssert(INPUT success AS LOGICAL):
     HandleAssert(success, ?, ?, TRUE, ?).
  END METHOD.

  /*----------------------------------------------------------------------------
    If success is false, then throws an AssertionFailedError with the given
    failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC PRIVATE VOID HandleAssert(INPUT success AS LOGICAL, INPUT failMessage AS CHARACTER):
     HandleAssert(success, ?, ?, TRUE, failMessage).
  END METHOD.

  /*----------------------------------------------------------------------------
    If success is false, then throws an AssertionFailedError describing the 
    expected and actual values.
  ----------------------------------------------------------------------------*/
  METHOD PRIVATE STATIC VOID HandleAssert(INPUT success AS LOGICAL, 
    INPUT expected AS CHARACTER, INPUT actual AS CHARACTER, INPUT expectedEqual AS LOGICAL):
     HandleAssert(success, expected, actual, expectedEqual, ?).
  END METHOD.

  /*----------------------------------------------------------------------------
    If success is false, then throws an AssertionFailedError. If failMessage is
    null then constructs a message from expected and actual strings. 
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE STATIC VOID HandleAssert(INPUT success AS LOGICAL, INPUT expected AS CHARACTER, 
    INPUT actual AS CHARACTER, INPUT expectedEqual AS LOGICAL, INPUT failMessage AS CHARACTER):

    IF NOT success THEN DO:
      IF failMessage = ? THEN DO:
        IF expectedEqual THEN
          failMessage = "Expecting: " + ReplaceNull(expected) + " but found: " + ReplaceNull(actual).
        ELSE
          failMessage = "Not expecting: " + ReplaceNull(expected) + " but found: " + ReplaceNull(actual).
      END.
      RETURN ERROR NEW AssertionFailedError(failMessage).
    END.
    
  END METHOD.
  
  /*----------------------------------------------------------------------------
    If the given string is null then returns "?". Otherwise returns the string.
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE STATIC CHARACTER ReplaceNull(INPUT str AS CHARACTER):
    IF str = ? THEN RETURN "?".
    RETURN str.
  END METHOD. 
  
END CLASS.