/*------------------------------------------------------------------------------
  File        :   AssertStringType.i
  Package     :   OEUnit.Assertion
  Description :   String assertion methods, for use in test methods. The 
                  {&DataType} argument defines the ABL data type for the 
                  assertion methods.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/


  /*----------------------------------------------------------------------------
    Asserts that val is not null and not empty (""). If null or empty then 
    throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNullOrEmpty(INPUT val AS {&DataType}):
    Assert:IsTrue(val = ? OR val = "").
  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that val is not null and not empty (""). If null or empty then 
    throws an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNullOrEmpty(INPUT val AS {&DataType}, INPUT failMessage AS CHARACTER):
    Assert:IsTrue(val = ? OR val = "", failMessage).
  END METHOD. 

  /*----------------------------------------------------------------------------
    Asserts that val is not null and not empty (""). If null or empty then 
    throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNotNullOrEmpty(INPUT val AS {&DataType}):
    Assert:IsNotNull(val).
    Assert:AreNotEqual(val, "").
  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that val is not null and not empty (""). If null or empty then 
    throws an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID IsNotNullOrEmpty(INPUT val AS {&DataType}, INPUT failMessage AS CHARACTER):
    Assert:IsNotNull(val, failMessage).
    Assert:AreNotEqual(val, "", failMessage).
  END METHOD.  

  /*----------------------------------------------------------------------------
    Asserts that val contains targetString. If not then throws an 
    AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID Contains(INPUT val AS {&DataType}, INPUT targetString AS CHARACTER):
    Assert:IsTrue(INDEX(val, targetString) > 0).
  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that val contains targetString. If not then throws an 
    AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID Contains(INPUT val AS {&DataType}, INPUT targetString AS CHARACTER,
                              INPUT failMessage AS CHARACTER):
    Assert:IsTrue(INDEX(val, targetString) > 0, failMessage).
  END METHOD.

  /*----------------------------------------------------------------------------
    Asserts that val does not contain targetString. If contains targetString then 
    throws an AssertionFailedError.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID DoesNotContain(INPUT val AS {&DataType}, INPUT targetString AS CHARACTER):
    Assert:IsFalse(INDEX(val, targetString) > 0).
  END METHOD. 
  
  /*----------------------------------------------------------------------------
    Asserts that val does not contain targetString. If contains targetString then 
    throws an AssertionFailedError with the given failMessage.
  ----------------------------------------------------------------------------*/
  METHOD STATIC VOID DoesNotContain(INPUT val AS {&DataType}, INPUT targetString AS CHARACTER,
                              INPUT failMessage AS CHARACTER):
    Assert:IsFalse(INDEX(val, targetString) > 0, failMessage).
  END METHOD.
  
  