/*------------------------------------------------------------------------------
  File        :   AssertString.cls
  Package     :   OEUnit.Assertion
  Description :   A set of assertion methods specific to ABL string data types 
                  (CHARACTER and LONGCHAR), for use in test methods. If an 
                  assertion fails, an AssertionFailedError is thrown. Messages 
                  are only recoreded when an assertion fails.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.Assert.

CLASS OEUnit.Assertion.AssertString: 

  /*----------------------------------------------------------------------------
    Assertion methods for string-based data-types
  ----------------------------------------------------------------------------*/
  {OEUnit/Assertion/AssertStringType.i &DataType="CHARACTER" }    
  {OEUnit/Assertion/AssertStringType.i &DataType="LONGCHAR" }  
  
END CLASS.