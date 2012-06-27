/*------------------------------------------------------------------------------
  File        :   TestSuite.cls
  Package     :   OEUnit.Runner
  Description :   The base test suite. Stores a list of child test objects. 
                  The list typically includes a number of test cases and/or 
                  other test suite objects. Custom test suites should inherit 
                  from this class. For example:

                    ROUTINE-LEVEL ON ERROR UNDO, THROW.
                    CLASS MyTestSuite INHERITS OEUnit.Runner.TestSuite:
                      
	                    CONSTRUCTOR MyTestSuite():
	                      AddTest(NEW MyTestCase1()).
	                      AddTest(NEW MyTestCase2()).
	                      AddTest(NEW MyTestSuite1()).
	                      AddTest(NEW MyTestSuite2()).
	                    END CONSTRUCTOR.
                                        
                    END CLASS.
                      
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Util.List.
USING Progress.Lang.Object.

CLASS OEUnit.Runner.TestSuite:

  /*----------------------------------------------------------------------------
    A list of child test objects.
  ----------------------------------------------------------------------------*/  
  DEFINE PRIVATE VARIABLE tests AS List NO-UNDO.
  
  /*----------------------------------------------------------------------------
    Number of child test objects
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY TestCount AS INTEGER NO-UNDO
    GET():
      RETURN tests:Size.
    END GET.
    
  CONSTRUCTOR TestSuite():
    tests = NEW List(TRUE).
  END CONSTRUCTOR.
 
  DESTRUCTOR TestSuite():
    DELETE OBJECT tests NO-ERROR.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Add a test.
  ----------------------------------------------------------------------------*/  
  METHOD PUBLIC VOID AddTest(INPUT test AS Object):
    tests:Add(test).
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the test at the given index.
  ----------------------------------------------------------------------------*/ 
  METHOD PUBLIC Object GetTest(INPUT indx AS INTEGER):
    RETURN tests:Get(indx).
  END METHOD.
  
END CLASS.