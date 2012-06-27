ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.*.
USING OEUnit.Tests.Assertion.*.

CLASS OEUnit.Tests.Assertion.AssertTest:
  
  @Test.
  METHOD PUBLIC VOID Fails():
    DO ON ERROR UNDO, THROW:
      Assert:Fail().
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.    		
      END CATCH.
    END.
    Assert:Fail().
  END METHOD.

  @Test.
  METHOD PUBLIC VOID SimpleTypesAreEqual():
    Assert:AreEqual(1,1).
    Assert:AreEqual("test","test").
    Assert:AreEqual(1.0,1.0).
    Assert:AreEqual(01/01/2000,01/01/2000).
    
    DO ON ERROR UNDO, THROW:
      Assert:AreEqual(1,2).
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.    		
      END CATCH.      
    END.
    Assert:Fail().    
  END METHOD.  

  @Test.
  METHOD PUBLIC VOID SimpleTypesAreNotEqual():
    Assert:AreNotEqual(1,2).
    Assert:AreNotEqual("test","fails").
    Assert:AreNotEqual(1.0,2.0).
    Assert:AreNotEqual(01/01/2000,01/01/2008).
    
    DO ON ERROR UNDO, THROW:
      Assert:AreNotEqual("test","test").
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.    		
      END CATCH.      
    END.
    Assert:Fail().    
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ObjectsAreEqual():
    DEFINE VARIABLE obj1 AS SimpleObject NO-UNDO.
    DEFINE VARIABLE obj2 AS SimpleObject NO-UNDO.

    ASSIGN 
      obj1 = NEW SimpleObject(11)
      obj2 = NEW SimpleObject(11).
    
    Assert:AreEqual(obj1,obj2).
    Assert:AreEqual(obj1,obj1).
    
    ASSIGN 
      obj1:SimpleProperty = 21
      obj2:SimpleProperty = 11.
    DO ON ERROR UNDO, THROW:
      Assert:AreEqual(obj1,obj2).
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
      FINALLY:
        DELETE OBJECT obj1 NO-ERROR.
        DELETE OBJECT obj2 NO-ERROR.           		
      END FINALLY.
    END.
    Assert:Fail().
    
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ObjectsAreNotEqual():
    DEFINE VARIABLE obj1 AS SimpleObject NO-UNDO.
    DEFINE VARIABLE obj2 AS SimpleObject NO-UNDO.

    ASSIGN 
      obj1 = NEW SimpleObject(21)
      obj2 = NEW SimpleObject(11).
    
    Assert:AreNotEqual(obj1,obj2).
    
    ASSIGN 
      obj1:SimpleProperty = 11
      obj2:SimpleProperty = 11.
    DO ON ERROR UNDO, THROW:
      Assert:AreNotEqual(obj1,obj2).
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
      FINALLY:
        DELETE OBJECT obj1 NO-ERROR.
        DELETE OBJECT obj2 NO-ERROR.           		
      END FINALLY.
    END.
    Assert:Fail().
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ObjectsAreSame():
    DEFINE VARIABLE obj1 AS Progress.Lang.Object NO-UNDO.
    DEFINE VARIABLE obj2 AS Progress.Lang.Object NO-UNDO.
    
    ASSIGN 
      obj1 = NEW Progress.Lang.Object()
      obj2 = NEW Progress.Lang.Object().
        
    Assert:AreSame(obj1,obj1).
    DO ON ERROR undo, THROW:
      Assert:AreSame(obj1,obj2).
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
      FINALLY:
        DELETE OBJECT obj1 NO-ERROR.
        DELETE OBJECT obj2 NO-ERROR. 
      END FINALLY.
    END.
    Assert:Fail().
    
  END METHOD.

  @Test.
  METHOD PUBLIC VOID ObjectIsNull():
    DEFINE VARIABLE obj1 AS Progress.Lang.Object NO-UNDO.
    Assert:IsNull(obj1).
    DO ON ERROR undo, THROW:
      obj1 = NEW Progress.Lang.Object().
      Assert:IsNull(obj1).
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
      FINALLY:
        DELETE OBJECT obj1 NO-ERROR.
      END FINALLY.
    END.
    Assert:Fail().
  END METHOD.

  @Test.
  METHOD PUBLIC VOID CharacterIsNotNull():
    DEFINE VARIABLE myChar AS CHARACTER NO-UNDO.
    myChar = "".
    Assert:IsNotNull(myChar).
    DO ON ERROR undo, THROW:
      myChar = ?.
      Assert:IsNotNull(myChar).
      CATCH e AS AssertionFailedError:
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
    END.
    Assert:Fail().
  END METHOD.

  @Test.
  METHOD PUBLIC VOID NullsAreEqual():
    DEFINE VARIABLE myChar1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE myChar2 AS CHARACTER NO-UNDO.
    
    ASSIGN 
      myChar1 = ?
      myChar2 = ?.

    Assert:AreEqual(myChar1,myChar2).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID TypeCastNumbersAreEqual():
    DEFINE VARIABLE myDec AS DECIMAL NO-UNDO INIT 1.0.
    DEFINE VARIABLE myInt AS INTEGER NO-UNDO INIT 1.
    DEFINE VARIABLE myInt64 AS INT64 NO-UNDO INIT 1.
    
    Assert:AreEqual(myDec, myInt).
    Assert:AreEqual(myInt64, myDec).
    Assert:AreEqual(myInt, myInt64).
           
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID TypeCastStringsAreEqual():
    DEFINE VARIABLE myChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE myLongChar AS LONGCHAR NO-UNDO.
    
    ASSIGN
      myChar = "test"
      myLongChar = "test".   
    Assert:AreEqual(myChar, myLongChar).
           
  END METHOD.
  
   
END CLASS.