
ROUTINE-LEVEL ON ERROR UNDO, THROW. 

USING Progress.Lang.*.
USING OEUnit.Assertion.Assert.
USING OEUnit.Util.*.
USING OEUnit.Tests.Util.*.

CLASS OEUnit.Tests.Util.ListTest :
    
  DEFINE PRIVATE VARIABLE list1 AS List NO-UNDO.
  DEFINE PRIVATE VARIABLE obj1 AS Object NO-UNDO.
  DEFINE PRIVATE VARIABLE obj2 AS Object NO-UNDO. 
  DEFINE PRIVATE VARIABLE obj3 AS Object NO-UNDO.
  DEFINE PRIVATE VARIABLE obj4 AS Object NO-UNDO.
  DEFINE PRIVATE VARIABLE refobj AS Object NO-UNDO.

  @Before.
  METHOD PUBLIC VOID CreateList():
    list1 = NEW List(TRUE).
    obj1 = NEW Object().
    obj2 = NEW Object().
    obj3 = NEW Object().
    obj4 = NEW Object().
    list1:Add(obj1).
    list1:Add(obj2).
    list1:Add(obj3).
    list1:Add(obj4).
  END METHOD.    

  @After.
  METHOD PUBLIC VOID DeleteList():
    DELETE OBJECT list1 NO-ERROR.
    DELETE OBJECT obj1 NO-ERROR.
    DELETE OBJECT obj2 NO-ERROR.
    DELETE OBJECT obj3 NO-ERROR.
    DELETE OBJECT obj4 NO-ERROR.
  END METHOD.    

  @Test.
  METHOD PUBLIC VOID TestMoveAndReset():
    list1:MoveNext().
    Assert:AreSame(obj1,list1:Current).
    list1:MoveNext().
    Assert:AreSame(obj2,list1:Current).  
    list1:Reset().                       
    list1:MoveNext().
    Assert:AreSame(obj1,list1:Current).  
    list1:MoveNext().
    Assert:AreSame(obj2,list1:Current).  
    list1:Reset().                       
    list1:MovePrevious().
    Assert:AreSame(obj4,list1:Current).
    list1:MovePrevious().
    Assert:AreSame(obj3,list1:Current).
    list1:MoveFirst().
    Assert:AreSame(obj1,list1:Current).
    list1:MoveLast().
    Assert:AreSame(obj4,list1:Current).    
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID MoveForwards():
    list1:Move(1,3).
    Assert:AreSame(obj1,list1:Get(3)).
    Assert:AreSame(obj2,list1:Get(1)).
    Assert:AreSame(obj3,list1:Get(2)).
    Assert:AreSame(obj4,list1:Get(4)).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID MoveBackwards():
    list1:Move(4,1).
    Assert:AreSame(obj1,list1:Get(2)).
    Assert:AreSame(obj2,list1:Get(3)).
    Assert:AreSame(obj3,list1:Get(4)).
    Assert:AreSame(obj4,list1:Get(1)).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID Insert():
    list1:Insert(obj1, 3).
    Assert:AreSame(obj1,list1:Get(1)).
    Assert:AreSame(obj2,list1:Get(2)).
    Assert:AreSame(obj1,list1:Get(3)).
    Assert:AreSame(obj3,list1:Get(4)).
    Assert:AreSame(obj4,list1:Get(5)).
  END METHOD.
   
  @Test.
  METHOD PUBLIC VOID InsertBeforeStart():    
    list1:Insert(obj4, -1).
    Assert:AreSame(obj4,list1:Get(1)).
    Assert:AreSame(obj1,list1:Get(2)).
    Assert:AreSame(obj2,list1:Get(3)).
    Assert:AreSame(obj3,list1:Get(4)).
    Assert:AreSame(obj4,list1:Get(5)).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID InsertAfterEnd():
    list1:Insert(obj2, 10).
    Assert:AreSame(obj1,list1:Get(1)).
    Assert:AreSame(obj2,list1:Get(2)).
    Assert:AreSame(obj3,list1:Get(3)).
    Assert:AreSame(obj4,list1:Get(4)).
    Assert:AreSame(obj2,list1:Get(5)).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID Remove():
    list1:Add(list1:Remove(1)).
    Assert:AreSame(obj2,list1:Get(1)).
    Assert:AreSame(obj1,list1:Get(4)).
    Assert:AreEqual(list1:Size, 4).
    list1:Remove(4).
    Assert:AreEqual(list1:Size, 3).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID RemoveAndDelete():
    list1:RemoveAndDelete(1).
    Assert:AreSame(obj2,list1:Get(1)).
    Assert:AreEqual(list1:Size, 3).
  END METHOD. 

  @Test.
  METHOD PUBLIC VOID Sort():
    DEFINE VARIABLE comp AS IComparator NO-UNDO.
    comp = NEW SimpleComparator().
    list1:Sort(comp).
    Assert:AreSame(obj1,list1:Get(4)).
    Assert:AreSame(obj2,list1:Get(3)).
    Assert:AreSame(obj3,list1:Get(2)).
    Assert:AreSame(obj4,list1:Get(1)).
    list1:Sort(comp).
    Assert:AreSame(obj1,list1:Get(1)).
    Assert:AreSame(obj2,list1:Get(2)).
    Assert:AreSame(obj3,list1:Get(3)).
    Assert:AreSame(obj4,list1:Get(4)).
    FINALLY:
      DELETE OBJECT comp NO-ERROR.
    END FINALLY.
  END METHOD.
  
  
END CLASS.

