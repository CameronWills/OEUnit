
ROUTINE-LEVEL ON ERROR UNDO, THROW. 

USING Progress.Lang.*.
USING OEUnit.Assertion.Assert.
USING OEUnit.Util.*.
USING OEUnit.Tests.Util.*.

CLASS OEUnit.Tests.Util.NamedListTest :
    
  DEFINE PRIVATE VARIABLE list1 AS NamedList NO-UNDO.
  
  DEFINE PRIVATE VARIABLE name1  AS CHARACTER NO-UNDO INITIAL "Name1".
  DEFINE PRIVATE VARIABLE value1 AS CHARACTER NO-UNDO INITIAL "Value1".
  DEFINE PRIVATE VARIABLE name2  AS CHARACTER NO-UNDO INITIAL "Name2".
  DEFINE PRIVATE VARIABLE value2 AS CHARACTER NO-UNDO INITIAL "Value2".
  DEFINE PRIVATE VARIABLE name3  AS CHARACTER NO-UNDO INITIAL "Name3".
  DEFINE PRIVATE VARIABLE value3 AS CHARACTER NO-UNDO INITIAL "Value3".
  DEFINE PRIVATE VARIABLE name4  AS CHARACTER NO-UNDO INITIAL "Name4".
  DEFINE PRIVATE VARIABLE value4 AS CHARACTER NO-UNDO INITIAL "Value4".

  @Before.
  METHOD PUBLIC VOID CreateList():
    list1 = NEW NamedList().
    list1:Add(name1, value1).
    list1:Add(name2, value2).
    list1:Add(name3, value3).
    list1:Add(name4, value4).
  END METHOD.

  @After.
  METHOD PUBLIC VOID DeleteList():
    DELETE OBJECT list1 NO-ERROR.
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID AddDuplicate():
    list1:Add(name1, value2).
    Assert:AreEqual(list1:GET(name1), value2).
  END METHOD.

  @Test.
  METHOD PUBLIC VOID TestMoveAndReset():
    list1:MoveFirst().
    Assert:AreEqual(list1:CurrentName, name1).
    Assert:AreEqual(list1:CurrentValue, value1).
    list1:MoveNext().
    Assert:AreEqual(list1:CurrentName, name2).
    Assert:AreEqual(list1:CurrentValue, value2).
    list1:Reset().                       
    list1:MoveNext().
    Assert:AreEqual(list1:CurrentName, name1).
    Assert:AreEqual(list1:CurrentValue, value1).
    list1:MoveNext().
    Assert:AreEqual(list1:CurrentName, name2).
    Assert:AreEqual(list1:CurrentValue, value2).
    list1:Reset().                       
    list1:MovePrevious().
    Assert:AreEqual(list1:CurrentName, name4).
    Assert:AreEqual(list1:CurrentValue, value4).
    list1:MovePrevious().
    Assert:AreEqual(list1:CurrentName, name3).
    Assert:AreEqual(list1:CurrentValue, value3).
    list1:MoveFirst().
    Assert:AreEqual(list1:CurrentName, name1).
    Assert:AreEqual(list1:CurrentValue, value1).
    list1:MoveLast().
    Assert:AreEqual(list1:CurrentName, name4).
    Assert:AreEqual(list1:CurrentValue, value4).
  END METHOD.

  @Test(expected="Progress.Lang.AppError").
  METHOD PUBLIC VOID Remove():
    Assert:AreEqual(list1:Get(name1), value1).
    list1:Remove(name1).
    list1:Get(name1).
  END METHOD.
  
  @Test(expected="Progress.Lang.AppError").
  METHOD PUBLIC VOID RemoveNonExisting():
    list1:Remove("does not exist").
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID Get():
    Assert:AreEqual(list1:Get(name1), value1).
  END METHOD.
  
  @Test(expected="Progress.Lang.AppError").
  METHOD PUBLIC VOID GetNonExisting():
    list1:Get("does not exist").
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID HasValue():
    Assert:IsTrue(list1:HasValue(name1)).
    Assert:IsFalse(list1:HasValue("does not exist")).
  END METHOD.
  
END CLASS.

