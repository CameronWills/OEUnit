ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.

CLASS OEUnit.Tests.Runners.TestCase:
  
  DEFINE PUBLIC PROPERTY BeforeCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY BeforeClassCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY TestCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY After1Called AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY After2Called AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AfterClass1Called AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AfterClass2Called AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.

  DEFINE PRIVATE VARIABLE throwErrors AS CHARACTER NO-UNDO.
  DEFINE PUBLIC VARIABLE calledMethods AS CHARACTER NO-UNDO.
  
  CONSTRUCTOR TestCase(INPUT methodsThrowingErrors AS CHARACTER):
    throwErrors = methodsThrowingErrors.
  END CONSTRUCTOR.
  
  METHOD PRIVATE LOGICAL ShouldThrowError(INPUT methodNames AS CHARACTER):
    RETURN (LOOKUP(methodNames, throwErrors) > 0).
  END METHOD.

  METHOD PUBLIC LOGICAL WasRunBefore(INPUT firstMethod AS CHARACTER, INPUT secondMethod AS CHARACTER):
    RETURN (LOOKUP(firstMethod, calledMethods) < LOOKUP(secondMethod, calledMethods)).
  END METHOD.

  @BeforeClass.
  METHOD PUBLIC VOID BeforeClass():
    calledMethods = "BeforeClass,".
    BeforeClassCalled = TRUE.
    IF ShouldThrowError("BeforeClass") THEN
      RETURN ERROR NEW AppError("Error occured in BeforeClass", 0).
  END METHOD. 

  @Before.
  METHOD PUBLIC VOID Before():
    calledMethods = calledMethods + "Before,".
    BeforeCalled = TRUE.
    IF ShouldThrowError("Before") THEN
      RETURN ERROR NEW AppError("Error occured in Before", 0).
  END METHOD. 

  @Test.
  METHOD PUBLIC VOID Test():
    calledMethods = calledMethods + "Test,".
    TestCalled = TRUE.
    IF ShouldThrowError("Test") THEN
      RETURN ERROR NEW AppError("Error occured in Test", 0).
  END METHOD. 
  
  @After.
  METHOD PUBLIC VOID After1():
    calledMethods = calledMethods + "After1,".
    After1Called = TRUE.
    IF ShouldThrowError("After") THEN
      RETURN ERROR NEW AppError("Error occured in After1", 0).
  END METHOD.  

  @After.
  METHOD PUBLIC VOID After2():
    calledMethods = calledMethods + "After2,".
    After2Called = TRUE.
    IF ShouldThrowError("After") THEN
      RETURN ERROR NEW AppError("Error occured in After2", 0).
  END METHOD.  

  @AfterClass.
  METHOD PUBLIC VOID AfterClass1():
    calledMethods = calledMethods + "AfterClass1,".
    AfterClass1Called = TRUE.
    IF ShouldThrowError("AfterClass") THEN
      RETURN ERROR NEW AppError("Error occured in AfterClass1", 0).
  END METHOD.  

  @AfterClass.
  METHOD PUBLIC VOID AfterClass2():
    calledMethods = calledMethods + "AfterClass2,".
    AfterClass2Called = TRUE.
    IF ShouldThrowError("AfterClass") THEN
      RETURN ERROR NEW AppError("Error occured in AfterClass2", 0).
  END METHOD.  

END CLASS.