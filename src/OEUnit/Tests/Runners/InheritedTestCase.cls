ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING OEUnit.Tests.Runners.*.

CLASS OEUnit.Tests.Runners.InheritedTestCase INHERITS TestCase:
  
  DEFINE PUBLIC PROPERTY SecondBeforeCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY SecondBeforeClassCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY SecondTestCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY OverridedTestCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY FirstAfterCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY SecondAfterCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY FirstAfterClassCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY SecondAfterClassCalled AS LOGICAL NO-UNDO INIT FALSE GET. PRIVATE SET.

  CONSTRUCTOR InheritedTestCase():
    SUPER("").
  END CONSTRUCTOR.

  @BeforeClass.
  METHOD PUBLIC VOID SecondBeforeClass():
    calledMethods = calledMethods + "SecondBeforeClass,".
    SecondBeforeClassCalled = TRUE.
  END METHOD. 

  @Before.
  METHOD PUBLIC VOID SecondBefore():
    calledMethods = calledMethods + "SecondBefore,".
    SecondBeforeCalled = TRUE.
  END METHOD. 

  @Test.
  METHOD PUBLIC VOID SecondTest():
    calledMethods = calledMethods + "SecondTest,".
    SecondTestCalled = TRUE.
  END METHOD. 

  @Test.
  METHOD PUBLIC OVERRIDE VOID Test():
    calledMethods = calledMethods + "Test,".
    OverridedTestCalled = TRUE.
  END METHOD. 
  
  @After.
  METHOD PUBLIC VOID SecondAfter():
    calledMethods = calledMethods + "SecondAfter,".
    SecondAfterCalled = TRUE.
  END METHOD.  

  @AfterClass.
  METHOD PUBLIC VOID SecondAfterClass():
    calledMethods = calledMethods + "SecondAfterClass,".
    SecondAfterClassCalled = TRUE.
  END METHOD.  

END CLASS.