ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestSuite.
USING OEUnit.Tests.Assertion.*.

CLASS OEUnit.Tests.Assertion.AllTestSuite INHERITS TestSuite:
  
  CONSTRUCTOR AllTestSuite():
    AddTest(NEW AssertStringTest()).
    AddTest(NEW AssertTest()).
    AddTest(NEW FailMessageTester()).
  END CONSTRUCTOR.

END CLASS.