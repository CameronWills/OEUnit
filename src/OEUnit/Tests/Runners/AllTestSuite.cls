ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Tests.Runners.*.

CLASS OEUnit.Tests.Runners.AllTestSuite INHERITS OEUnit.Runner.TestSuite:
  
  CONSTRUCTOR AllTestSuite():
    AddTest(NEW TestCaseTester()).
    AddTest(NEW Abstract()).
  END CONSTRUCTOR.

END CLASS.