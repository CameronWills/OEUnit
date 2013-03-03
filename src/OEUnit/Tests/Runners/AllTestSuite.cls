ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Tests.Runners.*.

CLASS OEUnit.Tests.Runners.AllTestSuite INHERITS OEUnit.Runner.TestSuite:
  
  CONSTRUCTOR AllTestSuite():
    AddTest(NEW TestCaseTester()).
  END CONSTRUCTOR.

END CLASS.