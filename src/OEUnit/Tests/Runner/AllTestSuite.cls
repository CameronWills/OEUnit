ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestSuite.
USING OEUnit.Runners.RunTest.
USING OEUnit.Tests.Runner.*.
USING OEUnit.Tests.Runner.Notification.*.
USING OEUnit.Tests.Runner.Manipulation.*.

CLASS OEUnit.Tests.Runner.AllTestSuite INHERITS TestSuite:
  
  CONSTRUCTOR AllTestSuite():
    AddTest(NEW StatusTester()).
    AddTest(NEW NotifierTester()).
    AddTest(NEW FilterTester()).
  END CONSTRUCTOR.

END CLASS.