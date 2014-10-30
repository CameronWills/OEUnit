ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestSuite.
USING OEUnit.Runners.RunTest.

CLASS OEUnit.Tests.AllTestSuite INHERITS TestSuite:
  
  CONSTRUCTOR AllTestSuite():
    AddTest(NEW OEUnit.Tests.Assertion.AllTestSuite()).
    AddTest(NEW OEUnit.Tests.Data.AllTestSuite()).
    AddTest(NEW OEUnit.Tests.Reflection.AllTestSuite()).
    AddTest(NEW OEUnit.Tests.Runner.AllTestSuite()).
    AddTest(NEW OEUnit.Tests.Runners.AllTestSuite()).
    AddTest(NEW OEUnit.Tests.Util.AllTestSuite()).
  END CONSTRUCTOR.

END CLASS.