ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestSuite.
USING OEUnit.Runners.RunTest.
USING OEUnit.Tests.Util.*.

CLASS OEUnit.Tests.Util.AllTestSuite INHERITS TestSuite:
  
  CONSTRUCTOR AllTestSuite():
    AddTest(NEW ListTest()).
    AddTest(NEW NamedListTest()).
  END CONSTRUCTOR.

END CLASS.