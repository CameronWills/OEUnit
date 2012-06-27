ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestSuite.
USING OEUnit.Tests.Reflection.*.

CLASS OEUnit.Tests.Reflection.AllTestSuite INHERITS TestSuite:
  
  CONSTRUCTOR AllTestSuite():
    AddTest(NEW InfoBuilderTester()).
    AddTest(NEW MethodInfoTest()).
    AddTest(NEW AnnotationInfoTest()).
  END CONSTRUCTOR.

END CLASS.