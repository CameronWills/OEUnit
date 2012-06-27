ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Tests.Runners.Fail.

@Ignore.
CLASS OEUnit.Tests.Runners.IgnoredSuite INHERITS OEUnit.Runner.TestSuite:
  
  CONSTRUCTOR IgnoredSuite():
  	AddTest(NEW Fail()).
  END CONSTRUCTOR.	
  
END CLASS.