/*------------------------------------------------------------------------------
  File        :   TextReporter.cls
  Package     :   OEUnit.Automation
  Description :   Basic Text Reporter Class.
  Author      :   Mark Abbott
------------------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING OEUnit.Automation.FileReporter.
USING OEUnit.Runner.*.

CLASS OEUnit.Automation.TextReporter INHERITS FileReporter:
    
  /*----------------------------------------------------------------------------
    Stream where file output will be sent.
  ----------------------------------------------------------------------------*/    
  DEFINE PRIVATE STREAM OutputStream.

  /*----------------------------------------------------------------------------
    Class constructor. fName is path and file name for output XML file.
  ----------------------------------------------------------------------------*/    
  CONSTRUCTOR PUBLIC TextReporter(INPUT fName AS CHARACTER):
      SUPER (fName).
  END CONSTRUCTOR.

  /*----------------------------------------------------------------------------
    Reports TestClassResult using built in ToString() method
  ----------------------------------------------------------------------------*/  
  METHOD OVERRIDE PUBLIC VOID Report( INPUT results AS OEUnit.Runner.TestClassResult ):
	    
      OUTPUT STREAM OutputStream TO VALUE(FileName) UNBUFFERED.
		
      PUT STREAM OutputStream UNFORMATTED results:ToString() SKIP.
		
      OUTPUT STREAM OutputStream CLOSE.

  END METHOD.

END CLASS.