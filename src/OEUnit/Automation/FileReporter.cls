/*------------------------------------------------------------------------------
  File        :   FileReporter.cls
  Package     :   OEUnit.Automation
  Description :   Base Reporter Class for outputing data to a file.
  Author      :   Mark Abbott
------------------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING OEUnit.Automation.BaseReporter.

CLASS OEUnit.Automation.FileReporter INHERITS BaseReporter ABSTRACT:
  
  /*----------------------------------------------------------------------------
    File name and path for output file.
  ----------------------------------------------------------------------------*/  
  DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET.
    
  /*----------------------------------------------------------------------------
    Class constructor. fName is path and file name for output XML file.
  ----------------------------------------------------------------------------*/    
  CONSTRUCTOR PUBLIC FileReporter(INPUT fName AS CHARACTER):
      SUPER ().
        
      Filename = fName.
        
  END CONSTRUCTOR.

END CLASS.