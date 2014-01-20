/*------------------------------------------------------------------------------
  File        :   IResultsLogger.cls
  Package     :   OEUnit.Logger
  Description :   Classes implementing the IResultLogger interface can be used
                  by the ResultsLogger to write the results of a test run to a
                  file in a given directory.
  Author      :   Jamie Townsend
  Revisions   :   1.0 - November, 2010
                  Initial Implementation.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Runner.TestClassResult.

INTERFACE OEUnit.Logger.IResultsLogger:
  
  /*----------------------------------------------------------------------------
    Sets the directory location where log files will be written.
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC VOID SetOutpuDirectory(INPUT outputDirectory AS CHARACTER).
  
  /*----------------------------------------------------------------------------
    Writes the test results to the output directory.
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC VOID WriteResults(INPUT results AS TestClassResult).
  
END INTERFACE.