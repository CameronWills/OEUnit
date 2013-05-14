/*------------------------------------------------------------------------------
  File        :   ResultsLoggerFactory.cls
  Package     :   OEUnit.Logger
  Description :   Factory for the different Results Loggers. 
  Author      :   Jamie Townsend
  Revisions   :   1.0 - November, 2010
                  Initial Implementation.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Logger.IResultsLogger.
USING OEUnit.Logger.ResultsSurefireLogger.

CLASS OEUnit.Logger.ResultsLoggerFactory:
  
  /*----------------------------------------------------------------------------
    Factory method for Results Loggers. Creates and returns an IResultsLogger 
    matching a given formatting. (Only Surefire-like log formats current
    supported.)
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC STATIC IResultsLogger GetResultsLogger():
      RETURN NEW ResultsSurefireLogger().
  END METHOD.
  
END CLASS.