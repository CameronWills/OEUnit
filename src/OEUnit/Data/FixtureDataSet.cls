/*------------------------------------------------------------------------------
  File        :   FixtureDataSet.cls
  Package     :   OEUnit.Data
  Description :   Stores and uses data provided in associated methods when
                  calling a test case to pre-load database tables for a test.
------------------------------------------------------------------------------*/
USING Progress.Lang.*.
USING OEUnit.Data.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Data.FixtureDataSet: 
	
  DEFINE PROTECTED VARIABLE dsData AS HANDLE NO-UNDO.
  
  /*----------------------------------------------------------------------------
    Read-only Property that returns the number of top level buffers in dataset
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY TableCount AS INTEGER NO-UNDO
    GET ():
      IF VALID-HANDLE(dsData) THEN RETURN dsData:NUM-TOP-BUFFERS.
      ELSE RETURN 0.
    END GET.

  CONSTRUCTOR PUBLIC FixtureDataSet():
	SUPER ().
  END CONSTRUCTOR.

  DESTRUCTOR PUBLIC FixtureDataSet():
    IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Import fixture data from dataset encoded in JSON via parameter
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromJSON(INPUT json AS LONGCHAR):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
    CREATE DATASET dsData.
    res = dsData:READ-JSON("LONGCHAR", json, "EMPTY").
    RETURN res.
  END METHOD.

  /*----------------------------------------------------------------------------
    Import fixture data from data encoded in JSON in given file
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromJSONFile(INPUT path AS CHARACTER):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
    CREATE DATASET dsData.
    res = dsData:READ-JSON("FILE", path, "EMPTY").
    RETURN res.
  END METHOD.

  /*----------------------------------------------------------------------------
    Import fixture data from dataset encoded in XML via parameter
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromXML(INPUT xml AS LONGCHAR):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
    CREATE DATASET dsData.
    res = dsData:READ-XML("LONGCHAR", xml, "EMPTY", ?, ?).
    RETURN res.
  END METHOD.

  /*----------------------------------------------------------------------------
    Import fixture data from data encoded in XML in given file
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromXMLFile(INPUT path AS CHARACTER):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
    CREATE DATASET dsData.
    res = dsData:READ-XML("FILE", path, "EMPTY", ?, ?).
    RETURN res.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Import fixture data by copying from an existing dataset
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromDataSet(INPUT dsSrc AS HANDLE):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
    CREATE DATASET dsData.
    res = dsData:COPY-DATASET(dsSrc).
    RETURN res.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Import fixture data by copying from a temp-table
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromTempTable(INPUT ttSrc AS HANDLE):
    DEFINE VARIABLE res       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE ttData    AS HANDLE NO-UNDO.
    DEFINE VARIABLE ttDataBuf AS HANDLE NO-UNDO.
    IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
    CREATE DATASET dsData.
    IF NOT VALID-HANDLE(dsData) THEN RETURN FALSE.
    CREATE TEMP-TABLE ttData.
    /* COPY-TEMP-TABLE will also copy data as well, and hence there
     * is no need to configure data sources and call DataSet:Fill()
     */
    res = ttData:COPY-TEMP-TABLE(ttSrc,FALSE,FALSE,FALSE,"").
    IF res = FALSE THEN
    DO:
      IF VALID-HANDLE(dsData) THEN DELETE OBJECT dsData.
      IF VALID-HANDLE(ttData) THEN DELETE OBJECT ttData.
      RETURN res.
    END.
    ttDataBuf = ttData:DEFAULT-BUFFER-HANDLE.
    dsData:ADD-BUFFER(ttDataBuf).
    RETURN TRUE.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Create data in the attached databases, based on data in dataset tables.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL CreateData():
    DEFINE VARIABLE hBuffer  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hQuery   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hQBuffer AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRBuffer AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER NO-UNDO.
    
    /* Ensure that any errors are thrown properly */
    DO ON ERROR UNDO, THROW:
      IF NOT VALID-HANDLE(dsData) THEN RETURN FALSE.
      DO iCount = 1 TO dsData:NUM-TOP-BUFFERS
        ON ERROR UNDO, THROW:
        hBuffer = dsData:GET-TOP-BUFFER(iCount).
        IF NOT VALID-HANDLE(hBuffer) THEN RETURN ERROR NEW FixtureError("Failed to fetch buffer from DataSet").
        IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
        IF VALID-HANDLE(hQBuffer) THEN DELETE OBJECT hQBuffer.
        CREATE BUFFER hQBuffer FOR TABLE hBuffer.
        CREATE BUFFER hRBuffer FOR TABLE hBuffer:NAME NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR NEW FixtureError(SUBSTITUTE("Could not create buffer for destination table &1.", hBuffer:NAME)).
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hQBuffer). /* NOTE: Because this is taken from Dataset, names of tables can overlap and doesn't cause a problem */
        IF(hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " NO-LOCK:") = FALSE) THEN
          RETURN ERROR NEW FixtureError(SUBSTITUTE("Could not query data in source &1.", hBuffer:NAME)).
        hQuery:QUERY-OPEN().
        hQuery:GET-FIRST(NO-LOCK).
        DO WHILE NOT hQuery:QUERY-OFF-END
          ON ERROR UNDO, THROW:
            hRBuffer:BUFFER-CREATE().
            hRBuffer:BUFFER-COPY(hQBuffer).
            hRBuffer:BUFFER-RELEASE().
            hQuery:GET-NEXT().
        END.
        hQuery:QUERY-CLOSE().
        IF VALID-HANDLE(hRBuffer) THEN DELETE OBJECT hRBuffer.
        IF VALID-HANDLE(hQBuffer) THEN DELETE OBJECT hQBuffer.
        IF VALID-HANDLE(hQuery)   THEN DELETE OBJECT hQuery.
      END.
    END.
  END METHOD.

END CLASS.