/*------------------------------------------------------------------------------
  File        :   DataProvider.cls
  Package     :   OEUnit.Data
  Description :   Stores and uses data provided in associated methods when
                  calling a test case so that one test case can be called
                  multiple times.
------------------------------------------------------------------------------*/
USING Progress.Lang.*.
USING OEUnit.Reflection.*.
USING OEUnit.Data.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Data.DataProvider:

  DEFINE PROTECTED VARIABLE ttData  AS HANDLE NO-UNDO.
  DEFINE PROTECTED VARIABLE ttQuery AS HANDLE NO-UNDO.

  /*----------------------------------------------------------------------------
    Property that holds the current size of data
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO GET. PRIVATE SET.

  CONSTRUCTOR PUBLIC DataProvider():
    SUPER().
    Size = 0.
  END CONSTRUCTOR.

  DESTRUCTOR PUBLIC DataProvider():
    IF VALID-HANDLE(ttData) THEN DELETE OBJECT ttData.
    IF VALID-HANDLE(ttQuery) THEN
    DO:
        ttQuery:QUERY-CLOSE().
        DELETE OBJECT ttQuery.
    END.
  END DESTRUCTOR.

  METHOD PROTECTED VOID CountSize():
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    Size = 0.
    ResetQuery().
    DO WHILE MoveNext() NE FALSE:
        ASSIGN Size = Size + 1.
    END.
    ResetQuery().
  END.

  METHOD PUBLIC LOGICAL FromJSON(INPUT json AS LONGCHAR):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(ttQuery) THEN DELETE OBJECT ttQuery.
    IF VALID-HANDLE(ttData) THEN DELETE OBJECT ttData.
    CREATE TEMP-TABLE ttData.
    res = ttData:READ-JSON("LONGCHAR", json, "EMPTY").
    CountSize().
    RETURN res.
  END METHOD.

  METHOD PUBLIC LOGICAL FromJSONFile(INPUT path AS CHARACTER):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(ttQuery) THEN DELETE OBJECT ttQuery.
    IF VALID-HANDLE(ttData) THEN DELETE OBJECT ttData.
    CREATE TEMP-TABLE ttData.
    res = ttData:READ-JSON("FILE", path, "EMPTY").
    CountSize().
    RETURN res.
  END METHOD.

  METHOD PUBLIC LOGICAL FromXML(INPUT xml AS LONGCHAR):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(ttQuery) THEN DELETE OBJECT ttQuery.
    IF VALID-HANDLE(ttData) THEN DELETE OBJECT ttData.
    CREATE TEMP-TABLE ttData.
    res = ttData:READ-XML("LONGCHAR", xml, "EMPTY", ?, ?).
    CountSize().
    RETURN res.
  END METHOD.

  METHOD PUBLIC LOGICAL FromXMLFile(INPUT path AS CHARACTER):
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF VALID-HANDLE(ttQuery) THEN DELETE OBJECT ttQuery.
    IF VALID-HANDLE(ttData) THEN DELETE OBJECT ttData.
    CREATE TEMP-TABLE ttData.
    res = ttData:READ-XML("FILE", path, "EMPTY", ?, ?).
    CountSize().
    RETURN res.
  END METHOD.

  METHOD PUBLIC Progress.Lang.ParameterList GetParameterList():
    DEFINE VARIABLE numParams   AS INTEGER NO-UNDO.
    DEFINE VARIABLE bufHandle   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER NO-UNDO.
    DEFINE VARIABLE fieldHandle AS HANDLE  NO-UNDO.
    DEFINE VARIABLE params      AS Progress.Lang.ParameterList NO-UNDO.
    IF NOT VALID-HANDLE(ttData) THEN RETURN ERROR NEW DataProviderError("Data has not been loaded into DataProvider").
    IF NOT VALID-HANDLE(ttQuery) OR ttQuery:IS-OPEN = FALSE THEN ResetQuery().
    IF ttQuery:QUERY-OFF-END = FALSE THEN
    DO:
        bufHandle  = ttQuery:GET-BUFFER-HANDLE(1).
        IF NOT bufHandle:AVAILABLE THEN MoveFirst().
        numParams = bufHandle:NUM-FIELDS.
        params    = NEW Progress.Lang.ParameterList(numParams).
        DO i = 1 TO numParams:
          fieldHandle = bufHandle:BUFFER-FIELD(i).
          params:SetParameter(i, fieldHandle:DATA-TYPE, "INPUT", fieldHandle:BUFFER-VALUE()).
        END.
    END.
    RETURN params.
  END METHOD.

  METHOD PROTECTED VOID ResetQuery():
    DEFINE VARIABLE qry AS CHARACTER NO-UNDO.
    IF NOT VALID-HANDLE(ttData) THEN RETURN.
    IF VALID-HANDLE(ttQuery) THEN
    DO:
      ttQuery:QUERY-CLOSE().
      DELETE OBJECT ttQuery.
    END.
    qry = "FOR EACH " + ttData:NAME.
    CREATE QUERY ttQuery.
    ttQuery:SET-BUFFERS(ttData:DEFAULT-BUFFER-HANDLE).
    IF ttQuery:QUERY-PREPARE(qry) THEN ttQuery:QUERY-OPEN().
    ELSE IF VALID-HANDLE(ttQuery) THEN DELETE OBJECT ttQuery.
  END METHOD.

  METHOD PUBLIC LOGICAL MoveFirst():
    DEFINE VARIABLE res AS LOGICAL NO-UNDO INITIAL FALSE.
    IF NOT VALID-HANDLE(ttQuery) THEN
      ResetQuery().
    IF VALID-HANDLE(ttQuery) THEN
      res = ttQuery:GET-FIRST(NO-LOCK).
    RETURN res.
  END METHOD.

  METHOD PUBLIC LOGICAL MoveLast():
    DEFINE VARIABLE res AS LOGICAL NO-UNDO INITIAL FALSE.
    IF NOT VALID-HANDLE(ttQuery) THEN
      ResetQuery().
    IF VALID-HANDLE(ttQuery) THEN
      res = ttQuery:GET-LAST(NO-LOCK).
    RETURN res.
  END METHOD.

  METHOD PUBLIC LOGICAL MoveNext():
    DEFINE VARIABLE res AS LOGICAL NO-UNDO INITIAL FALSE.
    IF NOT VALID-HANDLE(ttQuery) THEN
      ResetQuery().
    IF VALID-HANDLE(ttQuery) THEN
      res = ttQuery:GET-NEXT(NO-LOCK).
    RETURN res.
  END METHOD.

  METHOD PUBLIC LOGICAL MovePrev():
    DEFINE VARIABLE res AS LOGICAL NO-UNDO INITIAL FALSE.
    IF NOT VALID-HANDLE(ttQuery) THEN
      ResetQuery().
    IF VALID-HANDLE(ttQuery) THEN
      res = ttQuery:GET-PREV(NO-LOCK).
    RETURN res.
  END METHOD.

END CLASS.