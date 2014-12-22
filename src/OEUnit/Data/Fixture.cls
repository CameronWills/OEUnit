/*------------------------------------------------------------------------------
  File        :   Fixture.cls
  Package     :   OEUnit.Data
  Description :   Stores and uses data provided in associated methods when
                  calling a test case to pre-load database tables for a test.
------------------------------------------------------------------------------*/
USING Progress.Lang.*.
USING OEUnit.Data.*.
USING OEUnit.Util.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Data.Fixture: 
	
  DEFINE PROTECTED VARIABLE Fixtures AS List NO-UNDO. 
  
  /*----------------------------------------------------------------------------
    Read-only Property that returns the number of top level buffers in dataset
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC PROPERTY TableCount AS INTEGER NO-UNDO
    GET ():
      DEFINE VARIABLE tCount AS INTEGER NO-UNDO INITIAL 0.
      DEFINE VARIABLE res AS LOGICAL NO-UNDO.
      DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
      IF VALID-OBJECT(Fixtures) THEN
      DO:
          res = Fixtures:MoveFirst().
          DO WHILE res = TRUE
            ON ERROR UNDO, THROW:
            fix = CAST(Fixtures:CURRENT, "OEUnit.Data.FixtureDataSet").
            tCount = tCount + fix:TableCount.
            res = Fixtures:MoveNext().
          END.
          RETURN tCount.
      END.
      RETURN 0.
    END GET.

  /*----------------------------------------------------------------------------
    The number of elements in the list  
  ----------------------------------------------------------------------------*/  
  DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO INIT 0
    GET ():
      IF VALID-OBJECT(Fixtures) THEN RETURN Fixtures:Size.
      ELSE RETURN 0.
    END GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC Fixture():
	SUPER ().
	Fixtures = NEW List(TRUE). /* TRUE => destory list objects on destruction */
  END CONSTRUCTOR.

  DESTRUCTOR PUBLIC Fixture():
    IF VALID-OBJECT(Fixtures) THEN DELETE OBJECT Fixtures.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Import fixture data from dataset encoded in JSON via parameter
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromJSON(INPUT json AS LONGCHAR):
    DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    fix = NEW FixtureDataSet().
    res = fix:FromJSON(INPUT json).
    IF res = TRUE THEN
      Fixtures:Add(fix).
    ELSE
      IF VALID-OBJECT(fix) THEN
        DELETE OBJECT fix.
    RETURN res.
  END METHOD.

  /*----------------------------------------------------------------------------
    Import fixture data from data encoded in JSON in given file
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromJSONFile(INPUT path AS CHARACTER):
    DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    fix = NEW FixtureDataSet().
    res = fix:FromJSONFile(INPUT path).
    IF res = TRUE THEN
      Fixtures:Add(fix).
    ELSE
      IF VALID-OBJECT(fix) THEN
        DELETE OBJECT fix.
    RETURN res.
  END METHOD.

  /*----------------------------------------------------------------------------
    Import fixture data from dataset encoded in XML via parameter
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromXML(INPUT xml AS LONGCHAR):
    DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    fix = NEW FixtureDataSet().
    res = fix:FromXML(INPUT xml).
    IF res = TRUE THEN
      Fixtures:Add(fix).
    ELSE
      IF VALID-OBJECT(fix) THEN
        DELETE OBJECT fix.
    RETURN res.
  END METHOD.

  /*----------------------------------------------------------------------------
    Import fixture data from data encoded in XML in given file
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromXMLFile(INPUT path AS CHARACTER):
    DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    fix = NEW FixtureDataSet().
    res = fix:FromXMLFile(INPUT path).
    IF res = TRUE THEN
      Fixtures:Add(fix).
    ELSE
      IF VALID-OBJECT(fix) THEN
        DELETE OBJECT fix.
    RETURN res.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Import fixture data by copying from an existing dataset
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromDataSet(INPUT dsSrc AS HANDLE):
    DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    fix = NEW FixtureDataSet().
    res = fix:FromDataSet(INPUT dsSrc).
    IF res = TRUE THEN
      Fixtures:Add(fix).
    ELSE
      IF VALID-OBJECT(fix) THEN
        DELETE OBJECT fix.
    RETURN res.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Import fixture data by copying from a temp-table
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL FromTempTable(INPUT ttSrc AS HANDLE):
    DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    fix = NEW FixtureDataSet().
    res = fix:FromTempTable(INPUT ttSrc).
    IF res = TRUE THEN
      Fixtures:Add(fix).
    ELSE
      IF VALID-OBJECT(fix) THEN
        DELETE OBJECT fix.
    RETURN res.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Create data in the attached databases, based on data in dataset tables.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL CreateData():
    DEFINE VARIABLE fix AS FixtureDataSet NO-UNDO.
    DEFINE VARIABLE res AS LOGICAL NO-UNDO.
    IF NOT VALID-OBJECT(Fixtures) THEN RETURN FALSE.
    res = fixtures:MoveFirst().
    DO WHILE res = TRUE
      ON ERROR UNDO, THROW:
      fix = CAST(fixtures:CURRENT, "OEUnit.Data.FixtureDataSet").
      IF VALID-OBJECT(fix) THEN
        fix:CreateData().
      res = fixtures:MoveNext().
    END.
  END METHOD.

END CLASS.