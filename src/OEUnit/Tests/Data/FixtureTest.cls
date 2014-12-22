
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING OEUnit.Tests.Data.*.
USING OEUnit.Assertion.Assert.
USING OEUnit.Data.Fixture.

CLASS OEUnit.Tests.Data.FixtureTest: 
    
    DEFINE PROTECTED TEMP-TABLE ttTestTable NO-UNDO
      FIELD StatusCode AS CHARACTER
      FIELD Accepted   AS LOGICAL
      INDEX StatusCode IS PRIMARY UNIQUE StatusCode ASCENDING.
    
    DEFINE PROTECTED TEMP-TABLE ttSecondTestTable NO-UNDO
      LIKE ttTestTable.
    
    DEFINE VARIABLE fixture AS Fixture NO-UNDO.

    @After.
    METHOD PUBLIC VOID DeleteFixture():
        DELETE OBJECT fixture NO-ERROR.
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromJSON():
        fixture = NEW Fixture().
        fixture:FromJSON("~{ ~"FirstTable~": ["
                         + "  ~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                         + "  ~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                         + " ], "
                         + " ~"SecondTable~": ["
                         + "  ~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                         + "  ~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0}"
                         + "]}").
        Assert:AreEqual(fixture:TableCount, 2).
        Assert:AreEqual(fixture:Size, 1).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromJSON_Multiple():
        fixture = NEW Fixture().
        fixture:FromJSON("~{ ~"FirstTable~": ["
                         + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                         + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                         + "]}").
        Assert:AreEqual(fixture:TableCount, 1).
        Assert:AreEqual(fixture:Size, 1).
        fixture:FromJSON("~{ ~"FirstTable~": ["
                         + "  ~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                         + "  ~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                         + " ], "
                         + " ~"SecondTable~": ["
                         + "  ~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                         + "  ~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0}"
                         + "]}").
        Assert:AreEqual(fixture:TableCount, 3).
        Assert:AreEqual(fixture:Size, 2).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromXML():
        fixture = NEW Fixture().
        fixture:FromXML('<?xml version="1.0"?>'
                        + '<ProDataSet>'
                        +   '<FirstTable>'
                        +     '<status>NEW</status>'
                        +     '<accepted>true</accepted>'
                        +   '</FirstTable>'
                        +   '<FirstTable>'
                        +     '<status>ACCEPTED</status>'
                        +     '<accepted>true</accepted>'
                        +   '</FirstTable>'
                        +   '<SecondTable>'
                        +     '<status>ACCEPTED</status>'
                        +     '<accepted>true</accepted>'
                        +   '</SecondTable>'
                        + '</ProDataSet>').
        Assert:AreEqual(fixture:TableCount, 2).
        Assert:AreEqual(fixture:Size, 1).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromXML_Multiple():
        fixture = NEW Fixture().
        fixture:FromXML('<?xml version="1.0"?>'
                        + '<ProDataSet>'
                        +   '<FirstTable>'
                        +     '<status>NEW</status>'
                        +     '<accepted>true</accepted>'
                        +   '</FirstTable>'
                        +   '<FirstTable>'
                        +     '<status>ACCEPTED</status>'
                        +     '<accepted>true</accepted>'
                        +   '</FirstTable>'
                        + '</ProDataSet>').
        Assert:AreEqual(fixture:TableCount, 1).
        Assert:AreEqual(fixture:Size, 1).
        fixture:FromXML('<?xml version="1.0"?>'
                        + '<ProDataSet>'
                        +   '<FirstTable>'
                        +     '<status>NEW</status>'
                        +     '<accepted>true</accepted>'
                        +   '</FirstTable>'
                        +   '<FirstTable>'
                        +     '<status>ACCEPTED</status>'
                        +     '<accepted>true</accepted>'
                        +   '</FirstTable>'
                        +   '<SecondTable>'
                        +     '<status>ACCEPTED</status>'
                        +     '<accepted>true</accepted>'
                        +   '</SecondTable>'
                        + '</ProDataSet>').
        Assert:AreEqual(fixture:TableCount, 3).
        Assert:AreEqual(fixture:Size, 2).
    END METHOD.
    
    METHOD PROTECTED HANDLE CreateDataSetFromTempTable(INPUT httTest AS HANDLE):
        
        DEFINE VARIABLE hdSet     AS HANDLE NO-UNDO.
        DEFINE VARIABLE htt       AS HANDLE NO-UNDO.
        DEFINE VARIABLE hdSource  AS HANDLE NO-UNDO.
        DEFINE VARIABLE httBuffer AS HANDLE NO-UNDO.
        DEFINE VARIABLE httTestBuf AS HANDLE NO-UNDO.
        
        CREATE TEMP-TABLE htt.
        htt:COPY-TEMP-TABLE(httTest, FALSE, TRUE).

        /* Create buffers for temp-tables */        
        CREATE BUFFER httBuffer FOR TABLE htt.
        CREATE BUFFER httTestBuf FOR TABLE httTest.
        
        CREATE DATASET hdSet.
        hdSet:ADD-BUFFER(httBuffer).
        
        CREATE DATA-SOURCE hdSource.
        hdSource:ADD-SOURCE-BUFFER(httTestBuf,?).
        httBuffer:ATTACH-DATA-SOURCE(hdSource).
        hdSet:FILL().
        
        RETURN hdSet.
        
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromDataSet():
        
        fixture = NEW Fixture().
        EMPTY TEMP-TABLE ttTestTable.
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "NEW"
               ttTestTable.Accepted = TRUE. 
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "ACCEPTED"
               ttTestTable.Accepted = TRUE.
               
        fixture:FromDataSet(CreateDataSetFromTempTable(INPUT TEMP-TABLE ttTestTable:HANDLE)).
        Assert:AreEqual(fixture:TableCount, 1).
        Assert:AreEqual(fixture:Size, 1).
        
    END METHOD.

    @Test.
    METHOD PUBLIC VOID FromDataSet_Multiple():
        
        DEFINE BUFFER ttBufTestTable FOR ttTestTable.
        DEFINE BUFFER ttSecondTestTable FOR ttSecondTestTable.
        
        fixture = NEW Fixture().
        EMPTY TEMP-TABLE ttTestTable.
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "NEW"
               ttTestTable.Accepted = TRUE. 
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "ACCEPTED"
               ttTestTable.Accepted = TRUE.
               
        fixture:FromDataSet(CreateDataSetFromTempTable(INPUT TEMP-TABLE ttBufTestTable:HANDLE)).
        Assert:AreEqual(fixture:TableCount, 1).
        Assert:AreEqual(fixture:Size, 1).

        EMPTY TEMP-TABLE ttSecondTestTable.
        CREATE ttSecondTestTable.
        ASSIGN ttSecondTestTable.StatusCode = "NEW"
               ttSecondTestTable.Accepted = TRUE. 
        CREATE ttSecondTestTable.
        ASSIGN ttSecondTestTable.StatusCode = "ACCEPTED"
               ttSecondTestTable.Accepted = TRUE.
               
        fixture:FromDataSet(CreateDataSetFromTempTable(INPUT TEMP-TABLE ttSecondTestTable:HANDLE)).
        Assert:AreEqual(fixture:TableCount, 2).
        Assert:AreEqual(fixture:Size, 2).

    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromTempTable():
        
        DEFINE VARIABLE httTest   AS HANDLE NO-UNDO.
        DEFINE VARIABLE hdSet     AS HANDLE NO-UNDO.
        DEFINE VARIABLE htt       AS HANDLE NO-UNDO.
        DEFINE VARIABLE hdSource  AS HANDLE NO-UNDO.
        DEFINE VARIABLE httBuffer AS HANDLE NO-UNDO.

        fixture = NEW Fixture().
        EMPTY TEMP-TABLE ttTestTable.
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "NEW"
               ttTestTable.Accepted = TRUE. 
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "ACCEPTED"
               ttTestTable.Accepted = TRUE.
               
        fixture:FromTempTable(TEMP-TABLE ttTestTable:HANDLE).
        Assert:AreEqual(fixture:TableCount,1).
        Assert:AreEqual(fixture:Size, 1).
    END.

    @Test.
    METHOD PUBLIC VOID FromTempTable_Multiple():
        fixture = NEW Fixture().
        EMPTY TEMP-TABLE ttTestTable.
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "NEW"
               ttTestTable.Accepted = TRUE. 
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "ACCEPTED"
               ttTestTable.Accepted = TRUE.
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "PICKING"
               ttTestTable.Accepted = FALSE. 
        CREATE ttTestTable.
        ASSIGN ttTestTable.StatusCode = "POSTED"
               ttTestTable.Accepted = FALSE.
        fixture:FromTempTable(TEMP-TABLE ttTestTable:HANDLE).
        Assert:AreEqual(fixture:TableCount, 1).
        Assert:AreEqual(fixture:Size, 1).
        EMPTY TEMP-TABLE ttSecondTestTable.
        CREATE ttSecondTestTable.
        ASSIGN ttSecondTestTable.StatusCode = "NEW"
               ttSecondTestTable.Accepted = TRUE. 
        CREATE ttSecondTestTable.
        ASSIGN ttSecondTestTable.StatusCode = "ACCEPTED"
               ttSecondTestTable.Accepted = TRUE.
        fixture:FromTempTable(TEMP-TABLE ttSecondTestTable:HANDLE).
        Assert:AreEqual(fixture:TableCount, 2).
        Assert:AreEqual(fixture:Size, 2).
    END METHOD.
END CLASS.