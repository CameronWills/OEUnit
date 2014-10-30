
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING OEUnit.Tests.Data.*.
USING OEUnit.Assertion.Assert.
USING OEUnit.Data.DataProvider.

CLASS OEUnit.Tests.Data.DataProviderTest:
    
    DEFINE VARIABLE dataProvider AS DataProvider NO-UNDO.

    @After.
    METHOD PUBLIC VOID DeleteDataProvider():
        DELETE OBJECT dataProvider NO-ERROR.
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromJSON():
        dataProvider = NEW DataProvider().
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        Assert:AreEqual(dataProvider:Size, 2).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID FromJSON_Reset():
        dataProvider = NEW DataProvider().
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        Assert:AreEqual(dataProvider:Size, 2).
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"ame~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        Assert:AreEqual(dataProvider:Size, 4).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID MoveFirst():
        dataProvider = NEW DataProvider().
        
        /* Object not fully initialise, should return false */
        Assert:IsFalse(dataProvider:MoveFirst()).
        
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        Assert:IsTrue(dataProvider:MoveFirst()).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID MoveLast():
        dataProvider = NEW DataProvider().
        
        /* Object not fully initialise, should return false */
        Assert:IsFalse(dataProvider:MoveLast()).
        
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        Assert:IsTrue(dataProvider:MoveLast()).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID MoveNext():
        dataProvider = NEW DataProvider().
        
        /* Object not fully initialise, should return false */
        Assert:IsFalse(dataProvider:MoveNext()).
        
        
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        dataProvider:MoveLast().
        Assert:IsFalse(dataProvider:MoveNext()).
        dataProvider:MoveFirst().
        Assert:IsTrue(dataProvider:MoveNext()).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID MovePrev():
        dataProvider = NEW DataProvider().
        
        /* Object not fully initialise, should return false */
        Assert:IsFalse(dataProvider:MovePrev()).
        
        
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        dataProvider:MoveFirst().
        Assert:IsFalse(dataProvider:MovePrev()).
        dataProvider:MoveLast().
        Assert:IsTrue(dataProvider:MovePrev()).
    END METHOD.
    
    @Test.
    METHOD PUBLIC VOID GetParameterList():
        DEFINE VARIABLE params AS Progress.Lang.ParameterList NO-UNDO.
        dataProvider = NEW DataProvider().
        dataProvider:FromJSON("~{ ~"data~": ["
                            + "~{ ~"name~": ~"Char Value~", ~"intvalue~": 0, ~"bool~": true, ~"nullvalue~": null},"
                            + "~{ ~"name~": ~"Next Value~", ~"intvalue~": 15, ~"bool~": false, ~"nullvalue~": 0},"
                            + "]}").
        params = dataProvider:GetParameterList().
        Assert:IsNotNull(params).
        Assert:AreEqual(params:NumParameters, 4).
        DELETE OBJECT params.
        dataProvider:MoveNext().
        params = dataProvider:GetParameterList().
        Assert:IsNotNull(params).
        Assert:AreEqual(params:NumParameters, 4).
        DELETE OBJECT params.
        dataProvider:MoveNext().
        params = dataProvider:GetParameterList().
        Assert:IsNull(params).
    END METHOD.
    
    @Test(expected="OEUnit.Data.DataProviderError").
    METHOD PUBLIC VOID GetParameterList_NotInitialised():
        dataProvider = NEW DataProvider().
        dataProvider:GetParameterList().
    END METHOD.

END CLASS.