ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Assertion.*.

CLASS OEUnit.Tests.Assertion.AssertStringTest:

  @Test.
  METHOD PUBLIC VOID IsNullOrEmpty():
    DEFINE VARIABLE String1 AS CHARACTER NO-UNDO.
    String1 = "".
    AssertString:IsNullOrEmpty(String1).
    String1 = ?.
    AssertString:IsNullOrEmpty(String1).    
    DO ON ERROR UNDO, THROW:
      String1 = "a".
      AssertString:IsNullOrEmpty(String1).
      CATCH e AS AssertionFailedError :
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
    END.       
    Assert:Fail().
  END METHOD.

  @Test.
  METHOD PUBLIC VOID IsNotNullOrEmpty():
    DEFINE VARIABLE String1 AS LONGCHAR NO-UNDO.
    String1 = "a".
    AssertString:IsNotNullOrEmpty(String1).
    DO ON ERROR UNDO, THROW:
      String1 = ?.
      AssertString:IsNotNullOrEmpty(String1).
      CATCH e AS AssertionFailedError :
        DO ON ERROR UNDO, THROW:
          DELETE OBJECT e NO-ERROR.
          String1 = "".
          AssertString:IsNotNullOrEmpty(String1).
          CATCH ex AS AssertionFailedError :
            DELETE OBJECT ex NO-ERROR.
            RETURN.
          END CATCH.
        END.
        Assert:Fail().
      END CATCH.
    END.       
    Assert:Fail().
  END METHOD.  

  @Test.
  METHOD PUBLIC VOID Contains():
    DEFINE VARIABLE String1 AS LONGCHAR NO-UNDO.
    String1 = "abc".
    AssertString:Contains(String1, "bc").
    DO ON ERROR UNDO, THROW:
      String1 = "xyz".
      AssertString:Contains(String1, "bc").
      CATCH e AS AssertionFailedError :
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
    END.       
    Assert:Fail().
  END METHOD.  

  @Test.
  METHOD PUBLIC VOID DoesNotContain():
    DEFINE VARIABLE String1 AS CHARACTER NO-UNDO.
    String1 = "abc".
    AssertString:DoesNotContain(String1, "xy").
    DO ON ERROR UNDO, THROW:
      String1 = "xyz".
      AssertString:DoesNotContain(String1, "xy").
      CATCH e AS AssertionFailedError :
        DELETE OBJECT e NO-ERROR.
        RETURN.
      END CATCH.
    END.       
    Assert:Fail().
  END METHOD. 

END CLASS.