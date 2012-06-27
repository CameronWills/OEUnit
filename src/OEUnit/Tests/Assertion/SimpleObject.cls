ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Tests.Assertion.SimpleObject.

CLASS OEUnit.Tests.Assertion.SimpleObject:
  
  DEFINE PUBLIC PROPERTY SimpleProperty AS INTEGER NO-UNDO
    GET.
    SET.
  
  CONSTRUCTOR SimpleObject(INPUT val AS INTEGER):
    SimpleProperty = val.
  END CONSTRUCTOR.
  
  METHOD PUBLIC OVERRIDE LOGICAL Equals(INPUT obj AS Progress.Lang.Object):
    IF obj:GetClass():TypeName <> THIS-OBJECT:GetClass():TypeName THEN 
      RETURN FALSE.
    IF SimpleProperty <> CAST(obj, SimpleObject):SimpleProperty THEN
      RETURN FALSE.
    RETURN TRUE.
  END METHOD.
  
END CLASS. 