
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Util.IComparator.

CLASS OEUnit.Tests.Util.SimpleComparator IMPLEMENTS OEUnit.Util.IComparator:

  METHOD PUBLIC INTEGER Compare(INPUT obj1 AS Progress.Lang.Object,
                                INPUT obj2 AS Progress.Lang.Object):
    RETURN 1.
    /* Always greater than, this will simply reverse the order of the list */    
  END METHOD. 

END CLASS.