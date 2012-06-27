/*------------------------------------------------------------------------------
  File        : IComparator.cls
  Package     : OEUnit.Util
  Description : Objects implementing the IComparator interface can be passed to  
                the SortElements() method on OEUnit.Util.List, to control the 
                order of the list elements. 
  Author      : Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

INTERFACE OEUnit.Util.IComparator:
  
 /*----------------------------------------------------------------------------
  Compares the two objects passed. Returns 0 if obj1 = obj2. Returns a negative 
  integer if obj1 < obj2. Returns a positive integer if obj1 > obj2. 
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC INTEGER Compare(INPUT obj1 AS Progress.Lang.Object,
                                INPUT obj2 AS Progress.Lang.Object). 
  
END INTERFACE.