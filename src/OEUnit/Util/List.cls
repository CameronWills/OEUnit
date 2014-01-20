/*------------------------------------------------------------------------------
  File        :   List.cls
  Package     :   OEUnit.Util
  Description :   Stores an indexed list of object references. List elements can 
                  be accessed by their index. New elements can be inserted or
                  appended to the list.   
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.AppError.
USING OEUnit.Util.IComparator.

CLASS OEUnit.Util.List:

  DEFINE PRIVATE VARIABLE reset AS LOGICAL NO-UNDO.

  /*----------------------------------------------------------------------------
    Temp table to store elements (integer-object pairs). 
  ----------------------------------------------------------------------------*/
  DEFINE PROTECTED TEMP-TABLE element NO-UNDO
    FIELD ind AS INTEGER
    FIELD obj AS Progress.Lang.Object
      INDEX sequence AS PRIMARY ind ASCENDING.

  /*----------------------------------------------------------------------------
    The number of elements in the list  
  ----------------------------------------------------------------------------*/  
  DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO INIT 0
    GET.
    PRIVATE SET.

  /*----------------------------------------------------------------------------
    If true, list objects will be deleted when this list object is deleted.
  ----------------------------------------------------------------------------*/    
  DEFINE PUBLIC PROPERTY DeleteElementsOnDestruct AS LOGICAL  NO-UNDO
    GET.
    SET.

  /*----------------------------------------------------------------------------
    Return the object in the list that is currently being pointed to.
  ----------------------------------------------------------------------------*/    
  DEFINE PUBLIC PROPERTY Current AS Progress.Lang.Object NO-UNDO
    GET():
      IF AVAILABLE(element) THEN
        RETURN element.obj.
      RETURN ?.
    END GET.
    /*SET. */

  /*----------------------------------------------------------------------------
    Default Constructor.
  ----------------------------------------------------------------------------*/  
  CONSTRUCTOR List():
    DeleteElementsOnDestruct = FALSE.
  END CONSTRUCTOR.

  /*----------------------------------------------------------------------------
    Constructor. If destructElements is true, list objects will be deleted when 
    the list is deleted.
  ----------------------------------------------------------------------------*/
  CONSTRUCTOR List(INPUT destructElements AS LOGICAL):
    DeleteElementsOnDestruct = destructElements.
  END CONSTRUCTOR.
  
  DESTRUCTOR List():
    FOR EACH element:
      IF DeleteElementsOnDestruct THEN 
        DELETE OBJECT element.obj NO-ERROR.
      DELETE element.
    END.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Add the given object to the end of the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Add(INPUT obj AS Progress.Lang.Object):
    Size = Size + 1.
    CREATE element.
    ASSIGN element.ind = Size
           element.obj = obj.
  END METHOD.

  /*----------------------------------------------------------------------------
    Inserts the given object into the list at the given index. If the specified 
    index is less than 1, the object is inserted at the begining of the list. 
    If the specified index is greater than the size of the list, the object is 
    appended to the end of the list. 
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Insert(INPUT obj AS Progress.Lang.Object,
                            INPUT indx AS INTEGER):
    IF indx > Size THEN 
      THIS-OBJECT:Add(obj).
    ELSE IF indx < 1 THEN
      THIS-OBJECT:Insert(obj, 1).
    ELSE DO:
      ReIndex(indx, Size , 1).
      Size = Size + 1.
      CREATE element.
      ASSIGN element.ind = indx
             element.obj = obj.
    END.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the object at the given index in the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC Progress.Lang.Object Get(INPUT indx AS INTEGER):
    FIND element WHERE element.ind = indx NO-ERROR.
    IF AVAILABLE(element) THEN RETURN element.obj.
    RETURN ERROR NEW AppError("Index out of bounds: " + STRING(indx),0).
  END METHOD.

  /*----------------------------------------------------------------------------
    Removes the object at the given index from the list. Returning the removed 
    object.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC Progress.Lang.Object Remove(INPUT indx AS INTEGER):
    DEFINE VARIABLE returnObj AS Progress.Lang.Object NO-UNDO.
    IF Size > 0 THEN DO:
      FIND element WHERE element.ind = indx NO-ERROR.
      IF AVAILABLE(element) THEN DO:
        returnObj = element.obj.
        RemoveCurrent(FALSE).
        RETURN returnObj.
      END.
    END.
    RETURN ERROR NEW AppError("Index out of bounds: " + STRING(indx),0).
  END METHOD.

  /*----------------------------------------------------------------------------
    Removes the given object from the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Remove(INPUT obj AS Progress.Lang.Object):
    IF Size > 0 THEN DO:
      FIND element WHERE element.obj = obj NO-ERROR.
      IF AVAILABLE(element) THEN 
        RemoveCurrent(FALSE).
      ELSE
        RETURN ERROR NEW AppError("Object not found in list",0).  
    END.
  END METHOD.

  /*----------------------------------------------------------------------------
    Removes the object at the given index from the list and deletes the object.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID RemoveAndDelete(INPUT indx AS INTEGER):
    IF Size > 0 THEN DO:
      FIND element WHERE element.ind = indx NO-ERROR.
      IF AVAILABLE(element) THEN
        RemoveCurrent(TRUE). 
      ELSE
        RETURN ERROR NEW AppError("Element index out of bounds: " + STRING(indx),0).        
    END.
  END METHOD.


  /*----------------------------------------------------------------------------
    Removes the given object from the list and deletes the object.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID RemoveAndDelete(INPUT obj AS Progress.Lang.Object):
    IF Size > 0 THEN DO:
      FIND element WHERE element.obj = obj NO-ERROR.
      IF AVAILABLE(element) THEN
        RemoveCurrent(TRUE).
      ELSE 
        RETURN ERROR NEW AppError("Object not found in list",0).      
    END.
  END METHOD.

  /*----------------------------------------------------------------------------
    Removes the current element from the list. If deleteObject is true, the 
    removed object is also deleted.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID RemoveCurrent(INPUT deleteObject AS LOGICAL):
    DEFINE VARIABLE indx LIKE element.ind NO-UNDO.
    IF AVAILABLE(element) THEN DO:
        IF deleteObject THEN DELETE OBJECT element.obj NO-ERROR.
        indx = element.ind.
        DELETE element.
        ReIndex(indx, Size, -1).
        Size = Size - 1.
    END.
  END METHOD.

  /*----------------------------------------------------------------------------
    Reset the current pointer position. Calling MoveNext() after calling this 
    method will position the pointer at the first element in the list. Whereas 
    calling MovePrevious() will position the pointer at the last element in the 
    list.  
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Reset():
    reset = TRUE.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Positions the pointer at the first element in the list. Returns true if exists.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL MoveFirst():
    FIND FIRST element NO-ERROR.
    RETURN AVAILABLE(element).
  END METHOD.

  /*----------------------------------------------------------------------------
    Positions the pointer at the last element in the list. Returns true if exists.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL MoveLast():
    FIND LAST element NO-ERROR.
    RETURN AVAILABLE(element).
  END METHOD.

  /*----------------------------------------------------------------------------
    Position the pointer at the next element in the list. Returns true if exists.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL MoveNext():
    IF reset THEN DO:
      reset = FALSE.
      RETURN MoveFirst().
    END.
    FIND NEXT element NO-ERROR.
    RETURN AVAILABLE(element).
  END METHOD.

  /*----------------------------------------------------------------------------
    Position the pointer at the previous element in the list. Returns true if 
    exists.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL MovePrevious():
    IF reset THEN DO:
      reset = FALSE.
      RETURN MoveLast().
    END.    
    FIND PREV element NO-ERROR.
    RETURN AVAILABLE(element).
  END METHOD.


  /*----------------------------------------------------------------------------
    Sort list elements. The resulting order is determined by comparing the 
    elements against each other using the given comparator.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Sort(INPUT comp AS IComparator):
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE sortObj AS Progress.Lang.Object NO-UNDO.

    /* Insertion sort */
    DO i = 2 TO Size:
      sortObj = Get(i).
      j = i - 1.
      DO WHILE j >= 1 AND comp:Compare(Get(j), sortObj) > 0:
        Move(j + 1, j).
        j = j - 1.
      END.
    END. 
  END METHOD.

  /*----------------------------------------------------------------------------
    Move an object in the list. The moved object is inserted at the new index,
    surrounding elements shift to fill the empty position.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Move(INPUT fromIndex AS INTEGER, INPUT toIndex AS INTEGER):
    
    IF fromIndex > SIZE OR fromIndex < 1 THEN
      RETURN ERROR NEW AppError("Element index out of bounds: " + STRING(fromIndex),0). 
    ELSE IF toIndex > SIZE OR toIndex < 1 THEN
      RETURN ERROR NEW AppError("Element index out of bounds: " + STRING(toIndex),0).
      
    IF fromIndex = toIndex THEN LEAVE.

    FIND element WHERE element.ind = fromIndex.
    element.ind = 0.
      
    IF toIndex > fromIndex THEN
      ReIndex(fromIndex, toIndex, -1).
    ELSE 
      ReIndex(toIndex, fromIndex , 1).
    
    FIND element WHERE element.ind = 0.
    element.ind = toIndex.

  END METHOD.


  /*----------------------------------------------------------------------------
    Re-index the list elements inside the given range. Adding the given increment 
    to each element inside the range.
  ----------------------------------------------------------------------------*/
  METHOD PRIVATE VOID ReIndex(INPUT startIndex AS INTEGER, 
                              INPUT endIndex AS INTEGER, 
                              INPUT increment AS INTEGER):
    
    IF increment > 0 THEN FOR EACH element WHERE element.ind >= startIndex
                                             AND element.ind <= endIndex
                                              BY element.ind DESCENDING:
      element.ind = element.ind + increment.                           
    END.
    ELSE FOR EACH element WHERE element.ind >= startIndex
                            AND element.ind <= endIndex:
        element.ind = element.ind + increment.
    END.
  END METHOD.
  
END CLASS.