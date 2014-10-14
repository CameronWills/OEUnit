 /*------------------------------------------------------------------------------
  File        :   NamedList.cls
  Package     :   OEUnit.Util
  Description :   Stores a named list of strings. List elements can be accessed 
                  by their name only.   
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.

CLASS OEUnit.Util.NamedList: 

  DEFINE PRIVATE VARIABLE reset AS LOGICAL NO-UNDO.

  /*----------------------------------------------------------------------------
    Temp table to store elements (integer-object pairs). 
  ----------------------------------------------------------------------------*/
  DEFINE PROTECTED TEMP-TABLE element NO-UNDO
    FIELD name AS CHARACTER
    FIELD val AS CHARACTER
      INDEX sequence IS PRIMARY UNIQUE name ASCENDING.

  /*----------------------------------------------------------------------------
    The number of elements in the list  
  ----------------------------------------------------------------------------*/  
  DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO INIT 0
    GET.
    PRIVATE SET.

  /*----------------------------------------------------------------------------
    Return the name of item in the list that is currently being pointed to.
  ----------------------------------------------------------------------------*/    
  DEFINE PUBLIC PROPERTY CurrentName AS CHARACTER NO-UNDO
    GET():
      IF AVAILABLE(element) THEN
        RETURN element.name.
      RETURN ?.
    END GET.
    /*SET. */

  /*----------------------------------------------------------------------------
    Return the value of item in the list that is currently being pointed to.
  ----------------------------------------------------------------------------*/    
  DEFINE PUBLIC PROPERTY CurrentValue AS CHARACTER NO-UNDO
    GET():
      IF AVAILABLE(element) THEN
        RETURN element.val.
      RETURN ?.
    END GET.
    /*SET. */

  /*----------------------------------------------------------------------------
    Default Constructor.
  ----------------------------------------------------------------------------*/  
  CONSTRUCTOR NamedList():
  END CONSTRUCTOR.

  DESTRUCTOR NamedList():
    FOR EACH element:
      DELETE element.
    END.
  END DESTRUCTOR.

  /*----------------------------------------------------------------------------
    Add the name and value pair to the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Add(INPUT nam AS CHARACTER, INPUT val AS CHARACTER):
    FIND element WHERE element.name = nam NO-ERROR.
    IF NOT AVAILABLE(element) THEN
    DO: 
      Size = Size + 1.
      CREATE element.
      ASSIGN element.name = nam
             element.val  = val.
    END.
    ELSE ASSIGN element.val  = val.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the value matching the given name from the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC CHARACTER Get(INPUT name AS CHARACTER):
    FIND element WHERE element.name = name NO-ERROR.
    IF AVAILABLE(element) THEN RETURN element.val.
    RETURN ERROR NEW AppError("Value not found with name: " + name,0).
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Removes the given object from the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID Remove(INPUT name AS CHARACTER):
    FIND element WHERE element.name = name NO-ERROR.
    IF AVAILABLE(element) THEN
      RemoveCurrent().
    ELSE
      RETURN ERROR NEW AppError("Value not found in list with name: " + name,0).  
  END METHOD.

  /*----------------------------------------------------------------------------
    Removes the current element from the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC VOID RemoveCurrent():
    IF AVAILABLE(element) THEN DO:
        DELETE element.
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
    Return true is a list element with the given name is in the list.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC LOGICAL HasValue(INPUT name AS CHARACTER):
    RETURN CAN-FIND(FIRST element WHERE element.name = name).
  END METHOD.

END CLASS.