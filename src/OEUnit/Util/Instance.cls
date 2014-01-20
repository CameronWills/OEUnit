/*------------------------------------------------------------------------------
  File        :   Instance.cls
  Package     :   OEUnit.Util
  Description :   Helper for getting a new instance of a class from a class file
                  or type name. Hacky but necessary as COMPILER:CLASS-TYPE does 
                  not always give the expected value following class compilation
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Util.*.
USING Progress.Lang.Object.

CLASS OEUnit.Util.Instance:

  /*----------------------------------------------------------------------------
    Construct a ClassInfo instance representing the given object.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC Object FromFile(INPUT classFile AS CHARACTER):
    
    IF NOT(classFile MATCHES "*.cls") THEN
      RETURN ERROR "Cannot create instance of '" + classFile + "'. Must be a class file (.cls).".
    
    DO ON ERROR UNDO, THROW:
      DEFINE VARIABLE xrefFile AS CHARACTER NO-UNDO.
      xrefFile = GUID(GENERATE-UUID) + ".xml".
     
      Errors:Initalize().
    
      /* Suppress warning codes before compile */
      Warnings:Suppress().
      COMPILE VALUE(classFile) XREF-XML VALUE(xrefFile).
    
      IF COMPILER:ERROR AND COMPILER:NUM-MESSAGES > 0 THEN
        RETURN ERROR "Failed to compile '" + classFile
          + "'.~r~n" + Errors:GetMessage(INPUT COMPILER:HANDLE).

      DEFINE VARIABLE classType AS CHARACTER NO-UNDO.
      classType = COMPILER:CLASS-TYPE.
      IF classType = ? OR TRIM(classType) = "" THEN
        classType = GetClassType(xrefFile).
    
      RETURN FromType(classType).
    
      FINALLY:
        Warnings:Restore().
        /* No need to delete typeClass, as instances of Progress.Lang.Class are
           singletons that cannot be deleted */
       
        OS-DELETE VALUE(xrefFile).
      END FINALLY.
    END.
    
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Parses the xref-xml file looking for the class type name. 
  ----------------------------------------------------------------------------*/   
  METHOD PUBLIC STATIC Object FromType(INPUT classType AS CHARACTER):
    DEFINE VARIABLE instance AS Object NO-UNDO.
    
    instance = DYNAMIC-NEW (classType) () NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      RETURN ERROR "Failed to create instance of '" + classType 
        + "'.~r~n" + Errors:GetMessage(INPUT ERROR-STATUS:HANDLE).
   
    RETURN instance.
  END METHOD.  

  /*----------------------------------------------------------------------------
    Parses the xref-xml file looking for the class type name. 
  ----------------------------------------------------------------------------*/   
  METHOD PRIVATE STATIC CHARACTER GetClassType(INPUT xrefFile AS CHARACTER):
    
    /* Nasty XML parsing code follows :( */
    
    DEFINE VARIABLE xmlDoc AS HANDLE NO-UNDO.
    DEFINE VARIABLE xmlDocElement AS HANDLE NO-UNDO.
    DEFINE VARIABLE xmlSourceElement AS HANDLE NO-UNDO.
    DEFINE VARIABLE xmlRefElement AS HANDLE NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    
    CREATE X-DOCUMENT xmlDoc. 
    CREATE X-NODEREF xmlDocElement.
    CREATE X-NODEREF xmlSourceElement.
    CREATE X-NODEREF xmlRefElement.
    
    xmlDoc:SUPPRESS-NAMESPACE-PROCESSING = TRUE.
    xmlDoc:LOAD("FILE", xrefFile, FALSE).

    xmlDoc:GET-DOCUMENT-ELEMENT(xmlDocElement).
    
    DO i = 1 TO xmlDocElement:NUM-CHILDREN:
      xmlDocElement:GET-CHILD(xmlSourceElement,i).
      IF xmlSourceElement:SUBTYPE = "ELEMENT" 
      AND xmlSourceElement:NAME = "Source" THEN 
        
        DO j = 1 TO xmlSourceElement:NUM-CHILDREN:
          xmlSourceElement:GET-CHILD(xmlRefElement, j).
          IF xmlRefElement:SUBTYPE = "ELEMENT" 
          AND xmlRefElement:NAME = "Reference" 
          AND xmlRefElement:GET-ATTRIBUTE("Reference-type") = "CLASS" THEN
            RETURN xmlRefElement:GET-ATTRIBUTE("Object-identifier").
        END.
    END. 
    
    RETURN ?.
    
    FINALLY:
      DELETE OBJECT xmlDoc NO-ERROR.   		
      DELETE OBJECT xmlDocElement NO-ERROR.
      DELETE OBJECT xmlSourceElement NO-ERROR.
      DELETE OBJECT xmlRefElement NO-ERROR.
    END FINALLY.
  END METHOD.

END CLASS.