/*------------------------------------------------------------------------------
  File        :   ClassType.cls
  Package     :   OEUnit.Util
  Description :   Helper for getting the class type name from a class file. 
                  Hacky but necessary as COMPILER:CLASS-TYPE does not give
                  the expected value following class compilation.
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Util.*.

CLASS OEUnit.Util.ClassType:

  /*----------------------------------------------------------------------------
    Construct a ClassInfo instance representing the given object.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC CHARACTER FromFile(INPUT classFile AS CHARACTER):

    DEFINE VARIABLE xrefFile AS CHARACTER NO-UNDO.
    xrefFile = GUID(GENERATE-UUID) + ".xml".
    
    Errors:Initalize().
    
    /* Suppress warning codes before compile */
    Warnings:Suppress().
    COMPILE VALUE(classFile) XREF-XML VALUE(xrefFile).
    
    IF COMPILER:ERROR AND COMPILER:NUM-MESSAGES > 0 THEN
      RETURN ERROR "Failed to compile '" + classFile
        + "'.~r~n" + Errors:GetMessage(INPUT COMPILER:HANDLE).

    RETURN GetClassType(xrefFile).
    
    FINALLY:
      Warnings:Restore().
      /* No need to delete typeClass, as instances of Progress.Lang.Class are
         singletons that cannot be deleted */
       
      OS-DELETE VALUE(xrefFile).
    END FINALLY.
    
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