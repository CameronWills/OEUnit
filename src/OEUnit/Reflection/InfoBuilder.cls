/*------------------------------------------------------------------------------
  File        :   InfoBuilder.cls
  Package     :   OEUnit.Reflection
  Description :   Constructs an instance of ClassInfo representing a given object 
                  (typically an instance of a test case or test suite). 
  Author      :   Cameron Wills
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.Reflection.*.
USING OEUnit.Util.*.

CLASS OEUnit.Reflection.InfoBuilder:

  /*----------------------------------------------------------------------------
    Skip any annotations found that precede these types of ABL statements.
  ----------------------------------------------------------------------------*/
  DEFINE PRIVATE STATIC PROPERTY StatementTypesSkipped AS CHARACTER NO-UNDO 
    INIT "CONSTRUCTOR,DATA-MEMBER,DATASET,DESTRUCTOR,INTERFACE,PROPERTY,EXTERN,EVENT" GET.
  
  /*----------------------------------------------------------------------------
    Construct a ClassInfo instance representing the given object.
  ----------------------------------------------------------------------------*/
  METHOD PUBLIC STATIC ClassInfo BuildClassInfo(INPUT obj AS Progress.Lang.Object):

    DEFINE VARIABLE xrefFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE inf AS ClassInfo NO-UNDO.
    DEFINE VARIABLE typeClass AS Progress.Lang.Class NO-UNDO.
    DEFINE VARIABLE classFile AS CHARACTER NO-UNDO.

    ASSIGN
      typeClass = obj:GetClass()
      classFile = GetClassFile(typeClass)
      xrefFile = GUID(GENERATE-UUID) + ".xml"
      inf = NEW ClassInfo(typeClass:TypeName, obj).
    
    /* Suppress warning codes before compile */
    Warnings:Suppress().
    
    /* Compile the class and all superclasses */
    DO WHILE SEARCH(classFile) <> ?:
      COMPILE VALUE(classFile) XREF-XML VALUE(xrefFile).
      ParseClassXref(xrefFile, inf).
      typeClass = typeClass:SuperClass.
      classFile = GetClassFile(typeClass).
    END.
    RETURN inf.
    FINALLY:
      Warnings:Restore().
      /* No need to delete typeClass, as instances of Progress.Lang.Class are
         singletons that cannot be deleted */
       
      OS-DELETE VALUE(xrefFile).
    END FINALLY.
    
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the relative path to the .cls file for the given class.
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE STATIC CHARACTER GetClassFile(INPUT typeClass AS Progress.Lang.Class):
    RETURN REPLACE(typeClass:TypeName, ".", "/") + ".cls".
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Parses the xref-xml file looking for methods and annotations. 
  ----------------------------------------------------------------------------*/   
  METHOD PRIVATE STATIC VOID ParseClassXref(INPUT xrefFile AS CHARACTER,
                                            INPUT inf AS ClassInfo):
    
    DEFINE VARIABLE xmlDoc AS HANDLE NO-UNDO.
    DEFINE VARIABLE xmlDocElement AS HANDLE NO-UNDO.
    DEFINE VARIABLE xmlSourceElement AS HANDLE NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    CREATE X-DOCUMENT xmlDoc. 
    CREATE X-NODEREF xmlDocElement.
    CREATE X-NODEREF xmlSourceElement.
    
    xmlDoc:SUPPRESS-NAMESPACE-PROCESSING = TRUE.
    xmlDoc:LOAD("FILE", xrefFile, FALSE).

    xmlDoc:GET-DOCUMENT-ELEMENT(xmlDocElement).
    
    DO i = 1 TO xmlDocElement:NUM-CHILDREN:
      xmlDocElement:GET-CHILD(xmlSourceElement,i).
      IF xmlSourceElement:SUBTYPE = "ELEMENT" AND xmlSourceElement:NAME = "Source" THEN 
        ParseSourceElement(xmlSourceElement, inf).
    END.
    FINALLY:
      DELETE OBJECT xmlDoc NO-ERROR.   		
      DELETE OBJECT xmlDocElement NO-ERROR.
      DELETE OBJECT xmlSourceElement NO-ERROR.
    END FINALLY.
  END METHOD.

  /*----------------------------------------------------------------------------
    Parses a <source> element in the xml-xref, looking for methods and 
    annotations. Methods preceded by annotations are added to the given 
    ClassInfo instance.
  ----------------------------------------------------------------------------*/   
  METHOD PRIVATE STATIC VOID ParseSourceElement(INPUT xmlSourceElement AS HANDLE,
                                                INPUT inf AS ClassInfo):
    
    DEFINE VARIABLE xmlRefElement AS HANDLE NO-UNDO.
    DEFINE VARIABLE nodeType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE methodName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE annotations AS List NO-UNDO.
    
    CREATE X-NODEREF xmlRefElement.
    
    DO i = 1 TO xmlSourceElement:NUM-CHILDREN:
      xmlSourceElement:GET-CHILD(xmlRefElement, i).
      IF xmlRefElement:SUBTYPE = "ELEMENT" AND xmlRefElement:NAME = "Reference" THEN DO:
        nodeType = GetRefTypeAttribute(xmlRefElement).
        
        CASE nodeType:
          
          WHEN "ANNOTATION" THEN DO:
            IF annotations = ? THEN 
              annotations = NEW List(TRUE).
            annotations:Add(NEW AnnotationInfo(GetObjectIdAttribute(xmlRefElement))).
          END.
          
          WHEN "CLASS" THEN DO:
            IF inf:Name = GetObjectIdAttribute(xmlRefElement) THEN
              ASSIGN 
                inf:Annotations = annotations
                annotations = ?.              
            ELSE 
              DELETE OBJECT annotations NO-ERROR.
          END.
          
          WHEN "METHOD" THEN DO:
            IF VALID-OBJECT(annotations) THEN DO: 
              methodName = GetObjectIdAttribute(xmlRefElement).
              IF NOT(VALID-OBJECT(inf:GetMethodNamed(methodName))) THEN DO:
                inf:AddMethod(NEW MethodInfo(methodName, annotations, inf, IsStaticMethod(xmlRefElement))).
                annotations = ?.
              END.
            END.
            DELETE OBJECT annotations NO-ERROR.
          END.
          
          OTHERWISE
            IF LOOKUP(nodeType, StatementTypesSkipped) > 0 AND VALID-OBJECT(annotations) THEN
              DELETE OBJECT annotations NO-ERROR.
        END CASE.
      END.
    END.
    FINALLY:
      DELETE OBJECT xmlRefElement NO-ERROR.
      DELETE OBJECT annotations NO-ERROR.   		
    END FINALLY.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns true if the method described in the given xref-xml element is static
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE STATIC LOGICAL IsStaticMethod(INPUT xmlMethodElement AS HANDLE):
    
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE xmlChildNode AS HANDLE NO-UNDO.
    
    CREATE X-NODEREF xmlChildNode.
    
    DO i = 1 TO xmlMethodElement:NUM-CHILDREN:
      xmlMethodElement:GET-CHILD(xmlChildNode, i).
      IF xmlChildNode:SUBTYPE = "ELEMENT" THEN DO:
        IF xmlChildNode:NAME = "Is-static" THEN 
          RETURN LOGICAL(GetInnerText(xmlChildNode)).
      END.
    END.
    RETURN FALSE.
    FINALLY:
      DELETE OBJECT xmlChildNode NO-ERROR. 		
    END FINALLY.
  END METHOD.
  
  /*----------------------------------------------------------------------------
    Returns the text value of the given Xml Element. 
  ----------------------------------------------------------------------------*/
  METHOD PRIVATE STATIC CHARACTER GetInnerText(INPUT xmlElement AS HANDLE):
    
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE xmlChildNode AS HANDLE NO-UNDO.
    DEFINE VARIABLE innerText AS CHARACTER NO-UNDO INIT "".

    CREATE X-NODEREF xmlChildNode.
    
    DO i = 1 TO xmlElement:NUM-CHILDREN:
      xmlElement:GET-CHILD(xmlChildNode, i) NO-ERROR.
      IF xmlChildNode:SUBTYPE = "TEXT" THEN
        innerText = xmlChildNode:NODE-VALUE.
    END.
        
    RETURN innerText.
    FINALLY:
      DELETE OBJECT xmlChildNode NO-ERROR.
    END FINALLY.
  END METHOD.

  /*----------------------------------------------------------------------------
    Returns the value of the 'Reference-type' attribute in the given XML element. 
  ----------------------------------------------------------------------------*/  
  METHOD PRIVATE STATIC CHARACTER GetRefTypeAttribute(INPUT xmlElement AS HANDLE):
    RETURN xmlElement:GET-ATTRIBUTE("Reference-type").
  END.
  
  /*----------------------------------------------------------------------------
    Returns the value of the 'Object-identifier' attribute in the given XML 
    element. 
  ----------------------------------------------------------------------------*/   
  METHOD PRIVATE STATIC CHARACTER GetObjectIdAttribute(INPUT xmlElement AS HANDLE):
    RETURN xmlElement:GET-ATTRIBUTE("Object-identifier").
  END METHOD.                                             
  
END CLASS.