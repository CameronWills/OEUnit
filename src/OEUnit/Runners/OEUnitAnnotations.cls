/*------------------------------------------------------------------------------
  File        : OEUnitAnnotations.cls
  Package     : OEUnit.Runners
  Description : Houses the syntax for ABL Annotations specific to OEUnit.
------------------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS OEUnit.Runners.OEUnitAnnotations:
    
  /*----------------------------------------------------------------------------
    OEUnit Annotation Types - Read Only Properties
  ----------------------------------------------------------------------------*/
  DEFINE PUBLIC STATIC PROPERTY Test        AS CHARACTER NO-UNDO INIT "Test" GET.
  DEFINE PUBLIC STATIC PROPERTY Ignore      AS CHARACTER NO-UNDO INIT "Ignore" GET.
  DEFINE PUBLIC STATIC PROPERTY Before      AS CHARACTER NO-UNDO INIT "Before" GET.
  DEFINE PUBLIC STATIC PROPERTY After       AS CHARACTER NO-UNDO INIT "After" GET.
  DEFINE PUBLIC STATIC PROPERTY BeforeClass AS CHARACTER NO-UNDO INIT "BeforeClass" GET.
  DEFINE PUBLIC STATIC PROPERTY AfterClass  AS CHARACTER NO-UNDO INIT "AfterClass" GET.
  
  /*----------------------------------------------------------------------------
    Expected Attribute for the Test Annotation.
  ----------------------------------------------------------------------------*/  
  DEFINE PUBLIC STATIC PROPERTY TestExpectedAttribute AS CHARACTER NO-UNDO INIT "Expected" GET.
  
END CLASS.