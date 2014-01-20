&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME detailsWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS detailsWindow 
/*------------------------------------------------------------------------------
  File        :   DetailsWindow.w
  Package     :   OEUnit.UI
  Description :   Display the details of a single test result and it's errors.
                 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */


/* ttError Temp-Table   */
{OEUnit/UI/ttError.i &reference = "REFERENCE-ONLY"}  

/* Parameters Definitions ---                                           */
/* Details of the test result */
DEFINE INPUT PARAMETER TABLE FOR ttError BIND.
DEFINE INPUT PARAMETER ipTestId AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipClassName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipMethodName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipResultStatus AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipResultStatusColour AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipDuration AS INT64 NO-UNDO.
DEFINE INPUT PARAMETER ipErrorCount AS INTEGER NO-UNDO.

/* Dimensions and position of the window */ 
DEFINE INPUT-OUTPUT PARAMETER oiHeight AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER oiWidth AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER oiRow AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER oiCol AS DECIMAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS resultMessage callstack btnClose btnNext ~
btnPrevious 
&Scoped-Define DISPLAYED-OBJECTS resultMessage callstack className ~
methodName resultStatus duration errorCount lblOf currentError lblErrors 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR detailsWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 18 BY 1.19.

DEFINE BUTTON btnNext 
     LABEL ">" 
     SIZE 6 BY 1.19
     FONT 6.

DEFINE BUTTON btnPrevious 
     LABEL "<" 
     SIZE 6 BY 1.19
     FONT 6.

DEFINE VARIABLE callstack AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL NO-BOX
     SIZE 95.4 BY 6.67 NO-UNDO.

DEFINE VARIABLE resultMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL NO-BOX
     SIZE 95.4 BY 2.86
     FONT 6 NO-UNDO.

DEFINE VARIABLE className AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 95.4 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE currentError AS INTEGER FORMAT ">9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE duration AS INT64 FORMAT ">,>>>,>>9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 23 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE errorCount AS INTEGER FORMAT ">9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE lblErrors AS CHARACTER FORMAT "X(7)":U INITIAL "Errors:" 
      VIEW-AS TEXT 
     SIZE 9 BY 1.19
     FGCOLOR 20 FONT 6 NO-UNDO.

DEFINE VARIABLE lblOf AS CHARACTER FORMAT "X(2)":U INITIAL "of" 
      VIEW-AS TEXT 
     SIZE 2.8 BY 1.19
     FGCOLOR 20 FONT 6 NO-UNDO.

DEFINE VARIABLE methodName AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 95.4 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE resultStatus AS CHARACTER FORMAT "X(10)":U 
      VIEW-AS TEXT 
     SIZE 29.4 BY 1.19
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     resultMessage AT ROW 7 COL 17.6 NO-LABEL WIDGET-ID 2 NO-TAB-STOP 
     callstack AT ROW 10.52 COL 17.6 NO-LABEL WIDGET-ID 22 NO-TAB-STOP 
     btnClose AT ROW 17.71 COL 95 WIDGET-ID 4
     btnNext AT ROW 17.71 COL 36.4 WIDGET-ID 30
     btnPrevious AT ROW 17.71 COL 17.4 WIDGET-ID 28
     className AT ROW 1.29 COL 15.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8 NO-TAB-STOP 
     methodName AT ROW 2.62 COL 15.6 COLON-ALIGNED NO-LABEL WIDGET-ID 24 NO-TAB-STOP 
     resultStatus AT ROW 3.95 COL 15.6 COLON-ALIGNED NO-LABEL WIDGET-ID 12 NO-TAB-STOP 
     duration AT ROW 5.29 COL 15.6 COLON-ALIGNED NO-LABEL WIDGET-ID 16 NO-TAB-STOP 
     errorCount AT ROW 17.71 COL 32 NO-LABEL WIDGET-ID 40 NO-TAB-STOP 
     lblOf AT ROW 17.71 COL 28.6 NO-LABEL WIDGET-ID 42 NO-TAB-STOP 
     currentError AT ROW 17.71 COL 27.6 RIGHT-ALIGNED NO-LABEL WIDGET-ID 38 NO-TAB-STOP 
     lblErrors AT ROW 17.71 COL 7.6 NO-LABEL WIDGET-ID 46 NO-TAB-STOP 
     "Class:" VIEW-AS TEXT
          SIZE 8 BY 1.19 AT ROW 1.29 COL 8.2 WIDGET-ID 6
          FGCOLOR 20 FONT 6
     "Duration:" VIEW-AS TEXT
          SIZE 12 BY 1.19 AT ROW 5.29 COL 4.6 WIDGET-ID 14
          FGCOLOR 20 FONT 6
     "Method:" VIEW-AS TEXT
          SIZE 10 BY 1.19 AT ROW 2.62 COL 5.8 WIDGET-ID 26
          FGCOLOR 20 FONT 6
     "Result:" VIEW-AS TEXT
          SIZE 9 BY 1.19 AT ROW 3.95 COL 7 WIDGET-ID 10
          FGCOLOR 20 FONT 6
     "Message:" VIEW-AS TEXT
          SIZE 12 BY 1.19 AT ROW 6.62 COL 4.2 WIDGET-ID 18
          FGCOLOR 20 FONT 6
     "Call Stack:" VIEW-AS TEXT
          SIZE 13 BY 1.19 AT ROW 10.14 COL 2.6 WIDGET-ID 20
          FGCOLOR 20 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 1 ROW 1
         SIZE 320 BY 100 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW detailsWindow ASSIGN
         HIDDEN             = YES
         TITLE              = "Details"
         HEIGHT             = 18.33
         WIDTH              = 114
         MAX-HEIGHT         = 100
         MAX-WIDTH          = 336
         VIRTUAL-HEIGHT     = 100
         VIRTUAL-WIDTH      = 336
         MAX-BUTTON         = NO
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW detailsWindow
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME R-To-L                                                    */
ASSIGN 
       callstack:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN className IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN currentError IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN duration IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN errorCount IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblErrors IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblOf IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN methodName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       resultMessage:AUTO-RESIZE IN FRAME DEFAULT-FRAME      = TRUE
       resultMessage:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN resultStatus IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(detailsWindow)
THEN detailsWindow:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME detailsWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL detailsWindow detailsWindow
ON END-ERROR OF detailsWindow /* Details */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL detailsWindow detailsWindow
ON WINDOW-CLOSE OF detailsWindow /* Details */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL detailsWindow detailsWindow
ON WINDOW-RESIZED OF detailsWindow /* Details */
DO:
  RUN resizeWindow(INPUT oiHeight, INPUT oiWidth,
                   INPUT detailsWindow:HEIGHT, INPUT detailsWindow:WIDTH).
  ASSIGN
    oiHeight = detailsWindow:HEIGHT
    oiWidth = detailsWindow:WIDTH.                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose detailsWindow
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "WINDOW-CLOSE":U TO DetailsWindow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext detailsWindow
ON CHOOSE OF btnNext IN FRAME DEFAULT-FRAME /* > */
DO:
  currentError = currentError + 1.
  FIND NEXT ttError WHERE ttError.TestId = ipTestId NO-ERROR.
  RUN setError.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious detailsWindow
ON CHOOSE OF btnPrevious IN FRAME DEFAULT-FRAME /* < */
DO:
  currentError = currentError - 1.
  FIND PREV ttError WHERE ttError.TestId = ipTestId NO-ERROR.
  RUN setError.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK detailsWindow 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  detailsWindow:LOAD-SMALL-ICON ("OEUnit/UI/Icons/OEUnit.ico").
  RUN showDetails.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteWindow detailsWindow 
PROCEDURE deleteWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  APPLY "WINDOW-CLOSE" TO DetailsWindow.
  APPLY "CLOSE" TO THIS-PROCEDURE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI detailsWindow  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(detailsWindow)
  THEN DELETE WIDGET detailsWindow.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI detailsWindow  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY resultMessage callstack className methodName resultStatus duration 
          errorCount lblOf currentError lblErrors 
      WITH FRAME DEFAULT-FRAME IN WINDOW detailsWindow.
  ENABLE resultMessage callstack btnClose btnNext btnPrevious 
      WITH FRAME DEFAULT-FRAME IN WINDOW detailsWindow.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW detailsWindow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWindow detailsWindow 
PROCEDURE resizeWindow :
/*------------------------------------------------------------------------------
  Resize the window. Moving and resizing widgets where needed.     
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER oldHeight AS DECIMAL NO-UNDO. 
  DEFINE INPUT PARAMETER oldWidth AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER newHeight AS DECIMAL NO-UNDO. 
  DEFINE INPUT PARAMETER newWidth AS DECIMAL NO-UNDO.  
  
  DEFINE VARIABLE heightDiff AS DECIMAL NO-UNDO.
  DEFINE VARIABLE widthDiff AS DECIMAL NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:    
    ASSIGN
      heightDiff = newHeight - oldHeight
      widthDiff = newWidth - oldWidth
      btnClose:COLUMN = btnClose:COLUMN + widthDiff
      btnClose:ROW = btnClose:ROW + heightDiff
      btnNext:ROW = btnNext:ROW + heightDiff
      btnPrevious:ROW = btnPrevious:ROW + heightDiff
      errorCount:ROW = errorCount:ROW + heightDiff
      lblErrors:ROW = lblErrors:ROW + heightDiff
      lblOf:ROW = lblOf:ROW + heightDiff
      currentError:ROW = currentError:ROW + heightDiff
      resultMessage:WIDTH = resultMessage:WIDTH + widthDiff
      callStack:WIDTH = callStack:WIDTH + widthDiff
      callStack:HEIGHT = callStack:HEIGHT + heightDiff.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setError detailsWindow 
PROCEDURE setError PRIVATE :
/*------------------------------------------------------------------------------
  Set the current error being displayed.     
------------------------------------------------------------------------------*/
  IF AVAILABLE(ttError) THEN
    ASSIGN
      resultMessage = ttError.ErrorMessage
      callstack = ttError.CallStack.
  ELSE
    ASSIGN
      resultMessage = ""
      callstack = "".
    
  ASSIGN
    resultMessage:SCREEN-VALUE IN FRAME DEFAULT-FRAME = resultMessage
    callstack:SCREEN-VALUE IN FRAME DEFAULT-FRAME = callstack
    currentError:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(currentError)
    btnNext:SENSITIVE IN FRAME DEFAULT-FRAME = NOT(currentError >= errorCount)
    btnPrevious:SENSITIVE IN FRAME DEFAULT-FRAME = NOT(currentError <= 1).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showDetails detailsWindow 
PROCEDURE showDetails :
/*------------------------------------------------------------------------------
  Show the details of the test result in the details window.       
------------------------------------------------------------------------------*/
  RUN resizeWindow(INPUT detailsWindow:HEIGHT, INPUT detailsWindow:WIDTH,
                   INPUT oiHeight, INPUT oiWidth).
  ASSIGN
    DetailsWindow:HEIGHT = oiHeight
    DetailsWindow:WIDTH = oiWidth
    DetailsWindow:ROW = oiRow
    DetailsWindow:COL = oiCol
    DetailsWindow:TITLE = ipClassName 
      + (IF ipMethodName <> "" THEN ":" + ipMethodName ELSE "")
    className = ipClassName
    methodName = ipMethodName
    resultStatus = ipResultStatus
    resultStatus:FGCOLOR IN FRAME DEFAULT-FRAME  = ipResultStatusColour
    duration = ipDuration
    errorCount = ipErrorCount
    currentError = (IF ipErrorCount > 0 THEN 1 ELSE 0).
  RUN enable_UI.
  
  FIND FIRST ttError WHERE ttError.TestId = ipTestId NO-ERROR.
  RUN setError.
  APPLY "ENTRY" TO btnClose IN FRAME DEFAULT-FRAME.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

