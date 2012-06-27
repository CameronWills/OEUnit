&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME ResultsWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ResultsWindow 
/*------------------------------------------------------------------------------
  File        :   ResultsWindow.w
  Package     :   OEUnit.UI
  Description :   Displays the results of a test run in a window.
  Author      :   Cameron Wills    
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OEUnit.UI.ResultsWindowView.
USING OEUnit.Runner.TestResult.

{adecomm/oeideservice.i}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */


CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE viewId AS CHARACTER INIT
    "com.openedge.pdt.text.views.OERuntimeView" NO-UNDO.
DEFINE VARIABLE viewTitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE viewHwnd AS INTEGER NO-UNDO.
DEFINE VARIABLE runningInArchitect AS LOGICAL NO-UNDO INIT TRUE.
DEFINE VARIABLE totalDisplayed AS INTEGER NO-UNDO.
DEFINE VARIABLE detailsWindowHeight AS DECIMAL NO-UNDO INIT 24.
DEFINE VARIABLE detailsWindowWidth AS DECIMAL NO-UNDO INIT 140.
DEFINE VARIABLE detailsWindowRow AS DECIMAL NO-UNDO INIT 5.
DEFINE VARIABLE detailsWindowCol AS DECIMAL NO-UNDO INIT 20.

/* Test Result and Error temp tables */
{OEUnit/UI/ttTestResult.i &reference = "REFERENCE-ONLY"}
{OEUnit/UI/ttError.i &reference = "REFERENCE-ONLY"} 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwResults

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTestResult

/* Definitions for BROWSE brwResults                                    */
&Scoped-define FIELDS-IN-QUERY-brwResults ttTestResult.DisplayName ttTestResult.ResultStatusString ttTestResult.Duration ttTestResult.ResultMessage   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwResults   
&Scoped-define SELF-NAME brwResults
&Scoped-define QUERY-STRING-brwResults FOR EACH ttTestResult WHERE ttTestResult.DisplayInBrowser
&Scoped-define OPEN-QUERY-brwResults OPEN QUERY {&SELF-NAME} FOR EACH ttTestResult WHERE ttTestResult.DisplayInBrowser.
&Scoped-define TABLES-IN-QUERY-brwResults ttTestResult
&Scoped-define FIRST-TABLE-IN-QUERY-brwResults ttTestResult


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwResults}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRerun rctStatus togPasses togFailures ~
togErrors togIgnored brwResults 
&Scoped-Define DISPLAYED-OBJECTS togPasses togFailures togErrors togIgnored passes ~
failures errors ignored 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR resultsWindow AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mniRerun 
       MENU-ITEM mniRerunSelected LABEL "Selected"      
       MENU-ITEM mniRerunAll    LABEL "All"           .

DEFINE MENU mnuTestResult 
       MENU-ITEM mniOpenInEditor LABEL "Open in Editor"
       MENU-ITEM mniViewDetails LABEL "View Details"  
       RULE
       SUB-MENU  mniRerun       LABEL "Rerun"         .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnRerun  NO-FOCUS
     LABEL "Rerun" 
     SIZE 14 BY 1.1 TOOLTIP "Rerun All Tests".

DEFINE VARIABLE errors AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .71 NO-UNDO.

DEFINE VARIABLE failures AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .71 NO-UNDO.

DEFINE VARIABLE ignored AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .71 NO-UNDO.

DEFINE VARIABLE passes AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .71 NO-UNDO.

DEFINE RECTANGLE rctStatus
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 82.4 BY 1 TOOLTIP "Test Run Result"
     BGCOLOR 17 .

DEFINE VARIABLE togErrors AS LOGICAL INITIAL YES 
     LABEL "Errors:" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .71 NO-UNDO.

DEFINE VARIABLE togIgnored AS LOGICAL INITIAL YES 
     LABEL "Ignored:" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.6 BY .71 NO-UNDO.

DEFINE VARIABLE togFailures AS LOGICAL INITIAL YES  
     LABEL "Failures:" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.6 BY .71 NO-UNDO.

DEFINE VARIABLE togPasses AS LOGICAL INITIAL YES 
     LABEL "Passes:" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwResults FOR 
      ttTestResult SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwResults resultsWindow _FREEFORM
  QUERY brwResults DISPLAY
      ttTestResult.DisplayName COLUMN-LABEL " Name"
      ttTestResult.ResultStatusString COLUMN-LABEL " Result"
      ttTestResult.Duration COLUMN-LABEL " Duration(ms) "
      ttTestResult.ResultMessage COLUMN-LABEL " Message"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 138 BY 7.62 ROW-HEIGHT-CHARS .6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRerun AT ROW 1.14 COL 85.8 WIDGET-ID 36
     togPasses AT ROW 2.29 COL 2.4 WIDGET-ID 56
     togFailures AT ROW 2.29 COL 24.2 WIDGET-ID 60
     togErrors AT ROW 2.29 COL 46.2 WIDGET-ID 58
     togIgnored AT ROW 2.29 COL 68.2 WIDGET-ID 62
     brwResults AT ROW 3.14 COL 1.6 WIDGET-ID 200
     passes AT ROW 2.24 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     failures AT ROW 2.24 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     errors AT ROW 2.24 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     ignored AT ROW 2.24 COL 78.6 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     rctStatus AT ROW 1.14 COL 1.6 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1 SCROLLABLE  WIDGET-ID 100.


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
  CREATE WINDOW resultsWindow ASSIGN
         HIDDEN             = YES
         TITLE              = "OEUnit"
         HEIGHT             = 9.91
         WIDTH              = 139.2
         MIN-HEIGHT         = 5
         MIN-WIDTH          = 100         
         MAX-HEIGHT         = 54.91
         MAX-WIDTH          = 336
         VIRTUAL-HEIGHT     = 54.91
         VIRTUAL-WIDTH      = 336
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW resultsWindow
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Size-to-Fit                                               */
/* BROWSE-TAB brwResults togErrors DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

ASSIGN 
       brwResults:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU mnuTestResult:HANDLE
       brwResults:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1
       brwResults:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       brwResults:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE.

/* SETTINGS FOR FILL-IN errors IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN failures IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ignored IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN passes IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(resultsWindow)
THEN resultsWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwResults
/* Query rebuild information for BROWSE brwResults
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTestResult WHERE ttTestResult.DisplayInBrowser.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwResults */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME resultsWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resultsWindow resultsWindow
ON END-ERROR OF resultsWindow /* OEUnit */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
    /* This case occurs when the user presses the "Esc" key.
          In a persistently run window, just ignore this. If we did
          not, the
          application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resultsWindow resultsWindow
ON WINDOW-CLOSE OF resultsWindow /* OEUnit */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resultsWindow resultsWindow
ON WINDOW-RESIZED OF resultsWindow /* OEUnit */
DO:
  ASSIGN
    FRAME DEFAULT-FRAME:HEIGHT = resultsWindow:HEIGHT
    FRAME DEFAULT-FRAME:WIDTH = resultsWindow:WIDTH
    brwResults:WIDTH IN FRAME DEFAULT-FRAME = (resultsWindow:WIDTH - brwResults:COL)
    brwResults:HEIGHT IN FRAME DEFAULT-FRAME = (resultsWindow:HEIGHT - brwResults:ROW) + 0.9 NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME brwResults
&Scoped-define SELF-NAME brwResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwResults resultsWindow
ON MOUSE-SELECT-DBLCLICK OF brwResults IN FRAME DEFAULT-FRAME
DO:
  RUN viewDetail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwResults resultsWindow
ON RETURN OF brwResults IN FRAME DEFAULT-FRAME
DO:
  RUN viewDetail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwResults resultsWindow
ON ROW-DISPLAY OF brwResults IN FRAME DEFAULT-FRAME
DO:
ttTestResult.ResultStatusString:HANDLE:FONT IN BROWSE brwResults = 6. /* Bold */
IF ttTestResult.ResultStatus = TestResult:StatusPassed THEN
  ttTestResult.ResultStatusString:HANDLE:FGCOLOR IN BROWSE brwResults = 2.  /* Green */
ELSE IF ttTestResult.ResultStatus = TestResult:StatusFailed 
  OR ttTestResult.ResultStatus = TestResult:StatusError THEN
  ttTestResult.ResultStatusString:HANDLE:FGCOLOR IN BROWSE brwResults = 12. /* Red */
ELSE IF ttTestResult.ResultStatus = ? THEN
  ttTestResult.ResultStatusString:HANDLE:FGCOLOR IN BROWSE brwResults = ?. /* Black */  
ELSE 
  ttTestResult.ResultStatusString:HANDLE:FGCOLOR IN BROWSE brwResults = 19. /* Orange */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRerun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRerun resultsWindow
ON CHOOSE OF btnRerun IN FRAME DEFAULT-FRAME /* Rerun */
DO:
    FIND FIRST ttTestResult NO-ERROR.
    RUN rerunTest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mniOpenInEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mniOpenInEditor resultsWindow
ON CHOOSE OF MENU-ITEM mniOpenInEditor /* Open in Editor */
DO:
  RUN openInEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mniRerunAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mniRerunAll resultsWindow
ON CHOOSE OF MENU-ITEM mniRerunAll /* All */
DO:
  FIND FIRST ttTestResult NO-ERROR.
  RUN rerunTest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mniRerunSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mniRerunSelected resultsWindow
ON CHOOSE OF MENU-ITEM mniRerunSelected /* Selected */
DO:
  RUN rerunTest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mniViewDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mniViewDetails resultsWindow
ON CHOOSE OF MENU-ITEM mniViewDetails /* View Details */
DO:
  RUN viewDetail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME togErrors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL togErrors resultsWindow
ON VALUE-CHANGED OF togErrors, togFailures, togPasses, togIgnored IN FRAME DEFAULT-FRAME
DO:
  APPLY "ENTRY" TO brwResults.
  RUN refreshResults.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK resultsWindow 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    MESSAGE "Procedure must be run persistent" VIEW-AS ALERT-BOX ERROR.
    LEAVE.
  END.
  
  RUN setupColours.
  
  /* If running from architect, then display the window docked in an eclipse view */
  runningInArchitect = VALID-HANDLE(hOEIDEService).  
  IF runningInArchitect THEN
  DO:
    viewTitle = "OEUnit - " + getProjectName().
    showView(viewId, viewTitle, {&VIEW_CREATE}).
    
    setViewTitle(viewId, viewTitle, viewTitle).
    RUN getViewHwnd IN hOEIDEService 
      (viewId, viewTitle, OUTPUT viewHwnd) NO-ERROR.
    ASSIGN
      resultsWindow:IDE-PARENT-HWND = viewHwnd
      resultsWindow:IDE-WINDOW-TYPE = 0.
    setEmbeddedWindow(viewId, viewTitle, {&WINDOW-NAME}:HANDLE).
    
  END.
  ELSE
   resultsWindow:LOAD-SMALL-ICON ("OEUnit/UI/Icons/OEUnit.ico"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI resultsWindow  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(resultsWindow)
  THEN DELETE WIDGET resultsWindow.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI resultsWindow  _DEFAULT-ENABLE
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
  DISPLAY togPasses togFailures togErrors togIgnored passes failures errors ignored 
      WITH FRAME DEFAULT-FRAME IN WINDOW resultsWindow.
  ENABLE btnRerun rctStatus togPasses togFailures togErrors togIgnored
         brwResults WHEN NOT(brwResults:SENSITIVE) 
      WITH FRAME DEFAULT-FRAME IN WINDOW resultsWindow.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW resultsWindow.
  APPLY "WINDOW-RESIZED" TO resultsWindow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openInEditor resultsWindow 
PROCEDURE openInEditor PRIVATE :
/*------------------------------------------------------------------------------
  Open the selected test class in the OpenEdge Architect ABL Editor.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE classPath AS CHARACTER NO-UNDO. 

  IF AVAILABLE(ttTestResult) AND runningInArchitect THEN DO:
    classPath = (IF ttTestResult.IsClass THEN ttTestResult.Name ELSE ttTestResult.ParentClass). 
    classPath = SEARCH(REPLACE(classPath,".","/") + ".cls").
    openEditor (getProjectName(), classPath, "_temp_oeunit.p", resultsWindow:HANDLE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshResults resultsWindow 
PROCEDURE refreshResults PRIVATE :
/*------------------------------------------------------------------------------
  Refresh the results browser.
------------------------------------------------------------------------------*/
  totalDisplayed = 0.
  FOR EACH ttTestResult:
    IF ttTestResult.IsClass THEN DO WITH FRAME DEFAULT-FRAME:
      ttTestResult.DisplayInBrowser = (ttTestResult.HasPasses AND togPasses:CHECKED)
                                    OR (ttTestResult.HasFailures AND togFailures:CHECKED)                           
                                    OR (ttTestResult.HasErrors AND togErrors:CHECKED)
                                    OR (ttTestResult.HasIgnored AND togIgnored:CHECKED).
    END.
    ELSE DO WITH FRAME DEFAULT-FRAME:
      ttTestResult.DisplayInBrowser = (ttTestResult.ResultStatus = TestResult:StatusPassed AND togPasses:CHECKED)
                                    OR (ttTestResult.ResultStatus = TestResult:StatusFailed AND togFailures:CHECKED)                           
                                    OR (ttTestResult.ResultStatus = TestResult:StatusError AND togErrors:CHECKED)
                                    OR (ttTestResult.ResultStatus = TestResult:StatusIgnored AND togIgnored:CHECKED).
    END.
    totalDisplayed = totalDisplayed + (IF ttTestResult.DisplayInBrowser THEN 1 ELSE 0).                                    
  END.
  
  CLOSE QUERY brwResults.
  brwResults:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME = totalDisplayed.
  OPEN QUERY brwResults FOR EACH ttTestResult WHERE ttTestResult.DisplayInBrowser.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rerunTest resultsWindow 
PROCEDURE rerunTest PRIVATE :
/*------------------------------------------------------------------------------
  Rerun the selected test. If the selected test is a method, then only that 
  method is run.   
------------------------------------------------------------------------------*/
  IF AVAILABLE(ttTestResult) THEN DO:
    IF ttTestResult.IsClass THEN
      ResultsWindowView:RerunTest(ttTestResult.Name,"").
    ELSE
      ResultsWindowView:RerunTest(ttTestResult.ParentClass , ttTestResult.Name).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTestResults resultsWindow 
PROCEDURE setTestResults :
/*------------------------------------------------------------------------------
  Set the test results and update the browser with the new results.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER TABLE FOR ttTestResult BIND .
  DEFINE INPUT PARAMETER TABLE FOR ttError BIND .
  DEFINE INPUT PARAMETER ipTestCount AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipPasses AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipFailures AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipErrors AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipIgnored AS INTEGER NO-UNDO.
  
  ASSIGN
    passes = ipPasses
    failures = ipFailures
    errors = ipErrors
    ignored = ipIgnored
    togPasses = TRUE
    togFailures = TRUE
    togErrors = TRUE
    togIgnored = TRUE
    totalDisplayed = ipTestCount
    brwResults:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME = totalDisplayed.

  /* Set colour of result status bar */
  FIND FIRST ttTestResult NO-ERROR.
  rctStatus:FILLED IN FRAME DEFAULT-FRAME = TRUE.
  IF AVAILABLE(ttTestResult) THEN DO:
    IF ttTestResult.ResultStatus = TestResult:StatusPassed THEN
      rctStatus:BGCOLOR IN FRAME DEFAULT-FRAME = 17.   /* Green */
    ELSE IF ttTestResult.ResultStatus = TestResult:StatusError 
        OR ttTestResult.ResultStatus = TestResult:StatusFailed THEN
      rctStatus:BGCOLOR IN FRAME DEFAULT-FRAME = 16.   /* Red */
    ELSE IF ttTestResult.ResultStatus = ? THEN
      rctStatus:FILLED IN FRAME DEFAULT-FRAME = FALSE.   /* Blank */
    ELSE
      rctStatus:BGCOLOR IN FRAME DEFAULT-FRAME = 18.   /* Yellow */
  END.
  ELSE 
    rctStatus:BGCOLOR IN FRAME DEFAULT-FRAME = 18.   /* Yellow */
  
  /* If browser not already displayed, set the 'Name' column width */
  IF NOT(brwResults:SENSITIVE) THEN brwResults:GET-BROWSE-COLUMN(1):WIDTH = 60.
  RUN enable_UI.
  IF NOT runningInArchitect  THEN 
    ASSIGN 
      SUB-MENU mniRerun:SENSITIVE IN MENU mnuTestResult = FALSE 
      MENU-ITEM mniOpenInEditor:SENSITIVE IN MENU mnuTestResult = FALSE.
  APPLY "ENTRY" TO brwResults.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewDetail resultsWindow 
PROCEDURE viewDetail PRIVATE :
/*------------------------------------------------------------------------------
  Show the DetailsWindow with the details of the selected test result.
------------------------------------------------------------------------------*/
  IF AVAILABLE(ttTestResult) THEN
     RUN OEUnit\UI\DetailsWindow.w (INPUT TABLE ttError BIND,  ttTestResult.testId,
      (IF ttTestResult.IsClass THEN ttTestResult.Name ELSE ttTestResult.ParentClass),
      (IF ttTestResult.IsClass THEN "" ELSE ttTestResult.Name),
      ttTestResult.ResultStatusString,
      ttTestResult.ResultStatusString:HANDLE:FGCOLOR IN BROWSE brwResults,  
      ttTestResult.Duration, ttTestResult.ErrorCount,
      INPUT-OUTPUT detailsWindowHeight, INPUT-OUTPUT detailsWindowWidth,
      INPUT-OUTPUT detailsWindowRow, INPUT-OUTPUT detailsWindowCol). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setupColours resultsWindow 
PROCEDURE setupColours PRIVATE :
/*------------------------------------------------------------------------------
  Add colours to the colour table.   
------------------------------------------------------------------------------*/
  COLOR-TABLE:NUM-ENTRIES = 21. 
    
  /* Red */
  COLOR-TABLE:SET-DYNAMIC (16, TRUE).
  COLOR-TABLE:SET-RED-VALUE(16,255).
  COLOR-TABLE:SET-GREEN-VALUE(16,113).
  COLOR-TABLE:SET-BLUE-VALUE(16,90).
  
  /* Green */
  COLOR-TABLE:SET-DYNAMIC (17, TRUE).
  COLOR-TABLE:SET-RED-VALUE(17,132).
  COLOR-TABLE:SET-GREEN-VALUE(17,239).
  COLOR-TABLE:SET-BLUE-VALUE(17,107).
  
  /* Yellow */
  COLOR-TABLE:SET-DYNAMIC (18, TRUE).
  COLOR-TABLE:SET-RED-VALUE(18,255).
  COLOR-TABLE:SET-GREEN-VALUE(18,255).
  COLOR-TABLE:SET-BLUE-VALUE(18,66).

  /* Orange */
  COLOR-TABLE:SET-DYNAMIC (19, TRUE).
  COLOR-TABLE:SET-RED-VALUE(19,230).
  COLOR-TABLE:SET-GREEN-VALUE(19,168).
  COLOR-TABLE:SET-BLUE-VALUE(19,0).
  
  /* Label Grey */
  COLOR-TABLE:SET-DYNAMIC (20, TRUE).
  COLOR-TABLE:SET-RED-VALUE(20,140).
  COLOR-TABLE:SET-GREEN-VALUE(20,140).
  COLOR-TABLE:SET-BLUE-VALUE(20,140).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ADEPersistent resultsWindow 
PROCEDURE ADEPersistent :
/*------------------------------------------------------------------------------
  Keeps the window from being deleted after running tests   
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

