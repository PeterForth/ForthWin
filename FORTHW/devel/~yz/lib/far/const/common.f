CONST FARMESSAGEFLAGS
  FMSG_WARNING             0x00000001
  FMSG_ERRORTYPE           0x00000002
  FMSG_KEEPBACKGROUND      0x00000004
  FMSG_DOWN                0x00000008
  FMSG_LEFTALIGN           0x00000010
  FMSG_ALLINONE            0x00000020
  FMSG_MB_OK               0x00010000
  FMSG_MB_OKCANCEL         0x00020000
  FMSG_MB_ABORTRETRYIGNORE 0x00030000
  FMSG_MB_YESNO            0x00040000
  FMSG_MB_YESNOCANCEL      0x00050000
  FMSG_MB_RETRYCANCEL      0x00060000
;

\ DialogItemTypes
0 enum:
  DI_TEXT
  DI_VTEXT
  DI_SINGLEBOX
  DI_DOUBLEBOX
  DI_EDIT
  DI_PSWEDIT
  DI_FIXEDIT
  DI_BUTTON
  DI_CHECKBOX
  DI_RADIOBUTTON
  DI_COMBOBOX
  DI_LISTBOX
; DROP
255 CONSTANT DI_USERCONTROL

CONST FarDialogItemFlags
  DIF_COLORMASK             0x000000FF
  DIF_SETCOLOR              0x00000100
  DIF_BOXCOLOR              0x00000200
  DIF_GROUP                 0x00000400
  DIF_LEFTTEXT              0x00000800
  DIF_MOVESELECT            0x00001000
  DIF_SHOWAMPERSAND         0x00002000
  DIF_CENTERGROUP           0x00004000
  DIF_NOBRACKETS            0x00008000
  DIF_MANUALADDHISTORY      0x00008000
  DIF_SEPARATOR             0x00010000
  DIF_VAREDIT               0x00010000
  DIF_SEPARATOR2            0x00020000
  DIF_EDITOR                0x00020000
  DIF_LISTNOAMPERSAND       0x00020000
  DIF_LISTNOBOX             0x00040000
  DIF_HISTORY               0x00040000
  DIF_BTNNOCLOSE            0x00040000
  DIF_CENTERTEXT            0x00040000
  DIF_EDITEXPAND            0x00080000
  DIF_DROPDOWNLIST          0x00100000
  DIF_USELASTHISTORY        0x00200000
  DIF_MASKEDIT              0x00400000
  DIF_SELECTONENTRY         0x00800000
  DIF_3STATE                0x00800000
  DIF_LISTWRAPMODE          0x01000000
  DIF_LISTAUTOHIGHLIGHT     0x02000000
  DIF_LISTNOCLOSE           0x04000000
  DIF_HIDDEN                0x10000000
  DIF_READONLY              0x20000000
  DIF_NOFOCUS               0x40000000
  DIF_DISABLE               0x80000000
;

\ FarMessagesProc
0 enum:
  DM_FIRST
  DM_CLOSE
  DM_ENABLE
  DM_ENABLEREDRAW
  DM_GETDLGDATA
  DM_GETDLGITEM
  DM_GETDLGRECT
  DM_GETTEXT
  DM_GETTEXTLENGTH
  DM_KEY
  DM_MOVEDIALOG
  DM_SETDLGDATA
  DM_SETDLGITEM
  DM_SETFOCUS
  DM_REDRAW
  DM_SETTEXT
  DM_SETMAXTEXTLENGTH
  DM_SHOWDIALOG
  DM_GETFOCUS
  DM_GETCURSORPOS
  DM_SETCURSORPOS
  DM_GETTEXTPTR
  DM_SETTEXTPTR
  DM_SHOWITEM
  DM_ADDHISTORY

  DM_GETCHECK
  DM_SETCHECK
  DM_SET3STATE

  DM_LISTSORT
  DM_LISTGETITEM
  DM_LISTGETCURPOS
  DM_LISTSETCURPOS
  DM_LISTDELETE
  DM_LISTADD
  DM_LISTADDSTR
  DM_LISTUPDATE
  DM_LISTINSERT
  DM_LISTFINDSTRING
  DM_LISTINFO
  DM_LISTGETDATA
  DM_LISTSETDATA
  DM_LISTSETTITLES
  DM_LISTGETTITLES

  DM_RESIZEDIALOG
  DM_SETITEMPOSITION

  DM_GETDROPDOWNOPENED
  DM_SETDROPDOWNOPENED

  DM_SETHISTORY

  DM_GETITEMPOSITION
  DM_SETMOUSEEVENTNOTIFY

  DM_EDITUNCHANGEDFLAG

  DM_GETITEMDATA
  DM_SETITEMDATA

  DM_LISTSET
  DM_LISTSETMOUSEREACTION

  DM_GETCURSORSIZE
  DM_SETCURSORSIZE

  DM_LISTGETDATASIZE

  DM_GETSELECTION
  DM_SETSELECTION

  DN_LISTHOTKEY
; DROP

0x1000 enum: 
  DN_FIRST
  DN_BTNCLICK
  DN_CTLCOLORDIALOG
  DN_CTLCOLORDLGITEM
  DN_CTLCOLORDLGLIST
  DN_DRAWDIALOG
  DN_DRAWDLGITEM
  DN_EDITCHANGE
  DN_ENTERIDLE
  DN_GOTFOCUS
  DN_HELP
  DN_HOTKEY
  DN_INITDIALOG
  DN_KILLFOCUS
  DN_LISTCHANGE
  DN_MOUSECLICK
  DN_DRAGGED
  DN_RESIZECONSOLE
  DN_MOUSEEVENT
  DN_DRAWDIALOGDONE
; DROP

CONST \ FarMessagesProc extra
 DM_SETREDRAW DM_REDRAW
 DM_SETTEXTLENGTH DM_SETMAXTEXTLENGTH
 DN_CLOSE DM_CLOSE
 DN_KEY DM_KEY
 DM_USER 0x4000
;

\ FARCHECKEDSTATE
0 enum:
  BSTATE_UNCHECKED
  BSTATE_CHECKED
  BSTATE_3STATE
  BSTATE_TOGGLE
; DROP

\ FARLISTMOUSEREACTIONTYPE{
0 enum:
  LMRT_ONLYFOCUS
  LMRT_ALWAYS
  LMRT_NEVER
; DROP

CONST LISTITEMFLAGS
  LIF_SELECTED           0x00010000
  LIF_CHECKED            0x00020000
  LIF_SEPARATOR          0x00040000
  LIF_DISABLE            0x00080000
  LIF_DELETEUSERDATA     0x80000000
;

CONST FARLISTFINDFLAGS
  LIFIND_EXACTMATCH 0x00000001
;

CONST FARLISTINFOFLAGS
  LINFO_SHOWNOBOX             0x00000400
  LINFO_AUTOHIGHLIGHT         0x00000800
  LINFO_REVERSEHIGHLIGHT      0x00001000
  LINFO_WRAPMODE              0x00008000
  LINFO_SHOWAMPERSAND         0x00010000
;

CONST FARDIALOGFLAGS
  FDLG_WARNING             0x00000001
  FDLG_SMALLDIALOG         0x00000002
  FDLG_NODRAWSHADOW        0x00000004
  FDLG_NODRAWPANEL         0x00000008
;

CONST MENUITEMFLAGS
  MIF_SELECTED   0x00010000
  MIF_CHECKED    0x00020000
  MIF_SEPARATOR  0x00040000
  MIF_DISABLE    0x00080000
  MIF_USETEXTPTR 0x80000000
;

CONST FARMENUFLAGS
  FMENU_SHOWAMPERSAND        0x0001
  FMENU_WRAPMODE             0x0002
  FMENU_AUTOHIGHLIGHT        0x0004
  FMENU_REVERSEAUTOHIGHLIGHT 0x0008
  FMENU_USEEXT               0x0020
  FMENU_CHANGECONSOLETITLE   0x0040
;

CONST PLUGINPANELITEMFLAGS
  PPIF_PROCESSDESCR           0x80000000
  PPIF_SELECTED               0x40000000
  PPIF_USERDATA               0x20000000
;

CONST PANELINFOFLAGS
  PFLAGS_SHOWHIDDEN         0x00000001
  PFLAGS_HIGHLIGHT          0x00000002
  PFLAGS_REVERSESORTORDER   0x00000004
  PFLAGS_USESORTGROUPS      0x00000008
  PFLAGS_SELECTEDFIRST      0x00000010
  PFLAGS_REALNAMES          0x00000020
  PFLAGS_NUMERICSORT        0x00000040
;

\ PANELINFOTYPE
0 enum:
  PTYPE_FILEPANEL
  PTYPE_TREEPANEL
  PTYPE_QVIEWPANEL
  PTYPE_INFOPANEL
; DROP

\ FILE_CONTROL_COMMANDS
0 enum:
  FCTL_CLOSEPLUGIN
  FCTL_GETPANELINFO
  FCTL_GETANOTHERPANELINFO
  FCTL_UPDATEPANEL
  FCTL_UPDATEANOTHERPANEL
  FCTL_REDRAWPANEL
  FCTL_REDRAWANOTHERPANEL
  FCTL_SETANOTHERPANELDIR
  FCTL_GETCMDLINE
  FCTL_SETCMDLINE
  FCTL_SETSELECTION
  FCTL_SETANOTHERSELECTION
  FCTL_SETVIEWMODE
  FCTL_SETANOTHERVIEWMODE
  FCTL_INSERTCMDLINE
  FCTL_SETUSERSCREEN
  FCTL_SETPANELDIR
  FCTL_SETCMDLINEPOS
  FCTL_GETCMDLINEPOS
  FCTL_SETSORTMODE
  FCTL_SETANOTHERSORTMODE
  FCTL_SETSORTORDER
  FCTL_SETANOTHERSORTORDER
  FCTL_GETCMDLINESELECTEDTEXT
  FCTL_SETCMDLINESELECTION
  FCTL_GETCMDLINESELECTION
  FCTL_GETPANELSHORTINFO
  FCTL_GETANOTHERPANELSHORTINFO
  FCTL_CHECKPANELSEXIST
  FCTL_SETNUMERICSORT
  FCTL_SETANOTHERNUMERICSORT
; DROP

CONST VIEWER_FLAGS
  VF_NONMODAL              0x00000001
  VF_DELETEONCLOSE         0x00000002
  VF_ENABLE_F6             0x00000004
  VF_DISABLEHISTORY        0x00000008
  VF_IMMEDIATERETURN       0x00000100
  VF_DELETEONLYFILEONCLOSE 0x00000200
;

CONST EDITOR_FLAGS
  EF_NONMODAL              0x00000001
  EF_CREATENEW             0x00000002
  EF_ENABLE_F6             0x00000004
  EF_DISABLEHISTORY        0x00000008
  EF_DELETEONCLOSE         0x00000010
  EF_IMMEDIATERETURN       0x00000100
  EF_DELETEONLYFILEONCLOSE 0x00000200
;

CONST EDITOR_EXITCODE
  EEC_OPEN_ERROR          0
  EEC_MODIFIED            1
  EEC_NOT_MODIFIED        2
  EEC_LOADING_INTERRUPTED 3
;

CONST FARCHARTABLE_COMMAND
  FCT_DETECT 0x40000000
;

CONST FarHelpFlags
  FHELP_NOSHOWERROR 0x80000000
  FHELP_SELFHELP    0x00000000
  FHELP_FARHELP     0x00000001
  FHELP_CUSTOMFILE  0x00000002
  FHELP_CUSTOMPATH  0x00000004
  FHELP_USECONTENTS 0x40000000
;

\  ADVANCED_CONTROL_COMMANDS{
0 enum:
  ACTL_GETFARVERSION
  ACTL_CONSOLEMODE
  ACTL_GETSYSWORDDIV
  ACTL_WAITKEY
  ACTL_GETCOLOR
  ACTL_GETARRAYCOLOR
  ACTL_EJECTMEDIA
  ACTL_KEYMACRO
  ACTL_POSTKEYSEQUENCE
  ACTL_GETWINDOWINFO
  ACTL_GETWINDOWCOUNT
  ACTL_SETCURRENTWINDOW
  ACTL_COMMIT
  ACTL_GETFARHWND
  ACTL_GETSYSTEMSETTINGS
  ACTL_GETPANELSETTINGS
  ACTL_GETINTERFACESETTINGS
  ACTL_GETCONFIRMATIONS
  ACTL_GETDESCSETTINGS
  ACTL_SETARRAYCOLOR
  ACTL_GETWCHARMODE
  ACTL_GETPLUGINMAXREADDATA
  ACTL_GETDIALOGSETTINGS
  ACTL_GETSHORTWINDOWINFO
; DROP

CONST FarSystemSettings
  FSS_CLEARROATTRIBUTE               0x00000001
  FSS_DELETETORECYCLEBIN             0x00000002
  FSS_USESYSTEMCOPYROUTINE           0x00000004
  FSS_COPYFILESOPENEDFORWRITING      0x00000008
  FSS_CREATEFOLDERSINUPPERCASE       0x00000010
  FSS_SAVECOMMANDSHISTORY            0x00000020
  FSS_SAVEFOLDERSHISTORY             0x00000040
  FSS_SAVEVIEWANDEDITHISTORY         0x00000080
  FSS_USEWINDOWSREGISTEREDTYPES      0x00000100
  FSS_AUTOSAVESETUP                  0x00000200
  FSS_SCANSYMLINK                    0x00000400
;

CONST FarPanelSettings
  FPS_SHOWHIDDENANDSYSTEMFILES       0x00000001
  FPS_HIGHLIGHTFILES                 0x00000002
  FPS_AUTOCHANGEFOLDER               0x00000004
  FPS_SELECTFOLDERS                  0x00000008
  FPS_ALLOWREVERSESORTMODES          0x00000010
  FPS_SHOWCOLUMNTITLES               0x00000020
  FPS_SHOWSTATUSLINE                 0x00000040
  FPS_SHOWFILESTOTALINFORMATION      0x00000080
  FPS_SHOWFREESIZE                   0x00000100
  FPS_SHOWSCROLLBAR                  0x00000200
  FPS_SHOWBACKGROUNDSCREENSNUMBER    0x00000400
  FPS_SHOWSORTMODELETTER             0x00000800
;

CONST FarDialogSettings
  FDIS_HISTORYINDIALOGEDITCONTROLS    0x00000001
  FDIS_PERSISTENTBLOCKSINEDITCONTROLS 0x00000002
  FDIS_AUTOCOMPLETEININPUTLINES       0x00000004
  FDIS_BSDELETEUNCHANGEDTEXT          0x00000008
;

CONST FarInterfaceSettings
  FIS_CLOCKINPANELS                  0x00000001
  FIS_CLOCKINVIEWERANDEDITOR         0x00000002
  FIS_MOUSE                          0x00000004
  FIS_SHOWKEYBAR                     0x00000008
  FIS_ALWAYSSHOWMENUBAR              0x00000010
  FIS_USERIGHTALTASALTGR             0x00000080
  FIS_SHOWTOTALCOPYPROGRESSINDICATOR 0x00000100
  FIS_SHOWCOPYINGTIMEINFO            0x00000200
  FIS_USECTRLPGUPTOCHANGEDRIVE       0x00000800
;

CONST FarConfirmationsSettings{
  FCS_COPYOVERWRITE                  0x00000001
  FCS_MOVEOVERWRITE                  0x00000002
  FCS_DRAGANDDROP                    0x00000004
  FCS_DELETE                         0x00000008
  FCS_DELETENONEMPTYFOLDERS          0x00000010
  FCS_INTERRUPTOPERATION             0x00000020
  FCS_DISCONNECTNETWORKDRIVE         0x00000040
  FCS_RELOADEDITEDFILE               0x00000080
  FCS_CLEARHISTORYLIST               0x00000100
  FCS_EXIT                           0x00000200
;

CONST FarDescriptionSettings
  FDS_UPDATEALWAYS                   0x00000001
  FDS_UPDATEIFDISPLAYED              0x00000002
  FDS_SETHIDDEN                      0x00000004
  FDS_UPDATEREADONLY                 0x00000008
;

CONST
  FAR_CONSOLE_GET_MODE       -2
  FAR_CONSOLE_TRIGGER        -1
  FAR_CONSOLE_SET_WINDOWED   0
  FAR_CONSOLE_SET_FULLSCREEN 1
  FAR_CONSOLE_WINDOWED       0
  FAR_CONSOLE_FULLSCREEN     1
;

CONST FAREJECTMEDIAFLAGS
 EJECT_NO_MESSAGE                    0x00000001
 EJECT_LOAD_MEDIA                    0x00000002
;

CONST FARKEYSEQUENCEFLAGS 
  KSFLAGS_DISABLEOUTPUT       0x00000001
  KSFLAGS_NOSENDKEYSTOPLUGINS 0x00000002
;

\  FARMACROCOMMAND
0 enum:
  MCMD_LOADALL
  MCMD_SAVEALL
  MCMD_POSTMACROSTRING
; DROP

CONST FARCOLORFLAGS
  FCLR_REDRAW                 0x00000001
;

\  WINDOWINFO_TYPE
1 enum:
  WTYPE_PANELS
  WTYPE_VIEWER
  WTYPE_EDITOR
  WTYPE_DIALOG
  WTYPE_VMENU
  WTYPE_HELP
; DROP

\ VIEWER_CONTROL_COMMANDS 
0 enum:
  VCTL_GETINFO
  VCTL_QUIT
  VCTL_REDRAW
  VCTL_SETKEYBAR
  VCTL_SETPOSITION
  VCTL_SELECT
; DROP

CONST VIEWER_OPTIONS 
  VOPT_SAVEFILEPOSITION 1
  VOPT_AUTODETECTTABLE  2
;

CONST VIEWER_SETPOS_FLAGS 
  VSP_NOREDRAW    0x0001
  VSP_PERCENT     0x0002
  VSP_RELATIVE    0x0004
  VSP_NORETNEWPOS 0x0008
;

CONST VIEWER_EVENTS 
  VE_READ    0
  VE_CLOSE   1
;

\ EDITOR_EVENTS 
0 enum:
  EE_READ
  EE_SAVE
  EE_REDRAW
  EE_CLOSE
; DROP

CONST
  EEREDRAW_ALL    0
  EEREDRAW_CHANGE 1
  EEREDRAW_LINE   2
;

\ EDITOR_CONTROL_COMMANDS 
0 enum:
  ECTL_GETSTRING
  ECTL_SETSTRING
  ECTL_INSERTSTRING
  ECTL_DELETESTRING
  ECTL_DELETECHAR
  ECTL_INSERTTEXT
  ECTL_GETINFO
  ECTL_SETPOSITION
  ECTL_SELECT
  ECTL_REDRAW
  ECTL_EDITORTOOEM
  ECTL_OEMTOEDITOR
  ECTL_TABTOREAL
  ECTL_REALTOTAB
  ECTL_EXPANDTABS
  ECTL_SETTITLE
  ECTL_READINPUT
  ECTL_PROCESSINPUT
  ECTL_ADDCOLOR
  ECTL_GETCOLOR
  ECTL_SAVEFILE
  ECTL_QUIT
  ECTL_SETKEYBAR
  ECTL_PROCESSKEY
  ECTL_SETPARAM
  ECTL_GETBOOKMARKS
  ECTL_TURNOFFMARKINGBLOCK
  ECTL_DELETEBLOCK
; DROP

\ EDITOR_SETPARAMETER_TYPES 
0 enum:
  ESPT_TABSIZE
  ESPT_EXPANDTABS
  ESPT_AUTOINDENT
  ESPT_CURSORBEYONDEOL
  ESPT_CHARCODEBASE
  ESPT_CHARTABLE
  ESPT_SAVEFILEPOSITION
  ESPT_LOCKMODE
  ESPT_SETWORDDIV
  ESPT_GETWORDDIV
; DROP

\ EXPAND_TABS 
0 enum:
  EXPAND_NOTABS
  EXPAND_ALLTABS
  EXPAND_NEWTABS
; DROP

CONST EDITOR_OPTIONS 
  EOPT_EXPANDALLTABS     0x00000001
  EOPT_PERSISTENTBLOCKS  0x00000002
  EOPT_DELREMOVESBLOCKS  0x00000004
  EOPT_AUTOINDENT        0x00000008
  EOPT_SAVEFILEPOSITION  0x00000010
  EOPT_AUTODETECTTABLE   0x00000020
  EOPT_CURSORBEYONDEOL   0x00000040
  EOPT_EXPANDONLYNEWTABS 0x00000080
;

\ EDITOR_BLOCK_TYPES 
0 enum:
  BTYPE_NONE
  BTYPE_STREAM
  BTYPE_COLUMN
; DROP

CONST EDITOR_CURRENTSTATE 
  ECSTATE_MODIFIED       0x00000001
  ECSTATE_SAVED          0x00000002
  ECSTATE_LOCKED         0x00000004
;

CONST EDITORCOLORFLAGS
  ECF_TAB1 0x10000
;

CONST INPUTBOXFLAGS
  FIB_ENABLEEMPTY      0x00000001
  FIB_PASSWORD         0x00000002
  FIB_EXPANDENV        0x00000004
  FIB_NOUSELASTHISTORY 0x00000008
  FIB_BUTTONS          0x00000010
  FIB_NOAMPERSAND      0x00000020
;

CONST PROCESSNAME_FLAGS
 PN_CMPNAME      0x00000000
 PN_CMPNAMELIST  0x00001000
 PN_GENERATENAME 0x00002000
 PN_SKIPPATH     0x00100000
;

CONST XLATMODE
  XLAT_SWITCHKEYBLAYOUT  0x00000001
  XLAT_SWITCHKEYBBEEP    0x00000002
;

CONST FRSMODE
  FRS_RETUPDIR             0x01
  FRS_RECUR                0x02
  FRS_SCANSYMLINK          0x04
;

CONST MKLINKOP
  FLINK_HARDLINK         1
  FLINK_SYMLINK          2
  FLINK_VOLMOUNT         3
  FLINK_SHOWERRMSG       0x10000
  FLINK_DONOTUPDATEPANEL 0x20000
;

CONST PLUGIN_FLAGS 
  PF_PRELOAD        0x0001
  PF_DISABLEPANELS  0x0002
  PF_EDITOR         0x0004
  PF_VIEWER         0x0008
  PF_FULLCMDLINE    0x0010
;

CONST OPENPLUGININFO_FLAGS 
  OPIF_USEFILTER               0x00000001
  OPIF_USESORTGROUPS           0x00000002
  OPIF_USEHIGHLIGHTING         0x00000004
  OPIF_ADDDOTS                 0x00000008
  OPIF_RAWSELECTION            0x00000010
  OPIF_REALNAMES               0x00000020
  OPIF_SHOWNAMESONLY           0x00000040
  OPIF_SHOWRIGHTALIGNNAMES     0x00000080
  OPIF_SHOWPRESERVECASE        0x00000100
  OPIF_FINDFOLDERS             0x00000200
  OPIF_COMPAREFATTIME          0x00000400
  OPIF_EXTERNALGET             0x00000800
  OPIF_EXTERNALPUT             0x00001000
  OPIF_EXTERNALDELETE          0x00002000
  OPIF_EXTERNALMKDIR           0x00004000
  OPIF_USEATTRHIGHLIGHTING     0x00008000
;

\ OPENPLUGININFO_SORTMODES 
0 enum:
  SM_DEFAULT
  SM_UNSORTED
  SM_NAME
  SM_EXT
  SM_MTIME
  SM_CTIME
  SM_ATIME
  SM_SIZE
  SM_DESCR
  SM_OWNER
  SM_COMPRESSEDSIZE
  SM_NUMLINKS
; DROP

CONST OPERATION_MODES 
  OPM_SILENT     0x0001
  OPM_FIND       0x0002
  OPM_VIEW       0x0004
  OPM_EDIT       0x0008
  OPM_TOPLEVEL   0x0010
  OPM_DESCR      0x0020
  OPM_QUICKVIEW  0x0040
;

8192 CONSTANT MAXSIZE_SHORTCUTDATA

\ OPENPLUGIN_OPENFROM
0 enum:
  OPEN_DISKMENU
  OPEN_PLUGINSMENU
  OPEN_FINDLIST
  OPEN_SHORTCUT
  OPEN_COMMANDLINE
  OPEN_EDITOR
  OPEN_VIEWER
; DROP

CONST FAR_PKF_FLAGS 
  PKF_CONTROL     0x00000001
  PKF_ALT         0x00000002
  PKF_SHIFT       0x00000004
  PKF_PREPROCESS  0x00080000
;

CONST FAR_EVENTS 
  FE_CHANGEVIEWMODE 0
  FE_REDRAW         1
  FE_IDLE           2
  FE_CLOSE          3
  FE_BREAK          4
  FE_COMMAND        5
;
