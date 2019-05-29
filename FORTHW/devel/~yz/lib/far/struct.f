REQUIRE STRUCT: lib/ext/struct.f

260 CONSTANT NM

STRUCT: TPluginStartupInfo
 4 -- StructSize \ integer;
NM -- ModuleName \ array[0..NM-1] of Char
 4 -- ModuleNumber \ integer;
 4 -- RootKey \ PChar;

 4 -- Menu \ TFarApiMenu;
 4 -- Dialog \ TFarApiDialog;
 4 -- Message \ TFarApiMessage;
 4 -- GetMsg \ TFarApiGetMsg;
 4 -- Control \ TFarApiControl;
 4 -- SaveScreen \ TFarApiSaveScreen;
 4 -- RestoreScreen \ TFarApiRestoreScreen;
 4 -- GetDirList \ TFarApiGetDirList;
 4 -- GetPluginDirList \ TFarApiGetPluginDirList;
 4 -- FreeDirList \ TFarApiFreeDirList;
 4 -- Viewer \ TFarApiViewer;
 4 -- Editor \ TFarApiEditor;
 4 -- CmpName \ TFarApiCmpName;
 4 -- CharTable \ TFarApiCharTable;
 4 -- Text \ TFarApiText;
 4 -- EditorControl \ TFarApiEditorControl;
\  ��������� �� ��������� � �������� �������� ������� �� far.exe
 4 -- FSF \ PFarStandardFunctions;
\  ������� ������ ������
 4 -- ShowHelp \ TFarApiShowHelp;
\  �������, ������� ����� ����������� � � ���������, � � �������, �...
 4 -- AdvControl \ TFarApiAdvControl;
\  ������� ��� ����������� �������
 4 -- InputBox \ TFarApiInputBox;
 4 -- DialogEx \ TFarApiDialogEx;
 4 -- SendDlgMessage \ TFarApiSendDlgMessage;
 4 -- DefDlgProc \ TFarApiDefDlgProc;
 4 -- Reserved \ DWORD;
 4 -- ViewerControl \ TFarApiViewerControl;
;STRUCT

STRUCT: TPluginInfo 
 4 -- StructSize \ integer;
 4 -- Flags \ DWORD;
 4 -- DiskMenuStrings \ PPCharArray;
 4 -- DiskMenuNumbers \ PIntegerArray;
 4 -- DiskMenuStringsNumber \ integer;
 4 -- PluginMenuStrings \ PPCharArr;
 4 -- PluginMenuStringsNumber \ integer;
 4 -- PluginConfigStrings \ PPCharArr;
 4 -- PluginConfigStringsNumber \ integer;
 4 -- CommandPrefix \ PChar;
 4 -- Reserved \ DWORD;
;STRUCT

REQUIRE ENUM ~ygrek/lib/enum.f

:NONAME 2DUP -- DROP ; ENUM UnionItem
: Union: ( ofs n -- ofs+n) UnionItem + ;

STRUCT: TFarDialogItem
  4 -- ItemType
  4 -- X1
  4 -- Y1
  4 -- X2
  4 -- Y2
  4 -- Focus
  4 Union:  Extra Selected History Mask ListItems ListPos VBuf ;
  4 -- Flags
  4 -- DefaultButton
  512 -- Data
;STRUCT
