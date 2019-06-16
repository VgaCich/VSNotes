unit MainForm;

//TODO: Service functions:
//Autopaste
//Directory capture (listing, .diz, etc)

interface

uses
  Windows, CommCtrl, Messages, ShellAPI, AvL, avlUtils, avlSettings, avlSplitter,
  Navigator, Editor, NotesFile, DataNode, ImportExport;

type
  TVSNotesMainForm = class(TForm)
    MainMenu: TMenu;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    Splitter: TSplitter;
    Navigator: TNavigator;
    Editor: TEditor;
  private
    FMinWidth, FMinHeight: Integer;
    FAccelTable: HAccel;
    FTBImages: TImageList;
    FFile: TNotesFile;
    procedure FileModified(Sender: TObject);
    function FormClose(Sender: TObject): Boolean;
    procedure FormResize(Sender: TObject);
    function FormProcessMsg(var Msg: TMsg): Boolean;
    function GetSettings: TSettings;
    procedure NavigatorNodeSelected(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure RepaintAll;
    procedure SaveClick(Sender: TObject; SaveAs: Boolean);
    procedure ImportClick(Sender: TObject);
    procedure OpenFile(const FileName: string);
    procedure ShowAbout;
    procedure SplitterMove(Sender: TObject);
    function RequestSave: Boolean;
    procedure SaveFile(const FileName: string);
    procedure UpdateCaptions;
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMSizing(var Msg: TWMMoving); message WM_SIZING;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  FormMain: TVSNotesMainForm;

const
  AppCaption = 'VS Notes';
  AppName = 'VSNotes';

implementation

type
  TTBButtons = (tbNew, tbOpen, tbSave, tbPrint, tbUndo, tbCut, tbCopy, tbPaste, tbOptions);

const
  SVFNFilter = 'VS Notes files|*.vsn|All files|*.*';
  SDefExt = 'vsn';
  SSaveNow = 'Current file has unsaved changes.'#13'Save now?';
  CRLF = #13#10;
  AboutIcon = 'MAINICON';
  AboutCaption = 'About ';
  AboutText = 'VgaSoft Notes 1.0 alpha'+CRLF+CRLF+
              'Copyright '#169' VgaSoft, 2015-2019'+CRLF+
              'vgasoft@gmail.com';
  IDMenuFile = 1000;
  IDNew = IDMenuFile + 1;
  IDOpen = IDNew + 1;
  IDSave = IDOpen + 1;
  IDSaveAs = IDSave + 1;
  IDImport = IDSaveAs + 1;
  IDPrint = IDImport + 1;
  IDOptions = IDPrint + 1;
  IDExit = IDOptions + 2;
  MenuFileCapt = '&File';
  MenuFile: array[0..9] of PChar = ('1001',
    '&New'#9'Ctrl-N',
    '&Open...'#9'Ctrl-O',
    '&Save'#9'Ctrl-S',
    'Save &As...',
    'Import...',
    '&Print...'#9'Ctrl-P',
    'Op&tions...',
    '-',
    'E&xit'#9'Alt-X');
  IDMenuEdit = 2000;
  IDUndo = IDMenuEdit + 1;
  IDRedo = IDUndo + 1;
  IDCut = IDRedo + 2;
  IDCopy = IDCut + 1;
  IDPaste = IDCopy + 1;
  IDSelectAll = IDPaste + 1;
  MenuEditCapt = '&Edit';
  MenuEdit: array[0..7] of PChar = ('2001',
    '&Undo'#9'Ctrl-Z',
    '&Redo'#9'Ctrl-Y',
    '-',
    'Cu&t'#9'Ctrl-X',
    '&Copy'#9'Ctrl-C',
    '&Paste'#9'Ctrl-V',
    'Se&lect All'#9'Ctrl-A');
  IDMenuHelp = 5000;
  IDAbout = IDMenuHelp + 1;
  MenuHelpCapt = '&Help';
  MenuHelp: array[0..1] of PChar = ('5001',
    '&About...'#9'F1');
  TBButtons: array[0..10] of record Caption: string; ImageIndex: Integer; end = (
    (Caption: 'New'; ImageIndex: 0),
    (Caption: 'Open'; ImageIndex: 1),
    (Caption: 'Save'; ImageIndex: 2),
    (Caption: 'Print'; ImageIndex: 7),
    (Caption: '-'; ImageIndex: -1),
    (Caption: 'Undo'; ImageIndex: 3),
    (Caption: 'Cut'; ImageIndex: 4),
    (Caption: 'Copy'; ImageIndex: 5),
    (Caption: 'Paste'; ImageIndex: 6),
    (Caption: '-'; ImageIndex: -1),
    (Caption: 'Options'; ImageIndex: 8));
  TBMenuIDs: array[TTBButtons] of Word = (IDNew, IDOpen, IDSave, IDPrint, IDUndo, IDCut, IDCopy, IDPaste, IDOptions);

var
  Accels: array[0..11] of TAccel = (
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('N'); Cmd: IDNew),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('O'); Cmd: IDOpen),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('S'); Cmd: IDSave),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('P'); Cmd: IDPrint),
    (fVirt: FALT or FVIRTKEY; Key: Ord('X'); Cmd: IDExit),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('Z'); Cmd: IDUndo),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('Y'); Cmd: IDRedo),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('X'); Cmd: IDCut),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('C'); Cmd: IDCopy),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('V'); Cmd: IDPaste),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('A'); Cmd: IDSelectAll),
    (fVirt: FVIRTKEY; Key: VK_F1; Cmd: IDAbout));

constructor TVSNotesMainForm.Create;

  procedure AddMenu(const Name: string; ID: Cardinal; const Template: array of PChar);
  var
    Menu: TMenu;
  begin
    Menu := TMenu.Create(Self, false, Template);
    InsertMenu(MainMenu.Handle, ID, MF_BYCOMMAND or MF_POPUP, Menu.Handle, PChar(Name));
  end;

var
  i: Integer;
begin
  inherited Create(nil, AppCaption);
  OnClose := FormClose;
  OnProcessMsg := FormProcessMsg;
  SetSize(600, 400);
  Position := poScreenCenter;
  FMinHeight := 200;
  FMinWidth := 400;
  MainMenu := TMenu.Create(Self, true, ['0']);
  AddMenu(MenuFileCapt, IDMenuFile, MenuFile);
  AddMenu(MenuEditCapt, IDMenuEdit, MenuEdit);
  AddMenu(MenuHelpCapt, IDMenuHelp, MenuHelp);
  SetMenu(Handle, MainMenu.Handle);
  FAccelTable := CreateAcceleratorTable(Accels[0], Length(Accels));
  FTBImages := TImageList.Create;
  FTBImages.AddMasked(LoadImage(hInstance, 'TBMAIN', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  ToolBar := TToolBar.Create(Self, true);
  ToolBar.Style := ToolBar.Style or TBSTYLE_TOOLTIPS or CCS_TOP;
  ToolBar.ExStyle := ToolBar.ExStyle or TBSTYLE_EX_MIXEDBUTTONS;
  ToolBar.Perform(TB_SETMAXTEXTROWS, 0, 0);
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  ToolBar.Images := FTBImages;
  for i := Low(TBButtons) to High(TBButtons) do
    ToolBar.ButtonAdd(TBButtons[i].Caption, TBButtons[i].ImageIndex);
  StatusBar := TStatusBar.Create(Self, '');
  Splitter := TSplitter.Create(Self, true);
  Splitter.SetBounds(200, ToolBar.Height, Splitter.Width, ClientHeight - ToolBar.Height - StatusBar.Height);
  Splitter.OnMove := SplitterMove;
  Navigator := TNavigator.Create(Self);
  Navigator.SetBounds(0, Splitter.Top, Splitter.Left, Splitter.Height);
  Navigator.OnModify := FileModified;
  Navigator.OnNodeSelected := NavigatorNodeSelected;
  Editor := TEditor.Create(Self);
  Editor.SetBounds(Splitter.Right, Splitter.Top, ClientWidth - Splitter.Right, Splitter.Height);
  Editor.OnModify := FileModified;
  for i := 1 to ParamCount do
    if FileExists(ParamStr(i)) then
      OpenFile(ExpandFileName(ParamStr(i)));
  DragAcceptFiles(Handle, true);
  OnResize := FormResize;
  FormResize(Self);
end;

function TVSNotesMainForm.GetSettings: TSettings;
begin
  Result := TSettings.Create(AppName);
end;

procedure TVSNotesMainForm.ShowAbout;
var
  Version: TOSVersionInfo;
  MsgBoxParamsW: TMsgBoxParamsW;
  MsgBoxParamsA: TMsgBoxParamsA;
begin
  Version.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Version);
  if Version.dwPlatformId = VER_PLATFORM_WIN32_NT then
  begin
    FillChar(MsgBoxParamsW, SizeOf(MsgBoxParamsW), #0);
    with MsgBoxParamsW do
    begin
      cbSize := SizeOf(MsgBoxParamsW);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := PWideChar(WideString(AboutText));
      lpszCaption := PWideChar(WideString(AboutCaption+Caption));
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectW(MsgBoxParamsW);
  end
  else begin
    FillChar(MsgBoxParamsA, SizeOf(MsgBoxParamsA), #0);
    with MsgBoxParamsA do
    begin
      cbSize := SizeOf(MsgBoxParamsA);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := PAnsiChar(AboutText);
      lpszCaption := PAnsiChar(AboutCaption+Caption);
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

procedure TVSNotesMainForm.SplitterMove(Sender: TObject);
begin
  Navigator.SetBounds(0, Splitter.Top, Splitter.Left, Splitter.Height);
  Editor.SetBounds(Splitter.Right, Splitter.Top, ClientWidth - Splitter.Right, Splitter.Height);
end;

procedure TVSNotesMainForm.WMCommand(var Msg: TWMCommand);
begin
  if (Msg.Ctl = 0) and (Msg.NotifyCode in [0, 1]) then
    case Msg.ItemID of
      IDExit: Close;
      IDNew: NewClick(Self);
      IDOpen: OpenClick(Self);
      IDSave: SaveClick(Self, false);
      IDSaveAs: SaveClick(Self, true);
      IDImport: ImportClick(Self);
      IDPrint: ShowMessage('No printing supported');
      IDOptions: ShowMessage('What? Options? Fuck that!');
      IDUndo: Editor.DoEditAction(eaUndo); //TODO: Send dat to active control
      IDRedo: Editor.DoEditAction(eaRedo);
      IDCut: Editor.DoEditAction(eaCut);
      IDCopy: Editor.DoEditAction(eaCopy);
      IDPaste: Editor.DoEditAction(eaPaste);
      IDSelectAll: Editor.DoEditAction(eaSelectAll);
      IDAbout: ShowAbout;
    end;
  if Assigned(ToolBar) and (Msg.Ctl = ToolBar.Handle) then
    Perform(WM_COMMAND, TBMenuIDs[TTBButtons(Msg.ItemID)], 0);
end;

procedure TVSNotesMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  //TODO: Import
  for i := 0 to DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0) - 1 do
  begin
    DragQueryFile(Msg.Drop, i, FileName, MAX_PATH + 1);
    if FileExists(string(FileName)) and IsVSNFile(string(FileName)) then
    begin
      if not RequestSave then Exit;
      OpenFile(string(FileName));
      Break;
    end;
  end;
  DragFinish(Msg.Drop);
end;

procedure TVSNotesMainForm.FormResize(Sender: TObject);
begin
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  StatusBar.Perform(WM_SIZE, 0, 0);
  Splitter.Height := ClientHeight - ToolBar.Height - StatusBar.Height;
  if ClientWidth > 0 then
    Splitter.MaxPos := ClientWidth - Splitter.Width;
  Navigator.Height := Splitter.Height;
  Editor.SetSize(ClientWidth - Splitter.Right, Splitter.Height);
  //RepaintAll;
end;

procedure TVSNotesMainForm.WMSizing(var Msg: TWMMoving);
begin
  with Msg do
  begin
    if DragRect.Right - DragRect.Left < FMinWidth then
      if (Edge = WMSZ_LEFT) or (Edge = WMSZ_TOPLEFT) or (Edge = WMSZ_BOTTOMLEFT)
        then DragRect.Left := DragRect.Right - FMinWidth
        else DragRect.Right := DragRect.Left + FMinWidth;
    if DragRect.Bottom - DragRect.Top < FMinHeight then
      if (Edge = WMSZ_TOP) or (Edge = WMSZ_TOPLEFT) or (Edge = WMSZ_TOPRIGHT)
        then DragRect.Top := DragRect.Bottom - FMinHeight
        else DragRect.Bottom := DragRect.Top + FMinHeight;
  end;
end;

procedure TVSNotesMainForm.RepaintAll;
var
  Cur: TWinControl;
begin
  Cur := NextControl;
  while Assigned(Cur) do
  begin
    Cur.Invalidate;
    UpdateWindow(Cur.Handle);
    Cur := Cur.NextControl;
  end;
end;

destructor TVSNotesMainForm.Destroy;
begin
  DestroyAcceleratorTable(FAccelTable);
  FreeAndNil(FTBImages);
  inherited;
end;

procedure TVSNotesMainForm.FileModified(Sender: TObject);
begin
  if Assigned(FFile) then
    FFile.Modify;
  UpdateCaptions;
end;

function TVSNotesMainForm.FormClose(Sender: TObject): Boolean;
begin
  Result := RequestSave;
end;

function TVSNotesMainForm.FormProcessMsg(var Msg: TMsg): Boolean;
begin
  Result := TranslateAccelerator(Handle, FAccelTable, Msg) <> 0;
end;

procedure TVSNotesMainForm.NavigatorNodeSelected(Sender: TObject);
begin
  Editor.Save;
  Editor.Node := Navigator.SelNode;
  UpdateCaptions;
end;

procedure TVSNotesMainForm.NewClick(Sender: TObject);
begin
  if not RequestSave then Exit;
  Navigator.Clear;
  Editor.Node := nil;
  FFile.Free;
  FFile := TNotesFile.Create;
  FFile.MakeBackup := true;
  //TODO: Fill metadata
  UpdateCaptions;
  Navigator.RootNode := FFile.RootNode;
end;

procedure TVSNotesMainForm.OpenClick(Sender: TObject);
var
  FileName: string;
begin
  if not RequestSave then Exit;
  if Assigned(FFile) then
    FileName := FFile.Name
  else
    FileName := '';
  if not OpenSaveDialog(Handle, true, '', SDefExt, SVFNFilter, '', 0, OFN_FILEMUSTEXIST, FileName) then Exit;
  OpenFile(FileName);
end;

procedure TVSNotesMainForm.SaveClick(Sender: TObject; SaveAs: Boolean);
var
  FileName: string;
begin
  if not Assigned(FFile) then Exit;
  FileName := FFile.Name;
  if ((FFile.Name = '') or SaveAs) and not OpenSaveDialog(Handle, false, '', SDefExt, SVFNFilter, '', 0, OFN_OVERWRITEPROMPT, FileName) then Exit;
  SaveFile(FileName);
end;

procedure TVSNotesMainForm.ImportClick(Sender: TObject);
var
  FileName, Key: string;
  i: Integer;
  DestNode, Data: TDataNode;
begin
  FileName := '';
  if not OpenSaveDialog(Handle, true, '', '', SImportFilter, '', 0, OFN_FILEMUSTEXIST, FileName) then Exit;
  Data := ImportFile(FileName);
  if not Assigned(Data) then Exit;
  if not Assigned(FFile) then
    NewClick(Self);
  DestNode := Navigator.SelNode;
  if not Assigned(DestNode) then
    DestNode := FFile.RootNode;
  for i := 0 to Data.RawMetadata.Count - 1 do
  begin
    Key := Copy(Data.RawMetadata[i], 1, FirstDelimiter('=', Data.RawMetadata[i]) - 1);
    if DestNode.RawMetadata.IndexOfName(Key) < 0 then
      DestNode.RawMetadata.Values[Key] := Data.RawMetadata.Values[Key];
  end;
  while Data.Children.Count > 0 do
    DestNode.Children.Add(Data.Children[0]);
  FFile.Modify;
  UpdateCaptions;
  Navigator.UpdateTree;
end;

procedure TVSNotesMainForm.OpenFile(const FileName: string);
begin
  if not Assigned(FFile) then
    FFile := TNotesFile.Create;
  FFile.MakeBackup := true;
  Navigator.Clear;
  Editor.Node := nil;
  FFile.Name := FileName;
  FFile.Load;
  UpdateCaptions;
  Navigator.RootNode := FFile.RootNode;
end;

function TVSNotesMainForm.RequestSave: Boolean;
begin
  Editor.Save;
  Result := true;
  if not Assigned(FFile) or not FFile.Modified then Exit;
  case MessageBox(Handle, SSaveNow, AppCaption, MB_YESNOCANCEL or MB_ICONEXCLAMATION) of
    IDYES: SaveClick(Self, false);
    IDCANCEL: Result := false;
  end;
end;

procedure TVSNotesMainForm.SaveFile(const FileName: string);
begin
  if not Assigned(FFile) then Exit;
  FFile.Name := FileName;
  Editor.Save;
  FFile.Save;
  UpdateCaptions;
end;

procedure TVSNotesMainForm.UpdateCaptions;
const
  Modified: array[Boolean] of string = ('', '*');
begin
  Caption := AppCaption;
  if Assigned(FFile) then
  begin
    Caption := Caption + ':';
    if FFile.RootNode.Metadata[SMKTitle, ''] <> '' then
      Caption := Caption + ' ' + FFile.RootNode.Metadata[SMKTitle, ''];
    if FFile.Name = '' then
      Caption := Caption + ' [unsaved]'
    else
      Caption := Caption + ' [' + ExtractFileName(FFile.Name) + ']';
    Caption := Caption + Modified[FFile.Modified];
  end;
end;

end.
