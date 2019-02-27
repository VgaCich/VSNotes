unit StreamsEditor;

//TODO: progressbar

interface

uses
  Windows, Messages, CommCtrl, ShellAPI, AvL, avlUtils, avlListViewEx,
  NotesFile, DataNode, Editor;

type
  TStreamsEditor = class(TEditorPage)
    ToolBar: TToolBar;
    StreamsList: TListViewEx;
    ListMenu, CompressionMenu: TMenu;
  private
    FTBImages, FFileIcons: TImageList;
    FTempDir: string;
    procedure Resize(Sender: TObject);
    procedure AddStreams;
    procedure DeleteStreams;
    function GetFileIconIndex(const Name: string): Integer;
    procedure StreamsListDblClick(Sender: TObject);
    procedure StreamsListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StreamsListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NewStream(const FileName: string);
    procedure OpenStreams;
    procedure SaveStreams;
    procedure SaveStream(const FileName: string; Stream: TDataStream);
    procedure SetCompression(Compression: string);
    procedure SetMIMEType(const MIMEType: string);
    procedure UpdateComprList;
    procedure UpdateList;
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
  protected
    procedure Modify;
    procedure SetNode(Value: TDataNode); override;
  public
    constructor Create(Parent: TEditor); override;
    destructor Destroy; override;
    procedure Save; override;
    procedure DoEditAction(Action: TEditAction); override;
  end;

implementation

uses
  MainForm;

type
  TTBButtons = (tbAdd, tbSave, tbDelete, tbMIMEType, tbCompression, tbShowSystem);

const
  SFilter = 'All files|*.*';
  SConfirmDelete = 'Delete attachment "%s"?';
  SConfirmMulDelete = 'Delete selected attachments?';
  SAskMIMEType = 'Enter MIME type:';
  TBButtons: array[TTBButtons] of record Caption: string; ImageIndex: Integer; end = (
    (Caption: 'Add'; ImageIndex: 0),
    (Caption: 'Save'; ImageIndex: 1),
    (Caption: 'Delete'; ImageIndex: 2),
    (Caption: 'MIME Type'; ImageIndex: 4),
    (Caption: 'Compression'; ImageIndex: 5),
    (Caption: 'Show system files'; ImageIndex: 3));
  ListColumns: array[0..3] of record Caption: string; Width: Integer end = (
    (Caption: 'Name'; Width: 140),
    (Caption: 'Size'; Width: 80),
    (Caption: 'MIME Type'; Width: 80),
    (Caption: 'Compr.'; Width: 60));
  IDRefresh = 15001;
  IDAdd = IDRefresh + 2;
  IDSave = IDAdd + 1;
  IDRename = IDSave + 1;
  IDDelete = IDRename + 1;
  IDMIMEType = IDDelete + 1;
  IDCompression = IDMIMEType + 1;
  MenuList: array[0..7] of PChar = ('15001',
    'Refresh'#9'F5',
    '-',
    'Add'#9'F7',
    'Save'#9'F8',
    'Rename'#9'F2',
    'Delete'#9'Del',
    'MIME Type');
  IDMenuCompression = 16000;
  MenuCompression: array[0..0] of PChar = ('16001');

{ TStreamsEditor }

constructor TStreamsEditor.Create(Parent: TEditor);
var
  Btn: TTBButtons;
  i: Integer;
begin
  inherited;
  FTBImages := TImageList.Create;
  FTBImages.AddMasked(LoadImage(hInstance, 'TBSTREAMSEDITOR', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  FFileIcons := TImageList.Create;
  FFileIcons.LoadSystemIcons(true);
  ToolBar := TToolBar.Create(Self, true);
  ToolBar.Style := ToolBar.Style or TBSTYLE_TOOLTIPS or CCS_TOP;
  ToolBar.ExStyle := ToolBar.ExStyle or TBSTYLE_EX_MIXEDBUTTONS;
  ToolBar.Perform(TB_SETMAXTEXTROWS, 0, 0);
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  ToolBar.Images := FTBImages;
  for Btn := Low(TTBButtons) to High(TTBButtons) do
    ToolBar.ButtonAdd(TBButtons[Btn].Caption, TBButtons[Btn].ImageIndex);
  StreamsList := TListViewEx.Create(Self);
  StreamsList.Style := StreamsList.Style and not LVS_SINGLESEL or LVS_SHOWSELALWAYS or LVS_EDITLABELS or LVS_NOSORTHEADER or LVS_SORTASCENDING; //TODO: switches for sorting & etc
  //StreamsList.ExStyle := StreamsList.ExStyle or WS_EX_STATICEDGE and not WS_EX_CLIENTEDGE;
  StreamsList.ViewStyle := LVS_REPORT;
  StreamsList.OptionsEx := StreamsList.OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  StreamsList.SetPosition(0, ToolBar.Height);
  StreamsList.SmallImages := FFileIcons;
  StreamsList.OnDblClick := StreamsListDblClick;
  StreamsList.OnKeyDown := StreamsListKeyDown;
  StreamsList.OnMouseDown := StreamsListMouseDown;
  DragAcceptFiles(Handle, true);
  for i := 0 to High(ListColumns) do
    with ListColumns[i] do
      StreamsList.ColumnAdd(Caption, Width);
  CompressionMenu := TMenu.Create(Self, false, MenuCompression);
  FTempDir := UniTempFile;
  ListMenu := TMenu.Create(Self, false, MenuList);
  InsertMenu(ListMenu.Handle, IDCompression, MF_BYCOMMAND or MF_POPUP, CompressionMenu.Handle, PChar(TBButtons[tbCompression].Caption));
  ForceDirectories(FTempDir);
  OnResize := Resize;
end;

destructor TStreamsEditor.Destroy;
begin
  FreeAndNil(FTBImages);
  FFileIcons.Handle := 0;
  FreeAndNil(FFileIcons);
  if (FTempDir <> '') and SameText(Copy(FTempDir, 1, Length(TempDir)), TempDir) then
    DeleteDir(FTempDir);
  inherited;
end;

procedure TStreamsEditor.AddStreams;
var
  FileName: string;
  Files: TStringList;
  i: Integer;
begin
  if not Assigned(FNode) then Exit;
  FileName := '';
  if not OpenSaveDialog(Handle, true, '', '', SFilter, '', 0, OFN_FILEMUSTEXIST or OFN_ALLOWMULTISELECT, FileName) then Exit;
  Files := TStringList.Create;
  try
    Files.Text := FileName;
    for i := 0 to Files.Count - 1 do
      NewStream(Files[i]);
  finally
    Files.Free;
  end;
end;

procedure TStreamsEditor.DeleteStreams;
var
  i: Integer;
  Stream: TDataStream;
  DoModify: Boolean;
begin
  DoModify := false;
  if (StreamsList.SelCount > 1) and (MessageBox(FormMain.Handle, SConfirmMulDelete,
    AppCaption, MB_YESNO or MB_ICONEXCLAMATION) = IDNO) then Exit;
  for i := 0 to StreamsList.SelCount - 1 do
  begin
    Stream := TDataStream(StreamsList.ItemObject[StreamsList.Selected[i]]);
    if not Assigned(Stream) then Continue;
    if (StreamsList.SelCount = 1) and (MessageBox(FormMain.Handle, PChar(Format(SConfirmDelete, [Stream.Name])),
      AppCaption, MB_YESNO or MB_ICONEXCLAMATION) = IDNO) then Exit;
    Stream.Free;
    DoModify := true;
  end;
  if DoModify then
    Modify;
end;

procedure TStreamsEditor.DoEditAction(Action: TEditAction);
begin
  //TODO: Clipboard
end;

function TStreamsEditor.GetFileIconIndex(const Name: string): Integer;
var
  SFI: TShFileInfo;
begin
  SHGetFileInfo(PChar(Name), FILE_ATTRIBUTE_NORMAL, SFI, SizeOf(SFI),
                SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  Result := SFI.iIcon;
end;

procedure TStreamsEditor.StreamsListDblClick(Sender: TObject);
begin
  OpenStreams;
end;

procedure TStreamsEditor.StreamsListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_F2: StreamsList.Perform(LVM_EDITLABEL, StreamsList.SelectedIndex, 0);
      VK_F5: UpdateList;
      VK_F7: AddStreams;
      VK_F8: SaveStreams;
      VK_RETURN: OpenStreams;
      VK_DELETE: DeleteStreams;
    end;
  if Shift = [ssCtrl] then
    case Key of
      Ord('I'): AddStreams;
      Ord('E'): SaveStreams;
    end;
end;

procedure TStreamsEditor.StreamsListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cur: TPoint;
begin
  Cur := Point(X, Y);
  ClientToScreen((Sender as TWinControl).Handle, Cur);
  if Button = mbRight then
  begin
    if StreamsList.SelCount <= 1 then
      StreamsList.SelectedIndex := StreamsList.ItemAtPoint(X, Y);
    UpdateComprList;
    ListMenu.Popup(Cur.X, Cur.Y);
  end;
end;

procedure TStreamsEditor.OpenStreams;
var
  Stream: TDataStream;
  FileName: string;
  i: Integer;
begin
  for i := 0 to StreamsList.SelCount - 1 do
  begin
    Stream := TDataStream(StreamsList.ItemObject[StreamsList.Selected[i]]);
    if not Assigned(Stream) then Exit;
    FileName := AddTrailingBackslash(FTempDir) + CorrectFileName(Stream.Name);
    SaveStream(FileName, Stream);
    ShellExecute(FormMain.Handle, 'open', PChar(FileName), '', '', SW_SHOWNORMAL);
  end;
end;

procedure TStreamsEditor.Resize(Sender: TObject);
begin
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  StreamsList.SetSize(ClientWidth, ClientHeight - ToolBar.Height); //TODO: Column autosizing
end;

procedure TStreamsEditor.Save;
begin

end;

procedure TStreamsEditor.SetNode(Value: TDataNode);
begin
  inherited;
  UpdateList;
end;

procedure TStreamsEditor.SaveStreams;
var
  Stream: TDataStream;
  FileName: string;
  i: Integer;
begin
  if StreamsList.SelCount > 1 then
    if not OpenDirDialog(FormMain.Handle, '', true, FileName) then Exit;
  for i := 0 to StreamsList.SelCount - 1 do
  begin
    Stream := TDataStream(StreamsList.ItemObject[StreamsList.Selected[i]]);
    if not Assigned(Stream) then Continue;
    if StreamsList.SelCount = 1 then
    begin
      FileName := CorrectFileName(Stream.Name);
      if not OpenSaveDialog(FormMain.Handle, false, '', '', SFilter, '', 0, OFN_OVERWRITEPROMPT, FileName) then Exit;
      SaveStream(FileName, Stream);
    end
    else
      SaveStream(AddTrailingBackslash(FileName) + CorrectFileName(Stream.Name), Stream);
  end;
end;

procedure TStreamsEditor.UpdateList;
var
  i: Integer;
  Row: Integer;
  ShowSystem: Boolean;
  SelectedItem: TObject;
begin
  SelectedItem := StreamsList.ItemObject[StreamsList.SelectedIndex];
  StreamsList.Clear;
  if not Assigned(FNode) then Exit;
  ShowSystem := ToolBar.ButtonCheck[Ord(tbShowSystem)];
  StreamsList.BeginUpdate;
  try
    for i := 0 to FNode.Streams.Count - 1 do
      with StreamsList, FNode.Streams[i] do
        if ShowSystem or ((Name <> '') and (Name[1] <> ':')) then
        begin
          Row := ItemAdd(Name);
          ItemImageIndex[Row] := GetFileIconIndex(Name);
          ItemObject[Row] := FNode.Streams[i];
          Items[Row, 1] := SizeToStr(Data.Size);
          Items[Row, 2] := MIMEType;
          Items[Row, 3] := Compression;
          if FNode.Streams[i] = SelectedItem then
            StreamsList.SelectedIndex := Row;
        end;
  finally
    StreamsList.EndUpdate;
  end;
end;

procedure TStreamsEditor.WMCommand(var Msg: TWMCommand);

  function AskMIMEType: string;
  begin
    if not InputQuery(Handle, AppCaption, SAskMIMEType, Result) then
      Result := '';
  end;

var
  Cur: TPoint;
  Compr: string;
begin
  if Assigned(ToolBar) and (Msg.Ctl = ToolBar.Handle) then
    case TTBButtons(Msg.ItemID) of
      tbAdd: AddStreams;
      tbSave: SaveStreams;
      tbDelete: DeleteStreams;
      tbMIMEType: SetMIMEType(AskMIMEType);
      tbCompression: begin
        GetCursorPos(Cur);
        UpdateComprList;
        CompressionMenu.Popup(Cur.X, Cur.Y);
      end;
      tbShowSystem: begin
        ToolBar.ButtonCheck[Ord(tbShowSystem)] := not ToolBar.ButtonCheck[Ord(tbShowSystem)];
        UpdateList;
      end;
    end;
  if (Msg.Ctl = 0) and (Msg.NotifyCode in [0, 1]) then
    case Msg.ItemID of
      IDRefresh: UpdateList;
      IDAdd: AddStreams;
      IDSave: SaveStreams;
      IDRename: StreamsList.Perform(LVM_EDITLABEL, StreamsList.SelectedIndex, 0);
      IDDelete: DeleteStreams;
      IDMIMEType: SetMIMEType(AskMIMEType);
      IDMenuCompression .. IDMenuCompression + 256: begin
        SetLength(Compr, GetMenuString(CompressionMenu.Handle, Msg.ItemID, nil, 0, MF_BYCOMMAND));
        GetMenuString(CompressionMenu.Handle, Msg.ItemID, PChar(Compr), Length(Compr) + 1, MF_BYCOMMAND);
        SetCompression(Compr);
      end;
    end;
end;

procedure TStreamsEditor.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  for i := 0 to DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0) - 1 do
  begin
    DragQueryFile(Msg.Drop, i, FileName, MAX_PATH + 1);
    NewStream(string(FileName));
  end;
  DragFinish(Msg.Drop);
end;

procedure TStreamsEditor.Modify;
begin
  inherited;
  UpdateList;
end;

procedure TStreamsEditor.NewStream(const FileName: string);
var
  F: TFileStream;
begin
  if not Assigned(FNode) then Exit;
  if not FileExists(FileName) then Exit;
  FNode.Streams.ItemByName[ExtractFileName(FileName)].Free;
  with FNode.Streams[FNode.Streams.Add(TDataStream.Create)] do
  begin
    Name := ExtractFileName(FileName);
    MIMEType := GetMIMEType(ExtractFileExt(Name));
    Compression := FNode.Metadata[SMKNodeDefaultCompression, ''];
    F := TFileStream.Create(FileName, fmOpenRead);
    try
      if Data is TMemoryStream then //TODO: remove when NRV2E stream will be optimised
        Data.Size := F.Size;
      Data.CopyFrom(F, 0);
    finally
      F.Free;
    end;
  end;
  Modify;
end;

procedure TStreamsEditor.SaveStream(const FileName: string; Stream: TDataStream);
var
  F: TFileStream;
begin
  if not DirectoryExists(ExtractFilePath(FileName)) then
    ForceDirectories(ExtractFilePath(FileName));
  F := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  try
    F.CopyFrom(Stream.Data, 0);
    F.Size := F.Position;
  finally
    F.Free;
  end;
end;

procedure TStreamsEditor.SetCompression(Compression: string);
var
  i: Integer;
  Stream: TDataStream;
  DoModify: Boolean;
begin
  DoModify := false;
  for i := 0 to StreamsList.SelCount - 1 do
  begin
    Stream := TDataStream(StreamsList.ItemObject[StreamsList.Selected[i]]);
    if not Assigned(Stream) then Continue;
    Stream.Compression := Compression;
    DoModify := true;
  end;
  if DoModify then
    Modify;
end;

procedure TStreamsEditor.SetMIMEType(const MIMEType: string);
var
  i: Integer;
  Stream: TDataStream;
  DoModify: Boolean;
begin
  DoModify := false;
  for i := 0 to StreamsList.SelCount - 1 do
  begin
    Stream := TDataStream(StreamsList.ItemObject[StreamsList.Selected[i]]);
    if not Assigned(Stream) then Continue;
    Stream.MIMEType := MIMEType;
    DoModify := true;
  end;
  if DoModify then
    Modify;
end;

procedure TStreamsEditor.UpdateComprList;
var
  Stream: TDataStream;
  i: Integer;
begin
  while DeleteMenu(CompressionMenu.Handle, 0, MF_BYPOSITION) do;
  Stream := TDataStream(StreamsList.ItemObject[StreamsList.SelectedIndex]);
  if not Assigned(Stream) then Exit;
  for i := 0 to Stream.AvailableCompressors.Count - 1 do
    CompressionMenu.AddItem(Stream.AvailableCompressors[i], i);
end;

procedure TStreamsEditor.WMNotify(var Msg: TWMNotify);
begin
  if Assigned(StreamsList) and (Msg.NMHdr.hwndFrom = StreamsList.Handle) then
  begin
    if Msg.NMHdr.code = LVN_BEGINLABELEDIT then
    begin
      Msg.Result := 0;
      SendMessage(StreamsList.Perform(LVM_GETEDITCONTROL, 0, 0), EM_SETLIMITTEXT, 255, 0);
    end;
    if Msg.NMHdr.code = LVN_ENDLABELEDIT then
      with PLVDispInfo(Msg.NMHdr)^ do
        if Assigned(item.pszText) and (item.lParam <> 0) then
        begin
          TDataStream(item.lParam).Name := item.pszText;
          Msg.Result := 0;
          Modify;
        end;
  end;
end;

end.
