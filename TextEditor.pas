unit TextEditor;

interface

//TODO: Tabulation settings
//TODO: save cursor position in each node (in meta or tag? dunno)
//TODO: TWebBrowser, maybe?
//TODO: Accept dragged files
//TODO: Autopaste

uses
  Windows, Messages, CommCtrl, RichEdit, AvL, RichEditW, NotesFile, DataNode, Editor;

type
  TTextEditor = class(TEditorPage)
    ToolBar: TToolBar;
    RichEdit: TRichEditW;
    FontCombo: TComboBox;
  private
    FTBImages: TImageList;
    procedure Resize(Sender: TObject);
  protected
    procedure SetNode(Value: TDataNode); override;
  public
    constructor Create(Parent: TEditor); override;
    destructor Destroy; override;
    procedure Save; override;
    procedure DoEditAction(Action: TEditAction); override;
  end;

implementation

type
  TTBButtons = (tbDummy);

const
  TBButtons: array[TTBButtons] of record Caption: string; ImageIndex: Integer; end = (
    (Caption: 'Dummy'; ImageIndex: 0));

{ TTextEditor }

constructor TTextEditor.Create(Parent: TEditor);
var
  Btn: TTBButtons;
begin
  inherited;
  FTBImages := TImageList.Create;
  FTBImages.AddMasked(LoadImage(hInstance, 'TBTEXTEDITOR', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  ToolBar := TToolBar.Create(Self, true);
  ToolBar.Style := ToolBar.Style or TBSTYLE_TOOLTIPS or CCS_TOP;
  ToolBar.ExStyle := ToolBar.ExStyle or TBSTYLE_EX_MIXEDBUTTONS;
  ToolBar.Perform(TB_SETMAXTEXTROWS, 0, 0);
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  ToolBar.Indent := 120;
  ToolBar.Images := FTBImages;
  for Btn := Low(TTBButtons) to High(TTBButtons) do
    ToolBar.ButtonAdd(TBButtons[Btn].Caption, TBButtons[Btn].ImageIndex);
  FontCombo := TComboBox.Create(ToolBar, csDropDownList);
  FontCombo.SetBounds(0, 0, ToolBar.Indent, ToolBar.Height);
  RichEdit := TRichEditW.Create(Self, '', true);
  RichEdit.SetPosition(0, ToolBar.Height);
  OnResize := Resize;
end;

destructor TTextEditor.Destroy;
begin
  FreeAndNil(FTBImages);
  inherited;
end;

procedure TTextEditor.DoEditAction(Action: TEditAction);
const
  Msgs: array[TEditAction] of TMessage = (
    (Msg: EM_UNDO; wParam: 0; lParam: 0),
    (Msg: EM_REDO; wParam: 0; lParam: 0),
    (Msg: EM_SETSEL; wParam: 0; lParam: -1),
    (Msg: WM_CUT; wParam: 0; lParam: 0),
    (Msg: WM_COPY; wParam: 0; lParam: 0),
    (Msg: WM_PASTE; wParam: 0; lParam: 0));
begin
  with Msgs[Action] do
    RichEdit.Perform(Msg, wParam, lParam);
  RichEdit.Invalidate;
  UpdateWindow(RichEdit.Handle);
end;

procedure TTextEditor.Resize(Sender: TObject);
begin
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  RichEdit.SetSize(ClientWidth, ClientHeight - ToolBar.Height);
end;

procedure TTextEditor.Save;
var
  Fmt: Integer;
begin
  if Assigned(FNode) and LongBool(RichEdit.Perform(EM_GETMODIFY, 0, 0)) then
  begin
    if GetWindowTextLength(RichEdit.Handle) > 0 then
    begin
      if FNode.Streams.FindItem(SNNodeText) < 0 then
        with FNode.Streams[FNode.Streams.Add(TDataStream.Create)] do
        begin
          Name := SNNodeText;
          MIMEType := FNode.Metadata[SMKNodeTextType, SMTPlainText];
          Compression := FNode.Metadata[SMKNodeTextCompression, ''];
        end;
      with FNode.Streams.ItemByName[SNNodeText] do
      begin
        if SameText(MIMEType, SMTRTFText)
          then Fmt := SF_RTF
          else Fmt := SF_TEXT;
        Data.Size := 0;
        RichEdit.SaveToStream(Data, Fmt);
      end;
    end
      else FNode.Streams.ItemByName[SNNodeText].Free;
    Modify;
  end;
end;

procedure TTextEditor.SetNode(Value: TDataNode);
var
  Fmt: Integer;
begin
  inherited;
  if Assigned(FNode) and (FNode.Streams.FindItem(SNNodeText) >= 0) then
    with FNode.Streams.ItemByName[SNNodeText] do
    begin
      Data.Position := 0;
      if SameText(MIMEType, SMTRTFText)
        then Fmt := SF_RTF
        else Fmt := SF_TEXT;
      RichEdit.LoadFromStream(Data, Fmt);
    end
    else RichEdit.Text := '';
end;

end.