unit Editor;

interface

uses
  Windows, Messages, AvL, NotesFile, DataNode;

type
  TEditAction = (eaUndo, eaRedo, eaSelectAll, eaCut, eaCopy, eaPaste);
  TPages = (pText, pImages, pAttaches);
  TEditor = class;
  TEditorPage = class(TSimplePanel)
  protected
    FNode: TDataNode;
    FEditor: TEditor;
    procedure SetNode(Value: TDataNode); virtual;
    procedure Modify;
  public
    constructor Create(Parent: TEditor); virtual;
    procedure Save; virtual; abstract;
    procedure DoEditAction(Action: TEditAction); virtual; abstract; 
    property Node: TDataNode read FNode write SetNode;
  end;
  TEditor = class(TSimplePanel)
    Tabs: TTabControl;
    Pages: array[TPages] of TEditorPage;
  private
    FOnModify: TOnEvent;
    FNode: TDataNode;
    FCurPage: TEditorPage;
    procedure Modify;
    procedure Resize(Sender: TObject);
    procedure SetNode(const Value: TDataNode);
    procedure TabChange(Sender: TObject);
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
  public
    constructor Create(AParent: TWinControl);
    procedure Save;
    procedure DoEditAction(Action: TEditAction);
    property Node: TDataNode read FNode write SetNode;
    property OnModify: TOnEvent read FOnModify write FOnModify;
  end;

implementation

uses
  TextEditor, ImagesEditor, StreamsEditor;

type
  TEditorPageClass = class of TEditorPage;

const
  TabNames: array[TPages] of string = ('Text', 'Images', 'Attachments');
  EditorPages: array[TPages] of TEditorPageClass = (TTextEditor, TImagesEditor, TStreamsEditor);

{ TEditor }

constructor TEditor.Create(AParent: TWinControl);
var
  Page: TPages;
begin
  inherited Create(AParent, '');
  Border := 2;
  ExStyle := 0;
  Tabs := TTabControl.Create(Self);
  Tabs.Style := tsTabs;
  //Tabs.TabPosition := tpBottom;
  Tabs.SetPosition(0, 0);
  for Page := Low(TPages) to High(TPages) do
  begin
    Pages[Page] := EditorPages[Page].Create(Self);
    Pages[Page].BringToFront;
    Pages[Page].Visible := false;
    Tabs.TabAdd(TabNames[Page]);
  end;
  FCurPage := Pages[Low(TPages)];
  FCurPage.Visible := true;
  Tabs.TabIndex := 0;
  OnResize := Resize;
end;

procedure TEditor.DoEditAction(Action: TEditAction);
begin
  FCurPage.DoEditAction(Action);
end;

procedure TEditor.Modify;
begin
  if Assigned(FOnModify) then
    FOnModify(Self);
end;

procedure TEditor.Resize(Sender: TObject);
var
  Rect: TRect;
  Page: TPages;
begin
  Tabs.SetSize(ClientWidth, ClientHeight);
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := Tabs.ClientWidth;
  Rect.Bottom := Tabs.ClientHeight;
  Tabs.Perform(TCM_ADJUSTRECT, 0, Integer(@Rect));
  for Page := Low(TPages) to High(TPages) do
    Pages[Page].SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

procedure TEditor.Save;
begin
  FCurPage.Save;
end;

procedure TEditor.SetNode(const Value: TDataNode);
begin
  FNode := Value;
  FCurPage.Node := FNode;
end;

procedure TEditor.TabChange(Sender: TObject);
begin
  FCurPage.Save;
  FCurPage.Visible := false;
  FCurPage := Pages[TPages(Tabs.TabIndex)];
  FCurPage.Node := FNode;
  FCurPage.Visible := true;
end;

procedure TEditor.WMNotify(var Msg: TWMNotify);
begin
  if Assigned(Tabs) and (Msg.NMHdr.hwndFrom = Tabs.Handle) and (Msg.NMHdr.code = TCN_SELCHANGE) then
    TabChange(Tabs);
end;

{ TEditorPage }

constructor TEditorPage.Create(Parent: TEditor);
begin
  inherited Create(Parent, '');
  FEditor := Parent;
  Border := 2;
  ExStyle := 0;
end;

procedure TEditorPage.SetNode(Value: TDataNode);
begin
  FNode := Value;
end;

procedure TEditorPage.Modify;
begin
  FEditor.Modify;
end;

end.