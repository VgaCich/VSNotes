unit ImagesEditor;

interface

uses
  Windows, Messages, CommCtrl, AvL, NotesFile, DataNode, Editor, ImageEx;

type
  TImagesEditor = class(TEditorPage)
    ToolBar: TToolBar;
    ImagesList: TComboBox;
    Image: TImageEx;
  private
    FTBImages: TImageList;
    procedure Resize(Sender: TObject);
    procedure ImagesListChange(Sender: TObject);
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
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
  TTBButtons = (tbPrev, tbNext);

const
  TBButtons: array[TTBButtons] of record Caption: string; ImageIndex: Integer; end = (
    (Caption: 'Previous image'; ImageIndex: 3),
    (Caption: 'Next image'; ImageIndex: 4));

{ TImagesEditor }

constructor TImagesEditor.Create(Parent: TEditor);
var
  Btn: TTBButtons;
begin
  inherited;
  FTBImages := TImageList.Create;
  FTBImages.AddMasked(LoadImage(hInstance, 'TBIMAGESEDITOR', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  ToolBar := TToolBar.Create(Self, true);
  ToolBar.Style := ToolBar.Style or TBSTYLE_TOOLTIPS or CCS_TOP;
  ToolBar.ExStyle := ToolBar.ExStyle or TBSTYLE_EX_MIXEDBUTTONS;
  ToolBar.Perform(TB_SETMAXTEXTROWS, 0, 0);
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  ToolBar.Images := FTBImages;
  ToolBar.Indent := 250;
  for Btn := Low(TTBButtons) to High(TTBButtons) do
    ToolBar.ButtonAdd(TBButtons[Btn].Caption, TBButtons[Btn].ImageIndex);
  ImagesList := TComboBox.Create(ToolBar, csDropDown);
  ImagesList.SetBounds(0, 0, ToolBar.Indent, ToolBar.Height);
  ImagesList.OnChange := ImagesListChange;
  Image := TImageEx.Create(Self);
  Image.SetPosition(0, ToolBar.Height);
  OnResize := Resize;
end;

destructor TImagesEditor.Destroy;
begin
  FreeAndNil(FTBImages);
  inherited;
end;

procedure TImagesEditor.DoEditAction(Action: TEditAction);
begin

end;

procedure TImagesEditor.Resize(Sender: TObject);
begin
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  Image.SetSize(ClientWidth, ClientHeight - ToolBar.Height);
  Image.Invalidate;
  UpdateWindow(Image.Handle);
end;

procedure TImagesEditor.Save;
begin

end;

procedure TImagesEditor.ImagesListChange(Sender: TObject);
var
  Stream: TDataStream;
begin
  if ImagesList.ItemCount = 0 then Exit;
  Stream := FNode.Streams.ItemByName[ImagesList.Items[ImagesList.ItemIndex]];
  if not Assigned(Stream) then Exit;
  Stream.Data.Position := 0;
  Image.LoadFromStream(Stream.Data);
  Resize(Self);
end;

procedure TImagesEditor.SetNode(Value: TDataNode);
var
  i: Integer;
begin
  inherited;
  Image.Clear;
  ImagesList.Clear;
  if not Assigned(FNode) then Exit;
  for i := 0 to FNode.Streams.Count - 1 do
    if SameText(Copy(FNode.Streams[i].MIMEType, 1, 6), 'image/') then
      ImagesList.ItemAdd(FNode.Streams[i].Name);
  ImagesList.ItemIndex := 0;
  ImagesList.OnChange(ImagesList);
end;

procedure TImagesEditor.WMCommand(var Msg: TWMCommand);

  procedure SetImage(Delta: Integer);
  begin
    ImagesList.ItemIndex := Max(0, Min(ImagesList.ItemIndex + Delta, ImagesList.ItemCount - 1));
    ImagesList.OnChange(ImagesList);
  end;

begin
  if Assigned(ToolBar) and (Msg.Ctl = ToolBar.Handle) then
    case TTBButtons(Msg.ItemID) of
      tbPrev: SetImage(-1);
      tbNext: SetImage(1);
    end;
  {if (Msg.Ctl = 0) and (Msg.NotifyCode in [0, 1]) then
    case Msg.ItemID of
      //menu items
    end;}
end;

end.
