unit ImagesEditor;

interface

uses
  Windows, Messages, CommCtrl, AvL, NotesFile, DataNode, Editor, ImageEx;

type
  TImagesEditor = class(TEditorPage)
    ToolBar: TToolBar;
    Image: TImageEx;
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
  for Btn := Low(TTBButtons) to High(TTBButtons) do
    ToolBar.ButtonAdd(TBButtons[Btn].Caption, TBButtons[Btn].ImageIndex);
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

procedure TImagesEditor.SetNode(Value: TDataNode);
var
  i: Integer;
begin
  inherited;
  Image.Clear;
  if not Assigned(FNode) then Exit;
  for i := 0 to FNode.Streams.Count - 1 do
    if SameText(Copy(FNode.Streams[i].MIMEType, 1, 6), 'image/') then
    begin
      FNode.Streams[i].Data.Position := 0;
      Image.LoadFromStream(FNode.Streams[i].Data);
      Image.Width := 1;
      Resize(Self);
      Exit;
    end;
end;

end.