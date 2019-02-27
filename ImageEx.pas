unit ImageEx;

interface

uses
  Windows, GDIPAPI, AvL, avlIStreamAdapter;

type
  EImageEx = class (Exception) end;
  TImageEx = class(TImage)
  private
    FImageWidth, FImageHeight: Cardinal;
    procedure Resize(Sender: TObject);
  public
    constructor Create(Parent: TWinControl);
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
  end;

implementation

const
  SGDIPError = 'GDI+ Error (%d)';

function CheckStatus(Status: TStatus): Boolean;
begin
  Result := Status = Ok;
  if not Result then
    raise EImageEx.CreateFmt(SGDIPError, [Integer(Status)]);
end;

{ TImageEx }

constructor TImageEx.Create(Parent: TWinControl);
begin
  inherited;
  Style := Style or SS_REALSIZEIMAGE or SS_CENTERIMAGE;
  OnResize := Resize;
end;

procedure TImageEx.Clear;
begin
  if BitmapHandle <> 0 then
  begin
    DeleteObject(BitmapHandle);
    BitmapHandle := 0;
  end;
end;

procedure TImageEx.LoadFromStream(Stream: TStream);
var
  StreamAdapter: IStream;
  GDIPBitmap: Pointer;
  Bitmap: HBitmap;
begin
  TIStreamAdapter.Create(Stream).GetInterface(IStream, StreamAdapter);
  GDIPBitmap := nil;
  if not CheckStatus(GdipCreateBitmapFromStream(StreamAdapter, GDIPBitmap)) then Exit;
  try
    Clear;
    if not CheckStatus(GdipCreateHBITMAPFromBitmap(GDIPBitmap, Bitmap, clBlack)) then Exit;
    GdipGetImageWidth(GDIPBitmap, FImageWidth);
    GdipGetImageHeight(GDIPBitmap, FImageHeight);
    BitmapHandle := Bitmap;
    SetSize(FImageWidth, FImageHeight);
  finally
    GdipDisposeImage(GDIPBitmap);
  end;
end;

procedure TImageEx.Resize(Sender: TObject);
var
  ScrollInfo: TScrollInfo;
begin
  with ScrollInfo do
  begin
    cbSize := SizeOf(ScrollInfo);
    fMask := SIF_PAGE or SIF_RANGE;
    nMin := 0;
    nMax := FImageHeight;
    nPage := ClientHeight;
  end;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, true);
  with ScrollInfo do
  begin
    nMax := FImageWidth;
    nPage := ClientWidth;
  end;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, true);
  ScrollWindowEx(Handle, FImageWidth - ClientWidth, FImageHeight - ClientHeight, nil, nil, 0, nil, 0);
end;

end.