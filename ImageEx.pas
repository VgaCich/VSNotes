unit ImageEx;

interface

uses
  Windows, GDIPAPI, AvL, avlIStreamAdapter;

type
  EImageEx = class (Exception) end;
  TImageEx = class(TImage)
  private

  public
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
  Clear;
  if not CheckStatus(GdipCreateHBITMAPFromBitmap(GDIPBitmap, Bitmap, clBlack)) then Exit;
  BitmapHandle := Bitmap;
end;

end.