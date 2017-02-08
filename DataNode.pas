unit DataNode;

//TODO: Check and complain too long names and metadatas
//TODO: Fix errors handling, try to read/save as much as possible, don't corrupt file structure on save

interface

uses
  AvL, avlCRC32, avlNRV2EStream;

type
  TDataCompression = (dcNone, dcNRV2E1, dcNRV2E2, dcNRV2E3, dcNRV2E4, dcNRV2E5, dcNRV2E6, dcNRV2E7, dcNRV2E8, dcNRV2E9, dcNRV2E10);
  TOnNodeSave = function(Sender: TObject; Dest: TStream): Boolean;
  EDataNode = class(Exception) end;
  EDataStream = class(Exception) end;
  TDataStream = class;
  TDataNode = class;
  TCustomDataItem = class
  private
    FParent: TDataNode;
    procedure SetParent(const Value: TDataNode);
  protected
    FName: string;
  public
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Parent: TDataNode read FParent write SetParent;
  end;
  TCustomDataList = class
  private
    FLocked: Boolean;
    function GetCount: Integer;
  protected
    FItems: TList;
    FParent: TDataNode;
    procedure SetItem(Index: Integer; Item: TCustomDataItem);
  public
    constructor Create(Parent: TDataNode);
    destructor Destroy; override;
    procedure Clear;
    function Add(Item: TCustomDataItem): Integer;
    procedure Insert(Index: Integer; Item: TCustomDataItem);
    procedure Delete(Index: Integer); overload;
    procedure Delete(Item: TCustomDataItem); overload;
    function IndexOf(Item: TCustomDataItem): Integer;
    function FindItem(const Name: string; Start: Integer = 0): Integer;
    procedure Swap(Index1, Index2: Integer);
    property Count: Integer read GetCount;
  end;
  TDataNodeList = class(TCustomDataList)
  private
    function GetItem(Index: Integer): TDataNode;
    function GetItemByName(const Name: string): TDataNode;
    procedure SetItem(Index: Integer; Item: TDataNode);
  public
    property Items[Index: Integer]: TDataNode read GetItem write SetItem; default;
    property ItemByName[const Name: string]: TDataNode read GetItemByName;
  end;
  TDataStreamList = class(TCustomDataList)
  private
    function GetItem(Index: Integer): TDataStream;
    function GetItemByName(const Name: string): TDataStream;
    procedure SetItem(Index: Integer; Item: TDataStream);
  public
    property Items[Index: Integer]: TDataStream read GetItem write SetItem; default;
    property ItemByName[const Name: string]: TDataStream read GetItemByName;
  end;
  TDataStream = class(TCustomDataItem)
  private
    FData: TMemoryStream;
    FNRV2EStream: TNRV2EStream;
    FMIMEType: string;
    FCompression: TDataCompression;
    function GetAvailableCompressors: TStringList;
    function GetCompression: string;
    function GetData: TStream;
    function GetNRV2ELevel(Compression: TDataCompression): Integer;
    function IsNRV2E(Compression: TDataCompression): Boolean;
    procedure SetCompression(Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(Source: TStream);
    procedure Save(Dest: TStream);
    property Data: TStream read GetData;
    property MIMEType: string read FMIMEType write FMIMEType;
    property Compression: string read GetCompression write SetCompression;
    property AvailableCompressors: TStringList read GetAvailableCompressors;
  end;
  TDataNode = class(TCustomDataItem)
  private
    FChildren: TDataNodeList;
    FStreams: TDataStreamList;
    FMetadata: TStringList;
    FOnSave: TOnNodeSave;
    FTag: TObject;
    procedure Remove(Item: TCustomDataItem);
    function GetMetadata(const Key, Def: string): string;
    procedure SetMetadata(const Key, Def, Value: string);
    procedure SetTag(Value: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(Source: TStream);
    procedure Save(Dest: TStream);
    procedure Clear(ClearMetadata: Boolean = true);
    property Children: TDataNodeList read FChildren;
    property Streams: TDataStreamList read FStreams;
    property RawMetadata: TStringList read FMetadata;
    property Metadata[const Key, Def: string]: string read GetMetadata write SetMetadata;
    property Tag: TObject read FTag write SetTag;
    property OnSave: TOnNodeSave read FOnSave write FOnSave;
  end;

var
  TagNodeOnCreate: procedure(Node: TDataNode) = nil;

implementation

var
  Compressors: TStringList;

const
  SStreamNotEnoughData = 'Not enough stream data';
  SStreamCRC32Mismatch = 'Stream CRC32 mismatch';
  SNodeSizeMismatch = 'Node size mismatch';
  SNodeNotEnoughData = 'Not enough node data';
  ComprNames: array[TDataCompression] of string =
    ('None', 'NRV2E-1', 'NRV2E-2', 'NRV2E-3', 'NRV2E-4', 'NRV2E-5', 'NRV2E-6', 'NRV2E-7', 'NRV2E-8', 'NRV2E-9', 'NRV2E-10');

type
  TStreamHeader = packed record
    DataSize: Integer;
    DataCRC32: LongWord;
    Compression: TDataCompression;
    NameLen, MIMETypeLen: Byte;
  end;
  TNodeHeader = packed record
    NodeSize: Integer;
    StreamsCount: Word;
    NameLen: Byte;
    MetadataLen: Word;
  end;

function StrToCompr(const S: string): TDataCompression;
begin
  for Result := Low(TDataCompression) to High(TDataCompression) do
    if SameText(ComprNames[Result], S) then Exit;
  Result := dcNone;
end;

function ComprToStr(C: TDataCompression): string;
begin
  Result := ComprNames[C];
end;

function Left(Stream: TStream): Integer;
begin
  Result := Stream.Size - Stream.Position;
end;

{ TCustomDataList }

function TCustomDataList.Add(Item: TCustomDataItem): Integer;
begin
  Result := FItems.IndexOf(Item);
  if not FLocked and ((Result < 0) or not Assigned(Item)) then
  begin
    Result := FItems.Add(Item);
    if Assigned(Item) then
      Item.Parent := FParent;
  end;
end;

procedure TCustomDataList.Clear;
var
  i: Integer;
begin
  FLocked := true;
  for i := 0 to FItems.Count - 1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FLocked := false;
end;

constructor TCustomDataList.Create(Parent: TDataNode);
begin
  FParent := Parent;
  FItems := TList.Create;
end;

procedure TCustomDataList.Delete(Index: Integer);
begin
  if not FLocked then
    FItems.Delete(Index);
end;

procedure TCustomDataList.Delete(Item: TCustomDataItem);
begin
  if not FLocked then
    FItems.Remove(Item);
end;

destructor TCustomDataList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TCustomDataList.FindItem(const Name: string; Start: Integer): Integer;
begin
  for Result := Start to FItems.Count - 1 do
    if SameText(TCustomDataItem(FItems[Result]).Name, Name) then
      Exit;
  Result := -1;
end;

function TCustomDataList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomDataList.IndexOf(Item: TCustomDataItem): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

procedure TCustomDataList.Insert(Index: Integer; Item: TCustomDataItem);
begin
  if not FLocked and ((FItems.IndexOf(Item) < 0) or not Assigned(Item)) then
  begin
    FItems.Insert(Index, Item);
    if Assigned(Item) then
      Item.Parent := FParent;
  end;
end;

procedure TCustomDataList.SetItem(Index: Integer; Item: TCustomDataItem);
begin
  if (FItems.IndexOf(Item) < 0) or not Assigned(Item) then
  begin
    FItems[Index] := Item;
    if Assigned(Item) then
      Item.Parent := FParent;
  end;
end;

procedure TCustomDataList.Swap(Index1, Index2: Integer);
var
  Temp: Pointer;
begin
  Temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Temp;
end;

{ TDataNodeList }

function TDataNodeList.GetItem(Index: Integer): TDataNode;
begin
  Result := TDataNode(FItems[Index]);
end;

function TDataNodeList.GetItemByName(const Name: string): TDataNode;
begin
  Result := TDataNode(FItems[FindItem(Name)]);
end;

procedure TDataNodeList.SetItem(Index: Integer; Item: TDataNode);
begin
  inherited SetItem(Index, Item);
end;

{ TDataStreamList }

function TDataStreamList.GetItem(Index: Integer): TDataStream;
begin
  Result := TDataStream(FItems[Index]);
end;

function TDataStreamList.GetItemByName(const Name: string): TDataStream;
begin
  Result := TDataStream(FItems[FindItem(Name)]);
end;

procedure TDataStreamList.SetItem(Index: Integer; Item: TDataStream);
begin
  inherited SetItem(Index, Item);
end;

{ TCustomDataItem }

destructor TCustomDataItem.Destroy;
begin
  if Assigned(FParent) then
    FParent.Remove(Self);
  inherited;
end;

procedure TCustomDataItem.SetParent(const Value: TDataNode);
begin
  if Assigned(FParent) and (FParent <> Value) then
    FParent.Remove(Self);
  FParent := Value;
end;

{ TDataStream }

constructor TDataStream.Create;
begin
  inherited Create;
  FCompression := dcNone;
  FData := TMemoryStream.Create;
end;

destructor TDataStream.Destroy;
begin
  FreeAndNil(FNRV2EStream);
  FreeAndNil(FData);
  inherited;
end;

function TDataStream.GetAvailableCompressors: TStringList;
begin
  Result := Compressors;
end;

function TDataStream.GetCompression: string;
begin
  Result := ComprToStr(FCompression);
end;

function TDataStream.GetData: TStream;
begin
  if IsNRV2E(FCompression) then
    Result := FNRV2EStream
  else Result := FData;
end;

function TDataStream.GetNRV2ELevel(Compression: TDataCompression): Integer;
begin
  if IsNRV2E(Compression) then
    Result := Ord(Compression) - Ord(dcNRV2E1) + 1
  else Result := 0;
end;

function TDataStream.IsNRV2E(Compression: TDataCompression): Boolean;
begin
  Result := (Compression >= dcNRV2E1) and (Compression <= dcNRV2E10);
end;

procedure TDataStream.Load(Source: TStream);
var
  Header: TStreamHeader;
begin
  if not Assigned(Source) then Exit;
  if Left(Source) < SizeOf(Header) then
    raise EDataStream.Create(SStreamNotEnoughData);
  Source.ReadBuffer(Header, SizeOf(Header));
  if Left(Source) < Header.DataSize + Header.NameLen + Header.MIMETypeLen then
    raise EDataStream.Create(SStreamNotEnoughData);
  SetLength(FName, Header.NameLen);
  SetLength(FMIMEType, Header.MIMETypeLen);
  Source.ReadBuffer(FName[1], Header.NameLen);
  Source.ReadBuffer(FMIMEType[1], Header.MIMETypeLen);
  FreeAndNil(FNRV2EStream);
  FData.Size := Header.DataSize;
  Source.ReadBuffer(FData.Memory^, Header.DataSize);
  if CRC32Done(CRC32Next(CRC32Init, FData.Memory^, FData.Size)) <> Header.DataCRC32 then
    raise EDataStream.Create(SStreamCRC32Mismatch);
  FCompression := Header.Compression; //TODO: Check and complain unknown compression
  if IsNRV2E(Header.Compression) then
  begin
    FNRV2EStream := TNRV2EStream.Create(FData);
    FNRV2EStream.CompressionLevel := GetNRV2ELevel(Header.Compression);
  end;
end;

procedure TDataStream.Save(Dest: TStream);
var
  Header: TStreamHeader;
begin
  if Assigned(FNRV2EStream) then
    FNRV2EStream.Flush;
  with Header do
  begin
    DataSize := FData.Size;
    DataCRC32 := CRC32Done(CRC32Next(CRC32Init, FData.Memory^, FData.Size));
    Compression := FCompression;
    NameLen := Length(FName);
    MIMETypeLen := Length(FMIMEType);
  end;
  Dest.WriteBuffer(Header, SizeOf(Header));
  Dest.WriteBuffer(FName[1], Header.NameLen);
  Dest.WriteBuffer(FMIMEType[1], Header.MIMETypeLen);
  Dest.WriteBuffer(FData.Memory^, FData.Size);
end;

procedure TDataStream.SetCompression(Value: string);
var
  Compr: TDataCompression;
  TempData: TMemoryStream;
  TempNRV2E: TNRV2EStream;
begin
  Compr := StrToCompr(Value);
  if Compr = FCompression then Exit;
  if IsNRV2E(Compr) then
  begin
    if not IsNRV2E(FCompression) then
    begin
      TempData := TMemoryStream.Create;
      TempNRV2E := TNRV2EStream.Create(TempData);
      TempNRV2E.CompressionLevel := GetNRV2ELevel(Compr);
      TempNRV2E.CopyFrom(Data, 0);
      TempNRV2E.Flush;
      FNRV2EStream.Free;
      FNRV2EStream := TempNRV2E;
      FData.Free;
      FData := TempData;
    end
      else FNRV2EStream.CompressionLevel := GetNRV2ELevel(Compr);
  end
  else begin
    TempData := TMemoryStream.Create;
    TempData.Size := Data.Size;
    Data.Position := 0;
    Data.ReadBuffer(TempData.Memory^, TempData.Size);
    FreeAndNil(FNRV2EStream);
    FData.Free;
    FData := TempData;
  end;
  FCompression := Compr;
end;

{ TDataNode }

procedure TDataNode.Clear(ClearMetadata: Boolean);
begin
  FStreams.Clear;
  FChildren.Clear;
  if ClearMetadata then
    FMetadata.Clear;
end;

constructor TDataNode.Create;
begin
  inherited;
  FChildren := TDataNodeList.Create(Self);
  FStreams := TDataStreamList.Create(Self);
  FMetadata := TStringList.Create;
  if Assigned(TagNodeOnCreate) then
    TagNodeOnCreate(Self);
end;

destructor TDataNode.Destroy;
begin
  FreeAndNil(FStreams);
  FreeAndNil(FChildren);
  FreeAndNil(FMetadata);
  FreeAndNil(FTag);
  inherited;
end;

function TDataNode.GetMetadata(const Key, Def: string): string;
var
  Node: TDataNode;
begin
  Node := Self;
  while Assigned(Node) do
  begin
    if Node.RawMetadata.IndexOfName(Key) >= 0 then
    begin
      Result := Node.RawMetadata.Values[Key];
      Exit;
    end;
    Node := Node.Parent;
  end;
  Result := Def;
end;

procedure TDataNode.Load(Source: TStream);
var
  Header: TNodeHeader;
  NodeEnd, i: Integer;
  Metadata: string;
begin
  if not Assigned(Source) then Exit;
  Clear;
  if Left(Source) < SizeOf(Header) then
    raise EDataNode.Create(SNodeNotEnoughData);
  Source.ReadBuffer(Header, SizeOf(Header));
  if Left(Source) < Header.NodeSize - SizeOf(Header) then
    raise EDataNode.Create(SNodeNotEnoughData);
  NodeEnd := Source.Position - SizeOf(Header) + Header.NodeSize;
  try
    SetLength(FName, Header.NameLen);
    Source.ReadBuffer(FName[1], Header.NameLen);
    SetLength(Metadata, Header.MetadataLen);
    Source.ReadBuffer(Metadata[1], Header.MetadataLen);
    FMetadata.Text := Metadata;
    for i := 0 to Header.StreamsCount - 1 do
      Streams[Streams.Add(TDataStream.Create)].Load(Source);
    while Source.Position < NodeEnd do
      Children[Children.Add(TDataNode.Create)].Load(Source);
  finally
    if Source.Position <> NodeEnd then
    begin
      Source.Position := NodeEnd;
      raise EDataNode.Create(SNodeSizeMismatch);
    end;
  end;
end;

procedure TDataNode.Remove(Item: TCustomDataItem);
begin
  if (Item is TDataStream) and Assigned(FStreams) then
    FStreams.Delete(Item)
  else if (Item is TDataNode) and Assigned(FChildren) then
    FChildren.Delete(Item);
end;

procedure TDataNode.Save(Dest: TStream);
var
  Header: TNodeHeader;
  NodeStart, i: Integer;
begin
  if not Assigned(Dest) then Exit;
  if Assigned(FOnSave) and not FOnSave(Self, Dest) then Exit;
  NodeStart := Dest.Position;
  Header.StreamsCount := Streams.Count;
  Header.NameLen := Length(FName);
  Header.MetadataLen := Length(FMetadata.Text);
  Dest.WriteBuffer(Header, SizeOf(Header));
  Dest.WriteBuffer(FName[1], Header.NameLen);
  Dest.WriteBuffer(FMetadata.Text[1], Header.MetadataLen);
  for i := 0 to Header.StreamsCount - 1 do
    Streams[i].Save(Dest);
  for i := 0 to Children.Count - 1 do
    Children[i].Save(Dest);
  Header.NodeSize := Dest.Position - NodeStart;
  Dest.Position := NodeStart;
  Dest.WriteBuffer(Header, SizeOf(Header));
  Dest.Seek(0, soFromEnd);
end;

procedure TDataNode.SetMetadata(const Key, Def, Value: string);
begin
  RawMetadata.Values[Key] := Value;
end;

procedure TDataNode.SetTag(Value: TObject);
begin
  if Value = FTag then Exit;
  FTag.Free;
  FTag := Value;
end;

var
  Compr: TDataCompression;

initialization
  CRC32Initialization;
  Compressors := TStringList.Create;
  for Compr := Low(TDataCompression) to High(TDataCompression)do
    Compressors.Add(ComprToStr(Compr));

finalization
  FreeAndNil(Compressors);  

end.
