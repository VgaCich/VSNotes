unit NotesFile;

interface

uses
  AvL, avlUtils, DataNode, NodeTag;

type
  ENotesFile = class(Exception) end;
  TNotesFile = class
  private
    FRootNode: TDataNode;
    FModified: Boolean;
    FMakeBackup: Boolean;
    FName: string;
    procedure UpdateNode(Tag: TNodeTag);
    procedure UpdateTag(Tag: TNodeTag);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Modify;
    procedure Load; overload;
    procedure Load(Source: TStream); overload;
    procedure Save; overload;
    procedure Save(Dest: TStream); overload;
    property RootNode: TDataNode read FRootNode;
    property Modified: Boolean read FModified;
    property Name: string read FName write FName;
    property MakeBackup: Boolean read FMakeBackup write FMakeBackup;
  end;

function IsVSNFile(const FileName: string): Boolean;
function GetMIMEType(const Ext: string): string;

const
  //Special stream names
  SNNodeText = ':nodetext';
  //Metadata keys
  SMKTitle = 'Title';
  SMKAuthor = 'Author';
  SMKCreated = 'Created';
  SMKModified = 'Modified';
  SMKNodeTextType = ':TextMIMEType';
  SMKNodeTextCompression = ':TextCompression';
  SMKNodeDefaultCompression = ':DefaultCompression';
  SMKNodeExpanded = ':Expanded';
  SMKNodeSelected = ':Selected';
  //MIME types
  SMTPlainText = 'text/plain';
  SMTRTFText = 'text/rtf';
  SMTOctetStream = 'application/octet-stream';
  MIMETypes: array[0..2] of string = (
    SMTOctetStream,
    SMTPlainText,
    SMTRTFText);

implementation

const
  SNoFileNameSpecified = 'No file name specified';
  SFileDoesntExists = 'File "%s" doesn''t exists';
  SInvalidFile = 'Invalid file';
  FileMagic: Cardinal = $314E5356;
  KnownExtensions: array[0..1] of record Ext, MIMEType: string end = (
    (Ext: '.txt'; MIMEType: SMTPlainText),
    (Ext: '.rtf'; MIMEType: SMTRTFText));

function IsVSNFile(const FileName: string): Boolean;
var
  F: TFileStream;
  Magic: Cardinal;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := (F.Read(Magic, SizeOf(Magic)) = SizeOf(Magic)) and (Magic = FileMagic);
  finally
    F.Free;
  end;
end;

function GetMIMEType(const Ext: string): string;
var
  i: Integer;
begin
  //TODO: Load known extensions from settings or smtx
  Result := SMTOctetStream;
  for i := 0 to High(KnownExtensions) do
    if SameText(Ext, KnownExtensions[i].Ext) then
    begin
      Result := KnownExtensions[i].MIMEType;
      Exit;
    end;
end;

{ TNotesFile }

constructor TNotesFile.Create;
begin
  FRootNode := TDataNode.Create;
  FRootNode.Metadata[SMKCreated, ''] := DateTimeToStr(Now);
end;

destructor TNotesFile.Destroy;
begin
  FreeAndNil(FRootNode);
  inherited;
end;

procedure TNotesFile.Load(Source: TStream);
var
  Magic: Cardinal;
begin
  if not Assigned(Source) then Exit;
  Source.Position := 0;
  if (Source.Read(Magic, SizeOf(Magic)) <> SizeOf(Magic)) or (Magic <> FileMagic) then
    raise ENotesFile.Create(SInvalidFile);
  FRootNode.Load(Source);
  IterateTags(UpdateTag);
  FModified := false;
end;

procedure TNotesFile.Load;
var
  F: TFileStream;
begin
  if not FileExists(FName) then
    raise ENotesFile.CreateFmt(SFileDoesntExists, [FName]);
  F := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
  try
    Load(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure TNotesFile.Modify;
begin
  FModified := true;
end;

procedure TNotesFile.Save(Dest: TStream);
begin
  if not Assigned(Dest) then Exit;
  FRootNode.Metadata[SMKModified, ''] := DateTimeToStr(Now);
  IterateTags(UpdateNode);
  Dest.Position := 0;
  Dest.WriteBuffer(FileMagic, SizeOf(FileMagic));
  FRootNode.Save(Dest);
  if Dest.Size <> Dest.Position then
    Dest.Size := Dest.Position;
  FModified := false;
end;

procedure TNotesFile.Save;
var
  F: TFileStream;
begin
  if FName = '' then
    raise ENotesFile.Create(SNoFileNameSpecified);
  if not DirectoryExists(ExtractFilePath(FName)) then
    ForceDirectories(ExtractFilePath(FName));
  if FileExists(FName) and FMakeBackup then
  begin
    DeleteFile(ChangeFileExt(FName, '.bak'));
    RenameFile(FName, ChangeFileExt(FName, '.bak'));
  end;
  F := TFileStream.Create(FName, fmOpenWrite or fmCreate or fmShareDenyWrite);
  try
    Save(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure TNotesFile.UpdateNode(Tag: TNodeTag);
const
  BoolValue: array[Boolean] of string = ('', '1');
begin
  Tag.Node.RawMetadata.Values[SMKNodeExpanded] := BoolValue[Tag.TreeItemExpanded];
  Tag.Node.RawMetadata.Values[SMKNodeSelected] := BoolValue[Tag.Selected];
end;

procedure TNotesFile.UpdateTag(Tag: TNodeTag);
begin
  if Tag.Node.RawMetadata.IndexOfName(SMKNodeExpanded) >= 0 then
    Tag.TreeItemExpanded := Boolean(StrToInt(Tag.Node.RawMetadata.Values[SMKNodeExpanded]));
  if Tag.Node.RawMetadata.IndexOfName(SMKNodeSelected) >= 0 then
    Tag.Selected := Boolean(StrToInt(Tag.Node.RawMetadata.Values[SMKNodeSelected]));
end;

end.