unit ImportExport;

interface

uses
  AvL, DataNode;

type
  EImport = class(Exception) end;

function ImportFile(const FileName: string): TDataNode;
procedure ExportFile(const FileName: string; Data: TDataNode);

const
  SImportFilter = 'Notik files|*.nib|All files|*.*';

implementation

uses
  NotesFile;

const
  SNotEnoughData = 'Not enough data';
  SNotANIBFile = 'Not a NIB file';
  SUnknownFileExtension = 'Unknown file extension';
  NIBMagic = $0142494E;

function ImportNIB(Source: TStream): TDataNode;

  function ReadStr: string;
  var
    StrLen: Integer;
  begin
    if (Source.Read(StrLen, SizeOf(StrLen)) <> SizeOf(StrLen)) or (Source.Size - Source.Position < StrLen) then
      raise EImport.Create(SNotEnoughData);
    SetLength(Result, StrLen);
    Source.Read(Result[1], StrLen);
  end;

  function GetLevel(const S: string): Integer;
  begin
    for Result := 1 to Length(S) do
      if S[Result] <> #09 then Exit;
  end;

var
  i, Level: Integer;
  Magic: Cardinal;
  List: TStringList;
  CurNode: TDataNode;
  Text: string;
begin
  Result := nil;
  Source.ReadBuffer(Magic, SizeOf(Magic));
  if Magic <> NIBMagic then
    raise EImport.Create(SNotANIBFile);
  List := TStringList.Create;
  try
    List.Text := ReadStr;
    Result := TDataNode.Create;
    Result.Metadata[SMKCreated, ''] := List.Values['flCreate'];
    Result.Metadata[SMKAuthor, ''] := List.Values['Author'];
    Result.Metadata[SMKTitle, ''] := List.Values['Name'];
    List.Text := ReadStr;
    CurNode := Result;
    Level := 0;
    for i := 0 to List.Count - 1 do
    begin
      if GetLevel(List[i]) = Level then
        CurNode := CurNode.Parent
      else if GetLevel(List[i]) > Level then
        Level := Level + 1
      else begin
        while GetLevel(List[i]) < Level do
        begin
          CurNode := CurNode.Parent;
          Level := Level - 1;
        end;
        CurNode := CurNode.Parent;
      end;
      CurNode := CurNode.Children[CurNode.Children.Add(TDataNode.Create)];
      CurNode.Name := TrimLeft(List[i]);
      Text := ReadStr;
      if Text <> '' then
        with CurNode.Streams[CurNode.Streams.Add(TDataStream.Create)] do
        begin
          Name := SNNodeText;
          MIMEType := SMTPlainText;
		  //TODO: Apply default compression
          Data.Write(Text[1], Length(Text));
        end;
    end;
  finally
    List.Free;
  end;
end;

function ImportFile(const FileName: string): TDataNode;
var
  Source: TFileStream;
begin
  Result := nil;
  if not FileExists(FileName) then Exit;
  Source := TFileStream.Create(FileName, fmOpenRead);
  try
    if LowerCase(ExtractFileExt(FileName)) = '.nib' then
      Result := ImportNIB(Source)
    else raise EImport.Create(SUnknownFileExtension);
  finally
    Source.Free;
  end;
end;

procedure ExportFile(const FileName: string; Data: TDataNode);
begin

end;

end.