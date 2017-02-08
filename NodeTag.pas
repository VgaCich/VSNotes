unit NodeTag;

interface

uses
  AvL, DataNode;

type
  TNodeTag = class
  private
    FNode: TDataNode;
    FNext, FPrev: TNodeTag;
    FSelected: Boolean;
    procedure ClearSelected(Tag: TNodeTag);
    procedure SetSelected(Value: Boolean);
  public
    TreeItem: Integer;
    TreeItemExpanded: Boolean;
    constructor Create(Node: TDataNode);
    destructor Destroy; override;
    property Node: TDataNode read FNode;
    property Prev: TNodeTag read FPrev;
    property Next: TNodeTag read FNext;
    property Selected: Boolean read FSelected write SetSelected;
  end;
  TOnTag = procedure(Tag: TNodeTag) of object;

procedure IterateTags(OnTag: TOnTag);

implementation

var
  RootTag: TNodeTag;

procedure IterateTags(OnTag: TOnTag);
var
  CurTag: TNodeTag;
begin
  if not Assigned(OnTag) or not Assigned(RootTag) then Exit;
  CurTag := RootTag;
  repeat
    OnTag(CurTag);
    CurTag := CurTag.Next;
  until CurTag = RootTag;
end;

procedure TagNode(Node: TDataNode);
begin
  Node.Tag := TNodeTag.Create(Node);
end;

{ TNodeTag }

procedure TNodeTag.ClearSelected(Tag: TNodeTag);
begin
  Tag.FSelected := false;
end;

constructor TNodeTag.Create(Node: TDataNode);
begin
  inherited Create;
  FNode := Node;
  if Assigned(RootTag) then
  begin
    FNext := RootTag.FNext;
    FPrev := RootTag;
    RootTag.FNext := Self;
    FNext.FPrev := Self;
  end
  else begin
    FNext := Self;
    FPrev := Self;
    RootTag := Self;
  end;
end;

destructor TNodeTag.Destroy;
begin
  if Assigned(FPrev) and Assigned(FNext) then
  begin
    FPrev.FNext := FNext;
    FNext.FPrev := FPrev;
  end;
  if FPrev = Self then
    FPrev := nil;
  if RootTag = Self then
    RootTag := FPrev;
  inherited;
end;

procedure TNodeTag.SetSelected(Value: Boolean);
begin
  if Value then
    IterateTags(ClearSelected);
  FSelected := Value;
end;

initialization

  DataNode.TagNodeOnCreate := TagNode;

end.