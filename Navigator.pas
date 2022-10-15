unit Navigator;

//TODO: Undo list
//TODO: Clipboard
//TODO: Drag'n'Drop: nodes expanding, scrolling, reordering by dnd
//TODO: Insertion/Importing and Exporting of hives
//TODO: Mass operations on nodes (setting compression for all (or selected - as :nodetext) streams of all selected nodes, for example)
//TODO: Collapsing on '/' button

interface

uses
  Windows, CommCtrl, Messages, AvL, avlNatCompare, avlTreeViewEx, DataNode, NodeTag;

type
  TCompFunc = function(const S1, S2: string): Integer;
  TNavigator = class(TSimplePanel)
    ToolBar: TToolBar;
    NodeTree: TTreeViewEx;
    NaviMenu, SortMenu: TMenu;
  private
    FOnModify: TOnEvent;
    FOnNodeSelected: TOnEvent;
    FSortRecursive: Boolean;
    FTBImages: TImageList;
    FNodeImages: TImageList;
    FRootNode: TDataNode;
    FDragNode, FMenuNode: Integer;
    FDragImage: HImageList;
    function AddNode(Parent, After: Integer; Node: TDataNode): Integer;
    procedure DeleteNode(Node: Integer);
    function GetSelNode: TDataNode;
    function NewNode(Parent: Integer; InsertAfter: Integer = Integer(TVI_LAST)): Integer;
    procedure NodeProperties(Node: Integer);
    procedure NodeSort(Node: TDataNode; Recursive, Ascending: Boolean; CompFunc: TCompFunc);
    procedure NodeTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NodeTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure NodeTreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NodeTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RefreshNode(Node: Integer);
    procedure Resize(Sender: TObject);
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    procedure SetRootNode(const Value: TDataNode);
    procedure SortNode(Node: Integer; Recursive, Ascending: Boolean; CompFunc: TCompFunc);
    procedure SwapNodes(Node1, Node2: Integer);
    procedure ToggleNode(Node: Integer; Mode: TExpandMode = emToggle);
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    procedure UpdateTree;
    procedure Clear;
    property RootNode: TDataNode read FRootNode write SetRootNode;
    property SelNode: TDataNode read GetSelNode;
    property OnModify: TOnEvent read FOnModify write FOnModify;
    property OnNodeSelected: TOnEvent read FOnNodeSelected write FOnNodeSelected;
  end;

implementation

uses
  MainForm;

type
  TTBButtons = (tbAdd, tbAddChild, tbProperties, tbMoveUp, tbMoveDown, tbDelete, tbSort, tbToggle);

const
  SConfirmDelete = 'Delete node "%s"?';
  TBButtons: array[TTBButtons] of record Caption: string; ImageIndex: Integer; end = (
    (Caption: 'Add'; ImageIndex: 0),
    (Caption: 'Add child'; ImageIndex: 1),
    (Caption: 'Properties'; ImageIndex: 2),
    (Caption: 'Move up'; ImageIndex: 3),
    (Caption: 'Move down'; ImageIndex: 4),
    (Caption: 'Delete'; ImageIndex: 5),
    (Caption: 'Sort'; ImageIndex: 6),
    (Caption: 'Toggle'; ImageIndex: 8));
  IDRefresh = 9001;
  IDAdd = IDRefresh + 2;
  IDAddChild = IDAdd + 1;
  IDRename = IDAddChild + 1;
  IDProperties = IDRename + 1;
  IDDelete = IDProperties + 1;
  IDToggle = IDDelete + 1;
  IDSort = IDToggle + 1;
  MenuNavi: array[0..8] of PChar = ('9001',
    'Refresh'#9'F5',
    '-',
    'Add'#9'F7',
    'Add child'#9'F8',
    'Rename'#9'F2',
    'Properties'#9'F3',
    'Delete'#9'Del',
    'Toggle');
  IDSortAlphaAsc = 10001;
  IDSortAlphaDesc = IDSortAlphaAsc + 1;
  IDSortNatAsc = IDSortAlphaDesc + 1;
  IDSortNatDesc = IDSortNatAsc + 1;
  IDSortRecursive = IDSortNatDesc + 2;
  MenuSort: array[0..6] of PChar = ('10001',
    'Alphabetical ascending',
    'Alphabetical descending',
    'Natural ascending',
    'Natural descending',
    '-',
    'Recursive');

{ TNavigator }

constructor TNavigator.Create(AParent: TWinControl);
var
  Btn: TTBButtons;
begin
  inherited Create(AParent, '');
  Border := 2;
  ExStyle := 0;
  FTBImages := TImageList.Create;
  FTBImages.AddMasked(LoadImage(hInstance, 'TBNAVI', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  FNodeImages := TImageList.Create;
  FNodeImages.AddMasked(LoadImage(hInstance, 'TVNODES', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  ToolBar := TToolBar.Create(Self, true);
  ToolBar.Style := ToolBar.Style or TBSTYLE_TOOLTIPS or CCS_TOP;
  ToolBar.ExStyle := ToolBar.ExStyle or TBSTYLE_EX_MIXEDBUTTONS;
  ToolBar.Perform(TB_SETMAXTEXTROWS, 0, 0);
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  ToolBar.Images := FTBImages;
  for Btn := Low(TTBButtons) to High(TTBButtons) do
    ToolBar.ButtonAdd(TBButtons[Btn].Caption, TBButtons[Btn].ImageIndex);
  NodeTree := TTreeViewEx.Create(Self);
  NodeTree.Style := NodeTree.Style or TVS_EDITLABELS or TVS_SHOWSELALWAYS;
  //NodeTree.CanvasInit;
  NodeTree.Images := FNodeImages;
  NodeTree.SetPosition(0, ToolBar.Height);
  NodeTree.OnKeyDown := NodeTreeKeyDown;
  NodeTree.OnMouseMove := NodeTreeMouseMove;
  NodeTree.OnMouseUp := NodeTreeMouseUp;
  NodeTree.OnMouseDown := NodeTreeMouseDown;
  SortMenu := TMenu.Create(Self, false, MenuSort);
  NaviMenu := TMenu.Create(Self, false, MenuNavi);
  InsertMenu(NaviMenu.Handle, IDSort, MF_BYCOMMAND or MF_POPUP, SortMenu.Handle, PChar(TBButtons[tbSort].Caption));
  OnResize := Resize;
end;

destructor TNavigator.Destroy;
begin
  FreeAndNil(FTBImages);
  FreeAndNil(FNodeImages);
  inherited;
end;

function TNavigator.AddNode(Parent, After: Integer; Node: TDataNode): Integer;
var
  i: Integer;
begin
  Result := NodeTree.ItemInsert(Parent, After, Node.Name, Node);
  (Node.Tag as TNodeTag).TreeItem := Result;
  for i := 0 to Node.Children.Count - 1 do
    AddNode(Result, Integer(TVI_LAST), Node.Children[i]);
  if (Node.Tag as TNodeTag).TreeItemExpanded then
    NodeTree.ExpandItem(Result, emExpand);
  if (Node.Tag as TNodeTag).Selected then
    NodeTree.Selected := Result;
end;

procedure TNavigator.DeleteNode(Node: Integer);
begin
  if Node = 0 then Exit;
  if MessageBox(FormMain.Handle, PChar(Format(SConfirmDelete, [TDataNode(NodeTree.GetItemObject(Node)).Name])),
    AppCaption, MB_YESNO or MB_ICONEXCLAMATION) = IDNO then Exit;
  NodeTree.GetItemObject(Node).Free;
  NodeTree.DeleteItem(Node);
  if Assigned(FOnModify) then
    FOnModify(Self);
end;

function TNavigator.NewNode(Parent: Integer; InsertAfter: Integer = Integer(TVI_LAST)): Integer;
var
  Node: TDataNode;
begin
  Result := 0;
  if not Assigned(FRootNode) then Exit;
  Node := TDataNode.Create;
  if (Parent = 0) or (Parent = Integer(TVI_ROOT))
    then Node.Parent := FRootNode
    else Node.Parent := TDataNode(NodeTree.GetItemObject(Parent));
  with Node.Parent.Children do
    if InsertAfter = Integer(TVI_LAST)
      then Add(Node)
      else Insert(IndexOf(TDataNode(NodeTree.GetItemObject(InsertAfter))) + 1, Node);
  Result := AddNode(Parent, InsertAfter, Node);
  if Assigned(FOnModify) then
    FOnModify(Self);
  NodeTree.ExpandItem(Parent, emExpand);
  NodeTree.Selected := Result;
  NodeTree.SetFocus;
  NodeTree.Perform(TVM_EDITLABEL, 0, Result);
end;

procedure TNavigator.UpdateTree;
var
  i: Integer;
begin
  NodeTree.BeginUpdate;
  try
    NodeTree.DeleteItem(Integer(TVI_ROOT));
    if Assigned(FRootNode) then
      for i := 0 to FRootNode.Children.Count - 1 do
        AddNode(Integer(TVI_ROOT), Integer(TVI_LAST), FRootNode.Children[i]);
  finally
    NodeTree.EndUpdate;
  end;
end;

procedure TNavigator.Resize(Sender: TObject);
begin
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  NodeTree.SetSize(ClientWidth, ClientHeight - ToolBar.Height);
end;

procedure TNavigator.WMNotify(var Msg: TWMNotify);
const
  ToggleImage: array[Boolean] of Integer = (8, 7);
  NodeImage: array[Boolean] of Integer = (0, 2);
var
  P: TPoint;
begin
  if Assigned(NodeTree) and (Msg.NMHdr.hwndFrom = NodeTree.Handle) then
  begin
    if Msg.NMHdr.code = TVN_SELCHANGED then
    begin
      if Assigned(FOnNodeSelected) then
        FOnNodeSelected(Self);
      ToolBar.ButtonImageIndex[Integer(tbToggle)] := ToggleImage[NodeTree.ItemExpanded(NodeTree.Selected)];
      if PNMTreeView(Msg.NMHdr).itemNew.lParam <> 0 then
        (TDataNode(PNMTreeView(Msg.NMHdr).itemNew.lParam).Tag as TNodeTag).Selected := true;
    end;
    if Msg.NMHdr.code = TVN_ITEMEXPANDED then
      with PNMTreeView(Msg.NMHdr)^ do
      begin
        if Integer(itemNew.hItem) = NodeTree.Selected then
          ToolBar.ButtonImageIndex[Integer(tbToggle)] := ToggleImage[NodeTree.ItemExpanded(NodeTree.Selected)];
        if itemNew.lParam <> 0 then
          (TDataNode(itemNew.lParam).Tag as TNodeTag).TreeItemExpanded := action = TVE_EXPAND;
      end;
    if Msg.NMHdr.code = TVN_BEGINLABELEDIT then
    begin
      Msg.Result := 0;
      SendMessage(NodeTree.Perform(TVM_GETEDITCONTROL, 0, 0), EM_SETLIMITTEXT, 255, 0);
    end;
    if Msg.NMHdr.code = TVN_ENDLABELEDIT then
      with PTVDispInfo(Msg.NMHdr)^ do
        if Assigned(item.pszText) and (item.lParam <> 0) then
        begin
          TDataNode(item.lParam).Name := item.pszText;
          if Assigned(FOnModify) then
            FOnModify(Self);
          Msg.Result := 1;
        end;
    if Msg.NMHdr.code = TVN_BEGINDRAG then
    begin
      FDragNode := Integer(PNMTreeView(Msg.NMHdr).itemNew.hItem);
      FDragImage := NodeTree.Perform(TVM_CREATEDRAGIMAGE, 0, FDragNode);
      ImageList_BeginDrag(FDragImage, 0, 0, 0);
      P := PNMTreeView(Msg.NMHdr).ptDrag;
      ClientToScreen(NodeTree.Handle, P);
      ImageList_DragEnter(GetDesktopWindow, P.X, P.Y);
      SetCapture(NodeTree.Handle);
    end;
    if (Msg.NMHdr.code = TVN_GETDISPINFO) and (PTVDispInfo(Msg.NMHdr).item.lParam <> 0) then
      with PTVDispInfo(Msg.NMHdr).item, TDataNode(PTVDispInfo(Msg.NMHdr).item.lParam).Tag as TNodeTag do
      begin
        if mask and TVIF_IMAGE <> 0 then
          iImage := NodeImage[state and TVIS_EXPANDED = TVIS_EXPANDED];
        if mask and TVIF_SELECTEDIMAGE <> 0 then
          iSelectedImage := iImage + 1;
      end;
  end;
end;

procedure TNavigator.SetRootNode(const Value: TDataNode);
begin
  FRootNode := Value;
  UpdateTree;
end;

procedure TNavigator.WMCommand(var Msg: TWMCommand);
const
  MenuCheck: array[Boolean] of UINT = (MF_UNCHECKED, MF_CHECKED);
var
  Cursor: TPoint;
begin
  if Assigned(ToolBar) and (Msg.Ctl = ToolBar.Handle) then
    case TTBButtons(Msg.ItemID) of
      tbAdd: NewNode(NodeTree.GetItemParent(NodeTree.Selected), NodeTree.Selected);
      tbAddChild: NewNode(NodeTree.Selected);
      tbProperties: NodeProperties(NodeTree.Selected);
      tbMoveUp: SwapNodes(NodeTree.Selected, NodeTree.Perform(TVM_GETNEXTITEM, TVGN_PREVIOUS, NodeTree.Selected));
      tbMoveDown: SwapNodes(NodeTree.Selected, NodeTree.Perform(TVM_GETNEXTITEM, TVGN_NEXT, NodeTree.Selected));
      tbDelete: DeleteNode(NodeTree.Selected);
      tbSort: begin
        GetCursorPos(Cursor);
        CheckMenuItem(SortMenu.Handle, IDSortRecursive, MenuCheck[FSortRecursive] or MF_BYCOMMAND);
        FMenuNode := NodeTree.Selected;
        SortMenu.Popup(Cursor.X, Cursor.Y);
      end;
      tbToggle: ToggleNode(NodeTree.Selected);
    end;
  if (Msg.Ctl = 0) and (Msg.NotifyCode in [0, 1]) then
    case Msg.ItemID of
      IDRefresh: RefreshNode(FMenuNode);
      IDAdd: NewNode(NodeTree.GetItemParent(FMenuNode), NodeTree.Selected);
      IDAddChild: NewNode(FMenuNode);
      IDRename: NodeTree.Perform(TVM_EDITLABEL, 0, FMenuNode);
      IDProperties: NodeProperties(FMenuNode);
      IDDelete: DeleteNode(FMenuNode);
      IDToggle: ToggleNode(FMenuNode);
      IDSortAlphaAsc: SortNode(FMenuNode, FSortRecursive, true, CompareText);
      IDSortAlphaDesc: SortNode(FMenuNode, FSortRecursive, false, CompareText);
      IDSortNatAsc: SortNode(FMenuNode, FSortRecursive, true, CompareTextNatural);
      IDSortNatDesc: SortNode(FMenuNode, FSortRecursive, false, CompareTextNatural);
      IDSortRecursive: FSortRecursive := not FSortRecursive;
    end;
end;

procedure TNavigator.NodeTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_F2: NodeTree.Perform(TVM_EDITLABEL, 0, NodeTree.Selected);
      VK_F3: NodeProperties(NodeTree.Selected);
      VK_F5: RefreshNode(NodeTree.Selected);
      VK_F7: NewNode(NodeTree.GetItemParent(NodeTree.Selected), NodeTree.Selected);
      VK_F8: NewNode(NodeTree.Selected);
      VK_DELETE: DeleteNode(NodeTree.Selected);
    end;
  if Shift = [ssShift] then
    case Key of
      VK_F5: UpdateTree;
    end;
  if Shift = [ssCtrl] then
  begin
    case Key of
      Ord('I'): NewNode(NodeTree.GetItemParent(NodeTree.Selected), NodeTree.Selected);
      VK_UP: SwapNodes(NodeTree.Selected, NodeTree.Perform(TVM_GETNEXTITEM, TVGN_PREVIOUS, NodeTree.Selected));
      VK_DOWN: SwapNodes(NodeTree.Selected, NodeTree.Perform(TVM_GETNEXTITEM, TVGN_NEXT, NodeTree.Selected));
      VK_LEFT: ToggleNode(NodeTree.Selected, emCollapse);
      VK_RIGHT: ToggleNode(NodeTree.Selected, emExpand);
    end;
    if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
      Key := 0;
  end;
  if Shift = [ssShift, ssCtrl] then
    case Key of
      Ord('I'): NewNode(NodeTree.Selected);
    end;
end;

procedure TNavigator.RefreshNode(Node: Integer);
var
  SelNode: TDataNode;
begin
  SelNode := Self.SelNode;
  if Node <> 0 then
  begin
    NodeTree.BeginUpdate;
    try
      AddNode(NodeTree.GetItemParent(Node), Node, TDataNode(NodeTree.GetItemObject(Node)));
      NodeTree.DeleteItem(Node);
    finally
      NodeTree.EndUpdate;
    end;
  end
    else UpdateTree;
  if Assigned(SelNode) then
    NodeTree.Selected := (SelNode.Tag as TNodeTag).TreeItem;
end;

procedure TNavigator.SwapNodes(Node1, Node2: Integer);
begin
  if (Node1 = 0) or (Node2 = 0) or
    (TDataNode(NodeTree.GetItemObject(Node1)).Parent <> TDataNode(NodeTree.GetItemObject(Node2)).Parent) then Exit;
  with TDataNode(NodeTree.GetItemObject(Node1)).Parent.Children do
    Swap(IndexOf(TDataNode(NodeTree.GetItemObject(Node1))), IndexOf(TDataNode(NodeTree.GetItemObject(Node2))));
  if Assigned(FOnModify) then
    FOnModify(Self);
  RefreshNode(NodeTree.GetItemParent(Node1));
end;

function TNavigator.GetSelNode: TDataNode;
begin
  if Assigned(FRootNode) then
    Result := TDataNode(NodeTree.GetItemObject(NodeTree.Selected))
  else Result := nil;
end;

procedure TNavigator.SortNode(Node: Integer; Recursive, Ascending: Boolean; CompFunc: TCompFunc);
begin
  NodeSort(TDataNode(NodeTree.GetItemObject(Node)), Recursive, Ascending, CompFunc);
  RefreshNode(Node);
  if Assigned(FOnModify) then
    FOnModify(Self);
end;

procedure TNavigator.NodeSort(Node: TDataNode; Recursive, Ascending: Boolean; CompFunc: TCompFunc);
var
  i, Count, LastSwap: Integer;
begin
  if not Assigned(Node) then Exit;
  Count := Node.Children.Count;
  while Count > 0 do
  begin
    LastSwap := 0;
    for i := 1 to Count - 1 do
      if (Ascending and (CompFunc(Node.Children[i - 1].Name, Node.Children[i].Name) > 0)) or
        (not Ascending and (CompFunc(Node.Children[i - 1].Name, Node.Children[i].Name) < 0)) then
      begin
        Node.Children.Swap(i - 1, i);
        LastSwap := i;
      end;
    Count := LastSwap;
  end;
  if Recursive then
    for i := 0 to Node.Children.Count - 1 do
      NodeSort(Node.Children[i], Recursive, Ascending, CompFunc);
end;

procedure TNavigator.ToggleNode(Node: Integer; Mode: TExpandMode);
const
  Modes: array[Boolean] of TExpandMode = (emExpand, emCollapse);
begin
  if Mode = emToggle then
    Mode := Modes[NodeTree.ItemExpanded(Node)];
  NodeTree.ExpandItem(Node, Mode);
  Node := NodeTree.Perform(TVM_GETNEXTITEM, TVGN_CHILD, Node);
  while Node <> 0 do
  begin
    ToggleNode(Node, Mode);
    Node := NodeTree.Perform(TVM_GETNEXTITEM, TVGN_NEXT, Node);
  end;
end;

procedure TNavigator.Clear;
begin
  NodeTree.DeleteItem(Integer(TVI_ROOT));
  FRootNode := nil;
end;

procedure TNavigator.NodeProperties(Node: Integer);
begin
  ShowMessage('Under construction');
end;

procedure TNavigator.NodeTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDragNode <> 0 then
  begin
    //ImageList_DragLeave(NodeTree.Handle);
    NodeTree.Perform(TVM_SELECTITEM, TVGN_DROPHILITE, NodeTree.ItemAtPoint(X, Y));
    //ImageList_DragEnter(NodeTree.Handle, X, Y);
    P := Point(X, Y);
    ClientToScreen(NodeTree.Handle, P);
    ImageList_DragMove(P.X, P.Y);
  end;
end;

procedure TNavigator.NodeTreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function CheckNode(Target, Dropped: Integer): Boolean;
  begin
    Result := false;
    while Target <> 0 do
      if Target = Dropped then
        Exit
      else
        Target := NodeTree.GetItemParent(Target);
    Result := true;
  end;

var
  Node: Integer;
begin
  if (Button = mbLeft) and (FDragNode <> 0) then
  begin
    Node := NodeTree.ItemAtPoint(X, Y);
    if (X >= 0) and (Y >= 0) and (X <= NodeTree.Width) and (Y <= NodeTree.Height) and CheckNode(Node, FDragNode) then
    begin
      if Node <> 0 then
        TDataNode(NodeTree.GetItemObject(Node)).Children.Add(TDataNode(NodeTree.GetItemObject(FDragNode)))
      else
        FRootNode.Children.Add(TDataNode(NodeTree.GetItemObject(FDragNode)));
      UpdateTree;
      if Assigned(FOnModify) then
        FOnModify(Self);
    end;
    ImageList_DragLeave(NodeTree.Handle);
    ImageList_EndDrag;
    ImageList_Destroy(FDragImage);
    FDragNode := 0;
    ReleaseCapture;
  end;
end;

procedure TNavigator.NodeTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cur: TPoint;
begin
  Cur := Point(X, Y);
  ClientToScreen((Sender as TWinControl).Handle, Cur);
  if Button = mbRight then
  begin
    FMenuNode := NodeTree.ItemAtPoint(X, Y);
    NaviMenu.Popup(Cur.X, Cur.Y);
  end;
end;

end.