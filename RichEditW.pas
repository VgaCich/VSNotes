unit RichEditW;

interface

uses
  Windows, Messages, RichEdit, AvL;

type
  TRichEditW = class(TWinControl)
  private
    RichEditDLL: THandle;
    FBkColor: Integer;
    procedure SetBkColor(const Value: Integer);
  public
    constructor Create(AParent: TWinControl; Text: string; WordWrap: Boolean = false);
    destructor Destroy; override;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure Undo;
    procedure Redo;
    procedure ClearUndo;
    procedure LoadFromStream(Stream: TStream; Fmt: Integer = SF_TEXT);
    procedure SaveToStream(Stream: TStream; Fmt: Integer = SF_TEXT);
    property Color: Integer read FBkColor write SetBkColor;
    property Text;
    property SelStart;
  end;

implementation

function EditStreamRead(dwCookie: Longint; pbBuff: PByte; cb: Longint; var pcb: Longint): Longint; stdcall;
begin
  pcb := TStream(dwCookie).Read(pbBuff^, cb);
  Result := 0;
end;

function EditStreamWrite(dwCookie: Longint; pbBuff: PByte; cb: Longint; var pcb: Longint): Longint; stdcall;
begin
  pcb := TStream(dwCookie).Write(pbBuff^, cb);
  Result := 0;
end;

{ TRichEditW }   

constructor TRichEditW.Create(AParent: TWinControl; Text: string; WordWrap: Boolean);
begin
  if AParent = nil then
    ExitProcess(0);
  InitCommonControls;
  RichEditDLL := LoadLibrary('RichEd20.dll');
  if RichEditDLL = 0 then
    ExitProcess(0);
  inherited Create(AParent);
  FClassName := 'RichEdit20W';
  FExStyle := WS_EX_STATICEDGE;//WS_EX_CLIENTEDGE;
  FStyle := WS_CHILD or WS_VISIBLE or ES_MULTILINE or WS_VSCROLL or ES_NOHIDESEL;
  if not WordWrap then //TODO: EM_SETOPTIONS may help
    FStyle := FStyle or WS_HSCROLL;
  FBkColor := clBtnFace;
  CreateWnd;
  Perform(EM_LIMITTEXT, -1, 0);
  Perform(EM_SETEVENTMASK, 0, ENM_CHANGE or ENM_SELCHANGE);
end;

procedure TRichEditW.SetBkColor(const Value: Integer);
begin
  FBkColor := Value;
  Perform(EM_SETBKGNDCOLOR, 0, Value);
end;

procedure TRichEditW.Undo;
begin
  Perform(EM_UNDO, 0, 0);
end;

procedure TRichEditW.ClearUndo;
begin
  Perform(EM_EMPTYUNDOBUFFER, 0, 0);
end;

function TRichEditW.CanRedo: Boolean;
begin
  Result := Boolean(Perform(EM_CANREDO, 0, 0));
end;

function TRichEditW.CanUndo: Boolean;
begin
  Result := Boolean(Perform(EM_CANUNDO, 0, 0));
end;

procedure TRichEditW.Redo;
begin
  Perform(EM_REDO, 0, 0);
end;

procedure TRichEditW.LoadFromStream(Stream: TStream; Fmt: Integer);
var
  ES: TEditStream;
begin
  BeginUpdate;
  try
    ES.dwCookie := Integer(Stream);
    ES.dwError := 0;
    ES.pfnCallback := @EditStreamRead;
    Perform(WM_SETTEXT, 0, 0);
    Perform(EM_STREAMIN, Fmt, Integer(@ES));
    Perform(EM_SETMODIFY, 0, 0);
  finally
    EndUpdate;
    Invalidate;
    UpdateWindow(Handle);
  end;
end;

procedure TRichEditW.SaveToStream(Stream: TStream; Fmt: Integer);
var
  ES: TEditStream;
begin
  ES.dwCookie := Integer(Stream);
  ES.dwError := 0;
  ES.pfnCallback := @EditStreamWrite;
  Perform(EM_STREAMOUT, Fmt, Integer(@ES));
end;

destructor TRichEditW.Destroy;
begin
  FreeLibrary(RichEditDLL);
  inherited;
end;

end.
