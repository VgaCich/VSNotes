program VSNotes;

uses
  SysSfIni, Windows, ActiveX, AvL, MainForm;

{$R *.res}
{$R VSNotesRes.res}

begin
  InitCommonControls;
  CoInitialize(nil);
  FormMain := TVSNotesMainForm.Create;
  FormMain.Run;
  FormMain.Free;
  CoUninitialize;
end.
