program RunOnceDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,    RunOnceMainFrm, ExSystemUtils;

{$R *.res}

begin
  LSGlobalAddAtom(CMyAppAtom);
  LSRunOnce;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

