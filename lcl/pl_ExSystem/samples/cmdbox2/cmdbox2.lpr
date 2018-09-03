program cmdbox2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  cmdbox2mw;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWMainForm, WMainForm);
  Application.Run;
end.

