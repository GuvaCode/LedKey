program ledkey;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mUnit, XTool, ExSystemUtils, uAbout
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  LSGlobalAddAtom(CMyAppAtom);
  LSRunOnce;
  Application.Initialize;
  Application.CreateForm(TmContainer, mContainer);
  Application.Run;
end.

