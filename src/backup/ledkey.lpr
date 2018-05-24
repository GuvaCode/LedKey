program ledkey;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, mUnit, ExSystemUtils
  { you can add units after this };

{$R *.res}

begin
  LSGlobalAddAtom(CMyAppAtom);
  LSRunOnce;
  //RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(THideFrm, HideFrm);
  Application.Run;
end.

