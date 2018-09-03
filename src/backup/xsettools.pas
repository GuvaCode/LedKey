unit xsettools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

{ Keyboard }
procedure LedLight(Light:Boolean);
function SrollLockPress:Boolean;


implementation

procedure LedLight(Light: Boolean);
Var ShProcess:Tprocess;
begin
    ShProcess := TProcess.Create(nil);
    ShProcess.Options:=[poWaitOnExit,poNoConsole];
    ShProcess.Executable := 'xset';
    If Light then
    ShProcess.Parameters.Add('-led');
    else
    ShProcess.Parameters.Add('led');
    ShProcess.Parameters.Add('3');
    ShProcess.Execute;
    ShProcess.Free;
end;


{ Keyboard }
function SrollLockPress: Boolean;
var ShProcess:TProcess;
    cArgs: String;
    Output:TStringList;
begin
    cArgs :=
    Trim(' xset q               ') +
    Trim(' | grep Scroll        ') +
    Trim(' | grep -Eo ''.{3}$'' ') ;

  Output:=TStringList.Create;
  ShProcess := TProcess.Create(nil);
  ShProcess.Executable := '/bin/sh';
  ShProcess.Parameters.Add('-c');
  ShProcess.Parameters.Add(cArgs);
  ShProcess.Options := ShProcess.Options + [poWaitOnExit, poUsePipes];
  ShProcess.Execute;

  Output.LoadFromStream(ShProcess.Output);

  case Trim(Output.Strings[Output.Count-1]) of
  'off' : Result := False;
  'on'  : Result := True;
  end;

  ShProcess.Free;
  Output.Free;
end;

end.

