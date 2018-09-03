program loop;

{$mode objfpc}

uses
  SysUtils;

var
  I, Times: Integer;

begin
  Times := StrToIntDef(ParamStr(1), 0);
  for I := 1 to 100 do
    Sleep(Times);
  WriteLn('Stoped on "', Times, '" times.');
end.

