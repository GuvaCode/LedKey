
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplPlayWAVUnit;

{$I ExSystem.inc}

interface

uses
{$IFDEF UNIX}
  Classes, SysUtils, FileUtil, Process, ExSystemUtils, ExSystemConsts, ExSystemMessages;
{$ENDIF}
{$IFDEF WINDOWS}
  MMSystem;
{$ENDIF}

type

  { ILSPlayWAV }

  ILSPlayWAV = interface
    ['{5267BB1A-D21D-B211-93B2-000FEA22BE72}']
    procedure Execute(const AWAVFile: string;
      const AAsynchronous: Boolean = True);
    function GetPlayer: string;
    procedure SetPlayer(const AValue: string);
    property Player: string read GetPlayer write SetPlayer;
  end;

  { TplPlayWAV }

  TplPlayWAV = class(TInterfacedObject, ILSPlayWAV)
  private
    FPlayer: string;
    function GetPlayer: string;
    procedure SetPlayer(const AValue: string);
  public
{$IFDEF UNIX}
    constructor Create;
{$ENDIF}
    procedure Execute(const AWAVFile: string;
      const AAsynchronous: Boolean = True);
    property Player: string read GetPlayer write SetPlayer;
  end;

{$IFDEF UNIX}
function LSGetLinuxPlayer: string;
{$ENDIF}
procedure LSPlayWAVFile(const AWAVFile: string;
  const AAsynchronous: Boolean = True);

implementation

{$IFDEF UNIX}
function LSGetLinuxPlayer: string;
var
  S: string;
  I: Integer;
  VPlayers, VPlayersPaths: TStringList;
begin
  Result := SLSPlayerNotFound;
  VPlayers := TStringList.Create;
  VPlayersPaths := TStringList.Create;
  try
    VPlayers.Text := CLSDistrosPlayer;
    VPlayers.NameValueSeparator := ':';
    VPlayersPaths.Clear;
    ExtractStrings([','], [' '], PChar(VPlayers.Values[LSGetLinuxDistro]),
      VPlayersPaths);
    for I := 0 to Pred(VPlayersPaths.Count) do
    begin
      S := FindDefaultExecutablePath(VPlayersPaths.ValueFromIndex[I]);
      if FileExists(S) then
      begin
        Result := S;
        Break;
      end;
    end;
  finally
    VPlayers.Free;
    VPlayersPaths.Free;
  end;
end;
{$ENDIF}

procedure LSPlayWAVFile(const AWAVFile: string;
  const AAsynchronous: Boolean = True);
var
  VLSPlayWAV: ILSPlayWAV;
begin
  VLSPlayWAV := TplPlayWAV.Create;
  VLSPlayWAV.Execute(AWAVFile, AAsynchronous);
end;

{ TplPlayWAV }

function TplPlayWAV.GetPlayer: string;
begin
  Result := FPlayer;
end;

procedure TplPlayWAV.SetPlayer(const AValue: string);
begin
  FPlayer := AValue;
end;

{$IFDEF UNIX}
constructor TplPlayWAV.Create;
var
  VPlayers: TStringList;
begin
  VPlayers := TStringList.Create;
  try
    VPlayers.NameValueSeparator := ':';
    VPlayers.Text := CLSDistrosPlayer;
    FPlayer := LSGetLinuxPlayer;
  finally
    VPlayers.Free;
  end;
end;
{$ENDIF}

procedure TplPlayWAV.Execute(const AWAVFile: string;
  const AAsynchronous: Boolean);
{$IFDEF UNIX}
var
  VProcess: TProcess;
{$ENDIF}
begin
{$IFDEF UNIX}
  if FPlayer = SLSPlayerNotFound then
    raise Exception.Create('ERROR: Implement the path of a player of this OS.');
  VProcess := TProcess.Create(nil);
  try
    if AAsynchronous then
      VProcess.Options := [poNoConsole]
    else
      VProcess.Options := [poNoConsole, poWaitOnExit];
    VProcess.CommandLine := FPlayer + ' ' + QuotedStr(AWAVFile);
    VProcess.Execute;
  finally
    VProcess.Free;
  end;
{$ENDIF}
{$IFDEF WINDOWS}
  if AAsynchronous then
    sndPlaySound({$IFDEF WINCE}PWideChar{$ELSE}PChar{$ENDIF}(AWAVFile), SND_ASYNC or SND_FILENAME)
  else
    sndPlaySound({$IFDEF WINCE}PWideChar{$ELSE}PChar{$ENDIF}(AWAVFile), SND_SYNC or SND_FILENAME)
{$ENDIF}

end;

end.

