
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit ExSystemUtils;

{$I ExSystem.inc}

interface

uses
{$IFDEF MSWINDOWS}
  DynLibs, Windows, JwaTlHelp32,
{$ENDIF}
  ExSystemConsts, ExSystemMessages, SysUtils, Classes, Dialogs, ExtDlgs, Process, Forms,
  RegExpr;

type

  { TLSDialogType }

  TLSDialogType = (dtFile, dtPicture, dtDirectory);

{ Get current path, e.g.: /myproject/, or C:\myproject\. }
function LSCurrentPath: string;
{ Open a file or directory. }
function LSOpenDialog(const ALSDialogType: TLSDialogType = dtFile;
  const AInitialDir: string = ''; const AFilter: string = '';
  const AMultiSelect: Boolean = False;
  const AAbortIfNotExecute: Boolean = False): string;
{ Load a file to string. }
function LSLoadFile(const AFileName: TFileName): string;
{ Save a string to file. }
procedure LSSaveFile(const AFileName: TFileName; const AString: string);
{ Execute process and get large output. }
function LSExecProcess(const ACommandLine: string): string;
{ Execute process and get short output. }
function LSExecProcessWithShortOut(const ACommandLine: string): string;
{ Get current user name. }
function LSCurrentUserName: string;
{ Y/N to boolean. }
function LSYNToBool(const S: string): Boolean;
{ Boolean to Y/N. }
function LSBoolToYN(const B: Boolean): string;
{$IFDEF UNIX}
{ Get the name of the current Linux distro. }
function LSGetLinuxDistro: string;
{$ENDIF}
{ Get MAC address. (see: http://en.wikipedia.org/wiki/MAC_address) }
function LSGetMACAddress: string;
{ Get your work area rect. }
function LSGetWorkAreaRect(const AHandle: THandle = 0): TRect;
{ Get process ID by process name. }
function LSGetProcessID(const AProcessName: string;
  const AIgnoreCurrent: Boolean = False): Integer;
{ List active process. }
procedure LSListProcess(const AProcess: TStringList;
  const AShowPID: Boolean = False; const ASorted: Boolean = True;
  const AIgnoreCurrent: Boolean = False);
{ Get if process is running. }
function LSProcessIsRunning(const AProcessName: string;
  const AIgnoreCurrent: Boolean = False): Boolean;
{ Kill process by name. }
function LSKillProcess(const AProcessName: string): Boolean;
{ Run once application. }
function LSRunOnce(const AHalt: Boolean = True): Boolean;
{ Add a atom string. }
procedure LSGlobalAddAtom(const AAtomName: string);
{ Delete a atom string. }
procedure LSGlobalDeleteAtom(const AAtomName: string);
{ Find a atom string. }
function LSGlobalFindAtom(const AAtomName: string): Boolean;
{ Remove Cid from HTML.
  E.g.: From <img src="cid:MyImage.png"> To <img src="MyImage.png"> }
function LSRemoveCid(const AHTML: string): string;

implementation

{$IFDEF UNIX}
const
  CLSAtomTableFileName = '/tmp/.lsatomtable';

var
  _LSAtomList: TStringList = nil;

function LSAtomList: TStringList;
begin
  if not Assigned(_LSAtomList) then
    _LSAtomList := TStringList.Create;
  Result := _LSAtomList;
end;

function LSAtomTableExists: Boolean;
begin
  Result := FileExists(CLSAtomTableFileName);
end;
{$ENDIF}

function LSCurrentPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function LSOpenDialog(const ALSDialogType: TLSDialogType;
  const AInitialDir: string; const AFilter: string; const AMultiSelect: Boolean;
  const AAbortIfNotExecute: Boolean): string;
var
  VOpenDialog: TOpenDialog;
begin
  case ALSDialogType of
    dtFile: VOpenDialog := TOpenDialog.Create(nil);
    dtPicture: VOpenDialog := TOpenPictureDialog.Create(nil);
    dtDirectory: VOpenDialog := TSelectDirectoryDialog.Create(nil);
  end;
  try
    if AInitialDir <> '' then
      VOpenDialog.InitialDir := AInitialDir;
    if AFilter <> '' then
      VOpenDialog.Filter := AFilter;
    if AMultiSelect then
      VOpenDialog.Options := VOpenDialog.Options + [ofAllowMultiSelect]
    else
      VOpenDialog.Options := VOpenDialog.Options - [ofAllowMultiSelect];
    if VOpenDialog.Execute then
      Result := VOpenDialog.FileName
    else
      if AAbortIfNotExecute then
        Abort;
  finally
    VOpenDialog.Free;
  end;
end;

function LSLoadFile(const AFileName: TFileName): string;
begin
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    except
      Result := '';
      Free;
      raise;
    end;
    Free;
  end;
end;

procedure LSSaveFile(const AFileName: TFileName; const AString: string);
begin
  with TFileStream.Create(AFileName, fmCreate) do
    try
      Write(Pointer(AString)^, Length(AString));
    finally
      Free;
    end;
end;

function LSExecProcess(const ACommandLine: string): string;
const
  READ_BYTES = 2048;
var
  VStrTemp: TStringList;
  VMemoryStream: TMemoryStream;
  VProcess: TProcess;
  I64: LongInt;
  VBytesRead: LongInt;
begin
  VMemoryStream := TMemoryStream.Create;
  VProcess := TProcess.Create(nil);
  VStrTemp := TStringList.Create;
  try
    VBytesRead := 0;
    VProcess.CommandLine := ACommandLine;
    VProcess.Options := [poUsePipes, poNoConsole];
    VProcess.Execute;
    while VProcess.Running do
    begin
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64)
      else
        Sleep(100);
    end;
    repeat
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64);
    until I64 <= 0;
    VMemoryStream.SetSize(VBytesRead);
    VStrTemp.LoadFromStream(VMemoryStream);
    Result := Trim(VStrTemp.Text);
  finally
    VStrTemp.Free;
    VProcess.Free;
    VMemoryStream.Free;
  end;
end;

function LSExecProcessWithShortOut(const ACommandLine: string): string;
var
  VProcess: TProcess;
  VStrTemp: TStringList;
begin
  VProcess := TProcess.Create(nil);
  VStrTemp := TStringList.Create;
  try
    VProcess.Options := [poStderrToOutPut, poNoConsole, poUsePipes];
    VProcess.CommandLine := ACommandLine;
    VProcess.Execute;
    VStrTemp.LoadFromStream(VProcess.Output);
    Result := Trim(VStrTemp.Text);
  finally
    VProcess.Free;
    VStrTemp.Free;
  end;
end;

function LSCurrentUserName: string;
begin
{$IFDEF UNIX}
  Result := {$I %USER%};
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := {$I %USERNAME%};
{$ENDIF}
end;

function LSYNToBool(const S: string): Boolean;
begin
  Result := LowerCase(S) = 'y';
end;

function LSBoolToYN(const B: Boolean): string;
begin
  if B then
    Result := 'y'
  else
    Result := 'n';
end;

{$IFDEF UNIX}
function LSGetLinuxDistro: string;
var
  I, J: Integer;
  VDistros, VReleaseFiles: TStringList;
begin
  Result := SLSGetLinuxDistroError;
  VDistros := TStringList.Create;
  VReleaseFiles := TStringList.Create;
  try
    VDistros.Text := CLSDistros;
    VDistros.NameValueSeparator := ':';
    for I := 0 to Pred(VDistros.Count) do
    begin
      VReleaseFiles.Clear;
      ExtractStrings([','], [' '], PChar(VDistros.ValueFromIndex[I]), VReleaseFiles);
      for J := 0 to Pred(VReleaseFiles.Count) do
        if FileExists(VReleaseFiles.ValueFromIndex[J]) then
        begin
          Result := VDistros.Names[I];
          Break;
        end;
    end;
  finally
    VDistros.Free;
    VReleaseFiles.Free;
  end;
end;
{$ENDIF}

function LSGetMACAddress: string;
{$IFDEF MSWINDOWS}
type
  TCreateGUIDFunction = function(AGUID: PGUID): LongInt; stdcall;
{$ENDIF}
var
{$IFDEF UNIX}
  VPath, VDevice: string;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VLibHandle: TLibHandle;
  VCreateGUIDFunction: TCreateGUIDFunction;
  VGUID1, VGUID2: TGUID;
{$ENDIF}
begin
{$IFDEF UNIX}
  VDevice := 'eth0';
  VPath := Format('/sys/class/net/%s/address', [VDevice]);
  if FileExists(VPath) then
    Result := LSLoadFile(VPath)
  else
    Result := 'Could not find the device "' + VDevice + '".';
{$ENDIF}
{$IFDEF MSWINDOWS}
  VLibHandle := LoadLibrary('rpcrt4.dll');
  try
    if VLibHandle <> NilHandle then
    begin
      VCreateGUIDFunction := TCreateGUIDFunction(GetProcedureAddress(VLibHandle,
        'UuidCreateSequential'));
      if Assigned(VCreateGUIDFunction) then
        if (VCreateGUIDFunction(@VGUID1) = 0) and
         (VCreateGUIDFunction(@VGUID2) = 0) and (VGUID1.D4[2] = VGUID2.D4[2]) and
         (VGUID1.D4[3] = VGUID2.D4[3]) and (VGUID1.D4[4] = VGUID2.D4[4]) and
         (VGUID1.D4[5] = VGUID2.D4[5]) and (VGUID1.D4[6] = VGUID2.D4[6]) and
         (VGUID1.D4[7] = VGUID2.D4[7]) then
          Result := Format(CLSFormatMACMask, [VGUID1.D4[2], VGUID1.D4[3],
          VGUID1.D4[4], VGUID1.D4[5], VGUID1.D4[6], VGUID1.D4[7]]);
    end;
  finally
    UnloadLibrary(VLibHandle);
  end;
{$ENDIF}
end;

function LSGetWorkAreaRect(const AHandle: THandle): TRect;
begin
  if AHandle = 0 then
    Result := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkAreaRect
  else
    Result := Screen.MonitorFromWindow(AHandle).WorkAreaRect;
end;

function LSGetProcessID(const AProcessName: string;
  const AIgnoreCurrent: Boolean): Integer;
var
  VProcess: TStringList;
begin
  VProcess := TStringList.Create;
  try
    VProcess.NameValueSeparator := CLSProcessNameValueSeparator;
    LSListProcess(VProcess, True, False, AIgnoreCurrent);
    Result := StrToIntDef(VProcess.Values[AProcessName], -1);
  finally
    VProcess.Free;
  end;
end;

procedure LSListProcess(const AProcess: TStringList; const AShowPID: Boolean;
  const ASorted: Boolean; const AIgnoreCurrent: Boolean);
var
{$IFDEF UNIX}
  I, J: Integer;
  VOldNameValueSeparator: Char;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VSnapshotHandle: THandle;
  VProcessEntry32: TProcessEntry32;
{$ENDIF}
begin
{$IFDEF UNIX}
  VOldNameValueSeparator := AProcess.NameValueSeparator;
  AProcess.NameValueSeparator := CLSProcessNameValueSeparator;
  AProcess.Text := LSExecProcess('sh -c "ps -A | awk ''{ print $4 "' +
    CLSProcessNameValueSeparator + '" $1 }''"');
  J := AProcess.Count;
  for I := AProcess.Count downto 1 do
  begin
    if (I > J - 3) or (AIgnoreCurrent and (StrToIntDef(AProcess.ValueFromIndex[
      I - 1], -1) = Integer(GetProcessID))) then
    begin
      AProcess.Delete(I - 1);
      Continue;
    end;
    if not AShowPID then
      AProcess.Strings[I - 1] := AProcess.Names[I - 1];
  end;
  AProcess.NameValueSeparator := VOldNameValueSeparator;
{$ENDIF}
{$IFDEF MSWINDOWS}
  try
    VSnapshotHandle := CreateToolHelp32SnapShot(TH32CS_SNAPALL, 0);
    VProcessEntry32.dwSize := SizeOf(TProcessEntry32);
    Process32First(VSnapshotHandle, VProcessEntry32);
    repeat
      if AIgnoreCurrent and (GetProcessID = VProcessEntry32.th32ProcessID) then
        Continue;
      if AShowPID then
        AProcess.Add(VProcessEntry32.szExeFile + CLSProcessNameValueSeparator +
          IntToStr(VProcessEntry32.th32ProcessID))
      else
        AProcess.Add(VProcessEntry32.szExeFile);
    until (not Process32Next(VSnapshotHandle, VProcessEntry32));
  except
    on Exception do
      raise Exception.Create(SLSListProcessError);
  end;
{$ENDIF}
  if AProcess.Count > 0 then
    AProcess.Delete(0);
  AProcess.Sorted := ASorted;
end;

function LSProcessIsRunning(const AProcessName: string;
  const AIgnoreCurrent: Boolean): Boolean;
var
  VProcess: TStringList;
begin
  VProcess := TStringList.Create;
  try
    LSListProcess(VProcess, False, False, AIgnoreCurrent);
    Result := VProcess.IndexOf(AProcessName) > -1;
  finally
    VProcess.Free;
  end;
end;

function LSKillProcess(const AProcessName: string): Boolean;
{$IFDEF MSWINDOWS}
const
  CPROCESS_TERMINATE = $0001;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  VContinueLoop: BOOL;
  VSnapshotHandle: THandle;
  VProcessEntry32: TProcessEntry32;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := LSProcessIsRunning(AProcessName, True);
  if Result then
  begin
    LSExecProcess('sh -c "kill ' + IntToStr(LSGetProcessID(
      AProcessName, True)) + '"');
    Result := not LSProcessIsRunning(AProcessName, True);
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := False;
  VSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  VProcessEntry32.dwSize := SizeOf(VProcessEntry32);
  VContinueLoop := Process32First(VSnapshotHandle, VProcessEntry32);
  while Integer(VContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(VProcessEntry32.szExeFile)) =
      UpperCase(AProcessName)) or (UpperCase(VProcessEntry32.szExeFile) =
      UpperCase(AProcessName))) and
      (VProcessEntry32.th32ProcessID <> GetProcessID) then
      Result := TerminateProcess(OpenProcess(CPROCESS_TERMINATE, BOOL(0),
                  VProcessEntry32.th32ProcessID), 0);
     VContinueLoop := Process32Next(VSnapshotHandle, VProcessEntry32);
  end;
  CloseHandle(VSnapshotHandle);
  Sleep(100);
{$ENDIF}
end;

function LSRunOnce(const AHalt: Boolean): Boolean;
var
  VProcess: string;
begin
  VProcess := ExtractFileName(Application.ExeName);
  Result := LSProcessIsRunning(VProcess, True);
  if Result and AHalt then
    Halt;
end;

procedure LSGlobalAddAtom(const AAtomName: string);
begin
{$IFDEF UNIX}
  if LSAtomTableExists then
    LSAtomList.LoadFromFile(CLSAtomTableFileName);
  if LSAtomList.IndexOf(AAtomName) = -1 then
    LSAtomList.Add(AAtomName);
  LSAtomList.SaveToFile(CLSAtomTableFileName);
{$ENDIF}
{$IFDEF MSWINDOWS}
  if GlobalFindAtom(LPCSTR(AAtomName)) = 0 then
    GlobalAddAtom(LPCSTR(AAtomName));
{$ENDIF}
end;

procedure LSGlobalDeleteAtom(const AAtomName: string);
{$IFDEF UNIX}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF UNIX}
  if LSAtomTableExists then
  begin
    LSAtomList.LoadFromFile(CLSAtomTableFileName);
    I := LSAtomList.IndexOf(AAtomName);
    if I <> -1 then
      LSAtomList.Delete(I);
    LSAtomList.SaveToFile(CLSAtomTableFileName);
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  GlobalDeleteAtom(GlobalFindAtom(LPCSTR(AAtomName)));
{$ENDIF}
end;

function LSGlobalFindAtom(const AAtomName: string): Boolean;
begin
{$IFDEF UNIX}
  Result := False;
  if LSAtomTableExists then
  begin
    LSAtomList.LoadFromFile(CLSAtomTableFileName);
    Result := LSAtomList.IndexOf(AAtomName) <> -1;
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := GlobalFindAtom(LPCSTR(AAtomName)) <> 0;
{$ENDIF}
end;

function LSRemoveCid(const AHTML: string): string;
var
  VOldRegExprModifierI: Boolean;
begin
  try
    VOldRegExprModifierI := RegExprModifierI;
    RegExprModifierI := True;
    Result := ReplaceRegExpr('src="cid:', AHTML, 'src="', True);
  finally
    RegExprModifierI := VOldRegExprModifierI;
  end;
end;

{$IFDEF UNIX}
initialization

finalization
  if Assigned(_LSAtomList) then
    _LSAtomList.Free;
{$ENDIF}

end.

