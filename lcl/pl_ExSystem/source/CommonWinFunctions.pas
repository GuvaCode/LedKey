
{ Common API Extra Windows functions }

unit CommonWinFunctions;
interface

uses Windows,Classes,SysUtils,StrUtils,Graphics,math,Masks;

Type

TOpSystem = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;

      {Execute a process.
        IN : FileName   -> Full path name
        Params     -> Command line parameters
        DefaultDir -> Default path
        ShowCmd    -> Launch mode
        OUT      : Return the processus handle
        EXCEPT   : EAbort }
     function ExecuteFile(FileName : string;
                          Shell : boolean = false;
                          Params : string = '';
                          DefaultDir : string = '';
                          ShowCmd : word = SW_SHOW) : THandle;
       { Execute a process and wait for its termination.
           IN    : FileName -> Full path name
           ShowCmd  -> Launch mode
           Wait     -> True for waiting termination, false if not
           OUT  : Return the processus exit code, 0 if launch faild or nowait.}
     function  WinExec32(FileName : string; ShowCmd : word; Wait : boolean) : dword;

     Function  IsFileExists(FileName : string):boolean;
     function  IsFileInUse(FileName : string) : boolean;
     procedure DoCopyFile(Src, Dest : string);
     procedure DoMoveFile(Src, Dest : string);
     procedure DoDeleteFile(Src : string);
     procedure DoRenameFile(Src, Dest : string);
     function  GetFileSize(FileName : string) : Int64;
     function  GetFileCreationDateTime(const aFileName: string): TDateTime;
     function  GetFileLastWriteDateTime(const aFileName: string): TDateTime;
     function  GetFileLastAccessDateTime(const aFileName: string): TDateTime;
     Procedure SetFileCreationDateTime(Const aFileName: string; Const aCreationDateTime: TDateTime);
     //.............................................
     //
     Function  IsDirectoryExists(Directory: String):Boolean;
     Function  DirMakeGoodEndPath(Rep : string):string;
     Function  DirEmpty(Directory: String;
                        SubDirectory: Boolean;
                        Const RemoveEmptySubDirectory: Boolean = True;
                        Const FileNameMask: String = '*.*';
                        Const MinFileAge: TdateTime = 0): Boolean;

     Function  DirCopy(SrcDirectory, DestDirectory: String;
                       SubDirectory: Boolean;
                       Const FileNameMask: String = '*.*';
                       Const ErraseIfExist: Boolean = False): Boolean;

     //............................................
     //
     procedure DoDelay(Milliseconds: Integer);
 end;


function  FindFileInPaths(const fileName, paths : String) : String;
function  PathsToString(const paths : TStrings) : String;
procedure StringToPaths(const pathsString : String; paths : TStrings);
function  ReplaseTextToPath(const aPath,OldText,NewText: String) : String;

function  RectToString(aRect: TRect) : String;
function  StringToRect(Str: String): TRect;

function  FontToString(aFont: TFont) : String;
procedure StringToFont(Str: String; aFont: TFont);

function  UniColorToString(Color: Cardinal): string;
function  UniStringToColor(const S: string): Cardinal;
Function  ColorRGB(Const R,G,B:Byte):TColor;
Function  ColorR(Const aColor:TColor):Byte;
Function  ColorG(Const aColor:TColor):Byte;
Function  ColorB(Const aColor:TColor):Byte;
Function  ColorToStr(Const aColor:TColor):string;
Function  StrToColor(Const aStr:string):TColor;
Function  ColorRandom:TColor;

//.......................................
//
procedure Delay(Milliseconds: Integer);
//.......................................
//
Function  FixFilePath(Const Inpath,CheckPath:string):string;
Function  UnFixFilePath(Const Inpath,CheckPath:string):string;
procedure FindFilesToDir(AStrings : TStrings; const APath : string;  const WithExt,WithPath: boolean);
//................. ClipBoard functions .................................
//
procedure ClipBoard_CopyStream(stream:TStream; Const format:integer);
procedure ClipBoard_PasteStream(stream:TStream; Const format:integer);

Procedure ClipBoard_CopyComponent(aComp:TComponent);
Function  ClipBoard_PasteComponent:TComponent;
Function  ClipBoard_HasComponent:Boolean;

procedure ClipBoard_CopyText(const Text: String);
procedure ClipBoard_PasteText(var Text: String);
Function  ClipBoard_HasText:Boolean;

function  ClipBoard_HasFormat(const FormatName: String): Boolean;
procedure ClipBoard_Copy(const FormatName: String; Stream: TStream);
function  ClipBoard_Paste(const FormatName: String; Stream: TStream): Boolean;

//............Application Functions ...........................
//

   { AppGetParamStr: returns the parameter from the command line that corresponds to Index,
   or an empty string if Index is greater than AppGetParamCount. For example,
   an Index value of 2 returns the second command-line parameter.
   -Note1: ParamStr(0) returns the path and file name of the executing program (for example, C:\TEST\MYPROG.EXE).
   -Note2: Use double quotes to wrap multiple words as one parameter (such as long file names containing spaces).}
   function AppGetParamStr(Const Index: Integer): string;

{ AppGetParamCount: returns the number of parameters passed to the program on the command line.
 Separate parameters with spaces or tabs. Use double quotes to wrap multiple words as one
 parameter (such as long file names containing spaces). }
 function AppGetParamCount: Integer;

 var
    OpSystem:TOpSystem;

implementation
 uses ClipBrd,forms,ShellApi,dialogs;

 Const FixText='..\';
       errNoMem = 'Error! No memory?';
       errInvalidData = 'Invalid data in Clipboard!';

 type
  EBinClipboardError = class (Exception);

//==================== TOsSystem ============================================
constructor TOpSystem.Create;
begin
  inherited Create;
end;

destructor TOpSystem.Destroy;
begin
  inherited Destroy;
end;

Function TOpSystem.IsFileExists(FileName : string):boolean;
begin
  result:=FileExists(FileName);
end;

function TOpSystem.IsFileInUse(FileName : string ) : boolean;
var HFileRes : HFILE;
begin
   result := false;
   if not FileExists(FileName) then Exit;
   HFileRes := CreateFile(pchar(FileName), GENERIC_READ or GENERIC_WRITE, 0 {"the trick"}, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
   result   := (HFileRes = INVALID_HANDLE_VALUE);
   if (not result) then CloseHandle(HFileRes);
end;

function TOpSystem.ExecuteFile(FileName : string; Shell : boolean = false; Params : string = '';
                       DefaultDir : string = ''; ShowCmd : word = SW_SHOW) : THandle;
const tab : array[boolean] of string = ( '', 'open' );
var s : string;
begin
  result := ShellExecute(Application.Handle, PChar(tab[shell]), PChar(FileName), PChar(Params), PChar(DefaultDir), ShowCmd);
  if (result > 32) then Exit;
  case (result) of
    0	                      : s := 'Not enough memory or system ressources';
    SE_ERR_DLLNOTFOUND,
    SE_ERR_FNF              : s := 'File not found';
    SE_ERR_PNF              : s := 'Path not found';
    ERROR_BAD_FORMAT	      : s := 'Invalid file';
    SE_ERR_ACCESSDENIED	    : s := 'Access denied';
    SE_ERR_ASSOCINCOMPLETE	: s := 'Bad file association';
    SE_ERR_NOASSOC	        : s := 'No association found';
    SE_ERR_OOM	            : s := 'Not enough memory';
    SE_ERR_SHARE	          : s := 'Share violation';
  else
    s := IntToStr(result);
  end;

  ShowMessage('Error on executing : '+ s);
end;


function TOpSystem.WinExec32(FileName : string; ShowCmd : word; Wait : boolean) : dword;
var zAppName    : array[0..512] of char;
    StartupInfo : TStartupInfo;
    ProcessInfo : TProcessInformation;
begin
  result := 0;
  StrPCopy(zAppName, FileName);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb          := Sizeof(StartupInfo);
  StartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := ShowCmd;
  case (CreateProcess(nil, zAppName, nil, nil, false, CREATE_NEW_CONSOLE, nil, nil, StartupInfo, ProcessInfo) <> False) of
    True  : begin
              if (Wait) then
                begin
                  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
                  GetExitCodeProcess(ProcessInfo.hProcess, result);
                end;
              CloseHandle(ProcessInfo.hProcess);
              CloseHandle(ProcessInfo.hThread);
            end;
    False :
  end;
end;

procedure TOpSystem.DoCopyFile(Src, Dest : string);
begin
  if FileExists(Src)=false then
   begin
    ShowMessage('File not found : ' +Src);
    exit;
   end;

  try
    Win32Check( CopyFile(PChar(Src), PChar(Dest), true) );
  except
    on E : Exception do ShowMessage('Error on copy file : '+E.Message);
  end;
end;


procedure TOpSystem.DoMoveFile(Src, Dest : string);
begin
  if FileExists(Src)=false then
   begin
    ShowMessage('File not found : ' +Src);
    exit;
   end;
  try
    Win32Check( MoveFile(PChar(Src), PChar(Dest)) );
  except
    on E : Exception do ShowMessage('Error on Move File : '+E.Message);
  end;
end;

procedure TOpSystem.DoDeleteFile(Src : string);
begin
  if not FileExists(Src) then Exit;
  try
    Win32Check( DeleteFile(PChar(Src)) );
  except
    on E : Exception do ShowMessage('Error on Delete File : '+E.Message);
  end;
end;

procedure TOpSystem.DoRenameFile(Src, Dest : string);
begin
  if not FileExists(Src) then Exit;
  try
    Win32Check( RenameFile(PChar(Src),PChar(Dest)) );
  except
    on E : Exception do ShowMessage('Error on Delete File : '+E.Message);
  end;
end; 

function TOpSystem.GetFileSize(FileName : string) : Int64;
var rec : TSearchRec;
begin
  result:=0;

  if (FindFirst(FileName, faAnyFile, rec) <> 0) then
   begin
    ShowMessage('File not found : ' +FileName);
    exit;
   end;

  result := rec.Size;
  SysUtils.FindClose(rec);
end;

function  TOpSystem.GetFileCreationDateTime(const aFileName: string): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindData;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFile(PChar(aFileName), aFindData);
  if aHandle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(aHandle);
    if (aFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      FileTimeToLocalFileTime(aFindData.ftCreationTime, aLocalFileTime);
      if FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo) then Begin
        Result := filedatetodatetime(aFileDate);
        Exit;
      end;
    end;
  end;
  Result := -0.5; //invalid DateTime
end;

function  TOpSystem.GetFileLastWriteDateTime(const aFileName: string): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindData;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFile(PChar(aFileName), aFindData);
  if aHandle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(aHandle);
    if (aFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      FileTimeToLocalFileTime(aFindData.ftLastWriteTime, aLocalFileTime);
      if FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo) then begin
        Result := filedatetodatetime(aFileDate);
        Exit;
      end;
    end;
  end;
  Result := -0.5; //invalid DateTime
end;

function  TOpSystem.GetFileLastAccessDateTime(const aFileName: string): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindData;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFile(PChar(aFileName), aFindData);
  if aHandle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(aHandle);
    if (aFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      FileTimeToLocalFileTime(aFindData.ftLastAccessTime, aLocalFileTime);
      if FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo) then begin
        Result := filedatetodatetime(aFileDate);
        Exit;
      end;
    end;
  end;
  Result := -0.5; //invalid DateTime
end;

Procedure TOpSystem.SetFileCreationDateTime(Const aFileName: string; Const aCreationDateTime: TDateTime);
Var ahandle: integer;
    aSystemTime: TsystemTime;
    afiletime: TfileTime;
Begin
  aHandle := sysUtils.fileOpen(aFileName, fmOpenWrite or fmShareDenyNone);
  if aHandle < 0 then exit;
  Try
    dateTimeToSystemTime(aCreationDateTime, aSystemTime);
    SystemTimeToFileTime(aSystemTime, aFileTime);
    LocalFileTimeToFileTime(aFileTime, aFileTime);
    setFileTime(aHandle, @aFileTime, nil, nil);
  finally
    fileClose(aHandle);
  end;
End;
//..........................................................................
procedure TOpSystem.DoDelay(Milliseconds: Integer);
 begin
   Delay(Milliseconds);
 end;
//..........................................................................
Function TOpSystem.DirMakeGoodEndPath(Rep : string):string;
begin
  if (length(Rep) > 0) and (Rep[length(Rep)] <> '\') then Rep := Rep + '\';
  result := Rep;
end;

Function TOpSystem.IsDirectoryExists(Directory: String):Boolean;
 begin
   result:=Directoryexists(Directory);
 end;

Function TOpSystem.DirEmpty(Directory: String;
                            SubDirectory: Boolean;
                            Const RemoveEmptySubDirectory: Boolean = True;
                            Const FileNameMask: String = '*.*';
                            Const MinFileAge: TdateTime = 0): Boolean;
var sr: TSearchRec;
    aBool: Boolean;
begin
  Result := True;
  Directory := DirMakeGoodEndPath(Directory);
  if FindFirst(Directory + '*.*', faAnyFile	, sr) = 0 then begin
    repeat
      If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
        If ((sr.Attr and faDirectory) <> 0) then begin
          If SubDirectory then begin
            DirEmpty(Directory + sr.Name, True, RemoveEmptySubDirectory, fileNameMask, MinFileAge);
            If RemoveEmptySubDirectory then begin
              Abool := RemoveDir(Directory + sr.Name);
              If result and (fileNameMask = '*.*') then Result := Abool;
            end;
          end;
        end
        else If (
                 (FileNameMask = '*.*') or
                 MatchesMask(sr.Name, FileNameMask)
                )
                and
                (
                 (MinFileAge<=0) or
                 (FileDateToDateTime(sr.Time) < MinFileAge)
                )
        then begin
          abool := Deletefile(Directory + sr.Name);
          If result then result := aBool;
        end;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end
end;

{************************************}
Function TOpSystem.DirCopy(SrcDirectory,
                           DestDirectory: String;
                           SubDirectory: Boolean;
                           Const FileNameMask: String = '*.*';
                           Const ErraseIfExist: Boolean = False): Boolean;
var sr: TSearchRec;
    aBool: Boolean;
begin
  Result := True;
  SrcDirectory := DirMakeGoodEndPath(SrcDirectory);
  DestDirectory := DirMakeGoodEndPath(DestDirectory);
  If not DirectoryExists(DestDirectory) and (not Createdir(DestDirectory)) then begin
    result := False;
    exit;
  end;

  if FindFirst(SrcDirectory + '*.*', faAnyFile, sr) = 0 then begin
    repeat
      If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
        If ((sr.Attr and faDirectory) <> 0) then begin
          If SubDirectory and
             (
              not DirCopy(
                            SrcDirectory + sr.Name,
                            DestDirectory + sr.Name,
                            SubDirectory,
                            FileNameMask,
                            ErraseIfExist
                                 )
             )
          then Result := False;
        end
        else If (
                 (FileNameMask = '*.*') or
                 MatchesMask(sr.Name, FileNameMask)
                )
        then begin
          If (not fileExists(DestDirectory + sr.Name)) or ErraseIfExist
            then abool := Copyfile(
                                   Pchar(SrcDirectory + sr.Name),
                                   Pchar(DestDirectory + sr.Name),
                                   not ErraseIfExist
                                  )
            else aBool := True;
          If result then result := aBool;
        end;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end
end;


//===============================================================
 function AppGetParamStr(Const Index: Integer): string;
  begin
   result:=ParamStr(Index);
  end;

 function AppGetParamCount: Integer;
  begin
   result:=ParamCount;
  end;
//===============================================================
procedure Delay(Milliseconds: Integer);
var 
  Tick: DWord; 
  Event: THandle; 
begin 
  Event := CreateEvent(nil, False, False, nil); 
  try 
    Tick := GetTickCount + DWord(Milliseconds); 
    while (Milliseconds > 0) and 
          (MsgWaitForMultipleObjects(1, Event, False, Milliseconds, QS_ALLINPUT) <> WAIT_TIMEOUT) do 
    begin 
      Application.ProcessMessages; 
      if Application.Terminated then Exit; 
      Milliseconds := Tick - GetTickcount; 
    end; 
  finally 
    CloseHandle(Event); 
  end; 
end;
//=================================================================
function FindFileInPaths(const fileName, paths : String) : String;
var
   i : Integer;
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.Delimiter:=';';
      sl.CommaText:=paths;
      for i:=0 to sl.Count-1 do begin
         if FileExists(sl[i]+'\'+fileName) then begin
            Result:=sl[i]+'\'+fileName;
            Exit;
         end;
      end;
   finally
      sl.Free;
   end;
   Result:='';
end;

function PathsToString(const paths : TStrings) : String;
var
   i : Integer;
begin
   Result:='';
   for i:=0 to paths.Count-1 do if paths[i]<>'' then
      Result:=Result+paths[i]+';';
   if Result<>'' then
      SetLength(Result, Length(Result)-1);
end;

procedure StringToPaths(const pathsString : String; paths : TStrings);
var
   i, p, n : Integer;
begin
   paths.BeginUpdate;
   paths.Clear;
   p:=1;
   for i:=1 to Length(pathsString) do begin
      if pathsString[i]=';' then begin
         n:=i-p;
         if n>0 then
            paths.Add(Copy(pathsString, p, n));
         p:=i+1;
      end;
   end;
   n:=Length(pathsString)-p+1;
   if n>0 then
      paths.Add(Copy(pathsString, p, n));
   paths.EndUpdate;
end;

function ReplaseTextToPath(const aPath,OldText,NewText: String) : String;
begin
   Result:=aPath;
   Result:=StringReplace(aPath, OldText, NewText, [rfReplaceAll, rfIgnoreCase]);
end;
//==========================================
Procedure FillStringList(sl:TStringList;const aText:string);
 var p1,p2,l1:integer;
     ss:string;
     ex:boolean;
 begin
 if sl=nil then exit;
 if aText='' then exit;

 l1:=Length(aText); 
 //...............
 p1:=1;
 p2:=0;

 repeat
   ex:=true;

   p2:=PosEx('\',aText,p1);

   if (p1<p2) and (p2>0) then
     begin
      ss:=copy(aText,p1,p2-p1+1);
      sl.Add(ss);
      p1:=p2+1;
      ex:=false;
     end;
     
 if p1>=l1 then exit;

 until ex=true;

 end;


Function FixFilePath(Const Inpath,CheckPath:string):string;
var   i,si: Integer;
      sl1,sl2 : TStringList;
      s1,s2:string;
begin
 Result:='';
 if inPath='' then exit;
 Result:=inPath;
 if CheckPath='' then exit;

 if SameText(ExtractFiledrive(Inpath),ExtractFiledrive(CheckPath))=false then exit;

 sl1:=TStringList.Create;
 sl2:=TStringList.Create;

 try
  FillStringList(sl1,EXtractFilePath(Inpath));
  FillStringList(sl2,EXtractFilePath(CheckPath));

  //........... Find Common ..................
  si:=0;
  for i:=0 to Min(sl1.Count-1,sl2.Count-1) do
      if (sl1[i]<>'') and (sl2[i]<>'') and sameText(sl1[i],sl2[i]) then
        inc(si) else
        Break;

  //.......................................
  if si>0 then
  begin

    s1:='';
    if si<(sl2.Count) then
      For i:=0 to (sl2.Count-si) do s1:=FixText+s1;


    s2:='';
    if si<sl1.Count then
      For i:=(sl1.Count-1) downto si do s2:=sl1[i]+s2;



     result:=s1+s2+ExtractFileName(Inpath);
  end else
  begin
     result:=Inpath;
  end;

   finally
      sl1.Free;
      sl2.Free;
   end;

end;

Function UnFixFilePath(Const Inpath,CheckPath:string):string;
  var i,si : Integer;
      sl1,sl2 : TStringList;
      s1,s2:string;
begin
 Result:='';
 if inPath='' then exit;
 Result:=inPath;
 if CheckPath='' then exit;

if ExtractFiledrive(Inpath)<>'' then exit;

  sl1:=TStringList.Create;
  sl2:=TStringList.Create;

try
  FillStringList(sl1,EXtractFilePath(Inpath));
  FillStringList(sl2,EXtractFilePath(CheckPath));

  //.............................
  si:=0;

  for i:=0 to sl1.Count-1 do
    if sameText(sl1[i],'..\') then
        inc(si) else
        Break;

  if  si>0 then
  begin

      s1:='';
      if si<sl2.Count then
        For i:=0 to (sl2.Count-si) do s1:=s1+sl2[i];

      s2:='';
      if si<sl1.Count then
        For i:=sl1.Count-1 downto si do s2:=sl1[i]+s2;   

      result:=s1+s2+ExtractFileName(Inpath);
  end else
  begin
     result:=CheckPath+Inpath;
  end;

   finally
      sl1.Free;
      sl2.Free;
   end;
  end;

function GetPointer(index: integer; memblock: THandle):pointer;
begin
  result:=pointer(longint(GlobalLock(memblock))+index);
end;

procedure ClipBoard_CopyStream(stream:TStream; const format:integer);
const
  max_write = $8000;    (* must obey ($10000 mod max_write = 0) for Delphi 1 *)
var
  size: longint;
  s: word;
  curpos: longint;
  Memblock: THandle;
  FClipboardWindow: THandle;
begin
  FClipboardWindow := Application.Handle;
  if FClipboardWindow = 0 then
    FClipboardWindow := AllocateHWnd(NIL);
  OpenClipboard(FClipboardWindow);

  stream.seek(0,0);
  size:=stream.size;
  stream.seek(0,0);
  MemBlock:=GlobalAlloc(gmem_moveable or gmem_zeroinit,size+1);
  curpos:=0;
  while curpos+1<size do begin
    s:=stream.read(getPointer(curpos,MemBlock)^,min(max_write,size-curpos));
    inc(curpos,s);
    GlobalUnLock(MemBlock);
    if s=0 then BREAK;
    end;
  char(getPointer(curpos,memblock)^):=#0;
  GlobalUnLock(MemBlock);
  EmptyClipBoard;
  SetClipBoardData(format,memblock);

  CloseClipboard;
  if FClipboardWindow<>Application.Handle then
    DeallocateHWnd(FClipboardWindow);
  end;

procedure ClipBoard_PasteStream(stream:TStream; const format:integer);
const
  max_read = $8000;   (* must obey ($10000 mod max_read = 0) for Delphi 1 *)
var
  size: longint;
  curpos: longint;
  Memblock: THandle;
  FClipboardWindow: THandle;
begin
  FClipboardWindow := Application.Handle;
  if FClipboardWindow = 0 then
    FClipboardWindow := AllocateHWnd(NIL);
  OpenClipboard(FClipboardWindow);

  stream.seek(0,0);
  MemBlock:=GetClipboardData(format);
  size:=GlobalSize(Memblock);
  curpos:=0;
  while curpos+1<size do
   begin
     stream.write(getPointer(curpos,MemBlock)^,min(max_read,size-curpos-1));
     inc(curpos,min(max_read,size-curpos-1));
     GlobalUnLock(MemBlock);
    end;

  CloseClipboard;
  if FClipboardWindow<>Application.Handle then
    DeallocateHWnd(FClipboardWindow);
  end;

Procedure ClipBoard_CopyComponent(aComp:TComponent);
begin
   if aComp=nil then exit;
   ClipBoard.SetComponent(aComp);
end;

function ClipBoard_PasteComponent:TComponent;
var
  Data: THandle;
  DataPtr: Pointer;
  MemStream: TMemoryStream;
  Reader: TReader;
begin
   Result := nil;
   with ClipBoard do
   begin
      Open;
      try
         Data := GetClipboardData(CF_COMPONENT);
         if Data = 0 then Exit;
         DataPtr := GlobalLock(Data);
         if DataPtr = nil then Exit;
         try
            MemStream:=TMemoryStream.Create;
            try
               MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
               MemStream.Position:=0;
               Reader:=TReader.Create(MemStream, 256);
               try
                  Result:=Reader.ReadRootComponent(nil);
               finally
                  Reader.Free;
               end;
            finally
               MemStream.Free;
            end;
         finally
            GlobalUnlock(Data);
         end;
      finally
        Close;
      end;
   end;
end;

Function  ClipBoard_HasComponent:Boolean;
 begin
   Result:=ClipBoard.HasFormat(CF_COMPONENT);
 end;

Function  ClipBoard_HasText:Boolean;
 begin
   Result:=( ClipBoard.HasFormat(CF_TEXT) or ClipBoard.HasFormat(CF_UNICODETEXT) );
 end;

procedure ClipBoard_CopyText(const Text: String);
var
  Len, wLen: Integer;
  hClip: THandle;
  pwStr: PWideChar;
begin
  with Clipboard do
  begin
    Open;
    try
      if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      begin
        Len := Length(Text) + 1;
        wLen := Len shl 1;
        hClip := GlobalAlloc(GMEM_MOVEABLE, wLen);
        try
          pwStr := PWideChar(GlobalLock(hClip));
          MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(Text), Len, pwStr, wLen);
          GlobalUnlock(hClip);
          SetAsHandle(CF_UNICODETEXT, hClip);
        except
          GlobalFree(hClip);
          raise;
        end;
      end else
        SetTextBuf(PChar(Text));
    finally
      Close;
    end;
  end;
end;

procedure ClipBoard_PasteText(var Text: String);
var
  Len, wLen: Integer;
  hClip: THandle;
  pwStr: PWideChar;
begin
  Text := '';
  with Clipboard do
  try
    Open;
    if HasFormat(CF_TEXT) or HasFormat(CF_UNICODETEXT) then
    begin
      if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      begin
        hClip := GetAsHandle(CF_UNICODETEXT);
        wlen := GlobalSize(hClip); // lstrlen and StrLen doesn't work
        pwStr := GlobalLock(hClip);
        try
          Len := (wLen div 2) - 1;
          SetLength(Text, Len);
          WideCharToMultiByte(CP_ACP, 0, pwStr, wlen, PChar(Text), Len, nil, nil);
        finally
          GlobalUnlock(hClip);
        end;
      end else begin // Win95
        hClip := GetAsHandle(CF_TEXT);
        Len := GlobalSize(hClip);
        SetLength(Text, Len);
        SetLength(Text, GetTextBuf(PChar(Text), Len));
      end;
    end;
  finally
    Close;
  end;
end;


function  ClipBoard_HasFormat(const FormatName: String): Boolean;
begin
  Result := Clipboard.HasFormat(RegisterClipboardFormat(PChar(FormatName)));
end;

procedure ClipBoard_Copy(const FormatName: String; Stream: TStream);
var mem: Cardinal;
    ptr: PChar;
    Size: Integer;
begin
  Size := Stream.Size;
  mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SizeOf(Size)+Size);
  if mem=0 then
    raise EBinClipboardError.Create(errNoMem);
  ptr := PChar(GlobalLock(mem));
  Move(Size,           ptr^,                SizeOf(Size));
  Stream.Position := 0;
  Stream.ReadBuffer((ptr+SizeOf(Size))^, Size);
  GlobalUnlock(mem);
  Clipboard.SetAsHandle(RegisterClipboardFormat(PChar(FormatName)),mem);
end;

function  ClipBoard_Paste(const FormatName: String; Stream: TStream): Boolean;
var cf: Cardinal;
    mem: Cardinal;
    ptr: PChar;
    Size, MemSize: Integer;
begin
  Result := False;
  cf := RegisterClipboardFormat(PChar(FormatName));
  if not Clipboard.HasFormat(cf) then exit;
  Clipboard.Open;
  try
    mem := Clipboard.GetAsHandle(cf);
    if mem=0 then
      raise EBinClipboardError.Create(errNoMem);
    MemSize := GlobalSize(mem);
    if MemSize<SizeOf(Size) then
      raise EBinClipboardError.Create(errInvalidData);
    ptr := PChar(GlobalLock(mem));
    try
      Move(ptr^,Size, SizeOf(Size));
      if MemSize<SizeOf(Size)+Size then
        raise EBinClipboardError.Create(errInvalidData);
      Stream.Size := Size;
      Stream.Position := 0;
      Stream.WriteBuffer((ptr+SizeOf(Size))^, Size);
      Stream.Position := 0;      
    finally
      GlobalUnlock(mem);
    end;
  finally
    Clipboard.Close;
  end;
  Result := True;
end;  
//==========================================================
function FontToString(aFont: TFont) : String;
begin
  // name, size, bold, italic, underline, strikethrough, colour
  if aFont=nil then Exit;

  Result := Format('%s,%d,%d%d%d%d,%s', [aFont.Name, aFont.Size,
                   Integer(fsBold in aFont.Style), Integer(fsItalic in aFont.Style),
                   Integer(fsUnderline in aFont.Style), Integer(fsStrikeOut in aFont.Style),
                   ColorToString(aFont.Color)]);
end;

procedure StringToFont(Str: String; aFont: TFont);
const  SEP = ',';
var
  i: Integer;
begin
  if aFont=nil then Exit;
  
  // name
  i := Pos(SEP, Str);
  if i > 0 then
   begin
     aFont.Name := Copy(Str, 1, i-1);
     Delete(Str, 1, i);
   end;

  // size
  i := Pos(SEP, Str);
  if i > 0 then
   begin
     aFont.Size := StrToInt(Copy(Str, 1, i-1));
     Delete(Str, 1, i);
   end;   

  // bold, italic, underline, strikethrough
  if Pos(SEP, Str) = 5 then
    begin
      aFont.Style := [];
      if Str[1] = '1' then aFont.Style := aFont.Style + [fsBold];
      if Str[2] = '1' then aFont.Style := aFont.Style + [fsItalic];
      if Str[3] = '1' then aFont.Style := aFont.Style + [fsUnderline];
      if Str[4] = '1' then aFont.Style := aFont.Style + [fsStrikeOut];
      Delete(Str, 1, 5);
  end;

  aFont.Color := StringToColor(Str);
end;

function  RectToString(aRect: TRect) : String;
begin
  Result := Format('%d,%d,%d,%d', [aRect.Left, aRect.Top, aRect.Right, aRect.Bottom]);
end;

function StringToRect(Str: String): TRect;
 const  SEP = ',';
 var    i: Integer;
begin
   Result:=Rect(0,0,0,0);

  // left
  i := Pos(SEP, Str);
  if i > 0 then
   begin
     Result.Left := StrToInt(Copy(Str, 1, i-1));
     Delete(Str, 1, i);
   end;

  // top
  i := Pos(SEP, Str);
  if i > 0 then
   begin
     Result.Top := StrToInt(Copy(Str, 1, i-1));
     Delete(Str, 1, i);
   end;

  // width
  i := Pos(SEP, Str);
  if i > 0 then
   begin
    Result.Right := StrToInt(Copy(Str, 1, i-1));
    Delete(Str, 1, i);
   end;

  // height
  Result.Bottom := StrToInt(Str);
end;

function UniColorToString(Color: Cardinal): string;
begin
    FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
end;

function UniStringToColor(const S: string): Cardinal;
 var col:Tcolor;
begin
    if IdentToColor(S,Longint(col)) then
      Result := Cardinal(col) else
      Result := Cardinal(StrToInt(S));
end;

Function  ColorRGB(Const R,G,B:Byte):TColor;
 begin
   Result:=RGB(R,G,B);
 end;

Function  ColorR(Const aColor:TColor):Byte;
begin
   Result:=GetRValue(aColor);
end;
Function  ColorG(Const aColor:TColor):Byte;
begin
   Result:=GetGValue(aColor);
end;
Function  ColorB(Const aColor:TColor):Byte;
begin
   Result:=GetBValue(aColor);
end;

Function  ColorToStr(Const aColor:TColor):string;
 begin
   Result:=UniColorToString(aColor);
 end;
Function  StrToColor(Const aStr:string):TColor;
begin
   Result:=UniStringToColor(aStr);
 end;

Function  ColorRandom:TColor;
begin
   Result:=RGB(Random(255),Random(255),Random(255));
 end;

procedure FindFilesToDir(AStrings : TStrings; const APath : string;  const WithExt,WithPath: boolean);
 var
   vFound,p:integer;
   vSearchRec:TSearchRec;   fn: string;
 begin
   if AStrings=nil then exit;
	AStrings.Clear;	vFound := FindFirst(APath, faAnyFile, vSearchRec);	while vFound = 0 do   begin		p := Pos('.',vSearchRec.Name);		if p > 1 then                  begin                    fn := vSearchRec.Name;                    if not WithExt then fn := ChangeFileExt( fn, '' );                    if WithPath then  fn := ExtractFilePath( APath ) + fn;                    AStrings.Add( fn );                  end;		vFound := FindNext( vSearchRec );    end;        FindClose( vSearchRec );
 end;

//=========================================================================
 procedure Init_OpSystem;
  begin
   if (OpSystem=nil) then
     begin
      OpSystem:=TOpSystem.create;
     end;
  end;

 procedure Free_OpSystem;
 begin
  if NOT (OpSystem=nil) then
   begin
   OpSystem.free;
   end;
  end;


initialization
  Init_OpSystem;
finalization
  Free_OpSystem;

end.
