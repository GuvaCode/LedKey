
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit vd_core;

{$I vd.inc}

interface

uses
  SysUtils, Classes, vd_masks, vd_resource;

type
  TSign = array[0..6] of char;

const
  VFSSignature: TSign = 'VFILEST';
  EmptyBlock: longword = $FFFFFFFC;
  EndBlock: longword = $FFFFFFFE;
  BadBlock: longword = $FFFFFFFA;
  BlockSize = 4096;
  DirDivider = '\/';
  EraseSymbol = #254;
  deSelf = 0;
  deParent = 1;

type

  TFileSystem = class;

  TFAT = array of longword;

  TFileEntry = record
    Name: string;
    Size: longword;
    Attr: longword;
    Date: TDateTime;
    Link: longword;
    Res: array [0..15] of byte;
  end;

  TFileEntryArray = array of TFileEntry;

  TDir = class;

  TFile = class
  private
    FEntry: TFileEntry;
    FPosition: longword;
    FMode: word;
    FDir: TDir;
    FIndex: longword;
  public
    constructor CreateEmpty(const AFileName: string; ADir: TDir; AMode: word);
    constructor CreateOpen(const AFileName: string; ADir: TDir; AMode: word);
    destructor Destroy; override;
    { access }
    function Size: longword;
    function Date: TDateTime;
  end;

  TDirEntry = record
    Count: longword;
    Files: TFileEntryArray;
  end;

  TDir = class
  private
    FEntry: TDirEntry;
    FFileSystem: TFileSystem;
  public
    constructor CreateEmpty(AFileSystem: TFileSystem; ParentLink, CurLink: longword);
    constructor CreateOpen(AFileSystem: TFileSystem; Link: longword);
    function GetSize: longword;
    procedure SaveDir;
    function AddEntry(AName: string; AAttr: longword; ALink: longword): boolean;
    function CreateDirEntry(Parent: longword): TDir;
    function OpenDirEntry(DirLink: longword): TDir;
    function DirExists(AName: string): boolean;
  end;

  TFileSystem = class
  private
    Sign: TSign;
    FATSize: longword;
    FATPos: longword;
    FAT: TFAT;
    RootLink: longword;
    Reserved: array [1..1020] of byte;
    ReadOnly: longbool;
    Compress: longbool;
    FRoot: TDir;
    FStream: TStream;
    FDirs: TStringList;
    FFiles: TStringList;
    FMode: word;
    function FATOffset(): longword;
    function DataOffset(): longword;
    function ReadOnlyOffset(): longword;
    procedure SaveFAT(const Index, Value: longword);
  public
    constructor CreateEmpty(Stream: TStream; AReadOnly: boolean = False; ACompress: boolean = False; AMaxSize: longword = 16384);
    constructor CreateOpen(Stream: TStream; const Mode: word);
    destructor Destroy;
    function FindNextLink(const ALink: longword): longword;
    function LoadData(Link: longword; const Buf: PByteArray; const Pos, Size: longword): longword;
    function SaveData(Link: longword; const Buf: PByteArray; const Pos, Size: longword): longword;
    procedure EraseData(Link: longword);
    function GetDir(CurDir: TDir; Path, CurPath: string): TDir;
    procedure CreateDir(const ADirName: string);
    procedure ForceDir(ADirName: string);
    function CreateFile(const AFileName: string): TFile;
    function OpenFileRead(const AFileName: string): TFile;
    function OpenFileWrite(const AFileName: string): TFile;
    procedure CloseFile(AFile: TFile);
    procedure EraseFile(AFile: TFile);
    function FileExists(const AFileName: string): boolean;
    function DirectoryExists(const AFileName: string): boolean;
    function Write(const F: TFile; Buf: Pointer; const ASize: longword): longword;
    function Read(const F: TFile; Buf: Pointer; const ASize: longword): longword;
    procedure SetReadOnly(Value: boolean);
    function FindFirst(const Path: string; Attr: integer; var F: TSearchRec): integer;
    function FindNext(var F: TSearchRec): integer;
    procedure FindClose(var F: TSearchRec);
    property Root: TDir read FRoot;
    property Compressed: longbool read Compress;
    property IsReadOnly: longbool read ReadOnly;
  end;

  TFileSystemStream = class(TStream)
  private
    FFileSystem: TFileSystem;
    FFileHandle: TFile;
  protected
    procedure SetSize(NewSize: longint); override;
    procedure Setsize(const NewSize: int64); override;
  public
    constructor Create(const AFileSystem: TFileSystem; const FileName: string; Mode: word); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
    property FileHandle: TFile read FFileHandle;
  end;

implementation {===============================================================}

procedure Log(AText: string);
begin
  { error log }
end;

function GetFirstDir(var Path: string): string;
var
  i: byte;
  CopyS: string;
begin
  Result := '';
  if Path = '' then
    Exit;
  if Pos(Path[1], DirDivider) > 0 then
    Delete(Path, 1, 1); // remove root

  CopyS := Path;
  for i := 1 to Length(CopyS) do
  begin
    Delete(Path, 1, 1);
    if Pos(CopyS[i], DirDivider) > 0 then
      Break;
    Result := Result + CopyS[i];
  end;
end;

function GetPath(const FileName: string): string;
var
  I: integer;
begin
  I := LastDelimiter(DirDivider, FileName);
  Result := Copy(FileName, 1, I);
end;

function GetName(const FileName: string): string;
var
  I: integer;
begin
  I := LastDelimiter(DirDivider, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

{ TFile }

constructor TFile.CreateEmpty(const AFileName: string; ADir: TDir; AMode: word);
var
  CurLink: longword;
begin
  inherited Create;

  FDir := ADir;
  CurLink := FDir.FFileSystem.FindNextLink(EmptyBlock);
  FDir.FFileSystem.SaveFAT(CurLink, EndBlock);
  if FDir.AddEntry(AFileName, 0, CurLink) then
  begin
    FIndex := ADir.FEntry.Count - 1;
    FEntry := ADir.FEntry.Files[FIndex];
    FEntry.Date := Now;
    FEntry.Size := 0;
    FMode := AMode;
    FPosition := 0;

    FDir.FEntry.Files[FIndex] := FEntry;
    FDir.SaveDir;
  end
  else
  begin
    Log('can''t add new entry');
    FPosition := BadBlock;
  end;
end;

constructor TFile.CreateOpen(const AFileName: string; ADir: TDir; AMode: word);
var
  i: integer;
begin
  inherited Create;
  FDir := ADir;
  for i := 0 to FDir.FEntry.Count - 1 do
    if CompareText(FDir.FEntry.Files[i].Name, AFileName) = 0 then
    begin
      FIndex := i;
      FEntry := ADir.FEntry.Files[FIndex];
      FMode := AMode;
      FPosition := 0;
      Exit;
    end;
  //  Log('Can''t found file ' + AFileName);
  FPosition := BadBlock;
  FDir := nil;
end;

destructor TFile.Destroy;
begin
  if (FDir <> nil) and (FPosition <> BadBlock) then
  begin
    FDir.FEntry.Files[FIndex] := FEntry;
    FDir.SaveDir;
  end;
  inherited;
end;

function TFile.Size: longword;
begin
  Result := FEntry.Size;
end;

function TFile.Date: TDateTime;
begin
  Result := FEntry.Date;
end;

{ TDir ========================================================================}

constructor TDir.CreateEmpty(AFileSystem: TFileSystem; ParentLink, CurLink: longword);
begin
  inherited Create;
  FFileSystem := AFileSystem;

  FEntry.Count := 2;
  SetLength(FEntry.Files, FEntry.Count);
  with FEntry.Files[deSelf] do
  begin
    Name := '.';
    Size := 0;
    Date := Now;
    Attr := faDirectory;
    Link := CurLink;
  end;
  with FEntry.Files[deParent] do
  begin
    Name := '..';
    Size := 0;
    Date := Now;
    Attr := faDirectory;
    Link := ParentLink;
  end;
  SaveDir;
end;

constructor TDir.CreateOpen(AFileSystem: TFileSystem; Link: longword);
var
  i: integer;
  M: TMemoryStream;
  S: longword;
begin
  inherited Create;
  FFileSystem := AFileSystem;
  M := TMemoryStream.Create;

  FFileSystem.LoadData(Link, PByteArray(@S), 0, SizeOf(S));
  M.Size := S;
  FFileSystem.LoadData(Link, PByteArray(M.Memory), SizeOf(S), M.Size);
  M.Position := 0;

  FEntry.Count := ReadLongword(M);
  SetLength(FEntry.Files, FEntry.Count);
  for i := 0 to FEntry.Count - 1 do
    with FEntry do
    begin
      Files[i].Name := ReadString(M);
      Files[i].Size := ReadLongword(M);
      Files[i].Attr := ReadLongword(M);
      Files[i].Date := ReadDouble(M);
      Files[i].Link := ReadLongword(M);
      ReadBuf(M, @Files[i].Res, SizeOf(Files[i].Res));
    end;
  M.Free;
end;

function TDir.GetSize: longword;
begin
  Result := SizeOf(FEntry.Count) + (FEntry.Count * SizeOf(TFileEntry));
end;

procedure TDir.SaveDir;
var
  i: integer;
  M: TMemoryStream;
  S: longword;
begin
  M := TMemoryStream.Create;
  WriteLongword(M, FEntry.Count);
  for i := 0 to FEntry.Count - 1 do
    with FEntry do
    begin
      WriteString(M, Files[i].Name);
      WriteLongword(M, Files[i].Size);
      WriteLongword(M, Files[i].Attr);
      WriteDouble(M, Files[i].Date);
      WriteLongword(M, Files[i].Link);
      WriteBuf(M, @Files[i].Res, SizeOf(Files[i].Res));
    end;
  S := M.Size;
  FFileSystem.SaveData(FEntry.Files[deSelf].Link, PByteArray(@S), 0, SizeOf(S));
  FFileSystem.SaveData(FEntry.Files[deSelf].Link, PByteArray(M.Memory), SizeOf(S), M.Size);
  M.Free;
end;

function TDir.AddEntry(AName: string; AAttr, ALink: longword): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FEntry.Count - 1 do
    if CompareText(FEntry.Files[i].Name, AName) = 0 then
    begin
      Log('Entry ' + AName + ' alredy exists');
      Result := False;
      Exit;
    end;

  Result := True;

  FEntry.Count := FEntry.Count + 1;
  SetLength(FEntry.Files, FEntry.Count);
  with FEntry.Files[FEntry.Count - 1] do
  begin
    Name := AName;
    Attr := AAttr;
    Link := ALink;
  end;
  SaveDir;
end;

{ directories }

function TDir.CreateDirEntry(Parent: longword): TDir;
var
  CurLink: longword;
begin
  CurLink := FFileSystem.FindNextLink(EmptyBlock);
  FFileSystem.SaveFAT(CurLink, EndBlock);

  Result := TDir.CreateEmpty(FFileSystem, Parent, CurLink);
end;

function TDir.OpenDirEntry(DirLink: longword): TDir;
begin
  Result := TDir.CreateOpen(FFileSystem, DirLink);
end;

function TDir.DirExists(AName: string): boolean;
var
  i: integer;
begin
  for i := 0 to FEntry.Count - 1 do
    if CompareText(FEntry.Files[i].Name, AName) = 0 then
    begin
      Log('Entry ' + AName + ' alredy exists');
      Result := True;
      Exit;
    end;
  Result := False;
end;

{ TFileSystem =================================================================}

constructor TFileSystem.CreateEmpty(Stream: TStream; AReadOnly: boolean = False; ACompress: boolean = False; AMaxSize: longword = 16384);
var
  i: integer;
begin
  inherited Create;
  FStream := Stream;
  FMode := fmCreate or fmOpenRead or fmOpenWrite;

  FDirs := TStringList.Create;
  FFiles := TStringList.Create;
  FFiles.CaseSensitive := False;
  FDirs.CaseSensitive := False;


  { Fill empty storage }
  Sign := VFSSignature;
  FATSize := AMaxSize;
  FATPos := FATOffset;
  SetLength(FAT, FATSize);
  for i := 0 to FATSize - 1 do
    FAT[i] := EmptyBlock;
  RootLink := 0;
  FAT[RootLink] := EndBlock;
  FillChar(Reserved, SizeOf(Reserved), 0);
  ReadOnly := AReadOnly;
  Compress := ACompress;

  { Save to Stream}
  WriteBuf(Stream, @Sign, SizeOf(Sign));
  WriteLongword(Stream, FATSize);
  WriteLongword(Stream, FATPos);
  WriteLongword(Stream, RootLink);
  WriteLongword(Stream, longword(ReadOnly));
  WriteLongword(Stream, longword(Compress));
  WriteBuf(Stream, @Reserved[1], SizeOf(Reserved));
  for i := 0 to FATSize - 1 do
    WriteLongword(Stream, FAT[i]);

  { Create Root }
  FRoot := TDir.CreateEmpty(Self, 0, 0);
  RootLink := FRoot.FEntry.Files[deSelf].Link;
  { Save Root }
  FRoot.SaveDir;
end;

constructor TFileSystem.CreateOpen(Stream: TStream; const Mode: word);
var
  i: integer;
begin
  inherited Create;
  FStream := Stream;
  FMode := Mode;

  FDirs := TStringList.Create;
  FFiles := TStringList.Create;
  FFiles.CaseSensitive := False;
  FDirs.CaseSensitive := False;

  { Load empty Stream}
  ReadBuf(Stream, @Sign, SizeOf(Sign));
  if Sign = VFSSignature then
  begin
    FATSize := ReadLongword(Stream);
    FATPos := ReadLongword(Stream);
    RootLink := ReadLongword(Stream);
    ReadOnly := longbool(ReadLongword(Stream));
    Compress := longbool(ReadLongword(Stream));
    ReadBuf(Stream, @Reserved[1], SizeOf(Reserved));
    SetLength(FAT, FATSize);
    for i := 0 to FATSize - 1 do
      FAT[i] := ReadLongword(Stream);
    { Open Root }
    FRoot := TDir.CreateOpen(Self, RootLink);
  end
  else
  begin
    Log('Unsupported file system ');
  end;

  if ReadOnly then
    FMode := fmOpenRead;
end;

procedure TFileSystem.SetReadOnly(Value: boolean);
var
  Save: boolean;
begin
  Save := ReadOnly;
  try
    ReadOnly := Value;
    FStream.Position := ReadOnlyOffset;
    WriteLongword(FStream, longword(ReadOnly));
  except
    ReadOnly := Save;
  end;
end;

destructor TFileSystem.Destroy;
begin
  FRoot.Free;
  inherited;
end;

function TFileSystem.FATOffset: longword;
begin
  Result := Length(Sign) + SizeOf(FATSize) + SizeOf(FATPos) + SizeOf(RootLink) + SizeOf(ReadOnly) + SizeOf(Compress) + SizeOf(Reserved);
end;

function TFileSystem.ReadOnlyOffset: longword;
begin
  Result := Length(Sign) + SizeOf(FATSize) + SizeOf(FATPos) + SizeOf(RootLink);
end;

function TFileSystem.DataOffset: longword;
begin
  Result := SizeOf(Sign) + SizeOf(FATSize) + SizeOf(FATPos) + SizeOf(RootLink) + SizeOf(ReadOnly) + SizeOf(Compress) +
    SizeOf(Reserved) + FATSize * SizeOf(FAT[0]);
end;

procedure TFileSystem.SaveFAT(const Index, Value: longword);
begin
  FAT[Index] := Value;
  FStream.Position := FATOffset + (Index * SizeOf(FAT[Index]));
  WriteLongword(FStream, Value);
end;

function TFileSystem.FindNextLink(const ALink: longword): longword;
var
  i: integer;
begin
  Result := BadBlock;
  if ALink = EmptyBlock then
  begin
    { Find first empty link }
    for i := 0 to High(FAT) do
      if FAT[i] = EmptyBlock then
      begin
        Result := i;
        Exit;
      end;
  end
  else
  begin
    { Fint first empty from current }
    for i := ALink to High(FAT) do
      if FAT[i] = EmptyBlock then
      begin
        Result := i;
        Exit;
      end;
    { find from start }
    for i := 0 to ALink do
      if FAT[i] = EmptyBlock then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TFileSystem.SaveData(Link: longword; const Buf: PByteArray; const Pos, Size: longword): longword;
var
  CurLinkNum, CurLink: longword;
  CurPos, CurSize: longword;
  Stride: longword;
  i: integer;
  ZeroBlock: PByteArray;
begin
  if Link = EmptyBlock then
  begin
    { Return new link, if we start write data first time }
    Result := FindNextLink(Link);
    SaveFAT(Result, EndBlock);
  end
  else
    Result := Link;

  CurLink := Result;
  { Link search }
  CurLinkNum := 1;
  while CurLinkNum * BlockSize <= Pos do
  begin
    { Find next block }
    if FAT[CurLink] = EmptyBlock then
    begin
      { Return new link, if we start write data first time }
      SaveFAT(CurLink, FindNextLink(CurLink));
      CurLink := FAT[CurLink];
      SaveFAT(CurLink, EndBlock);
    end
    else
    if FAT[CurLink] = EndBlock then
    begin
      SaveFAT(CurLink, FindNextLink(CurLink));
      CurLink := FAT[CurLink];
      SaveFAT(CurLink, EndBlock);
    end
    else
    begin
      CurLink := FAT[CurLink];
    end;
    Inc(CurLinkNum);
  end;
  { Write buffer }
  CurPos := Pos;
  CurSize := Size;
  while CurSize > 0 do
  begin
    { Write one block }
    Stride := (CurPos mod BlockSize);
    { write current block }
    FStream.Position := DataOffset + (CurLink * BlockSize) + Stride;
    if BlockSize - Stride < CurSize then
    begin
      FStream.Write(Buf[CurPos - Pos], BlockSize - Stride);
    end
    else
    begin
      { finish }
      FStream.Write(Buf[CurPos - Pos], CurSize);
      { write 0 if is a last block }
      if FStream.Position = FStream.Size then
      begin
        GetMem(ZeroBlock, BlockSize - CurSize);
        FillChar(ZeroBlock^, BlockSize - CurSize, byte(' '));
        FStream.Write(ZeroBlock[0], BlockSize - CurSize);
        FreeMem(ZeroBlock, BlockSize - CurSize);
      end;
      Break;
    end;

    CurSize := CurSize - (BlockSize - Stride);
    CurPos := CurPos + (BlockSize - Stride);

    { Find next block }
    if FAT[CurLink] = EmptyBlock then
    begin
      { Return new link, if we start write data first time }
      SaveFAT(CurLink, FindNextLink(CurLink));
      CurLink := FAT[CurLink];
      SaveFAT(CurLink, EndBlock);
    end
    else
    if FAT[CurLink] = EndBlock then
    begin
      SaveFAT(CurLink, FindNextLink(CurLink));
      CurLink := FAT[CurLink];
      SaveFAT(CurLink, EndBlock);
    end
    else
    begin
      CurLink := FAT[CurLink];
    end;
  end;
end;

function TFileSystem.LoadData(Link: longword; const Buf: PByteArray; const Pos, Size: longword): longword;
var
  CurLinkNum, CurLink: longword;
  CurPos, CurSize: longword;
  Stride: longword;
  i: integer;
  S: longword;
begin
  Result := 0;
  if Link = EmptyBlock then
    Exit;

  CurLink := Link;
  { Link search }
  CurLinkNum := 1;
  while CurLinkNum * BlockSize <= Pos do
  begin
    CurLink := FAT[CurLink];
    Inc(CurLinkNum);
  end;
  { Read buffer }
  CurPos := Pos;
  CurSize := Size;
  while CurSize > 0 do
  begin
    { Write one block }
    Stride := (CurPos mod BlockSize);
    { write current block }
    FStream.Position := DataOffset + (CurLink * BlockSize) + Stride;
    if BlockSize - Stride < CurSize then
    begin
      FStream.Read(Buf[CurPos - Pos], BlockSize - Stride);
    end
    else
    begin
      { finish }
      S := FStream.Read(Buf[CurPos - Pos], CurSize);
      if S <> CurSize then
      begin
        Log('Read error');
        Result := S;
        Exit;
      end;
      CurSize := 0;
      Break;
    end;

    CurSize := CurSize - (BlockSize - Stride);
    CurPos := CurPos + (BlockSize - Stride);

    { Break }
    if FAT[CurLink] = EndBlock then
    begin
      Break;
    end;
    if FAT[CurLink] = EmptyBlock then
    begin
      Break;
    end;
    { Find next block }
    CurLink := FAT[CurLink];
  end;
  Result := Size - CurSize;
end;

procedure TFileSystem.EraseData(Link: longword);
var
  CurLink: integer;
begin
  CurLink := Link;
  if FAT[CurLink] = EmptyBlock then
    Exit;

  while (FAT[CurLink] <> EndBlock) do
  begin
    CurLink := FAT[CurLink];
    SaveFAT(CurLink, EmptyBlock);
  end;
end;

{ Low level directory }

function TFileSystem.GetDir(CurDir: TDir; Path, CurPath: string): TDir;
var
  SubDir: TDir;
  CurDirName: string;
  i: integer;
begin
  Result := nil;
  if Path = '' then
  begin
    Result := FRoot;
    Exit;
  end;
  if FDirs.IndexOf(Path) >= 0 then
  begin
    Result := TDir(FDirs.Objects[FDirs.IndexOf(Path)]);
    Exit;
  end;

  CurDirName := GetFirstDir(CurPath);
  for i := 0 to CurDir.FEntry.Count - 1 do
  begin
    if CompareText(CurDir.FEntry.Files[i].Name, CurDirName) = 0 then
    begin
      if CurDir.FEntry.Files[i].Attr and faDirectory = faDirectory then
      begin
        if CurPath = '' then
        begin
          { End search }
          Result := CurDir.OpenDirEntry(CurDir.FEntry.Files[i].Link);
          FDirs.AddObject(Path, Result);
          Break;
        end
        else
        begin
          { Next level }
          SubDir := CurDir.OpenDirEntry(CurDir.FEntry.Files[i].Link);
          Result := GetDir(SubDir, Path, CurPath);
          SubDir.Free;
        end;
      end;
    end;
  end;
end;

{ High-level routines =========================================================}

procedure TFileSystem.CreateDir(const ADirName: string);
var
  ParentDir, Dir: TDir;
  DirName, ParentDirName: string;
begin
  if fmCreate and FMode = 0 then
  begin
    //    Log('Can''t create on read-only store');
    Exit;
  end;

  ParentDirName := GetPath(ADirName);
  DirName := GetName(ADirName);

  { Check if exists }
  ParentDir := GetDir(FRoot, ADirName, ADirName);
  if ParentDir <> nil then
  begin
    //    Log('Dir ' + ADirName + ' already exists');
    Exit;
  end;

  { Add }
  ParentDir := GetDir(FRoot, ParentDirName, ParentDirName);
  if ParentDir <> nil then
  begin
    Dir := ParentDir.CreateDirEntry(ParentDir.FEntry.Files[deSelf].Link);
    Dir.SaveDir;

    ParentDir.AddEntry(DirName, faDirectory, Dir.FEntry.Files[deSelf].Link);
  end;
end;

procedure TFileSystem.ForceDir(ADirName: string);
var
  Dir: string;
begin
  if ADirName = '' then
    Exit;
  if fmCreate and FMode = 0 then
  begin
    //    Log('Can''t create on read-only store');
    Exit;
  end;

  Dir := GetFirstDir(ADirName);
  while Dir <> '' do
  begin
    CreateDir(Dir);
    Dir := GetFirstDir(ADirName);
  end;
end;

procedure TFileSystem.EraseFile(AFile: TFile);
begin
  if AFile <> nil then
  begin
    AFile.FEntry.Name[1] := EraseSymbol;
    CloseFile(AFile);
  end;
end;

function TFileSystem.CreateFile(const AFileName: string): TFile;
var
  Dir: TDir;
  Idx: integer;
begin
  Result := nil;
  Idx := FFiles.IndexOfName(AFileName);
  if Idx >= 0 then
  begin
    if TFile(FFiles.Objects[Idx]).FMode <> fmOpenRead then
    begin
      Log('Can''t create opened file');
      Result := nil;
      Exit;
    end;
  end;

  if fmCreate and FMode = 0 then
  begin
    //    Log('Can''t create on read-only store');
    Exit;
  end;

  ForceDir(GetPath(AFileName));

  Dir := GetDir(FRoot, GetPath(AFileName), GetPath(AFileName));
  if Dir <> nil then
  begin
    { erase first }
    Result := OpenFileRead(AFileName);
    if Result <> nil then
    begin
      EraseFile(Result);
    end;
    { crate new }
    Result := TFile.CreateEmpty(GetName(AFileName), Dir, fmCreate);
    if (Result.FPosition <> BadBlock) then
      FFiles.AddObject(AFileName, Result)
    else
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

function TFileSystem.FileExists(const AFileName: string): boolean;
var
  F: TFile;
  Idx: integer;
  Dir: TDir;
begin
  Result := False;
  Idx := FFiles.IndexOfName(AFileName);
  if Idx >= 0 then
  begin
    Result := True;
    Exit;
  end;

  Dir := GetDir(FRoot, GetPath(AFileName), GetPath(AFileName));
  if Dir <> nil then
  begin
    F := TFile.CreateOpen(GetName(AFileName), Dir, fmOpenRead);
    Result := (F.FPosition <> BadBlock) and (F.FEntry.Attr and faDirectory = 0);
    F.Free;
  end;
end;

function TFileSystem.DirectoryExists(const AFileName: string): boolean;
begin
  Result := GetDir(FRoot, AFileName, AFileName) <> nil;
end;

function TFileSystem.OpenFileRead(const AFileName: string): TFile;
var
  Idx: integer;
  Dir: TDir;
begin
  Result := nil;
  Idx := FFiles.IndexOfName(AFileName);
  if Idx >= 0 then
  begin
    if TFile(FFiles.Objects[Idx]).FMode <> fmOpenRead then
    begin
      Log('Can''t create opened file');
      Result := nil;
      Exit;
    end;
  end;

  Dir := GetDir(FRoot, GetPath(AFileName), GetPath(AFileName));
  if Dir <> nil then
  begin
    Result := TFile.CreateOpen(GetName(AFileName), Dir, fmOpenRead);
    if (Result.FPosition = BadBlock) or (Result.FEntry.Attr and faDirectory = faDirectory) then
    begin
      Result.Free;
      Result := nil;
    end
    else
      FFiles.AddObject(AFileName, Result);
  end;
end;

function TFileSystem.OpenFileWrite(const AFileName: string): TFile;
var
  Dir: TDir;
  Idx: integer;
begin
  Result := nil;
  if fmOpenWrite and FMode = 0 then
  begin
    Log('Can''t create opened file');
    Exit;
  end;
  Idx := FFiles.IndexOfName(AFileName);
  if Idx >= 0 then
  begin
    if TFile(FFiles.Objects[Idx]).FMode <> fmOpenRead then
    begin
      //      Log('Can''t opwn for write on read-only store');
      Result := nil;
      Exit;
    end;
  end;

  Dir := GetDir(FRoot, GetPath(AFileName), GetPath(AFileName));
  if Dir <> nil then
  begin
    Result := TFile.CreateOpen(GetName(AFileName), Dir, fmOpenWrite);
    if (Result.FPosition = BadBlock) or (Result.FEntry.Attr and faDirectory = faDirectory) then
    begin
      Result.Free;
      Result := nil;
    end
    else
      FFiles.AddObject(AFileName, Result);
  end;
end;

procedure TFileSystem.CloseFile(AFile: TFile);
var
  Idx: integer;
begin
  Idx := FFiles.IndexOfObject(AFile);
  if Idx >= 0 then
  begin
    FFiles.Delete(Idx);
    AFile.Free;
  end;
end;


function TFileSystem.Write(const F: TFile; Buf: Pointer; const ASize: longword): longword;
var
  ResLink: longword;
begin
  Result := 0;
  if (fmOpenWrite and FMode = 0) and (fmCreate and FMode = 0) then
  begin
    //    Log('Can''t write on read-only store');
    Exit;
  end;

  if F <> nil then
    with F do
    begin
      ResLink := SaveData(FEntry.Link, Buf, FPosition, ASize);
      if (ResLink <> BadBlock) and (ResLink <> EmptyBlock) then
      begin
        FPosition := FPosition + ASize;
        if FPosition > FEntry.Size then
          FEntry.Size := FPosition;
        Result := ASize;
      end;
    end;
end;

function TFileSystem.Read(const F: TFile; Buf: Pointer; const ASize: longword): longword;
begin
  Result := 0;
  if F <> nil then
    with F do
    begin
      if FPosition > FEntry.Size then
      begin
        Log('File ' + F.FEntry.Name + ' read error');
        Exit;
      end;
      Result := ASize;
      if FPosition + Result > FEntry.Size then
        Result := FEntry.Size - FPosition;
      Result := LoadData(FEntry.Link, Buf, FPosition, Result);
      FPosition := FPosition + Result;
    end;
end;


function TFileSystem.FindFirst(const Path: string; Attr: integer; var F: TSearchRec): integer;
var
  Dir: TDir;
  i: integer;
  Mask: string;
begin
  F.ExcludeAttr := 0;
  Dir := GetDir(FRoot, GetPath(Path), GetPath(Path));
  Mask := GetName(Path);
  if Dir <> nil then
  begin
    for i := 0 to Dir.FEntry.Count - 1 do
      if (Length(Dir.FEntry.Files[i].Name) > 1) and (Dir.FEntry.Files[i].Name[1] <> EraseSymbol) and
        (vs_matchesMask(Dir.FEntry.Files[i].Name, Mask)) then
      begin
        if F.ExcludeAttr = 0 then
        begin
          F.Time := DateTimeToFileDate(Dir.FEntry.Files[i].Date);
          F.Size := Dir.FEntry.Files[i].Size;
          F.Attr := Dir.FEntry.Files[i].Attr;
          F.Name := Dir.FEntry.Files[i].Name;

          F.ExcludeAttr := ptrint(TList.Create);
          TList(ptrint(F.ExcludeAttr)).Add(Dir);
        end
        else
        begin
          TList(ptrint(F.ExcludeAttr)).Add(Pointer(i));
        end;
      end;
    Result := 0;
  end
  else
    Result := 1;
end;

function TFileSystem.FindNext(var F: TSearchRec): integer;
var
  i: integer;
  Dir: TDir;
begin
  if (F.ExcludeAttr <> 0) and (TList(ptrint(F.ExcludeAttr)).Count > 1) then
  begin
    Dir :=TDir( TList(ptrint(F.ExcludeAttr))[0] );
    with Dir.FEntry.Files[ ptrint(TList(ptrint(F.ExcludeAttr))[1])] do
    begin
      F.Time := DateTimeToFileDate(Date);
      F.Size := Size;
      F.Attr := Attr;
      F.Name := Name;
    end;
    TList(PtrInt(F.ExcludeAttr)).Delete(1);
    Result := 0;
  end
  else
    Result := 1;
end;

procedure TFileSystem.FindClose(var F: TSearchRec);
begin
  if F.ExcludeAttr <> 0 then
  begin
    TList(PtrInt(F.ExcludeAttr)).Free;
    F.ExcludeAttr := 0;
  end;
end;

{ TFileSystemStream ===========================================================}

constructor TFileSystemStream.Create(const AFileSystem: TFileSystem; const FileName: string; Mode: word);
begin
  inherited Create;
  FFileSystem := AFileSystem;

  if fmCreate and Mode = fmCreate then
  begin
    FFileHandle := FFileSystem.CreateFile(FileName);
  end
  else
  if fmOpenWrite and Mode = fmOpenWrite then
  begin
    FFileHandle := FFileSystem.OpenFileWrite(FileName);
  end
  else
  begin
    FFileHandle := FFileSystem.OpenFileRead(FileName);
  end;
end;

destructor TFileSystemStream.Destroy;
begin
  if FFileHandle <> nil then
  begin
    FFileSystem.CloseFile(FFileHandle);
  end;
  inherited;
end;

procedure TFileSystemStream.SetSize(const NewSize: int64);
begin
  { Do nothing }
end;

procedure TFileSystemStream.SetSize(NewSize: integer);
begin
  SetSize(int64(NewSize));
end;

function TFileSystemStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  case Origin of
    soBeginning:
    begin
      TFile(FFileHandle).FPosition := Offset;
    end;
    soCurrent:
    begin
      TFile(FFileHandle).FPosition := TFile(FFileHandle).FPosition + Offset;
    end;
    soEnd:
    begin
      TFile(FFileHandle).FPosition := TFile(FFileHandle).FEntry.Size - Offset;
    end;
  end;
  Result := TFile(FFileHandle).FPosition;
end;


function TFileSystemStream.Read(var Buffer; Count: integer): longint;
begin
  if FFileHandle.FPosition + Count > FFileHandle.Size then
    Count := FFileHandle.Size - FFileHandle.FPosition;
  Result := FFileSystem.Read(FFileHandle, @Buffer, Count);
end;

function TFileSystemStream.Write(const Buffer; Count: integer): longint;
begin
  Result := FFileSystem.Write(FFileHandle, @Buffer, Count);
end;

end.
