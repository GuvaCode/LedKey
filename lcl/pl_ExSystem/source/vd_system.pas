
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit vd_system;

{$I vd.inc}

interface

uses
  SysUtils, Classes, vd_compress, vd_resource, vd_core;

type

  TvsCompressionLevel = (vclNone, vclFastest, vclDefault, vclMax);

  TvsSystem = class(TComponent)
  private
    FStream: TStream;
    FFileSystem: TFileSystem;
    FCompress: boolean;
    FFileName: string;
    FReadOnly: boolean;
    FCompressed: boolean;
    FCompressionLevel: TvsCompressionLevel;
    procedure SetReadOnly(const Value: boolean);
    procedure SetCompressed(const Value: boolean);
    function CorrectPath(const S: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateEmpty(const AFileName: string; AMaxSizeMb: longword = 64);
    procedure CreateFromFile(const AFileName: string);
    procedure CreateFromStream(const Stream: TStream);
    procedure Clear;

    function CreateStream(const fileName: string; mode: word): TStream;
    function StreamExists(const fileName: string): boolean;
    function DirectoryExists(const fileName: string): boolean;

    function FileDate(const fileName: string): integer;

    procedure CreateDir(const ADirName: string);

    procedure EraseFile(const AFileName: string);

    function FindFirst(const Path: string; Attr: integer; var F: TSearchRec): integer;
    function FindNext(var F: TSearchRec): integer;
    procedure FindClose(var F: TSearchRec);
  published
    property FileName: string read FFileName;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property Compressed: boolean read FCompressed write SetCompressed;
    property CompressionLevel: TvsCompressionLevel read FCompressionLevel write FCompressionLevel default vclFastest;
  end;

  TvsStream = class(TStream)
  private
    FStream: TStream;
  protected
    procedure SetSize(NewSize: longint); override;
    procedure SetSize(const NewSize: int64); override;
  public
    constructor Create(const ASystem: TvsSystem; const FileName: WideString; const Mode: word); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  end;

implementation

type
  THackMemoryStream = class(TMemoryStream);

  TvsCompressStream = class(TStream)
  private
    FFileStream: TStream;
    FMemoryStream: TStream;
    FMode: word;
    FCompressionLevel: TvsCompressionLevel;
  protected
    procedure SetSize(NewSize: longint); override;
    procedure SetSize(const NewSize: int64); override;
  public
    constructor Create(const AFileStream: TStream; Mode: word; CompressionLevel: TvsCompressionLevel); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  end;

//=============== TvsCompressStream ========================================

constructor TvsCompressStream.Create(const AFileStream: TStream; Mode: word; CompressionLevel: TvsCompressionLevel);
var
  D: TDecompressionStream;
  xSize: longword;
begin
  inherited Create;
  FMode := Mode;
  FCompressionLevel := CompressionLevel;
  FFileStream := AFileStream;
  FMemoryStream := TMemoryStream.Create;
  if Mode and fmCreate = 0 then
  begin
    FFileStream.Position := 0;
    D := TDecompressionStream.Create(FFileStream);
    xSize := ReadLongword(D);
    FMemoryStream.CopyFrom(D, xSize);
    FMemoryStream.Position := 0;
    D.Free;
  end;
end;

destructor TvsCompressStream.Destroy;
var
  C: TCompressionStream;
  xSize: longword;
begin
  if (FMode and fmCreate = fmCreate) or (FMode and fmOpenWrite = fmOpenWrite) then
  begin
    FFileStream.Position := 0;
    FMemoryStream.Position := 0;
    C := TCompressionStream.Create(TCompressionLevel(FCompressionLevel), FFileStream);
    xSize := FMemoryStream.Size;
    WriteLongword(C, xSize);
    C.CopyFrom(FMemoryStream, xSize);
    C.Free;
  end;
  FMemoryStream.Free;
  FFileStream.Free;
  inherited;
end;

function TvsCompressStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  Result := FMemoryStream.Seek(Offset, Origin);
end;

procedure TvsCompressStream.SetSize(const NewSize: int64);
begin
  THackMemoryStream(FMemoryStream).SetSize(NewSize);
end;

procedure TvsCompressStream.SetSize(NewSize: integer);
begin
  THackMemoryStream(FMemoryStream).SetSize(NewSize);
end;

function TvsCompressStream.Read(var Buffer; Count: integer): longint;
begin
  Result := FMemoryStream.Read(Buffer, Count);
end;

function TvsCompressStream.Write(const Buffer; Count: integer): longint;
begin
  Result := FMemoryStream.Write(Buffer, Count);
end;

//================ TvsStream =========================

constructor TvsStream.Create(const ASystem: TvsSystem; const FileName: WideString; const Mode: word);
begin
  inherited Create;
  if ASystem <> nil then
    FStream := ASystem.CreateStream(FileName, Mode);
end;

destructor TvsStream.Destroy;
begin
  if FStream <> nil then
    FStream.Free;
  inherited;
end;

function TvsStream.Read(var Buffer; Count: integer): longint;
begin
  if FStream <> nil then
    Result := FStream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TvsStream.Write(const Buffer; Count: integer): longint;
begin
  if FStream <> nil then
    Result := FStream.Write(Buffer, Count)
  else
    Result := 0;
end;


function TvsStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if FStream <> nil then
    Result := FStream.Seek(Offset, Origin)
  else
    Result := 0;
end;

procedure TvsStream.SetSize(NewSize: integer);
begin
  if FStream <> nil then
    FStream.Size := NewSize;
end;


procedure TvsStream.SetSize(const NewSize: int64);
begin
  if FStream <> nil then
    FStream.Size := NewSize;
end;


//============== TvsSystem =================================

constructor TvsSystem.Create(AOwner: TComponent);
begin
  inherited;
  FCompressionLevel := vclFastest;
end;

destructor TvsSystem.Destroy;
begin
  Clear;
  inherited;
end;

procedure TvsSystem.Clear;
begin
  if FFileSystem <> nil then
  begin
    FFileSystem.Free;
    FFileSystem := nil;
  end;
  if FStream <> nil then
  begin
    FStream.Free;
    FStream := nil;
  end;
  FFileName := '';
end;

procedure TvsSystem.CreateFromFile(const AFileName: string);
begin
  Clear;
  if not ReadOnly then
  begin
    FStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite);
    FFileSystem := TFileSystem.CreateOpen(FStream, fmOpenRead or fmOpenWrite or fmCreate);
  end
  else
  begin
    FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    FFileSystem := TFileSystem.CreateOpen(FStream, fmOpenRead);
  end;
  if FFileSystem <> nil then
  begin
    FFileName := AFileName;
    FReadOnly := FFileSystem.IsReadOnly;
    FCompressed := FFileSystem.Compressed;
  end;
end;

procedure TvsSystem.CreateEmpty(const AFileName: string; AMaxSizeMb: longword = 64);
begin
  Clear;
  FStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  if AMaxSizeMb < 32 then
    AMaxSizeMb := 32;
  if AMaxSizeMb > 2048 then
    AMaxSizeMb := 2048;
  FFileSystem := TFileSystem.CreateEmpty(FStream, FReadOnly, FCompressed, (AMaxSizeMb * 1024 * 1024) div 4096);
  if FFileSystem <> nil then
    FFileName := AFileName;
end;

procedure TvsSystem.CreateFromStream(const Stream: TStream);
begin
  if FReadOnly then
    FFileSystem := TFileSystem.CreateOpen(Stream, fmOpenRead)
  else
    FFileSystem := TFileSystem.CreateOpen(Stream, fmOpenRead or fmOpenWrite or fmCreate);
  FFileName := '';
  if FFileSystem <> nil then
  begin
    FReadOnly := FFileSystem.IsReadOnly;
    FCompressed := FFileSystem.Compressed;
  end;
end;

function TvsSystem.StreamExists(const fileName: string): boolean;
begin
  Result := FFileSystem.FileExists(CorrectPath(fileName));
end;

function TvsSystem.DirectoryExists(const fileName: string): boolean;
begin
  Result := FFileSystem.DirectoryExists(CorrectPath(fileName));
end;

type
  PCharArray = ^TCharArray;
  TCharArray = array [0..MaxInt - 2] of char;

function TvsSystem.CreateStream(const fileName: string; mode: word): TStream;
begin
  if (mode and fmCreate = fmCreate) then
  begin
    Result := TFileSystemStream.Create(FFileSystem, CorrectPath(fileName), fmCreate);
  end
  else
  if (mode and fmOpenWrite = fmOpenWrite) then
  begin
    Result := TFileSystemStream.Create(FFileSystem, CorrectPath(fileName), fmOpenWrite);
  end
  else
  begin
    Result := TFileSystemStream.Create(FFileSystem, CorrectPath(fileName), fmOpenRead);
  end;

  if (Result <> nil) and (TFileSystemStream(Result).FileHandle = nil) then
  begin
    Result.Free;
    Result := nil;
  end
  else
  if FFileSystem.Compressed then
  begin
    Result := TvsCompressStream.Create(Result, Mode, FCompressionLevel);
  end;
end;

function TvsSystem.FileDate(const fileName: string): integer;
var
  F: TFile;
begin
  F := FFileSystem.OpenFileRead(CorrectPath(fileName));
  if F <> nil then
  begin
    Result := DateTimeToFileDate(F.Date);
    F.Free;
  end
  else
    Result := -1;
end;

procedure TvsSystem.SetReadOnly(const Value: boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if not (csLoading in ComponentState) then
    begin
      FFileSystem.SetReadOnly(Value);
      FReadOnly := FFileSystem.IsReadOnly;
    end;
  end;
end;

procedure TvsSystem.SetCompressed(const Value: boolean);
begin
  if FCompressed <> Value then
  begin
    FCompressed := Value;
  end;
end;

procedure TvsSystem.FindClose(var F: TSearchRec);
begin
  if FFileSystem <> nil then
    FFileSystem.FindClose(F);
end;

function TvsSystem.FindFirst(const Path: string; Attr: integer; var F: TSearchRec): integer;
begin
  if FFileSystem <> nil then
    Result := FFileSystem.FindFirst(Path, Attr, F)
  else
    Result := -1;
end;

function TvsSystem.FindNext(var F: TSearchRec): integer;



begin
  if FFileSystem <> nil then
    Result := FFileSystem.FindNext(F)
  else
    Result := -1;
end;

procedure TvsSystem.CreateDir(const ADirName: string);
begin
  if FFileSystem <> nil then
    FFileSystem.CreateDir(ADirName);
end;

procedure TvsSystem.EraseFile(const AFileName: string);
var
  F: TFile;
begin
  if FFileSystem <> nil then
  begin
    F := FFileSystem.OpenFileRead(AFileName);
    if F <> nil then
      FFileSystem.EraseFile(F);
  end;
end;

function TvsSystem.CorrectPath(const S: string): string;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[1] in ['\', '/']) then
    Delete(Result, 1, 1);
end;

end.
