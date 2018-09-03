
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit vd_resource;

{$H+}
{$I vd.inc}

interface

uses
  Classes, SysUtils, TypInfo;

{$IFNDEF FPC}
{$DEFINE FPC_LITTLE_ENDIAN}
{$ENDIF}

const
  cMaxArray = (MaxInt shr 4);

type
  PlongwordArray = ^TLongwordArray;
  TLongwordArray = array[0..cMaxArray] of longword;

  PWord = ^word;

  PWordArray = ^TWordArray;
  TWordArray = array[0..cMaxArray] of word;

procedure ReverseBytes(p: Pointer; Count: integer);
procedure ReverseByteOrderInWords(p: PWord; Count: integer);

function ReadBuf(s: TStream; Buf: Pointer; Size: longword): longword;
function ReadLongwordBuf(s: TStream; Buf: PLongwordArray; Count: longword): longword;
function ReadWordBuf(s: TStream; Buf: PWordArray; Count: longword): longword;
function ReadByte(s: TStream): byte;
function ReadWord(s: TStream): word;
function ReadInteger(s: TStream): integer;
function Readlongword(s: TStream): longword;
function ReadInt64(s: TStream): int64;
function ReadSingle(s: TStream): single;
function ReadDouble(s: TStream): double;
function ReadCurrency(s: TStream): currency;
function ReadWideString(s: TStream): WideString;
function ReadString(S: TStream): string;

procedure WriteBuf(s: TStream; Buf: Pointer; Size: longword);
procedure WriteLongwordBuf(s: TStream; Buf: PLongwordArray; Count: longword);
procedure WriteWordBuf(s: TStream; Buf: PWordArray; Count: longword);
procedure WriteByte(s: TStream; const b: byte);
procedure WriteWord(s: TStream; const w: word);
procedure WriteInteger(s: TStream; const i: integer);
procedure Writelongword(s: TStream; const c: longword);
procedure WriteSingle(s: TStream; const si: single);
procedure WriteDouble(s: TStream; const d: double);
procedure WriteInt64(s: TStream; const i: int64);
procedure WriteCurrency(s: TStream; const c: currency);
procedure WriteWideStringContent(s: TStream; const w: WideString);
procedure WriteString(S: TStream; Value: string);

procedure WriteReversedWord(s: TStream; w: word);
procedure Write4BytesReversed(s: TStream; p: Pointer);
procedure Write8BytesReversed(s: TStream; p: Pointer);
procedure Write10BytesReversed(s: TStream; p: Pointer);
procedure WriteNull(s: TStream; Count: integer);
procedure WriteReversedWords(s: TStream; p: Pointer; Count: integer);

implementation

{ LRS format converter functions }

procedure ReverseBytes(p: Pointer; Count: integer);
var
  p1: PChar;
  p2: PChar;
  c: char;
begin
  p1 := PChar(p);
  p2 := PChar(p) + Count - 1;
  while p1 < p2 do
  begin
    c := p1^;
    p1^ := p2^;
    p2^ := c;
    System.Inc(p1);
    System.Dec(p2);
  end;
end;

procedure ReverseByteOrderInWords(p: PWord; Count: integer);
var
  i: integer;
  w: word;
begin
  for i := 0 to Count - 1 do
  begin
    w := PWordArray(p)^[i];
    w := (w shr 8) or ((w and $ff) shl 8);
    PWordArray(p)^[i] := w;
  end;
end;

function ReadBuf(s: TStream; Buf: Pointer; Size: longword): longword;
begin
  Result := s.Read(Buf^, Size);
end;

function ReadLongwordBuf(s: TStream; Buf: PLongwordArray; Count: longword): longword;
{$IFDEF FPC_BIG_ENDIAN}
var
  i: integer;
{$ENDIF}
begin
  Result := s.Read(Buf[0], Count * 4);
  {$IFDEF FPC_BIG_ENDIAN}
  for i := 0 to Count - 1 do
    ReverseBytes(@Buf[i], 4);
  {$ENDIF}
end;

function ReadWordBuf(s: TStream; Buf: PWordArray; Count: longword): longword;
{$IFDEF FPC_BIG_ENDIAN}
var
  i: integer;
{$ENDIF}
begin
  Result := s.Read(Buf[0], Count * 2);
  {$IFDEF FPC_BIG_ENDIAN}
  for i := 0 to Count - 1 do
    ReverseBytes(@Buf[i], 2);
  {$ENDIF}
end;

function ReadByte(s: TStream): byte;
begin
  Result := 0;
  s.Read(Result, 1);
end;

function ReadWord(s: TStream): word;
begin
  Result := 0;
  s.Read(Result, 2);
  {$IFDEF FPC_BIG_ENDIAN}
  Result := ((Result and $ff) shl 8) or (Result shr 8);
  {$ENDIF}
end;

function ReadInteger(s: TStream): integer;
begin
  Result := 0;
  s.Read(Result, 4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result, 4);
  {$ENDIF}
end;

function Readlongword(s: TStream): longword;
begin
  Result := 0;
  s.Read(Result, 4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result, 4);
  {$ENDIF}
end;

function ReadInt64(s: TStream): int64;
begin
  Result := 0;
  s.Read(Result, 8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result, 8);
  {$ENDIF}
end;

function ReadSingle(s: TStream): single;
begin
  Result := 0;
  s.Read(Result, 4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result, 4);
  {$ENDIF}
end;

function ReadDouble(s: TStream): double;
begin
  Result := 0;
  s.Read(Result, 8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result, 8);
  {$ENDIF}
end;

function ReadCurrency(s: TStream): currency;
begin
  Result := 0;
  s.Read(Result, 8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result, 8);
  {$ENDIF}
end;

function ReadString(S: TStream): string;
var
  L: integer;
begin
  L := 0;
  S.Read(L, SizeOf(L));
  SetLength(Result, L);
  S.Read(Pointer(Result)^, L);
end;

function ReadWideString(s: TStream): WideString;
var
  Len: longint;
begin
  Len := ReadInteger(s);
  SetLength(Result, Len);
  if Len > 0 then
  begin
    s.Read(Result[1], Len * 2);
    {$IFDEF FPC_BIG_ENDIAN}
    ReverseByteOrderInWords(PWord(@Result[1]), Len);
    {$ENDIF}
  end;
end;

procedure WriteReversedWord(s: TStream; w: word);
begin
  w := (w shr 8) or ((w and $ff) shl 8);
  s.Write(w, 2);
end;

procedure Write4BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..3] of char;
  i: integer;
begin
  for i := 0 to 3 do
    a[i] := PChar(p)[3 - i];
  s.Write(a[0], 4);
end;

procedure Write8BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..7] of char;
  i: integer;
begin
  for i := 0 to 7 do
    a[i] := PChar(p)[7 - i];
  s.Write(a[0], 8);
end;

procedure Write10BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..9] of char;
  i: integer;
begin
  for i := 0 to 9 do
    a[i] := PChar(p)[9 - i];
  s.Write(a[0], 10);
end;

procedure WriteReversedWords(s: TStream; p: Pointer; Count: integer);
var
  w: word;
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    w := PWordArray(P)^[i];
    w := (w shr 8) or ((w and $ff) shl 8);
    s.Write(w, 2);
  end;
end;

procedure WriteNull(s: TStream; Count: integer);
var
  c: char;
  i: integer;
begin
  c := #0;
  for i := 0 to Count - 1 do
    s.Write(c, 1);
end;

procedure WriteBuf(s: TStream; Buf: Pointer; Size: longword);
begin
  s.Write(Buf^, Size);
end;

procedure WriteLongwordBuf(s: TStream; Buf: PLongwordArray; Count: longword);
{$IFDEF FPC_BIG_ENDIAN}
var
  i: integer;
{$ENDIF}
begin
  {$IFDEF FPC_BIG_ENDIAN}
  for i := 0 to Count - 1 do
    ReverseBytes(@Buf[i], 4);
  {$ENDIF}
  s.Write(Buf[0], Count * 4);
  {$IFDEF FPC_BIG_ENDIAN}
  for i := 0 to Count - 1 do
    ReverseBytes(@Buf[i], 4);
  {$ENDIF}
end;

procedure WriteWordBuf(s: TStream; Buf: PWordArray; Count: longword);
{$IFDEF FPC_BIG_ENDIAN}
var
  i: integer;
{$ENDIF}
begin
  {$IFDEF FPC_BIG_ENDIAN}
  for i := 0 to Count - 1 do
    ReverseBytes(@Buf[i], 2);
  {$ENDIF}
  s.Write(Buf[0], Count * 2);
  {$IFDEF FPC_BIG_ENDIAN}
  for i := 0 to Count - 1 do
    ReverseBytes(@Buf[i], 2);
  {$ENDIF}
end;

procedure WriteWord(s: TStream; const w: word);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(w, 2);
  {$ELSE}
  WriteReversedWord(s, w);
  {$ENDIF}
end;

procedure WriteByte(s: TStream; const b: byte);
begin
  s.Write(b, 1);
end;

procedure WriteInteger(s: TStream; const i: integer);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i, 4);
  {$ELSE}
  Write4BytesReversed(s, @i);
  {$ENDIF}
end;

procedure Writelongword(s: TStream; const c: longword);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(c, 4);
  {$ELSE}
  Write4BytesReversed(s, @c);
  {$ENDIF}
end;

procedure WriteSingle(s: TStream; const si: single);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(si, 4);
  {$ELSE}
  Write4BytesReversed(s, @si);
  {$ENDIF}
end;

procedure WriteDouble(s: TStream; const d: double);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(d, 8);
  {$ELSE}
  Write8BytesReversed(s, @d);
  {$ENDIF}
end;

procedure WriteInt64(s: TStream; const i: int64);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i, 8);
  {$ELSE}
  Write8BytesReversed(s, @i);
  {$ENDIF}
end;

procedure WriteCurrency(s: TStream; const c: currency);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(c, 8);
  {$ELSE}
  Write8BytesReversed(s, @c);
  {$ENDIF}
end;

procedure WriteWideStringContent(s: TStream; const w: WideString);
var
  Size: integer;
begin
  Size := length(w);
  if Size = 0 then
    exit;
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(w[1], Size * 2);
  {$ELSE}
  WriteReversedWords(s, @w[1], Size);
  {$ENDIF}
end;

procedure WriteString(S: TStream; Value: string);
var
  L: integer;
begin
  L := Length(Value);
  S.Write(L, SizeOf(L));
  S.Write(Pointer(Value)^, L);
end;

end.
