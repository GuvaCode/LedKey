{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplMemINIUnit;

interface

uses
  SysUtils, Classes,
  Controls, Forms, Graphics, Types;

type

  EIniFileException = class(Exception);


  TplCustomIniFile = class(TObject)
  private
    FFileName: string;
  public
    constructor Create(const FileName: string);
    function SectionExists(const Section: string): boolean;
    function ReadString(const Section, Ident, Default: string): string; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: string); virtual; abstract;
    function ReadInteger(const Section, Ident: string; Default: longint): longint; virtual;
    procedure WriteInteger(const Section, Ident: string; Value: longint); virtual;
    function ReadBool(const Section, Ident: string; Default: boolean): boolean; virtual;
    procedure WriteBool(const Section, Ident: string; Value: boolean); virtual;
    function ReadBinaryStream(const Section, Name: string; Value: TStream): integer; virtual;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; virtual;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; virtual;
    function ReadFloat(const Section, Name: string; Default: double): double; virtual;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; virtual;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream); virtual;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); virtual;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); virtual;
    procedure WriteFloat(const Section, Name: string; Value: double); virtual;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); virtual;
    procedure ReadSection(const Section: string; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); overload; virtual; abstract;
    procedure ReadSections(const Section: string; Strings: TStrings); overload; virtual;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); virtual; abstract;
    procedure EraseSection(const Section: string); virtual; abstract;
    procedure DeleteKey(const Section, Ident: string); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: string): boolean; virtual;
    property FileName: string read FFileName;
  end;

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;

  THashItem = record
    Next: PHashItem;
    Key: string;
    Value: integer;
  end;

  TplStringHash = class
  private
    Buckets: array of PHashItem;
  protected
    function Find(const Key: string): PPHashItem;
    function HashOf(const Key: string): cardinal; virtual;
  public
    constructor Create(Size: cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: string; Value: integer);
    procedure Clear;
    procedure Remove(const Key: string);
    function Modify(const Key: string; Value: integer): boolean;
    function ValueOf(const Key: string): integer;
  end;


  THashedStringList = class(TStringList)
  private
    FValueHash: TplStringHash;
    FNameHash: TplStringHash;
    FValueHashValid: boolean;
    FNameHashValid: boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    destructor Destroy; override;
    function IndexOf(const S: string): integer; //override;
    function IndexOfName(const Name: string): integer; //override;
  end;

  { TplMemIniFile - loads an entire INI file into memory and allows all
    operations to be performed on the memory image.  The image can then
    be written out to the disk file }

  TplMemIniFile = class(TplCustomIniFile)
  private
    FSections: TStringList;
    function AddSection(const Section: string): TStrings;
    function GetCaseSensitive: boolean;
    procedure LoadValues;
    procedure SetCaseSensitive(Value: boolean);
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    procedure Clear;

    procedure LoadFromFile(const afilename: string);
    procedure SaveToFile(const afilename: string);

    procedure DeleteKey(const Section, Ident: string); override;
    procedure EraseSection(const Section: string); override;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    function ReadColor(const Section, Ident: string; Default: TColor): TColor;
    function ReadRect(const Section, Ident: string; Default: TRect): TRect;
    procedure ReadFont(const Section, Ident: string; Font: TFont);
    procedure Rename(const aFileName: string; Reload: boolean);
    procedure SetStrings(List: TStrings);
    procedure UpdateFile; override;
    procedure WriteString(const Section, Ident, Value: string); override;
    procedure WriteColor(const Section, Ident: string; Value: TColor);
    procedure WriteRect(const Section, Ident: string; Value: TRect);
    procedure WriteFont(const Section, Ident: string; Value: TFont);

    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TplMemIni = class(TComponent)
  private
    fMemIniFile: TplMemIniFile;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MemIniFile: TplMemIniFile read fMemIniFile;
  end;


implementation

//==========================================================

function UniColorToString(Color: cardinal): string;
 var ss:ansistring;
begin
  FmtStr(ss, '%s%.8x', [HexDisplayPrefix, Color]);
  result:=ss;
end;

function UniStringToColor(const S: string): cardinal;
var
  col: Tcolor;
begin
  if IdentToColor(S, longint(col)) then
    Result := cardinal(col)
  else
    Result := cardinal(StrToInt(S));
end;

function FontToString(aFont: TFont): string;
begin
  // name, size, bold, italic, underline, strikethrough, colour
  if aFont = nil then
    Exit;

  Result := Format('%s,%d,%d%d%d%d,%s', [aFont.Name, aFont.Size, integer(fsBold in aFont.Style),
    integer(fsItalic in aFont.Style), integer(fsUnderline in aFont.Style), integer(fsStrikeOut in aFont.Style),
    ColorToString(aFont.Color)]);
end;

procedure StringToFont(Str: string; aFont: TFont);
const
  SEP = ',';
var
  i: integer;
begin
  if aFont = nil then
    Exit;

  // name
  i := Pos(SEP, Str);
  if i > 0 then
  begin
    aFont.Name := Copy(Str, 1, i - 1);
    Delete(Str, 1, i);
  end;

  // size
  i := Pos(SEP, Str);
  if i > 0 then
  begin
    aFont.Size := StrToInt(Copy(Str, 1, i - 1));
    Delete(Str, 1, i);
  end;

  // bold, italic, underline, strikethrough
  if Pos(SEP, Str) = 5 then
  begin
    aFont.Style := [];
    if Str[1] = '1' then
      aFont.Style := aFont.Style + [fsBold];
    if Str[2] = '1' then
      aFont.Style := aFont.Style + [fsItalic];
    if Str[3] = '1' then
      aFont.Style := aFont.Style + [fsUnderline];
    if Str[4] = '1' then
      aFont.Style := aFont.Style + [fsStrikeOut];
    Delete(Str, 1, 5);
  end;

  aFont.Color := StringToColor(Str);
end;


function RectToString(aRect: TRect): string;
begin
  Result := Format('%d,%d,%d,%d', [aRect.Left, aRect.Top, aRect.Right, aRect.Bottom]);
end;

function StringToRect(Str: string): TRect;
const
  SEP = ',';
var
  i: integer;
begin
  Result := Types.Rect(0, 0, 0, 0);

  // left
  i := Pos(SEP, Str);
  if i > 0 then
  begin
    Result.Left := StrToInt(Copy(Str, 1, i - 1));
    Delete(Str, 1, i);
  end;

  // top
  i := Pos(SEP, Str);
  if i > 0 then
  begin
    Result.Top := StrToInt(Copy(Str, 1, i - 1));
    Delete(Str, 1, i);
  end;

  // width
  i := Pos(SEP, Str);
  if i > 0 then
  begin
    Result.Right := StrToInt(Copy(Str, 1, i - 1));
    Delete(Str, 1, i);
  end;

  // height
  Result.Bottom := StrToInt(Str);
end;

//==================================================================


{ TplCustomIniFile }

constructor TplCustomIniFile.Create(const FileName: string);
begin
  FFileName := FileName;
end;

function TplCustomIniFile.SectionExists(const Section: string): boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.Count > 0;
  finally
    S.Free;
  end;
end;

function TplCustomIniFile.ReadInteger(const Section, Ident: string; Default: longint): longint;
var
  IntStr: string;
begin
  IntStr := ReadString(Section, Ident, '');
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
    IntStr := '$' + Copy(IntStr, 3, Maxint);
  Result := StrToIntDef(IntStr, Default);
end;

procedure TplCustomIniFile.WriteInteger(const Section, Ident: string; Value: longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TplCustomIniFile.ReadBool(const Section, Ident: string; Default: boolean): boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

function TplCustomIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
var
  DateStr: string;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
    try
      Result := StrToDate(DateStr);
    except
      on EConvertError do
        // Ignore EConvertError exceptions
      else
        raise;
    end;
end;

function TplCustomIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
  DateStr: string;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
    try
      Result := StrToDateTime(DateStr);
    except
      on EConvertError do
        // Ignore EConvertError exceptions
      else
        raise;
    end;
end;

function TplCustomIniFile.ReadFloat(const Section, Name: string; Default: double): double;
var
  FloatStr: string;
begin
  FloatStr := ReadString(Section, Name, '');
  Result := Default;
  if FloatStr <> '' then
    try
      Result := StrToFloat(FloatStr);
    except
      on EConvertError do
        // Ignore EConvertError exceptions
      else
        raise;
    end;
end;

function TplCustomIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
  TimeStr: string;
begin
  TimeStr := ReadString(Section, Name, '');
  Result := Default;
  if TimeStr <> '' then
    try
      Result := StrToTime(TimeStr);
    except
      on EConvertError do
        // Ignore EConvertError exceptions
      else
        raise;
    end;
end;

procedure TplCustomIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, DateToStr(Value));
end;

procedure TplCustomIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, DateTimeToStr(Value));
end;

procedure TplCustomIniFile.WriteFloat(const Section, Name: string; Value: double);
begin
  WriteString(Section, Name, FloatToStr(Value));
end;

procedure TplCustomIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, TimeToStr(Value));
end;

procedure TplCustomIniFile.WriteBool(const Section, Ident: string; Value: boolean);
const
  Values: array[boolean] of string = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

function TplCustomIniFile.ValueExists(const Section, Ident: string): boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.IndexOf(Ident) > -1;
  finally
    S.Free;
  end;
end;

function TplCustomIniFile.ReadBinaryStream(const Section, Name: string; Value: TStream): integer;
var
  Text: string;
  Stream: TMemoryStream;
  Pos: integer;
begin
  Text := ReadString(Section, Name, '');
  if Text <> '' then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      Pos := Stream.Position;
      Stream.SetSize(Stream.Size + Length(Text) div 2);
      HexToBin(@Text, PChar(integer(Stream.Memory) + Stream.Position), Length(Text) div 2);
      Stream.Position := Pos;
      if Value <> Stream then
        Value.CopyFrom(Stream, Length(Text) div 2);
      Result := Stream.Size - Pos;
    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end
  else
    Result := 0;
end;

procedure TplCustomIniFile.WriteBinaryStream(const Section, Name: string; Value: TStream);
var
  Text: string;
  Stream: TMemoryStream;
begin
  SetLength(Text, (Value.Size - Value.Position) * 2);
  if Length(Text) > 0 then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      if Stream <> Value then
      begin
        Stream.CopyFrom(Value, Value.Size - Value.Position);
        Stream.Position := 0;
      end;
      BinToHex(PChar(integer(Stream.Memory) + Stream.Position), @Text,
        Stream.Size - Stream.Position);
    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end;
  WriteString(Section, Name, Text);
end;

procedure TplCustomIniFile.ReadSections(const Section: string; Strings: TStrings);
var
  I: integer;
begin
  ReadSections(Strings);
  for I := Strings.Count - 1 downto 0 do
    if not SameText(Section, Copy(Strings[I], 1, Length(Section))) then
      Strings.Delete(I);
end;

{ TplStringHash }

procedure TplStringHash.Add(const Key: string; Value: integer);
var
  Hash: integer;
  Bucket: PHashItem;
begin
  Hash := HashOf(Key) mod cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TplStringHash.Clear;
var
  I: integer;
  P, N: PHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

constructor TplStringHash.Create(Size: cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TplStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TplStringHash.Find(const Key: string): PPHashItem;
var
  Hash: integer;
begin
  Hash := HashOf(Key) mod cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^^.Key = Key then
      Exit
    else
      Result := @Result^^.Next;
  end;
end;

function TplStringHash.HashOf(const Key: string): cardinal;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Key[I]);
end;

function TplStringHash.Modify(const Key: string; Value: integer): boolean;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TplStringHash.Remove(const Key: string);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TplStringHash.ValueOf(const Key: string): integer;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;

{ THashedStringList }

procedure THashedStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

destructor THashedStringList.Destroy;
begin
  FValueHash.Free;
  FNameHash.Free;
  inherited Destroy;
end;

function THashedStringList.IndexOf(const S: string): integer;
begin
  UpdateValueHash;
  if not CaseSensitive then
    Result := FValueHash.ValueOf(AnsiUpperCase(S))
  else
    Result := FValueHash.ValueOf(S);
end;

function THashedStringList.IndexOfName(const Name: string): integer;
begin
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(AnsiUpperCase(Name))
  else
    Result := FNameHash.ValueOf(Name);
end;

procedure THashedStringList.UpdateNameHash;
var
  I: integer;
  P: integer;
  Key: string;
begin
  if FNameHashValid then
    Exit;

  if FNameHash = nil then
    FNameHash := TplStringHash.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
  begin
    Key := Get(I);
    P := AnsiPos(NameValueSeparator, Key);
    if P <> 0 then
    begin
      if not CaseSensitive then
        Key := AnsiUpperCase(Copy(Key, 1, P - 1))
      else
        Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure THashedStringList.UpdateValueHash;
var
  I: integer;
begin
  if FValueHashValid then
    Exit;

  if FValueHash = nil then
    FValueHash := TplStringHash.Create
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    if not CaseSensitive then
      FValueHash.Add(AnsiUpperCase(Self[I]), I)
    else
      FValueHash.Add(Self[I], I);
  FValueHashValid := True;
end;

{ TplMemIniFile }

constructor TplMemIniFile.Create(const aFileName: string);
begin
  inherited Create(aFileName);
  FSections := THashedStringList.Create;
{$IFDEF UNIX}
  FSections.CaseSensitive := True;
{$ENDIF}
  LoadValues;
end;

destructor TplMemIniFile.Destroy;
begin
  if FSections <> nil then
    Clear;
  FSections.Free;
  inherited Destroy;
end;

function TplMemIniFile.AddSection(const Section: string): TStrings;
begin
  Result := THashedStringList.Create;
  try
    THashedStringList(Result).CaseSensitive := CaseSensitive;
    FSections.AddObject(Section, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TplMemIniFile.Clear;
var
  I: integer;
begin
  for I := 0 to FSections.Count - 1 do
    TObject(FSections.Objects[I]).Free;
  FSections.Clear;
end;

procedure TplMemIniFile.DeleteKey(const Section, Ident: string);
var
  I, J: integer;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    J := Strings.IndexOfName(Ident);
    if J >= 0 then
      Strings.Delete(J);
  end;
end;

procedure TplMemIniFile.EraseSection(const Section: string);
var
  I: integer;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    TStrings(FSections.Objects[I]).Free;
    FSections.Delete(I);
  end;
end;

function TplMemIniFile.GetCaseSensitive: boolean;
begin
  Result := FSections.CaseSensitive;
end;

procedure TplMemIniFile.GetStrings(List: TStrings);
var
  I, J: integer;
  Strings: TStrings;
begin
  List.BeginUpdate;
  try
    for I := 0 to FSections.Count - 1 do
    begin
      List.Add('[' + FSections[I] + ']');
      Strings := TStrings(FSections.Objects[I]);
      for J := 0 to Strings.Count - 1 do
        List.Add(Strings[J]);
      List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TplMemIniFile.LoadValues;
var
  List: TStringList;
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
    List := TStringList.Create;
    try
      List.LoadFromFile(FileName);
      SetStrings(List);
    finally
      List.Free;
    end;
  end
  else
    Clear;
end;

procedure TplMemIniFile.ReadSection(const Section: string; Strings: TStrings);
var
  I, J: integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStrings(FSections.Objects[I]);
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[J]);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TplMemIniFile.ReadSections(Strings: TStrings);
begin
  Strings.Assign(FSections);
end;

procedure TplMemIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  I: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
      Strings.Assign(TStrings(FSections.Objects[I]));
  finally
    Strings.EndUpdate;
  end;
end;

function TplMemIniFile.ReadString(const Section, Ident, Default: string): string;
var
  I: integer;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
    begin
      Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
      Exit;
    end;
  end;
  Result := Default;
end;

procedure TplMemIniFile.Rename(const aFileName: string; Reload: boolean);
begin
  FFileName := aFileName;
  if Reload then
    LoadValues;
end;

procedure TplMemIniFile.SetCaseSensitive(Value: boolean);
var
  I: integer;
begin
  if Value <> FSections.CaseSensitive then
  begin
    FSections.CaseSensitive := Value;
    for I := 0 to FSections.Count - 1 do
      with THashedStringList(FSections.Objects[I]) do
      begin
        CaseSensitive := Value;
        Changed;
      end;
    THashedStringList(FSections).Changed;
  end;
end;

procedure TplMemIniFile.SetStrings(List: TStrings);
var
  I, J: integer;
  S: string;
  Strings: TStrings;
begin
  Clear;
  Strings := nil;
  for I := 0 to List.Count - 1 do
  begin
    S := Trim(List[I]);
    if (S <> '') and (S[1] <> ';') then
      if (S[1] = '[') and (S[Length(S)] = ']') then
      begin
        Delete(S, 1, 1);
        SetLength(S, Length(S) - 1);
        Strings := AddSection(Trim(S));
      end
      else
      if Strings <> nil then
      begin
        J := Pos('=', S);
        if J > 0 then // remove spaces before and after '='
          Strings.Add(Trim(Copy(S, 1, J - 1)) + '=' + Trim(Copy(S, J + 1, MaxInt)))
        else
          Strings.Add(S);
      end;
  end;
end;

procedure TplMemIniFile.UpdateFile;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetStrings(List);
    List.SaveToFile(FFileName);
  finally
    List.Free;
  end;
end;

procedure TplMemIniFile.WriteString(const Section, Ident, Value: string);
var
  I: integer;
  S: string;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := TStrings(FSections.Objects[I])
  else
    Strings := AddSection(Section);
  S := Ident + '=' + Value;
  I := Strings.IndexOfName(Ident);
  if I >= 0 then
    Strings[I] := S
  else
    Strings.Add(S);
end;


procedure TplMemIniFile.LoadFromFile(const afilename: string);
begin
  Rename(afilename, True);
end;

procedure TplMemIniFile.SaveToFile(const afilename: string);
begin
  Rename(afilename, False);
  UpdateFile;
end;

function TplMemIniFile.ReadColor(const Section, Ident: string; Default: TColor): TColor;
var
  Str: string;
begin
  Str := ReadString(Section, Ident, uniColorToString(Default));
  try
    Result := uniStringToColor(Str);
  finally
  end;
end;

procedure TplMemIniFile.WriteColor(const Section, Ident: string; Value: TColor);
begin
  WriteString(Section, Ident, UniColorToString(Value));
end;

procedure TplMemIniFile.ReadFont(const Section, Ident: string; Font: TFont);
var
  DefaultStr, Str: string;
begin
  DefaultStr := FontToString(Font);
  Str := ReadString(Section, Ident, DefaultStr);
  try
    StringToFont(Str, Font);
  finally

  end;
end;

procedure TplMemIniFile.WriteFont(const Section, Ident: string; Value: TFont);
begin
  WriteString(Section, Ident, FontToString(Value));
end;

function TplMemIniFile.ReadRect(const Section, Ident: string; Default: TRect): TRect;
var
  DefaultStr, Str: string;
begin
  DefaultStr := RectToString(Default);
  Str := ReadString(Section, Ident, DefaultStr);
  try
    Result := StringToRect(Str);
  finally
  end;
end;

procedure TplMemIniFile.WriteRect(const Section, Ident: string; Value: TRect);
begin
  WriteString(Section, Ident, RectToString(Value));
end;

//====================== TplMemIni =====================================

constructor TplMemIni.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMemIniFile := TplMemIniFile.Create('');
end;

destructor TplMemIni.Destroy;
begin
  fMemIniFile.Free;
  inherited Destroy;
end;

end.
