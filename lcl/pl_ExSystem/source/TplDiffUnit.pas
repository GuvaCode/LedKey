
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplDiffUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Math, Forms, Dialogs;

const
  MAX_DIAGONAL = $FFFFFF; //~16 million

type

{$IFDEF UNICODE}
  P8Bits = PByte;
{$ELSE}
  P8Bits = PAnsiChar;
{$ENDIF}

  PDiags = ^TDiags;
  TDiags = array [-MAX_DIAGONAL .. MAX_DIAGONAL] of integer;

  PIntArray = ^TIntArray;
  TIntArray = array[0 .. MAXINT div sizeof(integer) - 1] of integer;
  PChrArray = ^TChrArray;
  TChrArray = array[0 .. MAXINT div sizeof(char) - 1] of char;

  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

  PCompareRec = ^TCompareRec;

  TCompareRec = record
    Kind: TChangeKind;
    oldIndex1,
    oldIndex2: integer;
    case boolean of
      False: (chr1, chr2: char);
      True: (int1, int2: integer);
  end;

  PDiffVars = ^TplDiffVars;

  TplDiffVars = record
    offset1: integer;
    offset2: integer;
    len1: integer;
    len2: integer;
  end;

  TplDiffStats = record
    matches: integer;
    adds: integer;
    deletes: integer;
    modifies: integer;
  end;

  TplDiff = class(TComponent)
  private
    fCompareList: TList;
    fDiffList: TList;      //this TList circumvents the need for recursion
    fCancelled: boolean;
    fExecuting: boolean;
    fCompareInts: boolean; //ie are we comparing integer arrays or char arrays
    DiagBufferF: pointer;
    DiagBufferB: pointer;
    DiagF, DiagB: PDiags;
    Ints1, Ints2: PIntArray;
    Chrs1, Chrs2: PChrArray;
    fDiffStats: TplDiffStats;
    fLastCompareRec: TCompareRec;
    procedure PushDiff(offset1, offset2, len1, len2: integer);
    function PopDiff: boolean;
    procedure InitDiagArrays(len1, len2: integer);
    procedure DiffInt(offset1, offset2, len1, len2: integer);
    procedure DiffChr(offset1, offset2, len1, len2: integer);
    function SnakeChrF(k, offset1, offset2, len1, len2: integer): boolean;
    function SnakeChrB(k, offset1, offset2, len1, len2: integer): boolean;
    function SnakeIntF(k, offset1, offset2, len1, len2: integer): boolean;
    function SnakeIntB(k, offset1, offset2, len1, len2: integer): boolean;
    procedure AddChangeChr(offset1, range: integer; ChangeKind: TChangeKind);
    procedure AddChangeInt(offset1, range: integer; ChangeKind: TChangeKind);
    function GetCompareCount: integer;
    function GetCompare(index: integer): TCompareRec;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    //compare either and array of characters or an array of integers ...
    function Execute(pints1, pints2: PInteger; len1, len2: integer): boolean; overload;
    function Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean; overload;
    //Cancel allows interrupting excessively prolonged comparisons
    procedure Cancel;
    procedure Clear;
    property Cancelled: boolean read fCancelled;
    property Count: integer read GetCompareCount;
    property Compares[index: integer]: TCompareRec read GetCompare; default;
    property DiffStats: TplDiffStats read fDiffStats;
  end;

implementation


constructor TplDiff.Create(aOwner: TComponent);
begin
  inherited;
  fCompareList := TList.Create;
  fDiffList := TList.Create;
end;

destructor TplDiff.Destroy;
begin
  Clear;
  fCompareList.Free;
  fDiffList.Free;
  inherited;
end;

function TplDiff.Execute(pints1, pints2: PInteger; len1, len2: integer): boolean;
var
  i, Len1Minus1: integer;
begin
  Result := not fExecuting;
  if not Result then
    exit;
  fCancelled := False;
  fExecuting := True;
  try
    Clear;

    Len1Minus1 := len1 - 1;
    fCompareList.Capacity := len1 + len2;
    fCompareInts := True;

    GetMem(DiagBufferF, sizeof(integer) * (len1 + len2 + 3));
    GetMem(DiagBufferB, sizeof(integer) * (len1 + len2 + 3));
    Ints1 := pointer(pints1);
    Ints2 := pointer(pints2);
    try
      PushDiff(0, 0, len1, len2);
      while PopDiff do ;
    finally
      freeMem(DiagBufferF);
      freeMem(DiagBufferB);
    end;

    if fCancelled then
    begin
      Result := False;
      Clear;
      exit;
    end;

    //correct the occasional missed match ...
    for i := 1 to Count - 1 do
      with PCompareRec(fCompareList[i])^ do
        if (Kind = ckModify) and (int1 = int2) then
        begin
          Kind := ckNone;
          Dec(fDiffStats.modifies);
          Inc(fDiffStats.matches);
        end;

    //finally, append any trailing matches onto compareList ...
    with fLastCompareRec do
      AddChangeInt(oldIndex1, len1Minus1 - oldIndex1, ckNone);
  finally
    fExecuting := False;
  end;
end;

function TplDiff.Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean;
var
  i, Len1Minus1: integer;
begin
  Result := not fExecuting;
  if not Result then
    exit;
  fCancelled := False;
  fExecuting := True;
  try
    Clear;

    Len1Minus1 := len1 - 1;
    fCompareList.Capacity := len1 + len2;
    fDiffList.Capacity := 1024;
    fCompareInts := False;

    GetMem(DiagBufferF, sizeof(integer) * (len1 + len2 + 3));
    GetMem(DiagBufferB, sizeof(integer) * (len1 + len2 + 3));
    Chrs1 := pointer(pchrs1);
    Chrs2 := pointer(pchrs2);
    try
      PushDiff(0, 0, len1, len2);
      while PopDiff do ;
    finally
      freeMem(DiagBufferF);
      freeMem(DiagBufferB);
    end;

    if fCancelled then
    begin
      Result := False;
      Clear;
      exit;
    end;

    //correct the occasional missed match ...
    for i := 1 to Count - 1 do
      with PCompareRec(fCompareList[i])^ do
        if (Kind = ckModify) and (chr1 = chr2) then
        begin
          Kind := ckNone;
          Dec(fDiffStats.modifies);
          Inc(fDiffStats.matches);
        end;

    //finally, append any trailing matches onto compareList ...
    with fLastCompareRec do
      AddChangeChr(oldIndex1, len1Minus1 - oldIndex1, ckNone);
  finally
    fExecuting := False;
  end;
end;

procedure TplDiff.PushDiff(offset1, offset2, len1, len2: integer);
var
  DiffVars: PDiffVars;
begin
  new(DiffVars);
  DiffVars.offset1 := offset1;
  DiffVars.offset2 := offset2;
  DiffVars.len1 := len1;
  DiffVars.len2 := len2;
  fDiffList.Add(DiffVars);
end;

function TplDiff.PopDiff: boolean;
var
  DiffVars: PDiffVars;
  idx: integer;
begin
  idx := fDiffList.Count - 1;
  Result := idx >= 0;
  if not Result then
    exit;
  DiffVars := PDiffVars(fDiffList[idx]);
  with DiffVars^ do
    if fCompareInts then
      DiffInt(offset1, offset2, len1, len2)
    else
      DiffChr(offset1, offset2, len1, len2);
  Dispose(DiffVars);
  fDiffList.Delete(idx);
end;

procedure TplDiff.InitDiagArrays(len1, len2: integer);
var
  i: integer;
begin
  //assumes that top and bottom matches have been excluded
  P8Bits(DiagF) := P8Bits(DiagBufferF) - sizeof(integer) * (MAX_DIAGONAL - (len1 + 1));
  for i := -(len1 + 1) to (len2 + 1) do
    DiagF[i] := -MAXINT;
  DiagF[1] := -1;

  P8Bits(DiagB) := P8Bits(DiagBufferB) - sizeof(integer) * (MAX_DIAGONAL - (len1 + 1));
  for i := -(len1 + 1) to (len2 + 1) do
    DiagB[i] := MAXINT;
  DiagB[len2 - len1 + 1] := len2;
end;

procedure TplDiff.DiffInt(offset1, offset2, len1, len2: integer);
var
  p, k, delta: integer;
begin
  //trim matching bottoms ...
  while (len1 > 0) and (len2 > 0) and (Ints1[offset1] = Ints2[offset2]) do
  begin
    Inc(offset1);
    Inc(offset2);
    Dec(len1);
    Dec(len2);
  end;
  //trim matching tops ...
  while (len1 > 0) and (len2 > 0) and (Ints1[offset1 + len1 - 1] = Ints2[offset2 + len2 - 1]) do
  begin
    Dec(len1);
    Dec(len2);
  end;

  //stop diff'ing if minimal conditions reached ...
  if (len1 = 0) then
  begin
    AddChangeInt(offset1, len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeInt(offset1, len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    AddChangeInt(offset1, 1, ckDelete);
    AddChangeInt(offset1, 1, ckAdd);
    exit;
  end;

  p := -1;
  delta := len2 - len1;
  InitDiagArrays(len1, len2);
  if delta < 0 then
  begin
    repeat
      Inc(p);
      if (p mod 1024) = 1023 then
      begin
        Application.ProcessMessages;
        if fCancelled then
          exit;
      end;
      //nb: the Snake order is important here
      for k := p downto delta + 1 do
        if SnakeIntF(k, offset1, offset2, len1, len2) then
          exit;
      for k := -p + delta to delta - 1 do
        if SnakeIntF(k, offset1, offset2, len1, len2) then
          exit;
      for k := delta - p to -1 do
        if SnakeIntB(k, offset1, offset2, len1, len2) then
          exit;
      for k := p downto 1 do
        if SnakeIntB(k, offset1, offset2, len1, len2) then
          exit;
      if SnakeIntF(delta, offset1, offset2, len1, len2) then
        exit;
      if SnakeIntB(0, offset1, offset2, len1, len2) then
        exit;
    until (False);
  end
  else
  begin
    repeat
      Inc(p);
      if (p mod 1024) = 1023 then
      begin
        Application.ProcessMessages;
        if fCancelled then
          exit;
      end;
      //nb: the Snake order is important here
      for k := -p to delta - 1 do
        if SnakeIntF(k, offset1, offset2, len1, len2) then
          exit;
      for k := p + delta downto delta + 1 do
        if SnakeIntF(k, offset1, offset2, len1, len2) then
          exit;
      for k := delta + p downto 1 do
        if SnakeIntB(k, offset1, offset2, len1, len2) then
          exit;
      for k := -p to -1 do
        if SnakeIntB(k, offset1, offset2, len1, len2) then
          exit;
      if SnakeIntF(delta, offset1, offset2, len1, len2) then
        exit;
      if SnakeIntB(0, offset1, offset2, len1, len2) then
        exit;
    until (False);
  end;
end;

procedure TplDiff.DiffChr(offset1, offset2, len1, len2: integer);
var
  p, k, delta: integer;
begin
  //trim matching bottoms ...
  while (len1 > 0) and (len2 > 0) and (Chrs1[offset1] = Chrs2[offset2]) do
  begin
    Inc(offset1);
    Inc(offset2);
    Dec(len1);
    Dec(len2);
  end;
  //trim matching tops ...
  while (len1 > 0) and (len2 > 0) and (Chrs1[offset1 + len1 - 1] = Chrs2[offset2 + len2 - 1]) do
  begin
    Dec(len1);
    Dec(len2);
  end;

  //stop diff'ing if minimal conditions reached ...
  if (len1 = 0) then
  begin
    AddChangeChr(offset1, len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeChr(offset1, len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    AddChangeChr(offset1, 1, ckDelete);
    AddChangeChr(offset1, 1, ckAdd);
    exit;
  end;

  p := -1;
  delta := len2 - len1;
  InitDiagArrays(len1, len2);
  if delta < 0 then
  begin
    repeat
      Inc(p);
      if (p mod 1024 = 1023) then
      begin
        Application.ProcessMessages;
        if fCancelled then
          exit;
      end;
      //nb: the Snake order is important here
      for k := p downto delta + 1 do
        if SnakeChrF(k, offset1, offset2, len1, len2) then
          exit;
      for k := -p + delta to delta - 1 do
        if SnakeChrF(k, offset1, offset2, len1, len2) then
          exit;
      for k := delta - p to -1 do
        if SnakeChrB(k, offset1, offset2, len1, len2) then
          exit;
      for k := p downto 1 do
        if SnakeChrB(k, offset1, offset2, len1, len2) then
          exit;
      if SnakeChrF(delta, offset1, offset2, len1, len2) then
        exit;
      if SnakeChrB(0, offset1, offset2, len1, len2) then
        exit;
    until (False);
  end
  else
  begin
    repeat
      Inc(p);
      if (p mod 1024 = 1023) then
      begin
        Application.ProcessMessages;
        if fCancelled then
          exit;
      end;
      //nb: the Snake order is important here
      for k := -p to delta - 1 do
        if SnakeChrF(k, offset1, offset2, len1, len2) then
          exit;
      for k := p + delta downto delta + 1 do
        if SnakeChrF(k, offset1, offset2, len1, len2) then
          exit;
      for k := delta + p downto 1 do
        if SnakeChrB(k, offset1, offset2, len1, len2) then
          exit;
      for k := -p to -1 do
        if SnakeChrB(k, offset1, offset2, len1, len2) then
          exit;
      if SnakeChrF(delta, offset1, offset2, len1, len2) then
        exit;
      if SnakeChrB(0, offset1, offset2, len1, len2) then
        exit;
    until (False);
  end;
end;

function TplDiff.SnakeChrF(k, offset1, offset2, len1, len2: integer): boolean;
var
  x, y: integer;
begin
  if DiagF[k + 1] > DiagF[k - 1] then
    y := DiagF[k + 1]
  else
    y := DiagF[k - 1] + 1;
  x := y - k;
  while (x < len1 - 1) and (y < len2 - 1) and (Chrs1[offset1 + x + 1] = Chrs2[offset2 + y + 1]) do
  begin
    Inc(x);
    Inc(y);
  end;
  DiagF[k] := y;
  Result := (DiagF[k] >= DiagB[k]);
  if not Result then
    exit;

  Inc(x);
  Inc(y);
  PushDiff(offset1 + x, offset2 + y, len1 - x, len2 - y);
  PushDiff(offset1, offset2, x, y);
end;

function TplDiff.SnakeChrB(k, offset1, offset2, len1, len2: integer): boolean;
var
  x, y: integer;
begin
  if DiagB[k - 1] < DiagB[k + 1] then
    y := DiagB[k - 1]
  else
    y := DiagB[k + 1] - 1;
  x := y - k;
  while (x >= 0) and (y >= 0) and (Chrs1[offset1 + x] = Chrs2[offset2 + y]) do
  begin
    Dec(x);
    Dec(y);
  end;
  DiagB[k] := y;
  Result := DiagB[k] <= DiagF[k];
  if not Result then
    exit;

  Inc(x);
  Inc(y);
  PushDiff(offset1 + x, offset2 + y, len1 - x, len2 - y);
  PushDiff(offset1, offset2, x, y);
end;

function TplDiff.SnakeIntF(k, offset1, offset2, len1, len2: integer): boolean;
var
  x, y: integer;
begin
  if DiagF[k + 1] > DiagF[k - 1] then
    y := DiagF[k + 1]
  else
    y := DiagF[k - 1] + 1;
  x := y - k;
  while (x < len1 - 1) and (y < len2 - 1) and (Ints1[offset1 + x + 1] = Ints2[offset2 + y + 1]) do
  begin
    Inc(x);
    Inc(y);
  end;
  DiagF[k] := y;
  Result := (DiagF[k] >= DiagB[k]);
  if not Result then
    exit;

  Inc(x);
  Inc(y);
  PushDiff(offset1 + x, offset2 + y, len1 - x, len2 - y);
  PushDiff(offset1, offset2, x, y);
end;

function TplDiff.SnakeIntB(k, offset1, offset2, len1, len2: integer): boolean;
var
  x, y: integer;
begin
  if DiagB[k - 1] < DiagB[k + 1] then
    y := DiagB[k - 1]
  else
    y := DiagB[k + 1] - 1;
  x := y - k;
  while (x >= 0) and (y >= 0) and (Ints1[offset1 + x] = Ints2[offset2 + y]) do
  begin
    Dec(x);
    Dec(y);
  end;
  DiagB[k] := y;
  Result := DiagB[k] <= DiagF[k];
  if not Result then
    exit;

  Inc(x);
  Inc(y);
  PushDiff(offset1 + x, offset2 + y, len1 - x, len2 - y);
  PushDiff(offset1, offset2, x, y);
end;

procedure TplDiff.AddChangeChr(offset1, range: integer; ChangeKind: TChangeKind);
var
  i, j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (fLastCompareRec.oldIndex1 < offset1 - 1) do
  begin
    with fLastCompareRec do
    begin
      Kind := ckNone;
      Inc(oldIndex1);
      Inc(oldIndex2);
      chr1 := Chrs1[oldIndex1];
      chr2 := Chrs2[oldIndex2];
    end;
    New(compareRec);
    compareRec^ := fLastCompareRec;
    fCompareList.Add(compareRec);
    Inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckNone:
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin
          Kind := ckNone;
          Inc(oldIndex1);
          Inc(oldIndex2);
          chr1 := Chrs1[oldIndex1];
          chr2 := Chrs2[oldIndex2];
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        Inc(fDiffStats.matches);
      end;
    ckAdd:
    begin
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin

          //check if a range of adds are following a range of deletes
          //and convert them to modifies ...
          if Kind = ckDelete then
          begin
            j := fCompareList.Count - 1;
            while (j > 0) and (PCompareRec(fCompareList[j - 1]).Kind = ckDelete) do
              Dec(j);
            PCompareRec(fCompareList[j]).Kind := ckModify;
            Dec(fDiffStats.deletes);
            Inc(fDiffStats.modifies);
            Inc(fLastCompareRec.oldIndex2);
            PCompareRec(fCompareList[j]).oldIndex2 := fLastCompareRec.oldIndex2;
            PCompareRec(fCompareList[j]).chr2 := Chrs2[oldIndex2];
            if j = fCompareList.Count - 1 then
              fLastCompareRec.Kind := ckModify;
            continue;
          end;

          Kind := ckAdd;
          chr1 := #0;
          Inc(oldIndex2);
          chr2 := Chrs2[oldIndex2]; //ie what we added
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        Inc(fDiffStats.adds);
      end;
    end;
    ckDelete:
    begin
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin

          //check if a range of deletes are following a range of adds
          //and convert them to modifies ...
          if Kind = ckAdd then
          begin
            j := fCompareList.Count - 1;
            while (j > 0) and (PCompareRec(fCompareList[j - 1]).Kind = ckAdd) do
              Dec(j);
            PCompareRec(fCompareList[j]).Kind := ckModify;
            Dec(fDiffStats.adds);
            Inc(fDiffStats.modifies);
            Inc(fLastCompareRec.oldIndex1);
            PCompareRec(fCompareList[j]).oldIndex1 := fLastCompareRec.oldIndex1;
            PCompareRec(fCompareList[j]).chr1 := Chrs1[oldIndex1];
            if j = fCompareList.Count - 1 then
              fLastCompareRec.Kind := ckModify;
            continue;
          end;

          Kind := ckDelete;
          chr2 := #0;
          Inc(oldIndex1);
          chr1 := Chrs1[oldIndex1]; //ie what we deleted
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        Inc(fDiffStats.deletes);
      end;
    end;
  end;
end;

procedure TplDiff.AddChangeInt(offset1, range: integer; ChangeKind: TChangeKind);
var
  i, j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (fLastCompareRec.oldIndex1 < offset1 - 1) do
  begin
    with fLastCompareRec do
    begin
      Kind := ckNone;
      Inc(oldIndex1);
      Inc(oldIndex2);
      int1 := Ints1[oldIndex1];
      int2 := Ints2[oldIndex2];
    end;
    New(compareRec);
    compareRec^ := fLastCompareRec;
    fCompareList.Add(compareRec);
    Inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckNone:
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin
          Kind := ckNone;
          Inc(oldIndex1);
          Inc(oldIndex2);
          int1 := Ints1[oldIndex1];
          int2 := Ints2[oldIndex2];
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        Inc(fDiffStats.matches);
      end;
    ckAdd:
    begin
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin

          //check if a range of adds are following a range of deletes
          //and convert them to modifies ...
          if Kind = ckDelete then
          begin
            j := fCompareList.Count - 1;
            while (j > 0) and (PCompareRec(fCompareList[j - 1]).Kind = ckDelete) do
              Dec(j);
            PCompareRec(fCompareList[j]).Kind := ckModify;
            Dec(fDiffStats.deletes);
            Inc(fDiffStats.modifies);
            Inc(fLastCompareRec.oldIndex2);
            PCompareRec(fCompareList[j]).oldIndex2 := fLastCompareRec.oldIndex2;
            PCompareRec(fCompareList[j]).int2 := Ints2[oldIndex2];
            if j = fCompareList.Count - 1 then
              fLastCompareRec.Kind := ckModify;
            continue;
          end;

          Kind := ckAdd;
          int1 := $0;
          Inc(oldIndex2);
          int2 := Ints2[oldIndex2]; //ie what we added
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        Inc(fDiffStats.adds);
      end;
    end;
    ckDelete:
    begin
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin

          //check if a range of deletes are following a range of adds
          //and convert them to modifies ...
          if Kind = ckAdd then
          begin
            j := fCompareList.Count - 1;
            while (j > 0) and (PCompareRec(fCompareList[j - 1]).Kind = ckAdd) do
              Dec(j);
            PCompareRec(fCompareList[j]).Kind := ckModify;
            Dec(fDiffStats.adds);
            Inc(fDiffStats.modifies);
            Inc(fLastCompareRec.oldIndex1);
            PCompareRec(fCompareList[j]).oldIndex1 := fLastCompareRec.oldIndex1;
            PCompareRec(fCompareList[j]).int1 := Ints1[oldIndex1];
            if j = fCompareList.Count - 1 then
              fLastCompareRec.Kind := ckModify;
            continue;
          end;

          Kind := ckDelete;
          int2 := $0;
          Inc(oldIndex1);
          int1 := Ints1[oldIndex1]; //ie what we deleted
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        Inc(fDiffStats.deletes);
      end;
    end;
  end;
end;

procedure TplDiff.Clear;
var
  i: integer;
begin
  for i := 0 to fCompareList.Count - 1 do
    dispose(PCompareRec(fCompareList[i]));
  fCompareList.Clear;
  fLastCompareRec.Kind := ckNone;
  fLastCompareRec.oldIndex1 := -1;
  fLastCompareRec.oldIndex2 := -1;
  fDiffStats.matches := 0;
  fDiffStats.adds := 0;
  fDiffStats.deletes := 0;
  fDiffStats.modifies := 0;
  Ints1 := nil;
  Ints2 := nil;
  Chrs1 := nil;
  Chrs2 := nil;
end;


function TplDiff.GetCompareCount: integer;
begin
  Result := fCompareList.Count;
end;

function TplDiff.GetCompare(index: integer): TCompareRec;
begin
  Result := PCompareRec(fCompareList[index])^;
end;

procedure TplDiff.Cancel;
begin
  fCancelled := True;
end;

end.
