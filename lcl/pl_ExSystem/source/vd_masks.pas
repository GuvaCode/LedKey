
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit vd_masks;

interface

function vs_matchesMask(const Filename, Mask: string): boolean;

implementation

uses SysUtils;

const
  MaxCards = 30;

type
  TMask = class
  private
    FMask: Pointer;
    FSize: integer;
  public
    constructor Create(const MaskValue: string);
    destructor Destroy; override;
    function Matches(const Filename: string): boolean;
  end;

type
  PMaskSet = ^TMaskSet;
  TMaskSet = set of char;
  TMaskStates = (msLiteral, msAny, msSet, msMBCSLiteral);

  TMaskState = record
    SkipTo: boolean;
    case State: TMaskStates of
      msLiteral: (Literal: char);
      msAny: ();
      msSet: (
        Negate: boolean;
        CharSet: PMaskSet);
      msMBCSLiteral: (LeadByte, TrailByte: char);
  end;
  PMaskStateArray = ^TMaskStateArray;
  TMaskStateArray = array[0..128] of TMaskState;

function InitMaskStates(const Mask: string; var MaskStates: array of TMaskState): integer;
var
  I: integer;
  SkipTo: boolean;
  Literal: char;
  LeadByte, TrailByte: char;
  P: PChar;
  Negate: boolean;
  CharSet: TMaskSet;
  Cards: integer;

  procedure InvalidMask;
  begin
  end;

  procedure Reset;
  begin
    SkipTo := False;
    Negate := False;
    CharSet := [];
  end;

  procedure WriteScan(MaskState: TMaskStates);
  begin
    if I <= High(MaskStates) then
    begin
      if SkipTo then
      begin
        Inc(Cards);
        if Cards > MaxCards then
          InvalidMask;
      end;
      MaskStates[I].SkipTo := SkipTo;
      MaskStates[I].State := MaskState;
      case MaskState of
        msLiteral: MaskStates[I].Literal := UpCase(Literal);
        msSet:
        begin
          MaskStates[I].Negate := Negate;
          New(MaskStates[I].CharSet);
          MaskStates[I].CharSet^ := CharSet;
        end;
        msMBCSLiteral:
        begin
          MaskStates[I].LeadByte := LeadByte;
          MaskStates[I].TrailByte := TrailByte;
        end;
      end;
    end;
    Inc(I);
    Reset;
  end;

  procedure ScanSet;
  var
    LastChar: char;
    C: char;
  begin
    Inc(P);
    if P^ = '!' then
    begin
      Negate := True;
      Inc(P);
    end;
    LastChar := #0;
    while not (P^ in [#0, ']']) do
    begin
      // MBCS characters not supported in msSet!
      if P^ in LeadBytes then
        Inc(P)
      else
        case P^ of
          '-':
            if LastChar = #0 then
              InvalidMask
            else
            begin
              Inc(P);
              for C := LastChar to UpCase(P^) do
                Include(CharSet, C);
            end;
          else
            LastChar := UpCase(P^);
            Include(CharSet, LastChar);
        end;
      Inc(P);
    end;
    if (P^ <> ']') or (CharSet = []) then
      InvalidMask;
    WriteScan(msSet);
  end;

begin
  P := @Mask;
  I := 0;
  Cards := 0;
  Reset;
  while P^ <> #0 do
  begin
    case P^ of
      '*': SkipTo := True;
      '?': if not SkipTo then
          WriteScan(msAny);
      '[': ScanSet;
      else
        if P^ in LeadBytes then
        begin
          LeadByte := P^;
          Inc(P);
          TrailByte := P^;
          WriteScan(msMBCSLiteral);
        end
        else
        begin
          Literal := P^;
          WriteScan(msLiteral);
        end;
    end;
    Inc(P);
  end;
  Literal := #0;
  WriteScan(msLiteral);
  Result := I;
end;

function MatchesMaskStates(const Filename: string; MaskStates: array of TMaskState): boolean;
type
  TStackRec = record
    sP: PChar;
    sI: integer;
  end;
var
  T: integer;
  S: array[0..MaxCards - 1] of TStackRec;
  I: integer;
  P: PChar;

  procedure Push(P: PChar; I: integer);
  begin
    with S[T] do
    begin
      sP := P;
      sI := I;
    end;
    Inc(T);
  end;

  function Pop(var P: PChar; var I: integer): boolean;
  begin
    if T = 0 then
      Result := False
    else
    begin
      Dec(T);
      with S[T] do
      begin
        P := sP;
        I := sI;
      end;
      Result := True;
    end;
  end;

  function Matches(P: PChar; Start: integer): boolean;
  var
    I: integer;
  begin
    Result := False;
    for I := Start to High(MaskStates) do
      with MaskStates[I] do
      begin
        if SkipTo then
        begin
          case State of
            msLiteral:
              while (P^ <> #0) and (UpperCase(P^) <> Literal) do
                Inc(P);
            msSet:
              while (P^ <> #0) and not (Negate xor (UpCase(P^) in CharSet^)) do
                Inc(P);
            msMBCSLiteral:
              while (P^ <> #0) do
              begin
                if (P^ <> LeadByte) then
                  Inc(P, 2)
                else
                begin
                  Inc(P);
                  if (P^ = TrailByte) then
                    Break;
                  Inc(P);
                end;
              end;
          end;
          if P^ <> #0 then
            Push(@P[1], I);
        end;
        case State of
          msLiteral: if UpperCase(P^) <> Literal then
              Exit;
          msSet: if not (Negate xor (UpCase(P^) in CharSet^)) then
              Exit;
          msMBCSLiteral:
          begin
            if P^ <> LeadByte then
              Exit;
            Inc(P);
            if P^ <> TrailByte then
              Exit;
          end;
        end;
        Inc(P);
      end;
    Result := True;
  end;

begin
  Result := True;
  T := 0;
  P := @Filename;
  I := Low(MaskStates);
  repeat
    if Matches(P, I) then
      Exit;
  until not Pop(P, I);
  Result := False;
end;

procedure DoneMaskStates(var MaskStates: array of TMaskState);
var
  I: integer;
begin
  for I := Low(MaskStates) to High(MaskStates) do
    if MaskStates[I].State = msSet then
      Dispose(MaskStates[I].CharSet);
end;

//================== TMask ===========================================

constructor TMask.Create(const MaskValue: string);
var
  A: array[0..0] of TMaskState;
begin
  FSize := InitMaskStates(MaskValue, A);
  FMask := AllocMem(FSize * SizeOf(TMaskState));
  InitMaskStates(MaskValue, Slice(PMaskStateArray(FMask)^, FSize));
end;

destructor TMask.Destroy;
begin
  if FMask <> nil then
  begin
    DoneMaskStates(Slice(PMaskStateArray(FMask)^, FSize));
    FreeMem(FMask, FSize * SizeOf(TMaskState));
  end;
end;

function TMask.Matches(const Filename: string): boolean;
begin
  Result := MatchesMaskStates(Filename, Slice(PMaskStateArray(FMask)^, FSize));
end;

function vs_matchesMask(const Filename, Mask: string): boolean;
var
  CMask: TMask;
begin
  CMask := TMask.Create(Mask);
  try
    Result := CMask.Matches(Filename);
  finally
    CMask.Free;
  end;
end;

end.
