{ Copyright (C) 2007 Julian Schutsch

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU Lesser General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
  details.

  A copy of the GNU Lesser General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/lgpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit uCmdBox;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ExtCtrls, Controls, Graphics, Forms, LCLType, LCLIntf,
     LazUtf8, //=== ct9999 ================
     lmessages, lresources, ClipBrd, LCLProc;

type
  TCaretType = (cartLine, cartSubBar, cartBigBar, cartUser);
  TEscapeCodeType = (esctCmdBox, esctAnsi, esctNone);
  TEscapeMode = (escmNone, escmOperation, escmData2, escmData1,
                 escmAnsiOperation, escmAnsiSquare);
  TCharAttrib = (charaUnderline, charaItalic, charaBold, charaBlink);
  TWrapMode = (wwmChar, wwmWord);

type
  TCmdBox = class;

type
  TColorstring = class;

type
  EOnCmdBoxInput = procedure(ACmdBox: TCmdBox; Input: string) of object;

type
  EOnCmdBoxInputChange = procedure(ACmdBox: TCmdBox; InputData: TColorstring) of object;

type

  { TCmdBox }

  TCmdBox = class(TCustomControl)
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WMVScroll(var message: TLMVScroll); message LM_VSCROLL;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: Integer); override;
  private
    FLock:      System.TRTLCriticalSection;
    FCaretTimer: TTimer;
    FCaretVisible: boolean;
    FLineCount: LongInt;
    FLines:     array of TColorstring;
    FLineHeights: array of LongInt;
    FLineHeightSum: array of LongInt;
    FTopLine:   LongInt;
    FPageHeight: LongInt;
    FVisibleLines: LongInt;
    FVSBVisible: boolean;
    FVSBPos:    LongInt;
    FVSBWidth:  LongInt;
    FClientWidth: LongInt;
    FClientHeight: LongInt;
    FCaretX:    LongInt;
    FOutX, FOutY: LongInt;
    FInputX, FInputY: LongInt;
    FInputPos:  LongInt;
    FCharHeight: LongInt;
    FLineOfTopLine: LongInt;
    FVisibleLineCount: LongInt;
    FInput:     boolean;
    FInputBuffer: TColorstring;
    FInputVisible: boolean;
    FInputMinPos: LongInt;
    FUTF8InputMinPos: LongInt;
    FOnInput:   EOnCmdBoxInput;
    FOnAny:     EOnCmdBoxInputChange;
    FOnInputChange: EOnCmdBoxInputChange;
    FBackGroundColor: TColor;
    FCurrentColor: TColor;
    FCurrentBackGround: TColor;
    FFont:      TFont;
    FPassWordChar: TUTF8Char;
    FInputIsPassWord: boolean;
    FHistory:   array of TColorstring;
    FHistoryLength: LongInt;
    FHistoryMax: LongInt;
    FHistoryPos: LongInt;
    FInputColor: TColor;
    FInputBackground: TColor;
    FInputSelColor: TColor;
    FInputSelBackGround: TColor;
    FMouseDown: boolean;
    FSelStart, FSelEnd: LongInt;
    FMouseDownInputPos: LongInt;
    FCurrentString: string;
    FCaretColor: TColor;
    FCaretType: TCaretType;
    FCaretWidth: LongInt;
    FCaretHeight: LongInt;
    FCaretYShift: LongInt;
    FTabWidth:  LongInt;
    FGraphicCharWidth: LongInt;
    FEscapeCodeType: TEscapeCodeType;
    FEscapeMode: TEscapeMode;
    FEscapeData: string;
    FStringBuffer: TStringList;
    FAutoFollow: boolean;
    FCurrentAttrib: TCharAttrib;
    FInputAttrib: TCharAttrib;
    FWrapMode:  TWrapMode;
    FWriteInput: Boolean;
    procedure CaretTimerExecute(Sender: TObject);
    procedure SetLineCount(c: LongInt);
    procedure SetTopLine(Nr: LongInt);
    procedure AdjustScrollBars(const Recalc:Boolean=False);
    function  AdjustLineHeight(i: LongInt;const Recalc:Boolean=False): LongInt;
    procedure MakeInputVisible;
    procedure MakeOutVisible;
    procedure SetFont(F: TFont);
    procedure SetBackGroundColor(c: Tcolor);
    function  GetSystemMetricsGapSize(const Index: LongInt): LongInt;
    procedure ScrollBarRange(Which: LongInt; aRange, aPage: LongInt);
    procedure ScrollBarPosition(Which, Value: LongInt);
    function  UpdateLineHeights(const Recalc:Boolean=False): LongInt;
    procedure TranslateScrollBarPosition;
    procedure ScrollUp;
    procedure SetHistoryMax(v: LongInt);
    procedure InsertHistory;
    procedure SetHistoryPos(v: LongInt);
    function  GetHistory(i: LongInt): string;
    procedure DeleteHistoryEntry(i: LongInt);
    procedure MakeFirstHistoryEntry(i: LongInt);
    function  MoveInputCaretTo(x, y: LongInt; chl: boolean): boolean;
    procedure SetSelection(Start, Ende: LongInt);
    procedure LeftSelection(Start, Ende: LongInt);
    procedure RightSelection(Start, Ende: LongInt);
    procedure DeleteSelected;
    procedure SetOutY(v: LongInt);
    procedure IntWrite;
    procedure MultiWrite;
    procedure SetCaretType(ACaretType: TCaretType);
    procedure SetCaretWidth(AValue: LongInt);
    procedure SetCaretHeight(AValue: LongInt);
    procedure SetCaretYShift(AValue: LongInt);
    procedure SetTabWidth(AValue: LongInt);
    function  GetCaretInterval: LongInt;
    procedure SetCaretInterval(AValue: LongInt);
    procedure SetWrapMode(AValue:TWrapMode);

  public

    procedure SaveToFile(AFileName: string);
    function  HistoryHas(s: string): boolean;
    function  HistoryIndexOf(s: string): LongInt;
    procedure ClearHistory;
    procedure TextColor(C: TColor);
    procedure TextBackground(C: TColor);
    procedure TextColors(FC, BC: TColor);
    procedure Write(s: string);
    procedure Writeln(s: string);
    procedure WriteStream(Stream: TStream);
    procedure Clear;
    procedure StartRead(DFC, DBC: TColor; const Desc: string; IFC, IBC: TColor);
    procedure StartReadPassWord(DFC, DBC: TColor; const Desc: string; IFC, IBC: TColor);
    procedure StopRead;
    procedure CopyToClipBoard;
    procedure PasteFromClipBoard;
    procedure CutToClipBoard;
    procedure ClearLine;
    property  OutX: LongInt Read FOutX Write FOutX;
    property  OutY: LongInt Read FOutY Write SetOutY;
    property  TopLine: LongInt Read FTopLine Write SetTopLine;
    property  History[i: LongInt]: string Read GetHistory;
    property  InputPos: LongInt Read FInputPos;
    function  HistoryCount: LongInt;
  published
    property CaretColor: TColor Read FCaretColor Write FCaretColor;
    property CaretType: TCaretType Read FCaretType Write SetCaretType;
    property CaretWidth: LongInt Read FCaretWidth Write SetCaretWidth;
    property CaretHeight: LongInt Read FCaretHeight Write SetCaretHeight;
    property CaretYShift: LongInt Read FCaretYShift Write SetCaretYShift;
    property OnInput: EOnCmdBoxInput Read FOnInput Write FOnInput;
    property OnInputChange: EOnCmdBoxInputChange Read FOnInputChange Write FOnInputChange;
    property OnAny: EOnCmdBoxInputChange Read FOnAny Write FOnAny;
    property LineCount: LongInt Read FLineCount Write SetLineCount;
    property Font: TFont Read FFont Write SetFont;
    property BackGroundColor: TColor Read FBackgroundColor Write SetBackGroundColor;
    property TabWidth: LongInt Read FTabWidth Write SetTabWidth;
    property PassWordChar: TUTF8Char Read FPassWordChar Write FPassWordChar;
    property HistoryMax: LongInt Read FHistoryMax Write SetHistoryMax;
    property InputSelColor: TColor Read FInputSelColor Write FInputSelColor;
    property InputSelBackGround: TColor Read FInputSelBackGround write FInputSelBackGround;
    property CaretInterval: LongInt Read GetCaretInterval Write SetCaretInterval;
    property EscapeCodeType: TEscapeCodeType Read FEscapeCodeType Write FEscapeCodeType;
    property GraphicalCharacterWidth: LongInt Read FGraphicCharWidth Write FGraphicCharWidth;
    property AutoFollow: boolean Read FAutoFollow Write FAutoFollow default True;
    property WrapMode: TWrapMode Read FWrapMode Write SetWrapMode default wwmWord;
    property WriteInput:Boolean read FWriteInput write FWriteInput default True;
    //---------------------
    property Align;
    property Anchors;
    property ShowHint;
    property BorderSpacing;
    property PopupMenu;
    property Visible;
    property HelpType;
    property HelpKeyWord;
    property DragMode;
    property DragKind;
    property DragCursor;
    property Constraints;
    property ParentShowHint;
    property Enabled;
    property BorderStyle;
    property DoubleBuffered default True;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDblClick;
    property OnClick;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnExit;
    property OnEnter;
    property OnContextPopup;
    property OnShowHint;
    property OnUTF8KeyPress;
  end;

type
  TColorChar = packed record
    FChar:      TUTF8Char;
    FCharWidth: LongInt;
    FSumWidth:  LongInt;
    FWordStart: LongInt;
    FFrontColor: TColor;
    FBackColor: TColor;
    FAttrib:    TCharAttrib;
  end;

type
  TColorString = class
  private
    FChars:    packed array of TColorChar;
    FSumWidth: LongInt;
    FPassWordStart: LongInt;
    FPassWordChar: TUTF8Char;
    FTabWidth: LongInt;
    FWrapMode:TWrapMode;
    FStoredLineCount:LongInt;
    procedure MinimumLength(V: LongInt; FC, BC: TColor);
    procedure MaximumLength(V: LongInt);
    procedure UpdateSum;
    procedure UpdateAll;
  public
    constructor Create(AFont: TFont);
    destructor Destroy; override;
    procedure Clear;
    procedure OverWrite(S: string; Pos: LongInt; FC, BC: TColor; Attrib: TCharAttrib);
    procedure OverWriteChar(s: TUTF8Char; Pos, ADefWidth: LongInt; FC, BC: TColor; Attrib: TCharAttrib);
    procedure OverWrite(S: TColorstring; Pos: LongInt);
    procedure OverWritePW(S: TColorstring; PWS, Pos: LongInt; PWC: string);
    procedure PartOverWrite(S: TColorstring; Start, Ende, Pos: LongInt);
    procedure LineOutAndFill(ACanvas: TCanvas;
                             AX, AY, ALeftX, AWrapWidth, ACH, ACB, ACaretPos: LongInt;
                             ABC, ACC: TColor; ACaretHeight, ACaretWidth, ACaretYShift: LongInt;
                             ADrawCaret: boolean);
    function  Getstring: string;
    function  GetPartstring(Start, Ende: LongInt): string;
    procedure Delete(Index: LongInt);
    procedure Delete(Index, Len: LongInt);
    procedure Insert(Index: LongInt; C: string; FC, BC: TColor; Attrib: TCharAttrib);
    procedure BColorBlock(StartPos, EndPos: LongInt; C: TColor);
    procedure ColorBlock(StartPos, EndPos: LongInt; FC, BC: TColor);
    function  LineCount(AWrapWidth, ACaretPos, ACaretWidth: LongInt): LongInt;
    function  GetLength: LongInt;
    function  GetLineOfCaret(AWrapWidth, ACaretPos, ACaretWidth: LongInt): LongInt;
    function  GetCharPosition(AWrapWidth, ALine, AXPos: LongInt): LongInt;
  private
    FFont: TFont;
    FDefaultBackGround: TColor;
  public
    property TabWidth: LongInt Read FTabWidth Write FTabWidth;
    property PassWordChar: TUTF8Char Read FPassWordChar Write FPassWordChar;
    property PassWordStart: LongInt Read FPassWordStart Write FPassWordStart;
    property Length: LongInt Read GetLength;
    property DefaultBackGround: TColor Read FDefaultBackground Write FDefaultBackground;
    property Font: TFont Read FFont Write FFont;
  end;

implementation

procedure TCmdBox.SaveToFile(AFileName: string);
var
  Txt: System.Text;
  i:   LongInt;
begin
  AssignFile(Txt, AFileName);
  Rewrite(Txt);
  for i := 0 to LineCount - 1 do
  begin
    with FLines[i] do
    begin
      system.Writeln(Txt, GetString);
    end;
  end;
  CloseFile(Txt);
end;

procedure TColorString.UpdateAll;
var i:LongInt;
begin
  for i:=0 to High(FChars) do
  begin
    with FChars[i] do
    begin
      FCharWidth := FFont.GetTextWidth(FChar);
    end;
  end;
  UpdateSum;
end;

procedure TColorString.UpdateSum;
var
  i: LongInt;
  LastWordStart: LongInt;
  SumWidth: LongInt;
begin
  LastWordStart := 0;
  SumWidth      := 0;
  case FWrapMode of
    wwmChar:
    begin
      for i := 0 to High(FChars) do
      begin
        with FChars[i] do
        begin
          FWordStart := i;
          case FChar[1] of
            #9:
            begin
              FCharWidth    := (SumWidth div FTabWidth + 1) * FTabWidth - SumWidth;
            end;
            #27:
            begin
              case FChar[2] of
                #9:
                begin
                  FCharWidth    := (SumWidth div FTabWidth + 1) * FTabWidth - SumWidth;
                end;
                #10: LastWordStart := i + 1;
                #32, #46, #196, #205:
                begin
                  FCharWidth    := Ord(FChar[3]);
                end;
                #33, #47, #197, #206:
                begin
                  FCharWidth := (Ord(FChar[3]) + Ord(FChar[4]) * 256) - SumWidth;
                  if FCharWidth < 0 then FCharWidth  := 0;
                end;
              end;
            end;
          end;
          SumWidth  := SumWidth + FCharWidth;
          FSumWidth := SumWidth;
        end;
      end;
    end;
    wwmWord:
    begin
      for i := 0 to High(FChars) do
      begin
        with FChars[i] do
        begin
          FWordStart := LastWordStart;
          case FChar[1] of
            #9:
            begin
              FCharWidth    := (SumWidth div FTabWidth + 1) * FTabWidth - SumWidth;
              LastWordStart := i + 1;
            end;
            #27:
            begin
              case FChar[2] of
                #9:
                begin
                  FCharWidth    := (SumWidth div FTabWidth + 1) * FTabWidth - SumWidth;
                  LastWordStart := i + 1;
                end;
                #10: LastWordStart := i + 1;
                #32, #46, #196, #205:
                begin
                  FCharWidth    := Ord(FChar[3]);
                  LastWordStart := i + 1;
                end;
                #33, #47, #197, #206:
                begin
                  FCharWidth := (Ord(FChar[3]) + Ord(FChar[4]) * 256) - SumWidth;
                  if FCharWidth < 0 then
                    FCharWidth  := 0;
                  LastWordStart := i + 1;
                end;
              end;
            end;
            else if FChar = ' ' then LastWordStart := i + 1;
          end;
          SumWidth  := SumWidth + FCharWidth;
          FSumWidth := SumWidth;
        end;
      end;
    end;
  end;
  FSumWidth := SumWidth;
  FStoredLineCount:=-1;
end;

function TColorString.GetLength: LongInt;
begin
  Result := System.Length(FChars);
end;

procedure TCmdBox.SetWrapMode(AValue:TWrapMode);
var i:LongInt;
begin
  if AValue<>FWrapMode then
  begin
    FWrapMode:=AValue;
    for i:=0 to FLineCount-1 do
    begin
      FLines[i].FWrapMode:=AValue;
      FLines[i].UpdateSum;
    end;
    FInputBuffer.FWrapMode:=AValue;
    FInputBuffer.UpdateSum;
    UpdateLineHeights;
    Invalidate;
  end;
end;

procedure TCmdBox.SetTabWidth(AValue: LongInt);
var
  i: LongInt;
begin
  FTabWidth := AValue;
  for i := 0 to FLineCount - 1 do
  begin
    FLines[i].TabWidth := AValue;
    FLines[i].UpdateSum;
  end;
  UpdateLineHeights;
  Invalidate;
end;

procedure TCmdBox.SetCaretWidth(AValue: LongInt);
begin
  FCaretWidth := AValue;
  FCaretType  := cartUser;
end;

procedure TCmdBox.SetCaretHeight(AValue: LongInt);
begin
  FCaretHeight := AValue;
  FCaretType   := cartUser;
end;

procedure TCmdBox.SetCaretYShift(AValue: LongInt);
begin
  FCaretYShift := AValue;
  FCaretType   := cartUser;
end;

procedure TCmdBox.SetCaretType(ACaretType: TCaretType);
begin
  case ACaretType of
    cartLine:
    begin
      if HandleAllocated then
        FCaretHeight := FFont.GetTextHeight('A') - 3
      else
        FCaretHeight := -1;
      FCaretWidth := 1;
      FCaretYShift := 3;
    end;
    cartSubBar:
    begin
      FCaretWidth  := -1;
      FCaretHeight := 3;
      FCaretYShift := 0;
    end;
    cartBigBar:
    begin
      if HandleAllocated then
        FCaretHeight := FFont.GetTextHeight('A') - 3
      else
        FCaretHeight := -1;
      FCaretWidth := -1;
      FCaretYShift := 3;
    end;
  end;
  Invalidate;
  FCaretType := ACaretType;
end;

// TOdo : Use string buffer instead of string (speed improvement expected)
procedure TColorString.LineOutAndFill(ACanvas: TCanvas;
  AX, AY, ALeftX, AWrapWidth, ACH, ACB, ACaretPos: LongInt; ABC, ACC: TColor;
  ACaretHeight, ACaretWidth, ACaretYShift: LongInt; ADrawCaret: boolean);
var
  LineStart         : LongInt;
  LineEnd           : LongInt;
  MidWidth          : LongInt;
  LineStartSumWidth : LongInt;
  x                 : LongInt;
  LastLineSumWidth  : LongInt;
  ACHH              : LongInt;
  ACBH              : LongInt;
  SAX               : LongInt;
  SAY               : LongInt;

  procedure DrawLine;
  var
    SameColor: string;
    SameForeColor: TColor;
    SameBackColor: TColor;
    SameColorX: LongInt;
    SameColorWidth: LongInt;
    LP:     LongInt;
    CaretX: LongInt;
    CaretW: LongInt;
    CW:     LongInt;
    xp:     LongInt;
  begin
    if (AY <= -ACH) and (AY > ACanvas.Height) then
    begin
      Inc(AY, ACH);
      Ax := ALeftx;
      Exit;
    end;
    SameColor := '';
    SameForeColor := 0;
    SameColorX := 0;
    SameColorWidth := 0;
    ACanvas.Brush.Style := bsClear;
    // A thing for older versions!
    ACanvas.Font.GetTextWidth('%%%_$%_Hallo\\\\\\\\\32489738');
    // End of shit
    LP     := LineStart;
    CaretX := -1;
    while LineStart <> LineEnd + 1 do
    begin
      with FChars[LineStart] do
      begin
        CW := FCharWidth;
        if FChar = #9 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.Font.Color  := SameForeColor;
            ACanvas.TextOut(SameColorX, AY, SameColor);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
        end
        else
        if FChar[1] = #27 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.Font.Color  := SameForeColor;
            ACanvas.TextOut(SameColorX, AY, SameColor);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
          case FChar[2] of
            #9:
            begin
              case FChar[3] of
                #46:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.Pen.Style   := psDash;
                  xp := SameColorX;
                  if xp mod 2 <> 0 then
                    Inc(xp);
                  while xp < SameColorX + FCharWidth do
                  begin
                    ACanvas.Pixels[xp, AY + ACH - 3] := FFrontColor;
                    Inc(xp, 2);
                  end;
                end;
                #196:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.Pen.Style   := psSolid;
                  ACanvas.Line(SameColorX, AY + ACHH, SameColorX +
                    FCharWidth, AY + ACHH);
                end;
              end;
            end;
            #10:
            begin
              CW := AWrapWidth - SameColorX;
              case FChar[3] of
                #179:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.Line(SameColorX + CW - ACBH, AY, SameColorX +
                    CW - ACBH, AY + ACH);
                end;
                #180:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.Line(SameColorX + CW - ACBH, AY, SameColorX +
                    CW - ACBH, AY + ACH);
                  ACanvas.Line(SameColorX, AY + ACHH, SameColorX + CW - ACBH, AY + ACHH);
                end;
                #191:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.MoveTo(SameColorX, AY + ACHH);
                  ACanvas.LineTo(SameColorX + CW - ACBH, AY + ACHH);
                  ACanvas.LineTo(SameColorX + CW - ACBH, AY + ACH);
                end;
                #196:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.Line(SameColorX, AY + ACHH, SameColorX + CW, AY + ACHH);
                end;
                #205:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.Line(SameColorX, AY + ACHH - 1, SameColorX +
                    CW, AY + ACHH - 1);
                  ACanvas.Line(SameColorX, AY + ACHH + 1, SameColorX +
                    CW, AY + ACHH + 1);
                end;
                #217:
                begin
                  ACanvas.Pen.Color   := FFrontColor;
                  ACanvas.MoveTo(SameColorX, AY + ACHH);
                  ACanvas.LineTo(SameColorX + CW - ACBH, AY + ACHH);
                  ACanvas.LineTo(SameColorX + CW - ACBH, AY - 1);
                end;
              end;
            end;
            #32, #33:
            begin
            end;
            #46, #47:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              xp := SameColorX;
              if xp mod 2 <> 0 then
                Inc(xp);
              while xp < SameColorX + FCharWidth do
              begin
                ACanvas.Pixels[xp, AY + ACH - 3] := FFrontColor;
                Inc(xp, 2);
              end;
            end;
            #196, #197:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.Line(SameColorX, AY + ACHH, SameColorX + FCharWidth, AY + ACHH);
            end;
            #179:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.Line(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACH);
            end;
            #193:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.Line(SameColorX, AY + ACHH, SameColorX + ACB, AY + ACHH);
              ACanvas.Line(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACHH);
            end;
            #194:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.Line(SameColorX, AY + ACHH, SameColorX + ACB, AY + ACHH);
              ACanvas.Line(SameColorX + ACBH, AY + ACHH, SameColorX + ACBH, AY + ACH);
            end;
            #198:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.Line(SameColorX, AY + ACHH, SameColorX + ACB, AY + ACHH);
              ACanvas.Line(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACH);
            end;
            #195:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.Line(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACH);
              ACanvas.Line(SameColorX + ACBH, AY + ACHH, SameColorX + ACB, AY + ACHH);
            end;
            #217:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.MoveTo(SameColorX + ACBH, AY);
              ACanvas.LineTo(SameColorX + ACBH, AY + ACHH);
              ACanvas.LineTo(SameColorX + ACB, AY + ACHH);
            end;
            #218:
            begin
              ACanvas.Pen.Color   := FFrontColor;
              ACanvas.MoveTo(SameColorX + ACBH, AY + ACH);
              ACanvas.LineTo(SameColorX + ACBH, AY + ACHH);
              ACanvas.LineTo(SameColorX + ACB, AY + ACHH);
            end;
          end;
        end
        else
        if SameColor = '' then
        begin
          if (LP >= FPassWordStart) then
          begin
            SameColor      := FPassWordChar;
            SameColorWidth := FFont.GetTextWidth(FPassWordChar);
          end
          else
          begin
            SameColor      := FChar;
            SameColorWidth := FCharWidth;
          end;
          SameColorX    := AX;
          SameForeColor := FFrontColor;
          SameBackColor := FBackColor;
        end
        else
        begin
          if (SameForeColor = FFrontColor) and (SameBackColor = FBackColor) then
          begin
            if (LP >= FPassWordStart) then
            begin
              SameColor := SameColor + FPassWordChar;
              Inc(SameColorWidth, FFont.GetTextWidth(FPassWordChar));
            end
            else
            begin
              SameColor := SameColor + FChar;
              Inc(SameColorWidth, FCharWidth);
            end;
          end
          else
          begin
            ACanvas.Font.Color  := SameForeColor;
            ACanvas.TextOut(SameColorX, AY, SameColor);
            if (LP >= FPassWordStart) then
            begin
              SameColor      := FPassWordChar;
              SameColorWidth := FFont.GetTextWidth(FPassWordChar);
            end
            else
            begin
              SameColor      := FChar;
              SameColorWidth := FCharWidth;
            end;
            SameForeColor := FFrontColor;
            SameBackColor := FBackColor;
            SameColorX    := AX;
          end;
        end;
        if LP = ACaretPos then
        begin
          CaretX := AX;
          CaretW := FCharWidth;
        end;
        Inc(AX, CW);
        Inc(LP);
      end;
      Inc(LineStart);
    end;
    if SameColor <> '' then
    begin
      ACanvas.Font.Color  := SameForeColor;
      ACanvas.TextOut(SameColorX, AY, SameColor);
    end;
    AX := ALeftX;
    Inc(AY, ACH);
    if ADrawCaret and (CaretX >= 0) then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := ACC;
      if ACaretWidth >= 0 then
        CaretW := ACaretWidth;
      ACanvas.FillRect(CaretX, AY - ACaretHeight - ACaretYShift,
        CaretX + CaretW, AY - ACaretYShift);
    end;
  end;

  procedure DrawBack;
  var
    SameColor: string;
    SameForeColor: TColor;
    SameBackColor: TColor;
    SameColorX: LongInt;
    SameColorWidth: LongInt;
    LP:     LongInt;
    CW:     LongInt;
  begin
    if (AY <= -ACH) and (AY > ACanvas.Height) then
    begin
      Inc(AY, ACH);
      Ax := ALeftx;
      Exit;
    end;
    SameColor := '';
    SameBackColor := 0;
    SameColorX := 0;
    SameColorWidth := 0;
    ACanvas.Brush.Style := bsSolid;
    // A thing for older versions!
    ACanvas.Font.GetTextWidth('%%%_$%_Hallo\\\\\\\\\32489738');
    // End of shit
    LP     := LineStart;
    while LineStart <> LineEnd + 1 do
    begin
      with FChars[LineStart] do
      begin
        CW := FCharWidth;
        if FChar = #9 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.Brush.Color := SameBackColor;
            ACanvas.FillRect(SameColorX, AY, SameColorX + SameColorWidth, Ay + ACH);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
          ACanvas.Brush.Color := FBackColor;
          ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
        end
        else
        if FChar[1] = #27 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.Brush.Color := SameBackColor;
            ACanvas.FillRect(SameColorX, AY, SameColorX + SameColorWidth, Ay + ACH);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
          case FChar[2] of
            #9:
            begin
              case FChar[3] of
                #46:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
                end;
                #196:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
                end;
              end;
            end;
            #10:
            begin
              CW := AWrapWidth - SameColorX;
              case FChar[3] of
                #179:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + CW, AY + ACH);
                end;
                #180:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + CW, AY + ACH);
                end;
                #191:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + CW, AY + ACH);
                end;
                #196:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + CW, AY + ACH);
                end;
                #205:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + CW, AY + ACH);
                end;
                #217:
                begin
                  ACanvas.Brush.Color := FBackColor;
                  ACanvas.Fillrect(SameColorX, AY, SameColorX + CW, AY + ACH);
                end;
              end;
            end;
            #32, #33:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #46, #47:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #196, #197:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #179:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #193:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #194:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #198:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #195:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #217:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
            #218:
            begin
              ACanvas.Brush.Color := FBackColor;
              ACanvas.Fillrect(SameColorX, AY, SameColorX + FCharWidth, AY + ACH);
            end;
          end;
        end
        else
        if SameColor = '' then
        begin
          if (LP >= FPassWordStart) then
          begin
            SameColor      := FPassWordChar;
            SameColorWidth := FFont.GetTextWidth(FPassWordChar);
          end
          else
          begin
            SameColor      := FChar;
            SameColorWidth := FCharWidth;
          end;
          SameColorX    := AX;
          SameForeColor := FFrontColor;
          SameBackColor := FBackColor;
        end
        else
        begin
          if (SameForeColor = FFrontColor) and (SameBackColor = FBackColor) then
          begin
            if (LP >= FPassWordStart) then
            begin
              SameColor := SameColor + FPassWordChar;
              Inc(SameColorWidth, FFont.GetTextWidth(FPassWordChar));
            end
            else
            begin
              SameColor := SameColor + FChar;
              Inc(SameColorWidth, FCharWidth);
            end;
          end
          else
          begin
            ACanvas.Brush.Color := SameBackColor;
            ACanvas.FillRect(SameColorX, Ay, SameColorX + SameColorWidth, Ay + ACH);
            if (LP >= FPassWordStart) then
            begin
              SameColor      := FPassWordChar;
              SameColorWidth := FFont.GetTextWidth(FPassWordChar);
            end
            else
            begin
              SameColor      := FChar;
              SameColorWidth := FCharWidth;
            end;
            SameForeColor := FFrontColor;
            SameBackColor := FBackColor;
            SameColorX    := AX;
          end;
        end;
        Inc(AX, CW);
        Inc(LP);
      end;
      Inc(LineStart);
    end;
    if SameColor <> '' then
    begin
      ACanvas.Brush.Color := SameBackColor;
      ACanvas.FillRect(SameColorX, Ay, SameColorX + SameColorWidth, Ay + ACH);
    end;
    ACanvas.FillRect(AX, AY, AWrapWidth, AY + ACH);
    AX := ALeftX;
    Inc(AY, ACH);
  end;

begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ABC;
    ACanvas.FillRect(AX, AY, AWrapWidth, AY + ACH);
    Exit;
  end;
  ACHH     := ACH div 2;
  ACBH     := ACB div 2;
  SAX:=AX;
  SAY:=AY;
  MidWidth := FSumWidth div System.Length(FChars);
  // Draw background
  LineStart         := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth  := 0;
  x                 := 0;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LineEnd := x;
    DrawBack;
    LastLineSumWidth  := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart         := x + 1;
  end;
  // Draw foreground
  LineStart         := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth  := 0;
  x                 := 0;
  AX:=SAX;
  AY:=SAY;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LineEnd := x;
    DrawLine;
    LastLineSumWidth  := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart         := x + 1;
  end;
  // Draw Caret
  if ACaretPos >= LineStart then
  begin
    if ACaretWidth >= 0 then
      x := ACaretWidth
    else
      x := FFont.GetTextWidth('A');
    AX := LineStartSumWidth - LastLineSumWidth + (ACaretPos - LineStart) * x;
    if Ax + x > AWrapWidth then
    begin
      Ax := 0;
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := ABC;
      ACanvas.FillRect(0, AY, AWrapWidth, AY + ACH);
      Inc(Ay, ACH);
    end;
    if ADrawCaret then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := ACC;
      ACanvas.FillRect(AX, AY - ACaretHeight - ACaretYShift, AX + x, AY - ACaretYShift);
    end;
  end;
end;

function TColorString.GetCharPosition(AWrapWidth, ALine, AXPos: LongInt): LongInt;
var
  x, MidWidth, LineStart, LineStartSumWidth, LastLineSumWidth, LastLineStart: LongInt;
begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    Result := 0;
    Exit;
  end;
  MidWidth := FSumWidth div System.Length(FChars);
  if MidWidth = 0 then
  begin
    Result := 0;
    Exit;
  end;
  LineStart := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth := 0;
  LastLineStart    := 0;
  x := 0;
  while (LineStart < System.Length(FChars)) and (ALine >= 0) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LastLineSumWidth := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LastLineStart := LineStart;
    LineStart := x + 1;
    Dec(ALine);
  end;
  Result := LastLineStart;
  while (Result < LineStart) and (FChars[Result].FSumWidth -
      LastLineSumWidth <= AXPos) do
    Inc(Result);
end;

function TColorString.GetLineOfCaret(AWrapWidth, ACaretPos, ACaretWidth:
  LongInt): LongInt;
var
  x, MidWidth, LineStart, LineStartSumWidth, LastLineSumWidth: LongInt;
begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    Result := 0;
    Exit;
  end;
  MidWidth := FSumWidth div System.Length(FChars);
  if MidWidth = 0 then
  begin
    Result := 0;
    Exit;
  end;
  LineStart := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth := 0;
  Result := 0;
  x := 0;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LastLineSumWidth := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart := x + 1;
    if ACaretPos < x then
      Exit;
    Inc(Result);
  end;
  if ACaretWidth >= 0 then x := ACaretWidth else x := FFont.GetTextWidth('A');
  if (ACaretPos > LineStart) or (LineStartSumWidth - LastLineSumWidth +
    (ACaretPos - LineStart) * x + x <= AWrapWidth) then
    Dec(Result);
end;

function TColorString.LineCount(AWrapWidth, ACaretPos, ACaretWidth: LongInt): LongInt;
var
  x: LongInt;
  MidWidth: LongInt;
  LineStart: LongInt;
  LineStartSumWidth: LongInt;
  LastLineSumWidth: LongInt;
begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    Result := 1;
    Exit;
  end;
  MidWidth := FSumWidth div System.Length(FChars);
  if MidWidth = 0 then
  begin
    Result := 1;
    Exit;
  end;
  LineStart := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth := 0;
  Result := 0;
  x := 0;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <AWrapWidth) do Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LastLineSumWidth := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart := x + 1;
    Inc(Result);
  end;
  if ACaretWidth >= 0 then
    x := ACaretWidth
  else
    x := FFont.GetTextWidth('A');
  if (ACaretPos >= LineStart) and (LineStartSumWidth - LastLineSumWidth +
    (ACaretPos - LineStart) * x + x > AWrapWidth) then
    Inc(Result);
  if Result=0 then Inc(Result);
end;

constructor TColorString.Create(AFont: TFont);
begin
  inherited Create;
  FTabWidth := 1;
  FFont     := AFont;
  FPassWordStart := MaxInt;
  FStoredLineCount:= -1;
end;

procedure TColorstring.BColorBlock(StartPos, EndPos: LongInt; C: TColor);
var
  i: LongInt;
begin
  if StartPos < 0 then
    StartPos := 0;
  if EndPos > High(FChars) then
    EndPos := High(FChars);
  for i := StartPos to EndPos do
    FChars[i].FBackColor := C;
end;

procedure TColorstring.ColorBlock(StartPos, EndPos: LongInt; FC, BC: TColor);
var
  i: LongInt;
begin
  if StartPos < 0 then
    StartPos := 0;
  if EndPos > High(FChars) then
    EndPos := High(FChars);
  for i := StartPos to EndPos do
  begin
    FChars[i].FFrontColor := FC;
    FChars[i].FBackColor  := BC;
  end;
end;

procedure TColorstring.Insert(Index: LongInt; C: string; FC, BC: TColor;
  Attrib: TCharAttrib);
var
  i:      LongInt;
  l:      LongInt;
  Pp:     LongInt;
  OldLen: LongInt;
  SLen:   LongInt;
begin
  OldLen := System.Length(FChars);
  SLen   := UTF8Length(C);
  if OldLen < Index then
    MinimumLength(Index + SLen, FC, BC)
  else
  begin
    MinimumLength(SLen + OldLen, FC, BC);
    for i := OldLen - 1 downto Index do
      FChars[i + SLen] := FChars[i];
  end;
  pp := 1;
  for i := 0 to SLen - 1 do
  begin
    l := UTF8CharacterLength(@C[Pp]);
    with FChars[Index + i] do
    begin
      FChar := Copy(C, Pp, l);
      if Index + i >= FPassWordStart then
        FCharWidth := FFont.GetTextWidth(FPassWordChar)
      else
        FCharWidth := FFont.GetTextWidth(FChar);
      FFrontColor := FC;
      FBackColor := BC;
      FAttrib    := Attrib;
    end;
    Inc(pp, l);
  end;
  UpdateSum;
end;

procedure TColorstring.Delete(Index, Len: LongInt);
var
  i: LongInt;
begin
  if (Len = 0) or (Index >= System.Length(FChars)) then
    Exit;
  if Index + Len > System.Length(FChars) then
    Len := System.Length(FChars) - Index;
  for i := Index to System.Length(FChars) - Len - 1 do
    FChars[i] := FChars[i + Len];
  SetLength(FChars, System.Length(FChars) - Len);
  UpdateSum;
end;

procedure TColorstring.Delete(Index: LongInt);
var
  i: LongInt;
begin
  if (Index >= System.Length(FChars)) then
    Exit;
  for i := Index to System.Length(FChars) - 2 do
    FChars[i] := FChars[i + 1];
  SetLength(FChars, System.Length(FChars) - 1);
  UpdateSum;
end;

function TColorstring.GetPartstring(Start, Ende: LongInt): string;
var
  i, n: LongInt;
  Len:  LongInt;
begin
  if Start < 0 then
    Start := 0;
  if Ende > High(FChars) then
    Ende := High(FChars);
  Len    := 0;
  for i := Start to Ende do
    Inc(Len, System.Length(FChars[i].FChar));
  SetLength(Result, Len);
  Len := 1;
  for i := Start to Ende do
  begin
    with FChars[i] do
    begin
      for n := 1 to System.Length(FChar) do
      begin
        Result[Len] := FChar[n];
        Inc(Len);
      end;
    end;
  end;
end;

function TColorstring.Getstring: string;
var
  i, n: LongInt;
  Len:  LongInt;
begin
  Len := 0;
  for i := 0 to High(FChars) do
    Inc(Len, System.Length(FChars[i].FChar));
  SetLength(Result, Len);
  Len := 1;
  for i := 0 to High(FChars) do
  begin
    with FChars[i] do
    begin
      for n := 1 to System.Length(FChar) do
      begin
        Result[Len] := FChar[n];
        Inc(Len);
      end;
    end;
  end;
end;

procedure TColorstring.OverWritePW(S: TColorstring; PWS, Pos: LongInt; PWC: string);
var
  i: LongInt;
  CPassWordStart: LongInt;
begin
  MinimumLength(Pos + S.Length, CLSilver, S.FDefaultBackGround);
  CPassWordStart := PWS;
  for i := 0 to S.Length - 1 do
  begin
    FChars[i + Pos] := S.FChars[i];
    if CPassWordStart <= 0 then
      FChars[i + Pos].FChar := PWC;
    Dec(CPassWordStart);
  end;
  UpdateSum;
end;

procedure TColorstring.OverWrite(S: TColorstring; Pos: LongInt);
var
  i: LongInt;
begin
  MinimumLength(Pos + S.Length, CLSilver, S.FDefaultBackGround);
  for i := 0 to S.Length - 1 do
    FChars[i + Pos] := S.FChars[i];
  UpdateSum;
end;

procedure TColorstring.PartOverWrite(S: TColorstring; Start, Ende, Pos: LongInt);
var
  i: LongInt;
begin
  MinimumLength(Pos + Ende - Start, CLSilver, S.FDefaultBackGround);
  for i := 0 to Ende - Start - 1 do
    FChars[i + Pos] := S.FChars[i + Start];
  UpdateSum;
end;

procedure TColorstring.OverWrite(s: string; Pos: LongInt; FC, BC: TColor;
  Attrib: TCharAttrib);
var
  i, Pp, l: LongInt;
begin
  MinimumLength(Pos + UTF8Length(S), FC, BC);
  Pp := 1;
  for i := 0 to UTF8Length(S) - 1 do
  begin
    l := UTF8CharacterLength(@s[Pp]);
    with FChars[i + Pos] do
    begin
      FChar      := Copy(S, Pp, l);
      FCharWidth := FFont.GetTextWidth(FChar);
      FFrontColor := FC;
      FBackColor := BC;
      FAttrib    := Attrib;
    end;
    Inc(Pp, l);
  end;
  UpdateSum;
end;

procedure TColorstring.OverWriteChar(s: TUTF8Char; Pos, ADefWidth: LongInt;
  FC, BC: TColor; Attrib: TCharAttrib);
begin
  MinimumLength(Pos + 1, FC, BC);
  with FChars[Pos] do
  begin
    FChar      := s;
    FCharWidth := ADefWidth;
    FFrontColor := FC;
    FBackColor := BC;
    FAttrib    := Attrib;
  end;
  UpdateSum;
end;

procedure TColorstring.MinimumLength(V: LongInt; FC, BC: TColor);
var
  OldLen, i: LongInt;
begin
  if System.Length(FChars) < V then
  begin
    OldLen := System.Length(FChars);
    SetLength(FChars, V);
    for i := OldLen to High(FChars) do
    begin
      with FChars[i] do
      begin
        FChar      := ' ';
        FCharWidth := FFont.GetTextWidth(' ');
        FFrontColor := FC;
        FBackColor := BC;
      end;
    end;
  end;
end;

procedure TColorstring.MaximumLength(V: LongInt);
begin
  if System.Length(FChars) > V then
    SetLength(FChars, V);
end;

procedure TColorstring.Clear;
begin
  FStoredLineCount:=-1;
  FChars := nil;
end;

procedure TCmdBox.ClearLine;
begin
  if FLines[FOutY].Length <> 0 then
  begin
    FLines[FOutY].Clear;
    FOutX := 0;
    if FInput then FInputY := FOutY;
    Invalidate;
  end;
end;

function TCmdBox.GetCaretInterval: LongInt;
begin
  Result := FCaretTimer.Interval;
end;

procedure TCmdBox.SetCaretInterval(AValue: LongInt);
begin
  FCaretTimer.Interval := AValue;
end;

procedure TCmdBox.MultiWrite;
var
  DoWrite: boolean;
begin
  repeat
    System.EnterCriticalSection(FLock);
    DoWrite := FStringBuffer.Count <> 0;
    if DoWrite then
    begin
      FCurrentString := FStringBuffer[0];
      FStringBuffer.Delete(0);
    end;
    System.LeaveCriticalSection(FLock);
    if DoWrite then
      IntWrite;
  until not DoWrite;
end;

procedure TCmdBox.Write(S: string);
begin
  if ThreadID = MainThreadId then
  begin
    MultiWrite;
    FCurrentString := S;
    IntWrite;
  end
  else
  begin
    System.EnterCriticalSection(FLock);
    FStringBuffer.Add(S);
    System.LeaveCriticalSection(FLock);
    if Assigned(WakeMainThread) then
      TThread.Synchronize(nil, @MultiWrite);
  end;
end;

function TCmdBox.HistoryIndexOf(s: string): LongInt;
begin
  for Result := 0 to HistoryCount - 1 do
    if History[Result] = s then Exit;

  Result := -1;
end;

function TCmdBox.HistoryHas(s: string): boolean;
var
  i: LongInt;
begin
  Result := True;
  for i := 0 to HistoryCount - 1 do
    if History[i] = s then
      Exit;
  Result := False;
end;

function TCmdBox.HistoryCount: LongInt;
begin
  HistoryCount := FHistoryLength - Ord(FInput);
end;

function TCmdBox.GetHistory(i: LongInt): string;
begin
  Inc(i, Ord(FInput));
  if (i >= 0) and (i < FHistoryLength) then
    GetHistory := FHistory[i].Getstring
  else
    GetHistory := '';
end;

procedure TCmdBox.EraseBackGround(DC: HDC);
begin
end;

procedure TCmdBox.ClearHistory;
begin
  FHistoryLength := Ord(FInput);
  FHistoryPos    := 0;
end;

procedure TCmdBox.SetHistoryMax(v: LongInt);
var
  i: LongInt;
begin
  if v < 1 then v := 1;

  if v <> FHistoryMax then
  begin
    if FHistoryLength > v then FHistoryLength := v;
    for i := v to FHistoryMax - 1 do
      FHistory[i].Free;

    SetLength(FHistory, v);

    for i := FHistoryMax to v - 1 do
      FHistory[i] := TColorstring.Create(Canvas.Font);

    FHistoryMax   := v;
  end;
end;

procedure TCmdBox.WriteStream(Stream: TStream);
var
  c: WideString;
begin
  c:='';
  while Stream.Position < Stream.Size do
  begin
    // Not very efficient, but should work...
    Stream.Read(c, 1);
    Write(c);
  end;
end;

procedure TCmdBox.LeftSelection(Start, Ende: LongInt);
begin
  if FSelStart = -1 then
  begin
    SetSelection(Start, Ende);
  end
  else
  begin
    if FSelStart = Start then
      SetSelection(-1, 0)
    else
    begin
      if FSelStart < Start then
      begin
        SetSelection(FSelStart, Start);
      end
      else
        SetSelection(Start, FSelEnd + 1);
    end;
  end;
end;

procedure TCmdBox.RightSelection(Start, Ende: LongInt);
begin
  if FSelStart = -1 then
  begin
    SetSelection(Start, Ende);
  end
  else
  begin
    if FSelEnd + 1 = Ende then
      SetSelection(-1, 0)
    else
    begin
      if FSelstart < Start then
      begin
        SetSelection(FSelStart, Ende);
      end
      else
        SetSelection(Ende, FSelEnd + 1);
    end;
  end;
end;

procedure TCmdBox.SetSelection(Start, Ende: LongInt);
begin
  if FSelStart <> -1 then
    FInputBuffer.ColorBlock(FSelStart, FSelEnd, FInputColor, FInputBackGround);
  if Start = Ende then
    FSelStart := -1
  else
  begin
    if Start < Ende then
    begin
      FSelStart := Start;
      FSelEnd   := Ende - 1;
    end
    else
    begin
      FSelStart := Ende;
      FSelEnd   := Start - 1;
    end;
  end;
  if FSelStart <> -1 then
    FInputBuffer.ColorBlock(FSelStart, FSelEnd, FInputSelColor, FInputSelBackGround);
end;

procedure TCmdBox.CopyToClipBoard;
begin
  if FSelStart <> -1 then
  begin
    ClipBoard.AsText := FInputBuffer.GetPartstring(FSelStart, FSelEnd);
  end;
end;

procedure TCmdBox.PasteFromClipBoard;
var
  s:     WideString;
  l, Pp: LongInt;
begin
  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    s  := ClipBoard.AsText;
    Pp := 1;
    while pp <= Length(s) do
    begin
      l := UTF8CharacterLength(@S[Pp]);
      if (l = 1) and (byte(S[Pp]) < 32) then
        Delete(s, Pp, 1)
      else
        Inc(Pp, l);
    end;
    FInputBuffer.Insert(InputPos, s, FInputColor, FInputBackGround, FInputAttrib);
    Inc(FInputPos, UTF8Length(s));
    FCaretX := FInputX + InputPos;
    AdjustScrollBars;
    MakeInputVisible;
    FHistoryPos := 0;
    if Assigned(FOnInputChange) then
      FOnInputChange(Self, FInputBuffer);
    if Assigned(FOnAny) then
      FOnAny(Self, FInputBuffer);
  end;
end;

procedure TCmdBox.DeleteSelected;
begin
  if FSelStart <> -1 then
  begin
    FInputBuffer.Delete(FSelStart, FSelEnd - FSelStart + 1);
    FInputPos := FSelStart;
    FCaretX   := FInputX + FInputPos;
    FSelStart := -1;
  end;
end;

procedure TCmdBox.CutToClipBoard;
begin
  if FSelStart <> -1 then
  begin
    ClipBoard.AsText := FInputBuffer.GetPartstring(FSelStart, FSelEnd);
    DeleteSelected;
  end;
end;

procedure TCmdBox.MouseMove(Shift: TShiftState; x, y: Integer);
begin
  if FMouseDown then
  begin
    if MoveInputCaretTo(x, y, False) then
      SetSelection(FMouseDownInputPos, FInputPos);
  end;
  inherited MouseMove(Shift,x,y);
end;

function TCmdBox.MoveInputCaretTo(x, y: LongInt; chl: boolean): boolean;
var
  h, sl, q: LongInt;
begin
  if not FInput then
    Exit;
  y  := y div FCharHeight;
  h  := FLineHeightSum[FTopLine] + FLineOfTopLine + y;
  sl := FTopLine;
  while (sl < FLineCount - 1) and (FLineHeightSum[sl + 1] <= h) do
    Inc(sl);
  if (sl = FInputY) or (not chl) then
  begin
    Dec(h, FLineHeightSum[FInputY]);
    q := FInputBuffer.GetCharPosition(FClientWidth, h, x);
    if (q < FInputMinPos) then
      q := FInputMinPos;
    if (q - FInputX > FInputBuffer.Length) then
      q     := FInputBuffer.Length - FInputX;
    FCaretX := q;
    FInputPos := FCaretX - FInputX;
    if Assigned(FOnAny) then
      FOnAny(Self, FInputBuffer);
    Invalidate;
    Result := True;
  end
  else
    Result := False;
end;

procedure TCmdBox.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
begin
  SetFocus;
  MoveInputCaretTo(x, y, True);
  FMouseDown := True;
  SetSelection(-1, 0);
  FMouseDownInputPos := FInputPos;
  Invalidate;
  inherited MouseDown(Button,Shift,x,y);
end;

procedure TCmdBox.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
begin
  FMouseDown := False;
  inherited MouseUp(Button,Shift,x,y);
end;

destructor TColorstring.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCmdBox.ScrollUp;
var
  n: LongInt;
  Firstwidestring: TColorstring;
begin
  Firstwidestring := FLines[0];
  for n := 0 to Length(FLines) - 2 do
    Flines[n] := FLines[n + 1];
  Firstwidestring.Clear;
  Firstwidestring.FDefaultBackGround := FBackGroundColor;
  Flines[High(Flines)] := Firstwidestring;
end;

procedure TCmdBox.TextColors(FC, BC: TColor);
begin
  FCurrentColor      := FC;
  FCurrentBackGround := BC;
end;

procedure TCmdBox.TextColor(C: TColor);
begin
  FCurrentColor := C;
end;

procedure TCmdBox.TextBackGround(C: TColor);
begin
  FCurrentBackGround := C;
end;

procedure TCmdBox.TranslateScrollBarPosition;
var
  GLine, Line: LongInt;
  He: LongInt;
begin
  if (FLineOfTopLine < FLineHeights[FTopLine]) and
    (FLineHeightSum[FTopLine] + FLineOfTopLine = FVSBPos) then
    exit;

  UpdateLineHeights;
  Line  := 0;
  GLine := 0;
  He    := FLineHeights[Line];
  while (Line < LineCount - 1) and (Gline + He <= FVSBPos) do
  begin
    Inc(Line);
    Inc(Gline, He);
    He := FLineHeights[Line];
  end;
  FTopLine := Line;
  FLineOfTopLine := FVSBPos - GLine;
  Invalidate;
end;

procedure TCmdBox.WMVScroll(var message: TLMVScroll);
var
  CurrentPos: LongInt;
begin
  CurrentPos := FLineHeightSum[FTopLine] + FLineOfTopLine;
  case message.ScrollCode of
    SB_TOP: CurrentPos    := 0;
    SB_BOTTOM: CurrentPos := FVisibleLineCount - FPageHeight;
    SB_LINEDOWN: Inc(CurrentPos);
    SB_LINEUP: Dec(CurrentPos);
    SB_PAGEDOWN: Inc(CurrentPos, FPageHeight);
    SB_PAGEUP: Dec(CurrentPos, FPageHeight);
    SB_THUMBPOSITION: CurrentPos := message.Pos;
    SB_THUMBTRACK: CurrentPos    := message.Pos;
    SB_ENDSCROLL: Exit;
  end;

  if CurrentPos < 0 then
    CurrentPos := 0
  else if Currentpos > FVisibleLineCount - FPageHeight then
    CurrentPos := FVisibleLineCount - FPageHeight;
 {$IFNDEF LCLGTK}
  ScrollBarPosition(SB_VERT, CurrentPos);
 {$ENDIF}

  FVSBPos := CurrentPos;
  TranslateScrollBarPosition;
end;

procedure TCmdBox.ScrollBarRange(Which: LongInt; aRange, aPage: LongInt);
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask  := SIF_RANGE or SIF_PAGE or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin   := 0;
    ScrollInfo.nMax   := ARange;

    if APage < 0 then  APage := 0;

    ScrollInfo.nPage := APage;
    SetScrollInfo(Handle, Which, ScrollInfo, True);
  end;
end;

procedure TCmdBox.ScrollBarPosition(Which, Value: LongInt);
var
  ScrollInfo: TScrollInfo;
  Vis: boolean;
begin
  if HandleAllocated then
  begin
    Vis := FVSbVisible;
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask  := SIF_POS;
    ScrollInfo.nPos   := Value;
    SetScrollInfo(Handle, Which, ScrollInfo, Vis);
  end;
end;

function TCmdBox.GetSystemMetricsGapSize(const Index: LongInt): LongInt;
begin
 {$ifdef LCLWIN32}
  Result := 0;
 {$else}
  Result := 3;
 {$endif}
end;

procedure TCmdBox.SetBackGroundColor(c: TColor);
begin
  if c <> FBackGroundColor then
  begin
    FBackGroundColor := c;
    Invalidate;
  end;
end;

procedure TCmdBox.SetFont(F: TFont);
var
  DC:      HDC;
  Save:    THandle;
  Metrics: TTextMetric;
  i:LongInt;
begin
  FFont.Assign(F);
  Canvas.Font := FFont;
{  DC   := GetDC(0);
  Save := SelectObject(DC, FFont.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, Save);
  ReleaseDC(0, DC);}
  FCharHeight := abs(FFont.Height)+3;
  for i:=0 to FLineCount-1 do
  begin
    FLines[i].UpdateAll;
  end;
  FInputBuffer.UpdateAll;
  Invalidate;
end;

// Still a Bug: Try having a cmdline with more lines than fit on screen : update doesn't work anymore...

procedure TCmdBox.MakeInputVisible;
var
  y: LongInt;
begin
  if not FAutoFollow then
  begin
    Exit;
  end;
  UpdateLineHeights;
  y := FLineHeightSum[FInputY] + FInputBuffer.GetLineOfCaret(FClientWidth, FCaretX, FCaretWidth);
  if y >= FLineHeightSum[FTopLine] + FLineOfTopLine + FPageHeight - 1 then
  begin
    while y >= FLineHeightSum[FTopLine] + FLineHeights[FTopLine] + FPageHeight - 1 do
    begin
      Inc(FTopLine);
    end;
    FLineOfTopLine := y - (FLineHeightSum[FTopLine] + FPageHeight) + 1;
  end
  else if y < FLineHeightSum[FTopLine] + FLineOfTopLine then
  begin
    FLineOfTopLine := 0;
    while y < FLineHeightSum[FTopLine] do
    begin
      Dec(FTopLine);
    end;
    FLineOfTopLine := y - FLineHeightSum[FTopLine];
  end;
  y := FLineHeightSUm[FTopLine] + FLineOfTopLine;
  if y <> FVSBPos then
  begin
    FVSBPos := y;
    if HandleAllocated then
      ScrollBarPosition(SB_Vert, y);
  end;
end;

procedure TCmdBox.MakeOutVisible;
var
  y: LongInt;
begin
  if not FAutoFollow then
    Exit;
  UpdateLineHeights;
  y := FLineHeightSum[FOutY] + FLines[FOutY].GetLineOfCaret(FClientWidth,
    FOutX, FCaretWidth);
  if y >= FLineHeightSum[FTopLine] + FLineOfTopLine + FPageHeight then
  begin
    while y >= FLineHeightSum[FTopLine] + FLineHeights[FTopLine] + FPageHeight - 1 do
      Inc(FTopLine);
    FLineOfTopLine := y - (FLineHeightSum[FTopLine] + FPageHeight) + 1;
  end
  else if y < FLineHeightSum[FTopLine] + FLineOfTopLine then
  begin
    FLineOfTopLine := 0;
    while y < FLineHeightSum[FTopLine] do
      Dec(FTopLine);
    FLineOfTopLine := y - FLineHeightSum[FTopLine];
  end;
  y := FLineHeightSUm[FTopLine] + FLineOfTopLine;
  if y <> FVSBPos then
  begin
    FVSBPos := y;
    if HandleAllocated then
      ScrollBarPosition(SB_Vert, y);
  end;
end;

procedure TCmdBox.SetHistoryPos(v: LongInt);
begin
  if FInputIsPassWord then Exit;
  if v < 0 then
    v := FHistoryLength - 1
  else if v >= FHistoryLength then
    v := 0;
  if v <> FHistoryPos then
  begin
    if FHistoryPos = 0 then
    begin
      FHistory[0].Clear;
      FHistory[0].PartOverWrite(FInputBuffer, FInputMinPos, FInputBuffer.Length, 0);
    end;
    FInputBuffer.MaximumLength(FInputMinPos + FHistory[v].Length);
    FInputBuffer.OverWrite(FHistory[v], FInputMinPos);
    FInputPos := FInputBuffer.Length;
    FCaretX   := FInputX + FInputPos;
    FHistoryPos := v;
  end;
  if Assigned(FOnInputChange) then FOnInputChange(Self, FInputBuffer);
  MakeInputVisible;
  AdjustLineHeight(FInputY);
  AdjustScrollBars;
  Invalidate;
end;

procedure TCmdBox.UTF8KeyPress(var Key: TUTF8Char);
begin
  if not FInput then Exit;

  if key >= #32 then
  begin
    if FSelStart <> -1 then DeleteSelected;

    FInputBuffer.Insert(FInputPos, key, FInputColor, FInputBackGround, FCurrentAttrib);
    Inc(FInputPos);
    FCaretX     := FInputX + FInputPos;
    FHistoryPos := 0;
    if assigned(FOnInputChange) then FOnInputChange(Self, FInputBuffer);
  end;

  if Assigned(OnAny) then OnAny(Self, FInputBuffer);

  AdjustScrollBars;
  MakeInputVisible;
  if FInputVisible then Invalidate;

  inherited UTF8KeyPress(Key);
end;

procedure TCmdBox.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited KeyUp(key, shift);
  key:=0;
end;

procedure TCmdBox.KeyPress(var Key: char);
begin
  inherited KeyPress(key);
  key:=#0;
end;

procedure TCmdBox.KeyDown(var Key: word; Shift: TShiftState);
var
  s: string;
  i: LongInt;
begin
  if not FInput then
    Exit;
  case Key of
    VK_END:
    begin
      key := 0;
      if (not (ssAlt in Shift)) and FInput and (FInputPos <> FInputBuffer.Length) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          RightSelection(FInputPos, FInputBuffer.Length);
        FInputPos := FInputBuffer.Length;
        FCaretX   := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    VK_HOME:
    begin
      key := 0;
      if (not (ssAlt in Shift)) and FInput and (FInputPos <> FInputMinPos) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          LeftSelection(FInputMinPos, FInputPos);
        FInputPos := FInputMinPos;
        FCaretX   := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    VK_LEFT:
    begin
      key:=0;
      if (not (ssAlt in Shift)) and (FInput and (FInputPos > FInputMinPos)) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          LeftSelection(FInputPos - 1, FInputPos);
        Dec(FInputPos);
        FCaretX := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    VK_UP:
    begin
      key:=0;
      if (not (ssAlt in Shift)) and FInput then
      begin
        SetSelection(-1, 0);
        SetHistoryPos(FHistoryPos + 1);
      end;
    end;
    VK_DOWN:
    begin
      key:=0;
      if (not (ssAlt in Shift)) and FInput then
      begin
        SetSelection(-1, 0);
        SetHistoryPos(FHistoryPos - 1);
      end;
    end;
    VK_RIGHT:
    begin
      key:=0;
      if (not (ssAlt in Shift)) and FInput and (FInputPos < FInputBuffer.Length) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          RightSelection(FInputPos, FInputPos + 1);
        Inc(FInputPos);
        FCaretX := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    VK_DELETE:
    begin
      if FInput then
      begin
        if FSelStart <> -1 then
          DeleteSelected
        else
          FInputBuffer.Delete(FInputPos);
        FHistoryPos := 0;
        if assigned(FOnInputChange) then
          FOnInputChange(Self, FInputBuffer);
        MakeInputVisible;
        AdjustLineHeight(FInputY);
        AdjustScrollBars;
      end;
    end;
    VK_RETURN:
    begin
      if FInput then
      begin
        s := FInputBuffer.GetString;
        s := Copy(s, FUTF8InputMinPos + 1, Length(s));
        if (FHistoryPos = 0) then
        begin
          if (FInputBuffer.Length = FInputMinPos) or FInputIsPassWord then
          begin
            DeleteHistoryEntry(0);
          end
          else
          begin
            i := HistoryIndexOf(s);
            if i >= 0 then
            begin
              DeleteHistoryEntry(0);
              MakeFirstHistoryEntry(i);
            end
            else
            begin
              FHistory[0].Clear;
              FHistory[0].PartOverWrite(FInputBuffer, FInputMinPos,
                FInputBuffer.Length, 0);
            end;
          end;
        end
        else
        begin
          DeleteHistoryEntry(0);
          MakeFirstHistoryEntry(FHistoryPos);
        end;
        FInput := False;
        if FWriteInput then
        begin
          if FLines[FOutY].Length <> 0 then
          begin
            if FOutY >= FLineCount - 1 then
            begin
              ScrollUp;
              Dec(FOutY);
              FInputY := FOutY;
              AdjustLineHeight(FOutY);
              UpdateLineHeights;
              TranslateScrollBarPosition;
            end;
            FLines[FOutY + 1].Clear;
            FLines[FOutY + 1].OverWrite(FLines[FOutY], 0);
            FLines[FOutY].Clear;
            if FInputIsPassWord then
              FLines[FOutY].OverWritePW(FInputBuffer, FInputMinPos, FInputX, FPassWordChar)
            else
              FLines[FOutY].OverWrite(FInputBuffer, FInputX);
          end
          else
          begin
            if FInputIsPassWord then
              FLines[FOutY].OverWritePW(FInputBuffer, FInputMinPos, FInputX, FPassWordChar)
            else
              FLines[FOutY].OverWrite(FInputBuffer, FInputX);
          end;
          Inc(FOutY);
          if FOutY >= FLineCount then
          begin
            ScrollUp;
            Dec(FOutY);
            FInputY := FOutY;
            AdjustLineHeight(FOutY);
            UpdateLineHeights;
            TranslateScrollBarPosition;
          end;
          FOutX   := 0;
          FCaretX := 0;
        end;
        FInputBuffer.Clear;
        if Assigned(OnInput) then
          OnInput(Self, s);
        if Assigned(OnAny) then
          OnAny(Self, FInputBuffer);
        AdjustScrollBars;
        Invalidate;
      end;
    end;
    VK_BACK:
    begin
      if FInput then
      begin
        if FSelStart <> -1 then
          DeleteSelected
        else
        begin
          if (FInputPos > FInputMinPos) then
          begin
            Dec(FInputPos);
            FInputBuffer.Delete(FInputPos);
            FCaretX := FInputX + FInputPos;
          end;
        end;
        FHistoryPos := 0;
        if assigned(FOnInputChange) then
          FOnInputChange(Self, FInputBuffer);
        if Assigned(OnAny) then
          OnAny(Self, FInputBuffer);
        AdjustScrollBars;
        MakeInputVisible;
        if FInputVisible then
          Invalidate;
      end;
    end;
    VK_C:
    begin
      if (FInput) and (ssCtrl in Shift) then
        CopyToClipBoard;
    end;
    VK_V:
    begin
      if (FInput) and (ssCtrl in Shift) then
        PasteFromClipBoard;
    end;
    VK_X:
    begin
      if (FInput) and (ssCtrl in Shift) then
        CutToClipBoard;
    end;
    VK_A:
    begin
      if (FInput) and (ssCtrl in Shift) then
      begin
        SetSelection(FInputMinPos, FInputBuffer.Length);
        FInputPos := FInputBuffer.Length;
        MakeInputVisible;
        if FInputVisible then
          Invalidate;
      end;
    end;
  end;
  if Assigned(OnAny) then
    OnAny(Self, FInputBuffer);
  inherited KeyDown(Key,Shift);
end;

procedure TCmdBox.InsertHistory;
var
  i: LongInt;
  t: TColorstring;
begin
  t := FHistory[FHistoryMax - 1];
  for i := FHistoryMax - 2 downto 0 do
  begin
    FHistory[i + 1] := FHistory[i];
  end;
  FHistory[0] := t;
  FHistoryPos := 0;
  if FHistoryLength < FHistoryMax then
    Inc(FHistoryLength);
end;

procedure TCmdBox.StartRead(DFC, DBC: TColor; const Desc: string; IFC, IBC: TColor);
var
  Pp, i, l: LongInt;
begin
  Inc(FCaretX, UTF8Length(Desc));
  FInputX := 0;
  if FLines[FOutY].Length = 0 then
    FInputY := FOutY
  else
    FInputY := FOutY + 1;
  FInputVisible := True;
  FInput := True;
  FUTF8InputMinPos := Length(Desc);
  i      := 0;
  Pp     := 1;
  while Pp <= Length(Desc) do
  begin
    if Desc[Pp] = #27 then
    begin
      if Pp + 1 > Length(Desc) then
        Break;
      case Desc[Pp + 1] of
        #9, #10, #32, #46, #196:
        begin
          if Pp + 2 > Length(Desc) then
            Break; //Incomplete Escape Seq...ignore
          l := 3;
        end;
        #33, #47, #197:
        begin
          if Pp + 3 > Length(Desc) then
            Break; //Incomplete Escape Seq...ignore
          l := 4;
        end;
        else
        begin
          l := 2;
        end;
      end;
    end
    else
      l := UTF8CharacterLength(@Desc[PP]);
    FInputBuffer.OverWrite(Copy(Desc, Pp, l), i, DFC, DBC, FCurrentAttrib);
    Inc(i);
    Inc(Pp, l);
  end;
  FInputPos    := i;
  FInputMinPos := i;
  // FInputBuffer.OverWrite(Desc,0,DFC,DBC);
  FInputIsPassWord := False;
  FInputColor  := IFC;
  FInputBackground := IBC;
  FInputBuffer.PassWordStart := MaxInt;
  InsertHistory;
  MakeInputVisible;
end;

procedure TCmdBox.StartReadPassWord(DFC, DBC: TColor; const Desc: string;
  IFC, IBC: TColor);
begin
  StartRead(DFC, DBC, Desc, IFC, IBC);
  FInputBuffer.PassWordStart := UTF8Length(Desc);
  FInputBuffer.PassWordChar := FPassWordChar;
  FInputIsPassWord := True;
end;

procedure TCmdBox.StopRead;
begin
  FInput := False;
end;

procedure TCmdBox.DeleteHistoryEntry(i: LongInt);
var
  j:    LongInt;
  Temp: TColorstring;
begin
  Temp := FHistory[i];
  for j := i to FHistoryLength - 2 do
    FHistory[j] := FHistory[j + 1];
  FHistory[FHistoryLength - 1] := Temp;
  Dec(FHistoryLength);
  if FHistoryPos >= i then
    Dec(FHistoryPos);
end;

procedure TCmdBox.MakeFirstHistoryEntry(i: LongInt);
var
  Temp: TColorstring;
begin
  if FHistoryPos <> 0 then
  begin
    Temp := FHistory[i];
    for i := i - 1 downto 0 do
      FHistory[i + 1] := FHistory[i];
    FHistory[0] := Temp;
  end;
end;

procedure TCmdBox.Clear;
var
  i: LongInt;
begin
  for i := 0 to Length(FLines) - 1 do
    Flines[i].Clear;
  FCaretX := 0;
  FInputY := 0;
  FOutX   := 0;
  FOutY   := 0;
  if FInput then
    FInputY := 0;
  Invalidate;
end;

procedure TCmdBox.Writeln(s: string);
begin
  Write(s + #13#10);
end;

const
  AnsiColors: array['0'..'7'] of
    TColor = (clBlack, clRed, clGreen, clYellow, clBlue, clFuchsia, clAqua, clWhite);

procedure TCmdBox.IntWrite;
var
  Pp:     LongInt;
  l:      LongInt;
  s:      string;
  EscPos: LongInt;
  EscSubMode: LongInt;
begin
  S    := FCurrentString;
  Pp   := 1;
  while Pp <= Length(S) do
  begin
    l := 1;
    case FEscapeMode of
      escmNone:
      begin
        if S[Pp] = #27 then
        begin
          case FEscapeCodeType of
            esctCmdBox:
            begin
              FEscapeMode := escmOperation;
              FEscapeData := '';
            end;
            esctAnsi:
            begin
              FEscapeMode := escmAnsiOperation;
              FEscapeData := '';
            end;
            esctNone:
            begin
              // Simply ignore it
            end;
          end;
        end
        else
        begin
          l := UTF8CharacterLength(@S[Pp]);
          if l = 1 then
          begin
            case s[Pp] of
              #13: FOutX := 0;
              #10:
              begin
                AdjustLineHeight(FOutY);
                if FLines[FOutY].Length = 0 then
                  FLines[FOutY].DefaultBackGround := FCurrentBackGround;
                Inc(FOutY);
                if FOutY >= Length(FLines) then
                begin
                  ScrollUp;
                  Dec(FOutY);
                  AdjustLineHeight(FOutY);
                  UpdateLineHeights;
                  TranslateScrollBarPosition;
                end;
              end;
              else
              begin
                FLines[FOutY].OverWrite(s[Pp], FOutX, FCurrentColor, FCurrentBackGround,
                  FCurrentAttrib);
                Inc(FOutX);
              end;
            end;
          end
          else
          begin
            FLines[FOutY].OverWrite(Copy(s, Pp, l), FOutX, FCurrentColor,
              FCurrentBackGround, FCurrentAttrib);
            Inc(FOutX);
          end;
        end;
      end;
      escmOperation:
      begin
        case S[Pp] of
          #9, #10, #32, #46, #196:
          begin
            FEscapeData := S[Pp];
            FEscapeMode := escmData1;
          end;
          #33, #47, #197:
          begin
            FEscapeData := S[Pp];
            FEscapeMode := escmData2;
          end;
          else
          begin
            FLines[FOutY].OverWriteChar(#27 + S[Pp], FOutX, FGraphicCharWidth,
              FCurrentColor, FCurrentBackGround, FCurrentAttrib);
            Inc(FOutX);
            FEscapeMode := escmNone;
          end;
        end;
      end;
      escmData1:
      begin
        FLines[FOutY].OverWriteChar(#27 + FEscapeData + S[Pp], FOutX, FGraphicCharWidth,
          FCurrentColor, FCurrentBackGround, FCurrentAttrib);
        Inc(FOutX);
        FEscapeMode := escmNone;
      end;
      escmData2:
      begin
        FEscapeData := FEscapeData + S[Pp];
        FEscapeMode := escmData1;
      end;
      escmAnsiOperation:
      begin
        case S[Pp] of
          '[': FEscapeMode := escmAnsiSquare;
          else
            FEscapeMode := escmNone;
        end;
      end;
      escmAnsiSquare:
      begin
        case S[Pp] of
          'm':
          begin
            EscPos     := 1;
            EscSubMode := 0;
            while EscPos <= Length(FEscapeData) do
            begin
              case EscSubMode of
                0:
                begin
                  case FEscapeData[EscPos] of
                    '0':
                    begin
                      // No Reset Values know here...just assume
                      FCurrentColor      := clSilver;
                      FCurrentBackGround := clBlack;
                    end;
                    '7':
                    begin
                      // Reverse? What now...
                    end;
                    '3': EscSubMode := 3;
                    '4': EscSubMode := 4;
                  end;
                end;
                1:
                begin
                  // Just collect the expected ";", not sure what to do if it isn't there...
                  EscSubMode := 0;
                end;
                3:
                begin
                  if FEscapeData[EscPos] in ['0'..'7'] then
                    FCurrentColor := AnsiColors[FEscapeData[EscPos]];
                  EscSubMode      := 1;
                end;
                4:
                begin
                  if FEscapeData[EscPos] in ['0'..'7'] then
                    FCurrentBackGround := AnsiColors[FEscapeData[EscPos]];
                  EscSubMode := 1;
                end;
              end;
              Inc(EscPos);
            end;
            FEscapeMode := escmNone;
          end;
          else
          begin
            FEscapeData := FEscapeData + S[Pp];
          end;
        end;
      end;
    end;
    Inc(Pp, l);
  end;
  if FInput then
  begin
    if FLines[FOutY].Length = 0 then
    begin
      if (FInputY <> FOutY) then
        FInputY := FOutY;
    end
    else
    begin
      if FInputY <> FOutY + 1 then
        FInputY := FOutY + 1;
    end;
    if FInputY >= FLineCount then
    begin
      ScrollUp;
      Dec(FOutY);
      Dec(FInputY);
      FInputY := FOutY;
      AdjustLineHeight(FOutY);
      UpdateLineHeights;
      TranslateScrollBarPosition;
    end;
    MakeInputVisible;
  end
  else
    MakeOutVisible;
  AdjustLineHeight(FOutY);
  if not FInput then
    FCaretX := FOutX;
  AdjustScrollBars;
end;

procedure TCmdBox.SetOutY(v: LongInt);
begin
  if v > FLineCount - 1 then
    v   := FLineCount - 1;
  FOutY := v;
end;

procedure TCmdBox.Resize;
begin
  inherited Resize;
  AdjustScrollBars(True);
end;

function TCmdBox.AdjustLineHeight(i: LongInt;const Recalc:Boolean=False): LongInt;
var
  LineC:  LongInt;
  LineC2: LongInt;
begin
  with FLines[i] do
  begin
    if (not Recalc) and (FStoredLineCount>=0) then LineC:=FStoredLineCount else
    begin
      LineC := LineCount(FClientWidth, -1, FCaretWidth);
      FStoredLineCount:=LineC;
    end;
  end;
  if (FInputY = i) then
  begin
    with FInputBuffer do
    begin
      if (not Recalc) and (FStoredLineCount>=0) then LineC2:=FStoredLineCount else
      begin
        LineC2 := LineCount(FClientWidth, FCaretX, FCaretWidth);
        FStoredLineCount:=LineC2;
      end;
    end;
    if LineC2 > LineC then
      LineC := LineC2;
  end;
  Result  := LineC;
  FLineHeights[i] := Result;
end;

function TCmdBox.UpdateLineHeights(const Recalc:Boolean=False): LongInt;
var
  i: LongInt;
begin
  Result := 0;
  for i := 0 to FLineCount - 1 do
  begin
    FLineHeightSum[i] := Result;
    Inc(Result, AdjustLineHeight(i,Recalc));
  end;
end;

procedure TCmdBox.AdjustScrollBars(const Recalc:Boolean);
var
  LH: LongInt;
begin
  FClientWidth  := inherited ClientWidth;
  FClientHeight := inherited ClientHeight;
  FPageHeight   := FClientHeight div FCharHeight;
  FVisibleLines := FPageHeight + Ord(FClientHeight mod FCharHeight <> 0);
  LH            := UpdateLineHeights(Recalc);
  if LH <> FVisibleLineCount then
  begin
    FVisibleLineCount := LH;
    if FVisibleLineCount <= FVSBPos + FPageHeight then
    begin
      FVSBPos := FVisibleLineCount - FPageHeight;
      if FVSBPos < 0 then FVSBPos := 0;
      if HandleAllocated then ScrollBarPosition(SB_Vert, FVSBPos);
      TranslateScrollBarPosition;
    end;
  end;
  if FVisibleLineCount < FPageHeight then
  begin
    if HandleAllocated then
    begin
      ScrollBarPosition(SB_VERT, 0);
      ScrollBarRange(SB_VERT, 0, FPageHeight);
      ShowScrollBar(Handle, SB_VERT, True); { Disable the Scrollbar ! }
    end;
  end
  else
  begin
    if HandleAllocated then
    begin
      ScrollBarRange(SB_VERT, FVisibleLineCount, FPageHeight);
      ShowScrollBar(Handle, SB_VERT, True);
    end;
  end;
  Invalidate;
end;

procedure TCmdBox.SetTopLine(Nr: LongInt);
begin
  if Nr <> FTopLine then
  begin
    FTopLine := Nr;
    AdjustScrollBars;
  end;
end;

procedure TCmdBox.SetLineCount(c: LongInt);
var
  i: LongInt;
begin
  if c < 1 then
    c := 1;
  if c <> FLineCount then
  begin
    for i := 0 to FLineCount - 1 do
      FLines[i].Free;
    FLineCount := c;
    SetLength(FLines, FLinecount);
    for i := 0 to FlineCount - 1 do
    begin
      FLines[i] := TColorstring.Create(Canvas.Font);
      FLines[i].DefaultBackGround := FBackGroundColor;
      FLines[i].TabWidth := FTabWidth;
    end;
    SetLength(FLineHeights, FLineCount);
    SetLength(FLineHeightSum, FLineCount);
    AdjustScrollBars;
  end;
end;

procedure TCmdBox.Paint;
var y           : LongInt;
    m           : LongInt;
    CurrentLine : LongInt;
begin
  inherited Paint;
  with Canvas do
  begin
    if (csDesigning in ComponentState) then
    begin
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      FillRect(0, 0, FClientWidth, FClientHeight);
      Exit;
    end;
    Font := FFont;
    Brush.Style := bsSolid;
    m    := FVisibleLines - 1;
    y    := -FLineOfTopLine;
    CurrentLine := FTopLine;
    while (y <= m) and (CurrentLine < LineCount) do
    begin
      FLines[CurrentLine].LineOutAndFill(Canvas, 0, y * FCharHeight, 0,
        FClientWidth, FCharHeight, FGraphicCharWidth, -1, FBackGroundColor, FCaretColor,
        FCaretHeight, FCaretWidth, FCaretYShift, False);
      if (FInput) and (FInputY = CurrentLine) then
      begin
        if FInputIsPassWord then
        begin
          FInputBuffer.LineOutAndFill(Canvas, 0, y * FCharHeight, 0, FClientWidth,
            FCharHeight, FGraphicCharWidth, FCaretX, FBackGroundColor, FCaretColor,
            FCaretHeight, FCaretWidth, FCaretYShift, FCaretVisible and Focused);
        end
        else
        begin
          FInputBuffer.LineOutAndFill(Canvas, 0, y * FCharHeight, 0, FClientWidth,
            FCharHeight, FGraphicCharWidth, FCaretX, FBackGroundColor, FCaretColor,
            FCaretHeight, FCaretWidth, FCaretYShift, FCaretVisible and Focused);
        end;
      end;
      Inc(y, FLineHeights[CurrentLine]);
      Inc(CurrentLine);
    end;
    y := y * FCharHeight;
    if y < FClientHeight then
    begin
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(0, y, FClientWidth, FClientHeight);
    end;
  end;
end;

procedure TCmdBox.CaretTimerExecute(Sender: TObject);
begin
  if Focused then
  begin
    if not Assigned(WakeMainThread) then
      MultiWrite;
    FCaretVisible := not FCaretVisible;
    Invalidate;
  end;
end;

procedure TCmdBox.CreateWnd;
begin
  inherited CreateWnd;
  FVSBWidth := GetSystemMetrics(SM_CXVSCROLL) + GetSystemMetricsGapSize(SM_CXVSCROLL);
  SetFont(FFont);
  if FCaretHeight = -1 then
    FCaretHeight := FFont.GetTextHeight('A') - 3;
  { Little Hack to prevent "grey bar" Scrollbar at StartUp }
  ShowScrollBar(Handle, SB_VERT, False);
  ShowScrollBar(Handle, SB_VERT, True);
  AdjustScrollBars;
end;

procedure TCmdBox.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WIndowClass.Style and DWORD(not ClassStylesOff);
    Style := Style or WS_VSCROLL or WS_HSCROLL or WS_CLIPCHILDREN;
  end;
end;

constructor TCmdBox.Create(AComponent: TComponent);
var
  i: LongInt;
begin
  inherited Create(AComponent);
  System.InitCriticalSection(FLock);
  FStringBuffer     := TStringList.Create;
  FCharHeight       := 15; // Just a random value to prevent stupid exceptions
  FSelStart         := -1;
  FLineCount        := 1000;
  FInputVisible     := False;
  FWriteInput       := True;
  FBackGroundColor  := clBlack;
  FGraphicCharWidth := 10;
  FWrapMode         := wwmWord;
  FInputBuffer      := TColorstring.Create(Canvas.Font);
  FInputBuffer.FWrapMode := FWrapMode;
  FEscapeCodeType   := esctCmdBox;
  FAutoFollow       := True;
  SetLength(FLines, FLineCount);
  SetLength(FLineHeights, FLineCount);
  SetLength(FLineHeightSum, FLineCount);
  FTabWidth := 60;
  for i := 0 to FLineCount - 1 do
  begin
    FLines[i]                   := TColorstring.Create(Canvas.Font);
    FLines[i].DefaultBackGround := FBackGroundColor;
    FLines[i].TabWidth          := FTabWidth;
    FLines[i].FWrapMode         := FWrapMode;
  end;
  FCaretTimer          := TTimer.Create(self);
  FCaretTimer.Interval := 500;
  FCaretTimer.OnTimer  := @carettimerexecute;
  FCaretTimer.Enabled  := True;
  FCaretVisible        := True;
  FVSBVisible          := True;
  FFont                := Canvas.Font;
  FCurrentColor        := clSilver;
  FCurrentBackground   := clBlack;
  DoubleBuffered       := True;
  FFont.Color          := ClSilver;
  FCaretColor          := clWhite;
  FCaretType           := cartLine;
  FCaretWidth          := 1;
  FCaretHeight         := -1;
  FCaretYShift         := 3;
  FInputSelBackground  := clWhite;
  FInputSelColor       := clBlue;
  FHistoryMax          := 10;
  FHistoryLength       := 0;
  SetBounds(0, 0, 200, 200);
  SetLength(FHistory, FHistoryMax);
  for i := 0 to FHistoryMax - 1 do FHistory[i] := TColorstring.Create(Canvas.Font);
end;

destructor TCmdBox.Destroy;
var i : LongInt;
begin
  FCaretTimer.Enabled := False;
  System.DoneCriticalSection(FLock);
  FStringBuffer.Free;
  for i := 0 to FLineCount - 1 do FLines[i].Free;
  for i := 0 to FHistoryMax - 1 do FHistory[i].Free;
  FInputBuffer.Free;
  inherited Destroy;
end;

end.

