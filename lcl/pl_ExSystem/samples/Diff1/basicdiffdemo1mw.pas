unit BasicDiffDemo1mw;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math, TplDiffUnit, ExtCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    s1, s2: string;
    Diff: TplDiff;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure MarkupTextOut(canvas: TCanvas; x,y: integer; text: string);
var
  i,len, clr: integer;
  savedTextAlign, SavedBkColor, savedTextColor: cardinal;
  savedPt: TPoint;
begin
  i := pos('<',text);
  if i = 0 then begin canvas.TextOut(x,y,text); exit; end;

  savedTextColor := GetTextColor(canvas.Handle);
  SavedBkColor := GetBkColor(canvas.handle);
  //savedTextAlign := GetTextAlign(canvas.Handle);
 // SetTextAlign(canvas.Handle, savedTextAlign or TA_UPDATECP);
  MoveToEx(canvas.Handle, x, y, @savedPt);

  repeat
    if i > 1 then TextOut(canvas.handle,0,0,pchar(text),i-1);
    delete(text,1,i);
    len := length(text);
    if len < 3 then break
    else if (text[1] = 'F') and (text[2] ='C') and (text[3] = ':') and
      (len > 9) and (text[10] = '>') then
    begin
      clr := strtointdef('$'+copy(text,4,6),$0);
      SetTextColor(canvas.handle, clr);
      delete(text,1,10);
      dec(len,10);
    end
    else if (text[1] = 'B') and (text[2] ='C') and (text[3] = ':') and
      (len > 9) and (text[10] = '>') then
    begin
      clr := strtointdef('$'+copy(text,4,6),$1FFFFFF);
      if clr > $FFFFFF then
        SetBkColor(canvas.handle, SavedBkColor) else
        SetBkColor(canvas.handle, clr);
      delete(text,1,10);
      dec(len,10);
    end
    else break;
    i := pos('<',text);
  until (i = 0);
  TextOut(canvas.handle,0,0,pchar(text),len);

  SetTextColor(canvas.handle,savedTextColor);
  SetBkColor(canvas.handle, SavedBkColor);
  //SetTextAlign(canvas.Handle, savedTextAlign);
  with savedPt do MoveToEx(canvas.Handle, X,Y, nil);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  Diff := TplDiff.Create(self);
end;
//------------------------------------------------------------------------------

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  lastKind: TChangeKind;

  //AddCharToStr() adds color markup to strings which will be parsed later by
  //my MarkupTextOut() function where diffs (additions, modifications and
  //deletions) will be displayed in Paintbox1 with different colors ...
  //<BC:------> change background color to original (transparent) color
  //<BC:AAFFAA> change background color to pale green
  //<BC:AAAAFF> change background color to pale red
  //<BC:FFAAAA> change background color to pale blue
  procedure AddCharToStr(var s: string; c: char; kind, lastkind: TChangeKind);
  begin
    if (Kind = lastKind) then
      s := s + c //no need to change colors
    else
    case kind of
      ckNone: s := s + '<BC:------>' + c;
      ckAdd: s := s + '<BC:FFAAAA>' + c;
      ckDelete: s := s + '<BC:AAAAFF>' + c;
      ckModify: s := s + '<BC:AAFFAA>' + c;
    end;
  end;

begin
  //do the 'diff' here ...
  Diff.Execute(pchar(edit1.text), pchar(edit2.text), length(edit1.text), length(edit2.text));

  //now, display the diffs ...
  lastKind := ckNone;
  s1 := ''; s2 := '';
  for i := 0 to Diff.count-1 do
    with Diff.Compares[i] do
    begin

      //show changes to first string (with spaces for adds to align with second string)
      if Kind = ckAdd then AddCharToStr(s1,' ',Kind, lastKind)
      else AddCharToStr(s1,chr1,Kind,lastKind);

      //show changes to second string (with spaces for deletes to align with first string)
      if Kind = ckDelete then AddCharToStr(s2,' ',Kind, lastKind)
      else AddCharToStr(s2,chr2,Kind,lastKind);

      lastKind := Kind;
    end;

    PaintBox1.visible := true;
    PaintBox1.Invalidate; //ie: in case PaintBox1 is already visible
end;
//------------------------------------------------------------------------------

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
    MarkupTextOut(PaintBox1.canvas,0,5,s1);
    MarkupTextOut(PaintBox1.canvas,0,25,s2);
    PaintBox1.Canvas.TextOut(0,55,'Compare Statistics ...');
    with Diff.DiffStats do
    begin
      MarkupTextOut(PaintBox1.canvas,0,75, '  Matches : '+inttostr(matches));
      MarkupTextOut(PaintBox1.canvas,0,95, '  <BC:AAFFAA>Modifies:<BC:------> '+inttostr(modifies));
      MarkupTextOut(PaintBox1.canvas,0,115,'  <BC:FFAAAA>Adds    :<BC:------> '+inttostr(adds));
      MarkupTextOut(PaintBox1.canvas,0,135,'  <BC:AAAAFF>Deletes :<BC:------> '+inttostr(deletes));
    end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Button2Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

end.
