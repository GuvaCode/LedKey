{ Copyright (C) 2007 Julian Schutsch

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  
  This Software is GPL, not LGPL as the libary it uses !

}
unit cmdbox2mw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Graphics, Dialogs, ExtCtrls,LCLType,
  ucmdbox, StdCtrls, Controls, Buttons, Menus;

type

  { TWMainForm }

  TWMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    cbWordWrap: TCheckBox;
    CmdBox: TCmdBox;
    CbSetCaret: TComboBox;
    FontDialog: TFontDialog;
    Label1: TLabel;
    HistoryList: TListBox;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    RightPanel: TPanel;
    Splitter1: TSplitter;
    ReaderTimer: TTimer;
    ProcessTimer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbWordWrapChange(Sender: TObject);
    procedure CmdBoxInput(ACmdBox: TCmdBox; Input: String);
    procedure CbSetCaretChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ProcessTimerTimer(Sender: TObject);
    procedure ReaderTimerTimer(Sender: TObject);
  private
    TextPosition : Integer;
    DText        : TStringList;
    Rdpw         : Boolean;
    FProcess     : Integer;
  end; 

var WMainForm: TWMainForm;


implementation

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathdir = '..\cmdbox2\';
{$ENDIF}
{$IFDEF UNIX}
  pathdir = '../cmdbox2/';
{$ENDIF}

{ TWMainForm }

procedure TWMainForm.ReaderTimerTimer(Sender: TObject);
var i:Integer;
    s:String;
begin
 for i:=0 to 0 do
 begin
  s:='';
  s:=DText[TextPosition];{+#13#10;}
  Inc(TextPosition);
  CmdBox.TextColors(clAqua,clNavy);
  CmdBox.Writeln(s);
  if (TextPosition>=DText.Count) then
  begin
   CmdBox.ClearLine;
   CmdBox.TextColor(clYellow);
   CmdBox.Writeln(#27#10#196);
   TextPosition        := 0;
   ReaderTimer.Enabled := False;
  end;
 end;
end;

procedure TWMainForm.FormCreate(Sender: TObject);
begin
 DoubleBuffered := True;
 DText          := TStringList.Create;
 if FileExists(pathdir+'demotext.txt') then DText.LoadFromFile(pathdir+'demotext.txt');
 CmdBox.StartRead(clSilver,clNavy,'/example/prompt/>',clYellow,clNavy);
 CmdBox.TextColors(clWhite,clNavy);
 CmdBox.Writeln(#27#218#27#10#191);
 CmdBox.Writeln(#27#179'Type "help" to see a short list of available commands.'#27#10#179);
 CmdBox.Writeln(#27#217#27#10#217);
end;

procedure TWMainForm.CmdBoxInput(ACmdBox: TCmdBox; Input: String);
var i:Integer;
begin
 if rdpw then
 begin
  CmdBox.TextColors(clLime,clBlue);
  CmdBox.Writeln('Your Secret Password : '+Input);
  CmdBox.TextColors(clSilver,clNavy);
  rdpw:=false;
 end
 else
 begin
  rdpw:=false;
  Input:=LowerCase(Input);
  if Input='help' then
  begin
   CmdBox.TextColors(clLime,clNavy);
   CmdBox.Writeln(#27#218#27#197#128#0#27#194#27#10#191);
   CmdBox.Writeln(#27#179' Command'#27#33#128#0#27#179' Explanation'#27#10#179);
   CmdBox.Writeln(#27#195#27#197#128#0#27#198#27#10#180);
   CmdBox.Writeln(#27#179' help'#27#33#128#0#27#179' Gives this list of Commands'#27#10#179);
   CmdBox.Writeln(#27#179' clear'#27#33#128#0#27#179' Clears the Content of CmdBox'#27#10#179);
   CmdBox.Writeln(#27#179' start'#27#33#128#0#27#179' Outputs the Content of Demotext.txt from the beginning'#27#10#179);
   CmdBox.Writeln(#27#179' stop'#27#33#128#0#27#179' Stops output and resets to Start'#27#10#179);
   CmdBox.Writeln(#27#179' pause'#27#33#128#0#27#179' Interrupts output'#27#10#179);
   CmdBox.Writeln(#27#179' resume'#27#33#128#0#27#179' Resumes output from the last position'#27#10#179);
   CmdBox.Writeln(#27#179' clearhistory'#27#33#128#0#27#179' Clears all history entries'#27#10#179);
   CmdBox.Writeln(#27#179' readpwd'#27#33#128#0#27#179' Read a Password (just as a test)'#27#10#179);
   CmdBox.Writeln(#27#179' exit'#27#33#128#0#27#179' Exit program'#27#10#179);
   CmdBox.Writeln(#27#217#27#197#128#0#27#193#27#10#217);
   CmdBox.TextColor(clSilver);
  end else
  if Input='readpwd' then
  begin
   rdpw:=true;
  end else
  if Input='clearhistory' then
  begin
   CmdBox.TextColor(clYellow);
   CmdBox.Writeln('Clear History...');
   CmdBox.TextColor(clSilver);
   CmdBox.ClearHistory;
  end else
  if Input='start' then
  begin
   TextPosition:=0;
   ReaderTimer.Enabled:=true;
   CmdBox.TextColors(clLime,clBlue);
   CmdBox.Writeln('Start...');
  end else if Input='stop' then
  begin
   TextPosition:=0;
   ReaderTimer.Enabled:=false;
   CmdBox.TextColors(clRed,clBlue);
   CmdBox.Writeln('Stop...');
  end else if Input='pause' then
  begin
   ReaderTimer.Enabled:=false;
   CmdBox.TextColors(clPurple,clBlue);
   CmdBox.Writeln('Pause...');
  end else if Input='resume' then
  begin
   ReaderTimer.Enabled:=true;
   CmdBox.TextColors(clGreen,clBlue);
   CmdBox.Writeln('Continue...');
  end else if Input='clear' then
  begin
   CmdBox.Clear;
  end else if Input='exit' then close else
  begin
   CmdBox.TextColors(clYellow,ClRed);
   CmdBox.Writeln('Invalid Command!');
  end;
 end;
 if rdpw then CmdBox.StartReadPassWord(clYellow,clNavy,'Pwd:',clLime,clNavy) else
 CmdBox.StartRead(clSilver,clNavy,'/example/prompt/>',clYellow,clNavy);
 HistoryList.Clear;
 for i:=0 to CmdBox.HistoryCount-1 do HistoryList.Items.Add(CmdBox.History[i]);
end;

procedure TWMainForm.CbSetCaretChange(Sender: TObject);
begin
 case cbSetCaret.ItemIndex of
  0:CmdBox.CaretType := cartLine;
  1:CmdBox.CaretType := cartSubBar;
  2:CmdBox.CaretType := cartBigBar;
 end;
 CmdBox.SetFocus;
end;

procedure TWMainForm.Button2Click(Sender: TObject);
begin
 CmdBox.ClearHistory;
 HistoryList.Clear;
end;

procedure TWMainForm.Button3Click(Sender: TObject);
begin
 FProcess:=0;
 ProcessTimer.Enabled:=True;
end;

procedure TWMainForm.Button4Click(Sender: TObject);
begin
  FontDialog.Font:=CmdBox.Font;
  if FontDialog.Execute then
  begin
    CmdBox.Font:=FontDialog.Font;
  end;
end;

procedure TWMainForm.cbWordWrapChange(Sender: TObject);
begin
  if CmdBox.WrapMode=wwmWord then CmdBox.WrapMode:=wwmChar else CmdBox.WrapMode:=wwmWord;
end;

procedure TWMainForm.Button1Click(Sender: TObject);
begin
 Close;
end;

procedure TWMainForm.FormDestroy(Sender: TObject);
begin
 DText.Free;
end;

procedure TWMainForm.ProcessTimerTimer(Sender: TObject);
begin
 if FProcess=100 then
 begin
  CmdBox.ClearLine;
  ProcessTimer.Enabled:=False;
 end
 else
 begin
  CmdBox.TextColors(clRed,clBlue);
  CmdBox.Write('Processing ['+IntToStr(FProcess)+'%]'#13);
 end;
 Inc(FProcess);
end;

initialization
  {$I cmdbox2mw.lrs}
end.

