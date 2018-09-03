unit cmdbox1mw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uCmdBox, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    CmdBox1: TCmdBox;
    procedure CmdBox1Input(ACmdBox: TCmdBox; Input: string);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  
 CmdBox1.StartRead(clSilver,clNavy,'demo/>',clYellow,clNavy);
 CmdBox1.TextColors(clWhite,clNavy);
 CmdBox1.Writeln('Please give a command.');
end;

procedure TForm1.CmdBox1Input(ACmdBox: TCmdBox; Input: string);
begin
  CmdBox1.Writeln('command: '+Input);
  CmdBox1.StartRead(clSilver,clNavy,'demo/>',clYellow,clNavy);
end;

end.

