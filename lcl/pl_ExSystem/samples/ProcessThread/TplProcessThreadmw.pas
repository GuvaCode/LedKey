unit TplProcessThreadmw;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Buttons, ComCtrls,
  TplProcessThreadUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    ResultLabel: TLabel;
    StopBitBtn: TBitBtn;
    TimesEdit: TEdit;
    StartBitBtn: TBitBtn;
    TimesUpDown: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure StartBitBtnClick(Sender: TObject);
    procedure StopBitBtnClick(Sender: TObject);
  public
    procedure OnStop(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}


{ TMainForm }

procedure TMainForm.StartBitBtnClick(Sender: TObject);
begin
  StartBitBtn.Enabled := False;
  StopBitBtn.Enabled := True;
  ResultLabel.Caption := '';
  LSProcessThreadInstance.Execute(//ExtractFilePath(ParamStr(0)) +
    {$IFDEF MSWINDOWS}'loop.exe '{$ELSE}'./loop '{$ENDIF} + TimesEdit.Text);
end;

procedure TMainForm.StopBitBtnClick(Sender: TObject);
begin
  LSProcessThreadInstance.Stop;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ResultLabel.Caption := '';
  LSProcessThreadInstance.OnStop := @OnStop;
//  LSProcessThreadInstance.CanceledMsg := 'Stopped for you.';
end;

procedure TMainForm.OnStop(Sender: TObject);
begin
  StopBitBtn.Enabled := False;
  StartBitBtn.Enabled := True;
  ResultLabel.Caption := LSProcessThreadInstance.Output;
end;

end.

