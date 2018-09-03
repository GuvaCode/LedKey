unit pwMainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls, SysUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    AsynchronousCheckBox: TCheckBox;
    PlayButton: TButton;
    SongsRadioGroup: TRadioGroup;
    procedure PlayButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TplPlayWAVUnit;

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}
{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}

{ TMainForm }

procedure TMainForm.PlayButtonClick(Sender: TObject);
begin  {
  LSPlayWAVFile(ExtractFilePath(ParamStr(0)) +
    LowerCase(SongsRadioGroup.Items[SongsRadioGroup.ItemIndex]) + '.wav',
    AsynchronousCheckBox.Checked); }

  LSPlayWAVFile(pathMedia +
    LowerCase(SongsRadioGroup.Items[SongsRadioGroup.ItemIndex]) + '.wav',
    AsynchronousCheckBox.Checked);
end;

end.

