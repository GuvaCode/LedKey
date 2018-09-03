unit frmCreatedrive;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, vd_system;

type
  TfrmCreate = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    ComboBox2: TComboBox;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCreate: TfrmCreate;

implementation

uses vdexplorermw;

{$R *.lfm}

procedure TfrmCreate.Button1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Edit1.Text := SaveDialog1.FileName;
    Button2.Enabled := true;
  end;
end;

procedure TfrmCreate.CheckBox1Click(Sender: TObject);
begin
  ComboBox1.Enabled := CheckBox1.Checked;
end;

procedure TfrmCreate.Button2Click(Sender: TObject);
begin
  frmMain.Sys.Compressed := CheckBox1.Checked;
  frmMain.Sys.CompressionLevel := TvsCompressionLevel(ComboBox1.ItemIndex);
  frmMain.Sys.CreateEmpty(Edit1.Text, StrToInt(ComboBox2.Text));
end;

end.
