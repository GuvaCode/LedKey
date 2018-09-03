unit MainNotifierOS;

{$mode objfpc}{$H+}

interface

uses
  Forms, Buttons, Controls, Classes, Graphics, StdCtrls, ExtCtrls, Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    BottomRightSpeedButton: TSpeedButton;
    BottomTopLeftSpeedButton: TSpeedButton;
    AceptClickCheckBox: TCheckBox;
    HintButton: TButton;
    ImageList: TImageList;
    BottomPanel: TPanel;
    RetangleLabel: TLabel;
    TopPanel: TPanel;
    RoundLabel: TLabel;
    TopLeftSpeedButton: TSpeedButton;
    TopRightSpeedButton: TSpeedButton;
    procedure HintButtonClick(Sender: TObject);
    procedure TopLeftSpeedButtonClick(Sender: TObject);
    procedure TopRightSpeedButtonClick(Sender: TObject);
    procedure BottomRightSpeedButtonClick(Sender: TObject);
    procedure BottomTopLeftSpeedButtonClick(Sender: TObject);
  private
    FBitmap: TBitmap;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure HintClick(Sender: TObject);
  end;

const
  CInfo = 'NotifierOS 1.0, Shows a notification window for short messages.';
  CInfoBkColor = $00B9FFFF;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TplNotifierOSUnit;

{ TMainForm }

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBitmap := TBitmap.Create;
  ImageList.GetBitmap(0, FBitmap);
end;

destructor TMainForm.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TMainForm.HintClick(Sender: TObject);
begin
  ShowMessage('Hint clicked!');
end;

procedure TMainForm.TopRightSpeedButtonClick(Sender: TObject);
begin
  TplNotifierOS.Execute(Caption, CInfo, FBitmap);
end;

procedure TMainForm.BottomRightSpeedButtonClick(Sender: TObject);
begin
  TplNotifierOS.Execute(Caption, CInfo, FBitmap, npBottomRight,
    CDefaultPause, ntRetangle, CInfoBkColor, -1, clBlack);
end;

procedure TMainForm.BottomTopLeftSpeedButtonClick(Sender: TObject);
begin
  TplNotifierOS.Execute(Caption, CInfo, FBitmap, npBottomLeft,
    CDefaultPause, ntRetangle, clGray);
end;

procedure TMainForm.TopLeftSpeedButtonClick(Sender: TObject);
begin
  TplNotifierOS.Execute(Caption, CInfo, FBitmap, npTopLeft, CDefaultPause,
    ntRound, clTeal);
end;

procedure TMainForm.HintButtonClick(Sender: TObject);
begin
  if AceptClickCheckBox.Checked then
    TplNotifierOS.Execute(Caption, CInfo, FBitmap, npTopLeft, -1, ntRetangle,
      CInfoBkColor, -1, clBlack, CLeftGain, CTopGain, @HintClick)
  else
    TplNotifierOS.Execute(Caption, CInfo, FBitmap, npTopLeft, -1, ntRetangle,
      CInfoBkColor, -1, clBlack);
end;

end.

