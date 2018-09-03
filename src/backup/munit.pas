
unit mUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtCtrls, LCLType, Controls, Menus, uDCReadSVG,
  LCLIntf, Forms, uAbout,
  Codebot.Input.Hotkeys;

type
  { TmContainer }
  TmContainer = class(TDataModule)
    ImageList: TImageList;
    MnuImageList: TImageList;
    InfoMnu: TMenuItem;
    ExitMnu: TMenuItem;
    MyMenu: TPopupMenu;
    ScrollLock: TTrayIcon;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ExitMnuClick(Sender: TObject);
    procedure InfoMnuClick(Sender: TObject);
    procedure ScrollLockClick(Sender: TObject);
  private
    procedure KeyNotify(Sender: TObject; Key: Word; Shift: TShiftState);
    procedure ChangeIcon;
  public

  end;

const
  CMyAppAtom = 'ledkey';
  MenuNamesIcons = 'help-about,exit';

resourcestring
  AboutMnuStr = 'About LedKey';
  ExitMnuStr = 'Exit';

var
  mContainer: TmContainer;

implementation
 uses XTool;
{$R *.lfm}

 { TmContainer }

procedure TmContainer.DataModuleCreate(Sender: TObject);
 begin
  HotkeyCapture.RegisterNotify(VK_SCROLL,[],@KeyNotify);
  LoadImageList(MenuNamesIcons,MnuImageList);
  InfoMnu.ImageIndex:=0;
  InfoMnu.Caption:=AboutMnuStr;
  ExitMnu.ImageIndex:=1;
  ExitMnu.Caption:=ExitMnuStr;
  ChangeIcon;
 end;

procedure TmContainer.DataModuleDestroy(Sender: TObject);
 begin
  HotkeyCapture.UnRegisterNotify(VK_SCROLL,[]);
 end;

procedure TmContainer.ExitMnuClick(Sender: TObject);
begin
 application.Terminate;
end;

procedure TmContainer.InfoMnuClick(Sender: TObject);
begin
 if assigned(AboutFrm) then exit;
 AboutFrm:=TAboutFrm.Create(Self);
 AboutFrm.Show;
// AboutFrm.Position:=poDesktopCenter;
end;

procedure TmContainer.ScrollLockClick(Sender: TObject);
begin
  KeyNotify(self,VK_SCROLL,[]);
end;

{$HINTS OFF}
procedure TmContainer.KeyNotify(Sender: TObject; Key: Word; Shift: TShiftState);
 begin
  LedLight(ScrollLockPress);
  ChangeIcon;
 end;
{$HINTS ON}

procedure TmContainer.ChangeIcon;
 begin
  if ScrollLockPress then ImageList.GetIcon(0,ScrollLock.Icon)
  else ImageList.GetIcon(1,ScrollLock.Icon);
 end;




end.

