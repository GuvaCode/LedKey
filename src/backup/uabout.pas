unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAboutFrm }

  TAboutFrm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
  private

  public

  end;

resourcestring
  AboutMnuStr = 'About LedKey';

var
  AboutFrm: TAboutFrm;

implementation

{$R *.lfm}

end.

