unit mUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, LCLType, Codebot.Input.Hotkeys;

type

  { THideFrm }
  THideFrm= class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    LedState:Boolean;
    LedProcess: TProcess;
    procedure KeyNotify(Sender: TObject; Key: Word; Shift: TShiftState);
  public

  end;

const
  cMyHookAtom = 'ledkeyapp';

var
  HideFrm: THideFrm;

implementation

{$R *.lfm}

{ THideFrm }

procedure THideFrm.DataModuleCreate(Sender: TObject);
begin
  HotkeyCapture.RegisterNotify(VK_SCROLL,[],@KeyNotify);
end;

procedure THideFrm.DataModuleDestroy(Sender: TObject);
begin
  HotkeyCapture.UnRegisterNotify(VK_SCROLL,[]);
end;

procedure THideFrm.KeyNotify(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  LedProcess := TProcess.Create(nil);
   LedProcess.Options:=[poWaitOnExit,poNoConsole];
   LedProcess.Executable := 'xset';
     If LedState then
      begin
       LedProcess.Parameters.Add('-led');
       LedState:=false;
      end else
      begin
      LedProcess.Parameters.Add('led');
      LedState:=true;
      end;
    LedProcess.Parameters.Add('3');
    LedProcess.Execute;
    LedProcess.Free;
end;

end.

