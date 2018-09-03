{**********************************************************************
                PilotLogic Software House.
  
 Package etpackage.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllExSystemRegister;

interface


 uses
  Classes,SysUtils,TypInfo,lresources,PropEdits,ComponentEditors,

  TplResStoreUnit,
  TplMemStreamUnit,
  TplMemINIUnit,
  TplFileSearchUnit,
  TplTimerUnit,
  TplAppEventsUnit,
  TplDiffUnit,
  epiktimer,
  vd_system,
  uCmdBox;

procedure Register;

implementation

{$R AllExSystemRegister.res}

uses dialogs;

//=========================== For TplResStore =============================
type
  TROIntProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

function TROIntProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paReadOnly];
end;

type
  TPackedItemName = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TPackedItemName.GetAttributes: TPropertyAttributes;
begin
  Result:= inherited GetAttributes + [paDialog];
end;

procedure TPackedItemName.Edit;
var Dialog: TOpenDialog;
    F: TFileStream;
  procedure SetUniqueName(Name: string);
  var i: Integer;
      Base: string;
  begin
    try
      SetStrValue(Name);
    except
      Base:= Name; i:= 0;
      while Name <> GetStrValue do
      begin
        Inc(i);
        Name:= Base + IntToStr(i);
        try
          SetStrValue(Name);
        except end;
      end;
    end;
  end;
begin
  Dialog:= TOpenDialog.Create(nil);
  try
    Dialog.Options:= Dialog.Options - [ofHideReadOnly];
    if Dialog.Execute then
    begin
      F:= TFileStream.Create(Dialog.FileName, fmOpenRead or fmShareDenyNone);
      try
        SetUniqueName(ExtractFileName(Dialog.FileName));
        TPackedItem(GetComponent(0)).LoadFromStream(F);
        Modified;
      finally
        F.Free;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

//==========================================================
procedure Register;
begin

  RegisterComponents ('Extra System',[
                                       TplResStore,
                                       TplMemStream,
                                       TvsSystem,
                                       TplMemIni,
                                       TplFileSearch,
                                       TplTimer,
                                       TEpikTimer,
                                       TplAppEvents,
                                       TplDiff,
                                       TCmdBox
                                   ]);


 //-------------------------- For TplResStore ------------------------------
  RegisterPropertyEditor(TypeInfo(Integer), TplResStore, 'Size', TROIntProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TPackedItem, 'Size', TROIntProperty);
  RegisterPropertyEditor(TypeInfo(string), TPackedItem, 'Name', TPackedItemName);

end;

end.
