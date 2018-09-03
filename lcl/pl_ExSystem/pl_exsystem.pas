{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_exsystem;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExSystemRegister, EpikTimer, ExSystemConsts, ExSystemMessages, 
  ExSystemUtils, TplAppEventsUnit, TplDiffUnit, TplFileSearchUnit, 
  TplMemINIUnit, TplMemStreamUnit, TplPlayWAVUnit, TplProcessThreadUnit, 
  TplResStoreUnit, TplTimerUnit, uCmdBox, vd_compress, vd_core, vd_masks, 
  vd_resource, vd_system, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExSystemRegister', @AllExSystemRegister.Register);
end;

initialization
  RegisterPackage('pl_exsystem', @Register);
end.
