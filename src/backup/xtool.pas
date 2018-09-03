unit XTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Graphics, Controls, uDCReadSVG;

{ Graphics }
function GetNativePathIcon(const IconName:String; Size:Integer): String;
function GetBitmapIcon(const IconName:String; Size:Integer):TBitmap;
function GetIcon(const IconName:String; Size:Integer):TIcon;
Procedure LoadImageList(Const Names:String; Images:TimageList);
{ Keyboard }
procedure LedLight(Light:Boolean);
function ScrollLockPress:Boolean;


implementation
uses gtk2;

{ Graphics }
function GetNativePathIcon(const IconName: String; Size: Integer): String;
var gtk_icon:PGtkIconInfo;
begin
   gtk_icon :=gtk_icon_theme_lookup_icon(gtk_icon_theme_get_default(),PChar(IconName),Size,
   TGtkIconLookupFlags(GTK_ICON_LOOKUP_FORCE_SVG)) ;
   Result:=gtk_icon_info_get_filename(gtk_icon);
end;

function GetBitmapIcon(const IconName: String; Size: Integer): TBitmap;
Var Bmp:TPicture;

begin
 Bmp:=TPicture.Create;
    if ExtractFileExt (GetNativePathIcon(IconName,Size)) ='.svg' then
    Bmp.Bitmap:=BitmapLoadFromScalable(GetNativePathIcon(IconName,Size),Size,Size)
    else
    Bmp.LoadFromFile(GetNativePathIcon(IconName,Size));
    Result:=Bmp.Bitmap;
end;

function GetIcon(const IconName: String; Size: Integer): TIcon;
Var Bmp:TPicture;
begin
Bmp:=TPicture.Create;
 {   if ExtractFileExt (GetNativePathIcon(IconName,Size)) ='.svg' then
    Bmp.Bitmap:=BitmapLoadFromScalable(GetNativePathIcon(IconName,Size),Size,Size)
    else }
    Bmp.LoadFromFile(GetNativePathIcon(IconName,Size));
    Result:=Bmp.Icon;
end;

procedure LoadImageList(const Names: String; Images: TimageList);
var
  CBmp:TPicture;
  t:TStringList;
  i:integer;
begin
   t:=TStringList.Create;
   t.text:=stringReplace(Names,',',#13#10,[rfReplaceAll]);//мы заменяем все пробелы на символы конца строки
   CBmp:=TPicture.Create;
   for i:=0 to t.Count-1 do
   begin
    CBmp.LoadFromFile(GetNativePathIcon(t.Strings[i],Images.Width));
    Images.Add(CBmp.Bitmap,CBmp.Bitmap);
   end;
   CBmp.Free;
   t.Free;
end;

{ Keyboard }
// Set Keyboard Led
procedure LedLight(Light: Boolean);
Var ShProcess:Tprocess;
begin
    ShProcess := TProcess.Create(nil);
    ShProcess.Options:=[poWaitOnExit,poNoConsole];
    ShProcess.Executable := 'xset';
    If Light then
    ShProcess.Parameters.Add('-led')
    else
    ShProcess.Parameters.Add('led');
    ShProcess.Parameters.Add('3');
    ShProcess.Execute;
    ShProcess.Free;
end;

//  Get ScrollLockState
function ScrollLockPress: Boolean;
var ShProcess:TProcess;
    cArgs: String;
    Output:TStringList;
begin
    cArgs :=
    Trim(' xset q               ') +
    Trim(' | grep Scroll        ') +
    Trim(' | grep -Eo ''.{3}$'' ') ;

  Output:=TStringList.Create;
  ShProcess := TProcess.Create(nil);
  ShProcess.Executable := '/bin/sh';
  ShProcess.Parameters.Add('-c');
  ShProcess.Parameters.Add(cArgs);
  ShProcess.Options := ShProcess.Options + [poWaitOnExit, poUsePipes];
  ShProcess.Execute;

  Output.LoadFromStream(ShProcess.Output);

  case Trim(Output.Strings[Output.Count-1]) of
  'off' : Result := False;
  'on'  : Result := True;
  end;

  ShProcess.Free;
  Output.Free;
end;

end.

