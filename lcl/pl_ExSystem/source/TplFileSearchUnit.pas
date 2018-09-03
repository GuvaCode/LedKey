
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplFileSearchUnit;

interface

uses LResources,
  SysUtils, Classes, FileUtil, lazfileutils, Graphics, Controls, Forms, Dialogs;

type
  TplFindFileEvent = procedure(fullpath: string; info: TSearchRec) of object;
  TChangeFolderEvent = procedure(fullpath: string; info: TSearchRec) of object;

  TplFileSearch = class(TComponent)
  private
    fRec: boolean;
    fStop: boolean;
    fSearching: boolean;
    fFilesFound: TStringList;
    fFileFindEvent: TplFindFileEvent;
    fChangeFolderEvent: TChangeFolderEvent;
    fFinishEvent: TNotifyEvent;
    fdirName: shortstring;
  protected
    procedure ScanDir(ffdirName: string; attr: word);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    property Searching: boolean read fSearching;
  published
    property Stop: boolean read fStop write fStop default False;
    property SearchFile: shortstring read fdirName write fdirName;
    property FilesFound: TStringList read fFilesFound;
    property RecurseSubFolders: boolean read fRec write fRec default True;
    property OnFileFind: TplFindFileEvent read fFileFindEvent write fFileFindEvent;
    property OnChangeFolder: TChangeFolderEvent read fChangeFolderEvent write fChangeFolderEvent;
    property OnFinish: TNotifyEvent read fFinishEvent write fFinishEvent;
  end;


implementation
const
{$IFDEF WINDOWS}
  delimeter = '\';
  fi: string = '*.*';
{$ELSE}
  delimeter = '/';
  fi: string = '*';
{$ENDIF}
  p: string = '.';
  pp: string = '..';

constructor TplFileSearch.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fFilesFound := TStringList.Create;
  fRec := True;
  fSearching := False;
  fStop := False;
end;

destructor TplFileSearch.Destroy;
begin
  fFilesFound.Free;
  inherited Destroy;
end;

procedure TplFileSearch.Start;
var
  i, newWildCard: integer;
  curSearchPath, wildCards: string;
  srchPaths: TStringList;
begin
  fStop := False;

  fSearching := True;
  fFilesFound.Clear;


  newWildCard := Pos(';', fDirName);

  if newWildCard > 0 then
  begin
    curSearchPath := Copy(fdirName, 1, newWildCard - 1);
    wildCards := Copy(fdirName, newWildCard + 1, length(fDirName));

    srchPaths := TStringList.Create;
    srchPaths.Add(curSearchPath);


    while length(wildCards) > 0 do
    begin
      curSearchPath := ExtractFilePath(curSearchPath);
      newWildCard := Pos(';', wildCards);

      if newWildCard > 0 then
      begin
        curSearchPath := curSearchPath + Copy(wildCards, 1, newWildCard - 1);
        wildCards := Copy(wildCards, newWildCard + 1, length(wildCards));
      end
      else
      begin
        curSearchPath := curSearchPath + wildCards;
        wildCards := '';
      end;

      srchPaths.Add(curSearchPath);
    end;


    for i := 0 to srchPaths.Count - 1 do
      ScanDir(srchPaths.Strings[i], faAnyFile);


    srchPaths.Free;
  end
  else
    ScanDir(fdirName, faAnyFile);


  if Assigned(fFinishEvent) then
    fFinishEvent(Self);

  fSearching := False;
end;

procedure TplFileSearch.ScanDir(ffdirName: string; attr: word);
var
  path: string;
  doserror: integer;
  sfi: string;

  procedure showq(fullpath: string; FolderInfo: TSearchRec);
  var
    dirq: TSearchRec;
  begin
    if assigned(fChangeFolderEvent) then
      fChangeFolderEvent(fullpath, FolderInfo);

    doserror := FindFirstUTF8(fullpath + sfi, attr, dirq);

    while (doserror = 0) and (not fstop) do
    begin
      if (dirq.Name <> p) and (dirq.Name <> pp) and (assigned(fFileFindEvent)) then
      begin
        fFileFindEvent(fullpath, dirq);
        fFilesFound.Add(fullpath + dirq.Name);
      end;

      doserror := FindNextUTF8(dirq);
      application.ProcessMessages;
    end;
    FindCloseUTF8(dirq);

  end;

  procedure ScanLDir(fffdirName: string; fInfo: TSearchRec);
  var
    dirinfo: TSearchRec;
  begin
    showq(fffDirName, fInfo);
    dosError := FindFirstUTF8(fffDirName + fi, faAnyfile, dirInfo);

    while (doserror = 0) and (not fstop) do
    begin
      application.ProcessMessages;

      if (dirInfo.Name <> p) and (dirInfo.Name <> pp) then
        if (dirInfo.attr and faDirectory <> 0) and (frec) then
          ScanLDir(fffdirName + dirinfo.Name + delimeter, dirInfo);

      dosError := FindNextUTF8(dirInfo);
      application.ProcessMessages;
    end;
    FindCloseUTF8(dirInfo);

  end;

var
  fInfo: TSearchRec;
  fPath: string;
begin

  path := ExtractFilePath(ffDirName);
  sfi := ExtractFileName(ffDirName);

  fPath := Copy(path, 1, length(Path) - 1);

  FindFirstUTF8(fPath, faAnyfile, fInfo);
  ScanLDir(Path, fInfo);
  FindCloseUTF8(fInfo);
end;


end.
