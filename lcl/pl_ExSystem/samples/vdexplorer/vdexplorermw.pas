unit vdexplorermw;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,
  lazfileutils, LazUtf8,
  vd_compress, vd_masks, vd_resource, vd_core, vd_system,
  ComCtrls, ActnList, ImgList, ToolWin, ExtCtrls, StdCtrls, FileUtil;

type
  TfrmMain = class(TForm)
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    ImageList1: TImageList;
    actnNew: TAction;
    actnOpen: TAction;
    actnImport: TAction;
    actnExport: TAction;
    actnNewFolder: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    Sys: TvsSystem;
    Splitter1: TSplitter;
    Tree: TTreeView;
    Memo1: TMemo;
    OpenDialog2: TOpenDialog;
    ToolButton8: TToolButton;
    OpenDialog1: TOpenDialog;
    actnReadOnly: TAction;
    ToolButton9: TToolButton;
    actnErase: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    procedure actnNewExecute(Sender: TObject);
    procedure actnOpenExecute(Sender: TObject);
    procedure actnImportUpdate(Sender: TObject);
    procedure actnNewFolderUpdate(Sender: TObject);
    procedure actnImportExecute(Sender: TObject);
    procedure actnExportExecute(Sender: TObject);
    procedure actnNewFolderExecute(Sender: TObject);
    procedure actnReadOnlyExecute(Sender: TObject);
    procedure actnEraseExecute(Sender: TObject);
  private
    procedure UpdateControls;
    function GetCurPath: string;
    function GetFile(N: TTreeNode): string;
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses frmCreatedrive;

{$R *.lfm}

function GetPath(N: TTreeNode): string;
begin
  if DirectoryExistsUTF8(N.Text) then
    Result := N.Text
  else
    Result := '';

  N := N.Parent;
  while N <> nil do
  begin
    if Result <> '' then
      Result := N.Text + '\' + Result
    else
      Result := N.Text;
    N := N.Parent;
  end;

  if DirectoryExistsUTF8(Result) then
    Result := Result + '\';
end;

function TfrmMain.GetCurPath: string;
begin
  if Tree.Selected <> nil then
  begin
    Result := GetPath(Tree.Selected);
  end
  else
    Result := '';
end;

function TfrmMain.GetFile(N: TTreeNode): string;
begin
  Result := N.Text;

  N := N.Parent;
  while N <> nil do
  begin
    Result := N.Text + '\' + Result;
    N := N.Parent;
  end;
end;

procedure TfrmMain.UpdateControls;
  procedure UpdateTree(Path: string; Root: TTreeNode);
  var
    S: TSearchRec;
    N: TTreeNode;
    I: TListItem;
  begin
    if FindFirstUTF8(Path + '*',$FFFF,S)  = 0 then
    begin
      repeat
        if (S.Name = '.') or (S.Name = '..') then Continue;

        N := Tree.Items.AddChild(Root, S.Name);

        if (S.Attr and faDirectory = faDirectory) then
          UpdateTree(Path + '\' + S.Name + '\', N);
      until FindNextUTF8(S) <> 0;
      FindCloseUTF8(S);
    end;
  end;
begin
  Tree.Items.Clear;
  UpdateTree('', Tree.Items.GetFirstNode);
  with Memo1.Lines do
  begin
    Clear;
    if Sys.ReadOnly then
      Add('ReadOnly = true')
    else
      Add('ReadOnly = false');
    if Sys.Compressed then
      Add('Compressed = true')
    else
      Add('Compressed = false');
  end;
  actnReadOnly.Checked := Sys.ReadOnly;
end;

procedure TfrmMain.actnNewExecute(Sender: TObject);
begin
  frmCreate := TfrmCreate.Create(Self);
  if frmCreate.ShowModal = mrOK then
  begin
    UpdateControls;
  end;
  frmCreate.Free;
end;

procedure TfrmMain.actnOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Sys.CreateFromFile(OpenDialog1.FileName);
    UpdateControls;
  end;
end;

procedure TfrmMain.actnImportUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (Sys.FileName <> '') and (not Sys.ReadOnly);
end;

procedure TfrmMain.actnNewFolderUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (Sys.FileName <> '') and (not Sys.ReadOnly);
end;

procedure TfrmMain.actnImportExecute(Sender: TObject);
var
  i: integer;
  Source, Dest: TStream;
  DestPath: string;
begin
  if OpenDialog2.Execute then
  begin
    for i := 0 to OpenDialog2.Files.Count - 1 do
    begin
      Source := TFileStream.Create(OpenDialog2.Files[i], fmOpenRead);
      if Source <> nil then
      begin
        Dest := Sys.CreateStream(GetCurPath + ExtractFileName(OpenDialog2.Files[i]), fmCreate);
        Dest.CopyFrom(Source, Source.Size);
        Dest.Free;
        Source.Free;
      end;
    end;
    UpdateControls;
  end;
end;

procedure TfrmMain.actnExportExecute(Sender: TObject);
var
  i: integer;
  Source, Dest: TStream;
  Item: TTreeNode;
  DestPath: string;
  frmExport : TSelectDirectoryDialog ;
begin

  frmExport := TSelectDirectoryDialog.Create(Self);

  if frmExport.Execute then
  begin
    DestPath := frmExport.FileName;

    if Tree.SelectionCount > 0 then
    begin
      for i := 0 to Tree.SelectionCount - 1 do
      begin
        Item := Tree.Selections[i];
        Source := Sys.CreateStream(GetFile(Item), fmOpenRead);
        if (Source <> nil) and (ForceDirectoriesUTF8(ExtractFilePath(DestPath + GetFile(Item)))) then
        begin
          Dest := TFileStream.Create(DestPath + GetFile(Item), fmCreate);
          Dest.CopyFrom(Source, Source.Size);
          Dest.Free;
          Source.Free;
        end;
      end;
    end;
  end;
  frmExport.Free;
end;

procedure TfrmMain.actnNewFolderExecute(Sender: TObject);
var
  S: string;
begin
  S := 'new folder';
  if InputQuery('Create directory', 'Type new directory name:', S) then
  begin
    CreateDirUTF8(GetCurPath + S);
    UpdateControls;
  end;
end;

procedure TfrmMain.actnReadOnlyExecute(Sender: TObject);
begin
  Sys.ReadOnly := not Sys.ReadOnly;
  if not Sys.ReadOnly then
  begin
    ShowMessage('You must reopen storage');
    Sys.Clear;
  end;
  UpdateControls;
end;

procedure TfrmMain.actnEraseExecute(Sender: TObject);
begin
  if Tree.Selected <> nil then
  begin
    Sys.EraseFile(GetFile(Tree.Selected));
    UpdateControls;
  end;
end;

end.
