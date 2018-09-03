
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplResStoreUnit;

{$DEFINE USEZIP}

interface


uses
  SysUtils, Classes;

type
  ERessourceStoreError = class(Exception);
  TPackedRessources = class;
  TPackedItem = class;

  TplResStore = class(TComponent)
  private
    FRessources: TPackedRessources;
    procedure SetRessources(Ressources: TPackedRessources);
    function GetSize: Integer;
    procedure SetSize(NewSize: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToPath(PathName: string);
    function ItemIndex(ItemName: string): Integer;
    function GetResByName(ResName: string): TPackedItem;
    function GetResByIndex(Index: Integer): TPackedItem;
  published
    property Items: TPackedRessources read FRessources write SetRessources;
    property Size: Integer read GetSize write SetSize;
  end;

  TPackedRessources = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Idx: Integer): TPackedItem;
    procedure SetItem(Idx: Integer; Item: TPackedItem);
  protected
    function GetOwner: TPersistent; override;
    property Owner: TPersistent read FOwner write FOwner;
  public
    constructor Create;
    property Items[Idx: Integer]: TPackedItem read GetItem write SetItem; default;
  end;

  TStoreMode = (smStore{$IFDEF USEZIP}, smZipLow, smZipMed, smZipMax{$ENDIF});

  TPackedItem = class(TCollectionItem)
  private
    FData: TMemoryStream;
    FStoreMode: TStoreMode;
    FName: string;
    procedure SetStoreMode(StoreMode: TStoreMode);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    function  GetSize: Integer;
    procedure SetSize(NewSize: Integer);
    procedure SetName(Name: string);
  protected
    function  GetDisplayName: string; //override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    function  GetAsStream: TStream;
    property  AsStream: TStream read GetAsStream;
  published
    property StoreMode: TStoreMode read FStoreMode write SetStoreMode {$IFDEF USEZIP}default smZipMax{$ENDIF};
    property Size: Integer read GetSize write SetSize;
    property Name: string read FName write SetName;
  end;

implementation

{$IFDEF USEZIP}
uses zstream;
{$ENDIF}

const
  msgRessourceNotFound = 'Ressource "%s" not found';
  msgNameAlreadyExists = 'Item name "%s" already exists !';

{ ****************************** TplResStore ****************************** }

constructor TplResStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRessources:= TPackedRessources.Create;
  FRessources.Owner:= Self;
end;

destructor TplResStore.Destroy;
begin
  FRessources.Free;
  inherited Destroy;
end;

function TplResStore.ItemIndex(ItemName: string): Integer;
var i: Integer;
begin
  Result:= -1; i:= 0;
  while (i < FRessources.Count) and (CompareText(ItemName, TPackedItem(FRessources.Items[i]).Name) <> 0) do
    Inc(i);
  if i < FRessources.Count then
    Result:= i;
end;

function TplResStore.GetResByName(ResName: string): TPackedItem;
var Idx: Integer;
begin
  Idx:= ItemIndex(ResName);
  if Idx <> -1 then
    Result:= TPackedItem(FRessources.Items[Idx])
  else
    raise ERessourceStoreError.CreateFmt(msgRessourceNotFound, [ResName]);
end;

function TplResStore.GetResByIndex(Index: Integer): TPackedItem;
begin
  Result:= Items[Index];
end;

function TplResStore.GetSize: Integer;
var i: Integer;
begin
  Result:= 0;
  for i:= 0 to FRessources.Count - 1 do
    Result:= Result + TPackedItem(FRessources.Items[i]).Size;
end;

procedure TplResStore.SetSize(NewSize: Integer); // ReadOnly property
begin end;

procedure TplResStore.SetRessources(Ressources: TPackedRessources);
begin
  FRessources.Assign(Ressources);
end;

procedure TplResStore.SaveToPath(PathName: string);
var i: Integer;
begin
  if (PathName <> '') and (PathName[Length(PathName)] <> '\') then
    PathName:= PathName + '\';
  for i:= 0 to Items.Count - 1 do
    Items[i].SaveToFile(PathName + Items[i].Name);
end;

{ ******************************** TPackedItem ******************************** }

constructor TPackedItem.Create(aCollection: TCollection);
begin
  {$IFDEF USEZIP}FStoreMode:= smZipMax;{$ENDIF}
  inherited Create(aCollection);
  FData:= TMemoryStream.Create;
end;

destructor TPackedItem.Destroy;
begin
  FData.Free; FData:= nil;
  inherited Destroy;
end;

procedure TPackedItem.Assign(Source: TPersistent);
begin
  if Source is TPackedItem then
  begin
    with TPackedItem(Source) do
    begin
      Self.Name:= Name;
      Self.FData.Clear;
      Self.StoreMode:= StoreMode;
      Self.FData.LoadFromStream(FData);
    end;
  end else AssignTo(Source);
end;

{$IFDEF USEZIP}
procedure TPackedItem.SaveToStream(Stream: TStream);
var S: TDecompressionStream;
    L: Integer;
begin
  FData.Position:= 0;
  if (StoreMode <> smStore) and (FData.Size > 0) then
  begin
    S:= TDecompressionStream.Create(FData);
    try
      S.Read(L, SizeOf(Integer));
      if L <> 0 then
        Stream.CopyFrom(S, L);
    finally
      S.Free;
    end;
  end else
    Stream.CopyFrom(FData, FData.Size);
end;

procedure TPackedItem.LoadFromStream(Stream: TStream);
var S: TCompressionStream;
    L: TCompressionLevel;
    Tmp: Integer;
begin
  case StoreMode of
    smZipLow: L:= clFastest;
    smZipMed: L:= clDefault;
    smZipMax: L:= clMax
    else L:= clNone;
  end;
  Stream.Position:= 0;
  FData.Clear;
  if Stream.Size = 0 then
    Exit;
  if L <> clNone then
  begin
    S:= TCompressionStream.Create(L, FData);
    try
      Tmp:= Stream.Size;
      S.Write(Tmp, SizeOf(Integer));
      S.CopyFrom(Stream, Tmp);
    finally
      S.Free;
    end;
  end else
    FData.CopyFrom(Stream, 0);
end;
{$ELSE}
procedure TPackedItem.SaveToStream(Stream: TStream);
begin
  FData.Position:= 0;
  Stream.CopyFrom(FData, 0);
end;

procedure TPackedItem.LoadFromStream(Stream: TStream);
begin
  FData.CopyFrom(Stream, 0);
end;
{$ENDIF}

procedure TPackedItem.SaveToFile(FileName: string);
var F: TFileStream;
begin
  F:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TPackedItem.LoadFromFile(FileName: string);
var F: TFileStream;
begin
  F:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

function TPackedItem.GetAsStream: TStream;
begin
  SetStoreMode(smStore);
  FData.Position:= 0;
  Result:= FData;
end;

procedure TPackedItem.SetStoreMode(StoreMode: TStoreMode);
{$IFDEF USEZIP}
var M: TMemoryStream;
begin
  if csLoading in TplResStore(TPackedRessources(GetOwner).GetOwner).ComponentState then
    FStoreMode:= StoreMode
  else if StoreMode <> FStoreMode then
  begin
    M:= TMemoryStream.Create;
    try
      SaveToStream(M);
      FStoreMode:= StoreMode;
      LoadFromStream(M);
    finally
      M.Free;
    end;
  end;
end;
{$ELSE}
begin
  FStoreMode:= smStore;
end;
{$ENDIF}

function TPackedItem.GetDisplayName: string;
begin
  if FData.Size = 0 then
    Result:= ClassName
  else
    Result:= FName;
end;

function TPackedItem.GetSize: Integer;
begin
  Result:= FData.Size;
end;

procedure TPackedItem.SetSize(NewSize: Integer);
begin end;

procedure TPackedItem.SetName(Name: string);
var i: Integer;
begin
  for i:= 0 to Collection.Count - 1 do
  begin
    if (Collection.Items[i].ID <> ID) and (CompareText(Name, TPackedItem(Collection.Items[i]).Name) = 0) then
      raise ERessourceStoreError.CreateFmt(msgNameAlreadyExists, [Name]);
  end;
  FName:= Name;
end;

procedure TPackedItem.ReadData(Stream: TStream);
var L: Integer;
begin
  FData.Clear;
  Stream.Read(L, SizeOf(Integer));
  FData.Size:= L; { Allocates what's needed }
  FData.CopyFrom(Stream, L);
end;

procedure TPackedItem.WriteData(Stream: TStream);
var L: Integer;
begin
  FData.Position:= 0;
  L:= FData.Size;
  Stream.Write(L, SizeOf(Integer));
  Stream.CopyFrom(FData, FData.Size);
end;

procedure TPackedItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ResData', @ReadData, @WriteData, Assigned(FData) and (FData.Size > 0));
end;

{ ***************************** TPackedRessources ***************************** }

function TPackedRessources.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

function TPackedRessources.GetItem(Idx: Integer): TPackedItem;
begin
  Result:= TPackedItem(inherited Items[Idx]);
end;

procedure TPackedRessources.SetItem(Idx: Integer; Item: TPackedItem);
begin
  inherited Items[Idx]:= Item;
end;

constructor TPackedRessources.Create;
begin
  FOwner:= nil;
  inherited Create(TPackedItem);
end;
end.
