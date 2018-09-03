{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplMemStreamUnit;

interface

uses
  SysUtils, Classes;

type

  TplMemStream = class(TComponent)
  private
    FData: TMemoryStream;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    function GetSize: int64;
    procedure SetSize(NewSize: int64);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    function GetAsStream: TStream;
    property AsStream: TStream read GetAsStream;
    property Size: int64 read GetSize write SetSize;
  end;

implementation

constructor TplMemStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TMemoryStream.Create;
end;

destructor TplMemStream.Destroy;
begin
  FData.Free;
  FData := nil;
  inherited Destroy;
end;

procedure TplMemStream.Assign(Source: TPersistent);
begin
  if Source is TplMemStream then
  begin
    with TplMemStream(Source) do
    begin
      Self.Name := Name;
      Self.FData.Clear;

      Self.FData.LoadFromStream(FData);
    end;
  end
  else
    AssignTo(Source);
end;

procedure TplMemStream.SaveToStream(Stream: TStream);
begin
  FData.Position := 0;
  Stream.CopyFrom(FData, 0);
end;

procedure TplMemStream.LoadFromStream(Stream: TStream);
begin
  FData.CopyFrom(Stream, 0);
end;

function TplMemStream.GetAsStream: TStream;
begin
  FData.Position := 0;
  Result := FData;
end;


function TplMemStream.GetSize: int64;
begin
  Result := FData.Size;
end;

procedure TplMemStream.SetSize(NewSize: int64);
begin
end;

procedure TplMemStream.ReadData(Stream: TStream);
var
  L: integer;
begin
  FData.Clear;
  Stream.Read(L, SizeOf(integer));
  FData.Size := L;
  FData.CopyFrom(Stream, L);
end;

procedure TplMemStream.WriteData(Stream: TStream);
var
  L: integer;
begin
  FData.Position := 0;
  L := FData.Size;
  Stream.Write(L, SizeOf(integer));
  Stream.CopyFrom(FData, FData.Size);
end;

procedure TplMemStream.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ResData', @ReadData, @WriteData, Assigned(FData) and (FData.Size > 0));
end;

//..................................................
initialization
  RegisterClasses([TplMemStream]);
end.
