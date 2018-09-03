
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplProcessThreadUnit;

{$I ExSystem.inc}

interface

uses
  ExSystemMessages, Classes, Process;

type

  ILSProcessThread = interface
    ['{2360CF71-F4BE-4EC0-9BC0-E32B8C9BDBD5}']
    function GetCanceledMsg: string;
    function GetOnStop: TNotifyEvent;
    function GetOutput: string;
    procedure SetCanceledMsg(const AValue: string);
    procedure SetOnStop(const AValue: TNotifyEvent);
    procedure SetOutput(const AValue: string);
    procedure Execute(const ACommandLine: string);
    procedure Stop;
    property Output: string read GetOutput write SetOutput;
    property OnStop: TNotifyEvent read GetOnStop write SetOnStop;
    property CanceledMsg: string read GetCanceledMsg write SetCanceledMsg;
  end;

  TProcessThread = class(TThread)
  private
    FOnStart: TNotifyEvent;
    FProcess: TProcess;
    FProcessResult: TStringList;
    procedure DoExecute;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

  TplProcessThread = class(TInterfacedObject, ILSProcessThread)
  private
    FCanceled: boolean;
    FCanceledMsg: string;
    FOnStop: TNotifyEvent;
    FOutput: string;
    FProcessThread: TProcessThread;
    procedure DoStop(Sender: TObject);
    function GetCanceledMsg: string;
    function GetOnStop: TNotifyEvent;
    function GetOutput: string;
    procedure SetCanceledMsg(const AValue: string);
    procedure SetOnStop(const AValue: TNotifyEvent);
    procedure SetOutput(const AValue: string);
  public
    constructor Create;
    procedure Execute(const ACommandLine: string);
    procedure Stop;
    property Output: string read GetOutput write SetOutput;
    property OnStop: TNotifyEvent read GetOnStop write SetOnStop;
    property CanceledMsg: string read GetCanceledMsg write SetCanceledMsg;
  end;

function LSProcessThreadInstance: ILSProcessThread;

implementation

var
  _LSProcessThread: ILSProcessThread;

function LSProcessThreadInstance: ILSProcessThread;
begin
  if not Assigned(_LSProcessThread) then
    _LSProcessThread := TplProcessThread.Create;
  Result := _LSProcessThread;
end;

//=================== TProcessThread =======================================

constructor TProcessThread.Create(CreateSuspended: boolean; const StackSize: SizeUInt);
begin
  FProcess := TProcess.Create(nil);
  FProcessResult := TStringList.Create;
  FreeOnTerminate := True;
  FProcess.Options := [poWaitOnExit, poUsePipes, poNoConsole];
  inherited Create(CreateSuspended, StackSize);
end;

destructor TProcessThread.Destroy;
begin
  OnStart := nil;
  OnTerminate := nil;
  FProcessResult.Free;
  FProcess.Free;
  inherited Destroy;
end;

procedure TProcessThread.DoExecute;
begin
  FProcess.Execute;
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TProcessThread.Execute;
begin
  DoExecute;
end;

//====================== TplProcessThread ===================================

constructor TplProcessThread.Create;
begin
  inherited Create;
  FCanceledMsg := SLSProcessThreadCanceled;
end;

procedure TplProcessThread.DoStop(Sender: TObject);
begin
  FProcessThread.FProcessResult.LoadFromStream(FProcessThread.FProcess.Output);
  if FCanceled then
    FOutput := FCanceledMsg
  else
    FOutput := FProcessThread.FProcessResult.Text;
  FCanceled := False;
  if Assigned(FOnStop) then
    FOnStop(Sender);
end;

function TplProcessThread.GetCanceledMsg: string;
begin
  Result := FCanceledMsg;
end;

function TplProcessThread.GetOnStop: TNotifyEvent;
begin
  Result := FOnStop;
end;

function TplProcessThread.GetOutput: string;
begin
  Result := FOutput;
end;

procedure TplProcessThread.SetCanceledMsg(const AValue: string);
begin
  FCanceledMsg := AValue;
end;

procedure TplProcessThread.SetOnStop(const AValue: TNotifyEvent);
begin
  FOnStop := AValue;
end;

procedure TplProcessThread.SetOutput(const AValue: string);
begin
  FOutput := AValue;
end;

procedure TplProcessThread.Execute(const ACommandLine: string);
begin
  FProcessThread := TProcessThread.Create(True);
  FProcessThread.OnTerminate := @DoStop;
  FProcessThread.FProcess.CommandLine := ACommandLine;

{$IFDEF MSWINDOWS}
  FProcessThread.Start;
{$ELSE}
  FProcessThread.Resume;
{$ENDIF}
end;

procedure TplProcessThread.Stop;
begin
  if Assigned(FProcessThread) then
  begin
    FCanceled := True;
    FProcessThread.FProcess.Terminate(0);
    FProcessThread.Terminate;
  end;
end;

end.


