
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplAppEventsUnit;

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Controls, Forms, ActnList,
  Contnrs, StdActns;

type

  TplAppEventsCustom = class(TComponent)
  private
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnException: TExceptionEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnModalBegin: TNotifyEvent;
    FOnModalEnd: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    procedure DoActionExecute(Action: TBasicAction; var Handled: boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: boolean);
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: boolean);
    function  DoHelp(Command: word; Data: PtrInt; var CallHelp: boolean): boolean;
    procedure DoHint(Sender: TObject);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    procedure DoModalBegin(Sender: TObject);
    procedure DoModalEnd(Sender: TObject);
    procedure DoOnDestroy(Sender: TObject);
  protected
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnModalBegin: TNotifyEvent read FOnModalBegin write FOnModalBegin;
    property OnModalEnd: TNotifyEvent read FOnModalEnd write FOnModalEnd;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Activate;
    procedure CancelDispatch;
  end;


  TplAppEvents = class(TplAppEventsCustom)
  published
    property OnActionExecute;
    property OnActionUpdate;
    property OnActivate;
    property OnDeactivate;
    property OnException;
    property OnIdle;
    property OnHelp;
    property OnHint;
    property OnMinimize;
    property OnModalBegin;
    property OnModalEnd;
    property OnRestore;
    property OnDestroy;
  end;

implementation

type

  TMultiCaster = class(TComponent)
  private
    FAppEvents: TComponentList;
    FCacheAppEvent: TplAppEventsCustom;
    FCancelDispatching: boolean;
    FDispatching: integer;
    procedure BeginDispatch;
    procedure EndDispatch;
    procedure DoActionExecute(Action: TBasicAction; var Handled: boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: boolean);
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: boolean);
    function DoHelp(Command: word; Data: PtrInt; var CallHelp: boolean): boolean;
    procedure DoHint(Sender: TObject);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    procedure DoModalBegin(Sender: TObject);
    procedure DoModalEnd(Sender: TObject);
    procedure DoOnDestroy(Sender: TObject);
    function GetCount: integer;
    function GetAppEvents(Index: integer): TplAppEventsCustom;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate(AppEvent: TplAppEventsCustom);
    procedure AddAppEvent(AppEvent: TplAppEventsCustom);
    procedure CancelDispatch;
    function CheckDispatching(AppEvents: TplAppEventsCustom): boolean;

    property AppEvents[Index: integer]: TplAppEventsCustom read GetAppEvents; default;
    property Count: integer read GetCount;
  end;

var
  MultiCaster: TMultiCaster = nil;

//============ TplAppEventsCustom ==============================

procedure TplAppEventsCustom.Activate;
begin
  if Assigned(MultiCaster) then
    MultiCaster.Activate(Self);
end;

procedure TplAppEventsCustom.CancelDispatch;
begin
  if Assigned(MultiCaster) then
    MultiCaster.CancelDispatch;
end;

constructor TplAppEventsCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(MultiCaster) then
    MultiCaster.AddAppEvent(Self);
end;

procedure TplAppEventsCustom.DoActionExecute(Action: TBasicAction; var Handled: boolean);
begin
  if Assigned(FOnActionExecute) then
    FOnActionExecute(Action, Handled);
end;

procedure TplAppEventsCustom.DoActionUpdate(Action: TBasicAction; var Handled: boolean);
begin
  if Assigned(FOnActionUpdate) then
    FOnActionUpdate(Action, Handled);
end;

procedure TplAppEventsCustom.DoActivate(Sender: TObject);
begin
  if Assigned(FOnActivate) then
    FOnActivate(Sender);
end;

procedure TplAppEventsCustom.DoDeactivate(Sender: TObject);
begin
  if Assigned(FOnDeactivate) then
    FOnDeactivate(Sender);
end;

procedure TplAppEventsCustom.DoException(Sender: TObject; E: Exception);
begin
  if not (E is EAbort) and Assigned(FOnException) then
    FOnException(Sender, E);
end;

function TplAppEventsCustom.DoHelp(Command: word; Data: PtrInt; var CallHelp: boolean): boolean;
begin
  if Assigned(FOnHelp) then
    Result := FOnHelp(Command, Data, CallHelp)
  else
    Result := False;
end;

procedure TplAppEventsCustom.DoHint(Sender: TObject);
begin
  if Assigned(FOnHint) then
    FOnHint(Sender)
  else
    with THintAction.Create(Self) do
      try
        Hint := Application.Hint;
        Execute;
      finally
        Free;
      end;
end;

procedure TplAppEventsCustom.DoIdle(Sender: TObject; var Done: boolean);
begin
  if Assigned(FOnIdle) then
    FOnIdle(Sender, Done);
end;

procedure TplAppEventsCustom.DoMinimize(Sender: TObject);
begin
  if Assigned(FOnMinimize) then
    FOnMinimize(Sender);
end;

procedure TplAppEventsCustom.DoRestore(Sender: TObject);
begin
  if Assigned(FOnRestore) then
    FOnRestore(Sender);
end;

procedure TplAppEventsCustom.DoModalBegin(Sender: TObject);
begin
  if Assigned(FOnModalBegin) then
    FOnModalBegin(Sender);
end;

procedure TplAppEventsCustom.DoModalEnd(Sender: TObject);
begin
  if Assigned(FOnModalEnd) then
    FOnModalEnd(Sender);
end;

procedure TplAppEventsCustom.DoOnDestroy(Sender: TObject);
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Sender);
end;

//=========================== TMultiCaster ======================================

procedure TMultiCaster.Activate(AppEvent: TplAppEventsCustom);
begin
  if CheckDispatching(AppEvent) and (FAppEvents.IndexOf(AppEvent) < FAppEvents.Count - 1) then
  begin
    FAppEvents.Remove(AppEvent);
    FAppEvents.Add(AppEvent);
  end;
end;

procedure TMultiCaster.AddAppEvent(AppEvent: TplAppEventsCustom);
begin
  if FAppEvents.IndexOf(AppEvent) = -1 then
    FAppEvents.Add(AppEvent);
end;

procedure TMultiCaster.BeginDispatch;
begin
  Inc(FDispatching);
end;

procedure TMultiCaster.CancelDispatch;
begin
  FCancelDispatching := True;
end;

function TMultiCaster.CheckDispatching(AppEvents: TplAppEventsCustom): boolean;
begin
  Result := FDispatching = 0;
  if not Result then
    FCacheAppEvent := AppEvents;
end;

constructor TMultiCaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppEvents := TComponentList.Create(False);
  with Application do
  begin
    OnActionExecute := @DoActionExecute;
    OnActionUpdate := @DoActionUpdate;
    OnActivate := @DoActivate;
    OnDeactivate := @DoDeactivate;
    OnException := @DoException;
    OnHelp :=@DoHelp;
    OnHint := @DoHint;
    OnIdle := @DoIdle;
    OnMinimize := @DoMinimize;
    OnRestore := @DoRestore;
    OnModalBegin := @DoModalBegin;
    OnModalEnd := @DoModalEnd;
    OnDestroy := @DoOnDestroy;
  end;
end;

destructor TMultiCaster.Destroy;
begin
  MultiCaster := nil;
  with Application do
  begin
    OnActionExecute := nil;
    OnActionUpdate := nil;
    OnActivate := nil;
    OnDeactivate := nil;
    OnException := nil;
    OnHelp := nil;
    OnHint := nil;
    OnIdle := nil;
    OnMinimize := nil;
    OnRestore := nil;
    OnShowHint := nil;
    OnShortCut := nil;
    OnModalBegin := nil;
    OnModalEnd := nil;
    OnDestroy := nil;
  end;
  FAppEvents.Free;
  inherited Destroy;
end;

procedure TMultiCaster.DoActionExecute(Action: TBasicAction; var Handled: boolean);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoActionExecute(Action, Handled);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoActionUpdate(Action: TBasicAction; var Handled: boolean);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoActionUpdate(Action, Handled);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoActivate(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoActivate(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoDeactivate(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoDeactivate(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoException(Sender: TObject; E: Exception);
var
  I: integer;
  FExceptionHandled: boolean;
begin
  BeginDispatch;
  FExceptionHandled := False;
  try
    for I := Count - 1 downto 0 do
    begin
      if Assigned(AppEvents[I].OnException) then
      begin
        FExceptionHandled := True;
        AppEvents[I].DoException(Sender, E);
        if FCancelDispatching then
          Break;
      end;
    end;
  finally
    if not FExceptionHandled then
      if not (E is EAbort) then
        Application.ShowException(E);
    EndDispatch;
  end;
end;

function TMultiCaster.DoHelp(Command: word; Data: PtrInt; var CallHelp: boolean): boolean;
var
  I: integer;
begin
  BeginDispatch;
  try
    Result := False;
    for I := Count - 1 downto 0 do
    begin
      Result := Result or AppEvents[I].DoHelp(Command, Data, CallHelp);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoHint(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoHint(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoIdle(Sender: TObject; var Done: boolean);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoIdle(Sender, Done);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoMinimize(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoMinimize(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoRestore(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoRestore(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.EndDispatch;
begin
  if FDispatching > 0 then
  begin
    Dec(FDispatching);
    FCancelDispatching := False;
    if (FDispatching = 0) and (FCacheAppEvent <> nil) and (FAppEvents.IndexOf(FCacheAppEvent) < FAppEvents.Count - 1) then
    begin
      FAppEvents.Remove(FCacheAppEvent);
      FAppEvents.Add(FCacheAppEvent);
      FCacheAppEvent := nil;
    end;
  end;
end;

function TMultiCaster.GetAppEvents(Index: integer): TplAppEventsCustom;
begin
  Result := TplAppEventsCustom(FAppEvents[Index]);
end;

function TMultiCaster.GetCount: integer;
begin
  Result := FAppEvents.Count;
end;

procedure TMultiCaster.DoModalBegin(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoModalBegin(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoModalEnd(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoModalEnd(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoOnDestroy(Sender: TObject);
var
  I: integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoOnDestroy(Sender);
      if FCancelDispatching then
        Break;
    end;
  finally
    EndDispatch;
  end;
end;

initialization
  GroupDescendentsWith(TplAppEventsCustom, Controls.TControl);
  MultiCaster := TMultiCaster.Create(Application);
end.
