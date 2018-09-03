{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplTimerUnit;

interface
uses
  Classes, Forms,
{$IFDEF WINDOWS}
  Windows,
{$ELSE}
  Unix,
{$ENDIF}
  lmessages, SyncObjs;

type

  TplTimerMode = (cmManual, cmASAP, cmApplicationIdle);
  TplTimerTimeReference = (cmRTC, cmPerformanceCounter, cmExternal);

  TplTimerProgressTimes = record
    deltaTime, newTime: Double
  end;

  TplTimerProgressEvent = procedure(Sender: TObject; const deltaTime, newTime: Double) of object;

  IplNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IplProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TplTimerProgressTimes);
  end;


TplTimerProgressComponent = class(TComponent, IplProgessAble)
  public
    procedure DoProgress(const progressTime: TplTimerProgressTimes); virtual;
  end;

TplUpdatedComponent = class(TplTimerProgressComponent, IplNotifyAble)
  public
    procedure NotifyChange(Sender: TObject); virtual;
  end;

TplTimer = class(TComponent)
  private
    FSubscribedCadenceableComponents: TList;
    FTimeMultiplier: Double;
    lastTime, downTime, lastMultiplier: Double;
    FEnabled: Boolean;
    FSleepLength: Integer;
    FMode: TplTimerMode;
    FTimeReference: TplTimerTimeReference;
    FCurrentTime: Double;
    FOriginTime: Double;
    FMaxDeltaTime, FMinDeltaTime, FFixedDeltaTime: Double;
    FOnProgress: TplTimerProgressEvent;
    FProgressing: Integer;
    procedure SetCurrentTime(const Value: Double);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  StoreTimeMultiplier: Boolean;
    procedure SetEnabled(const val: Boolean);
    procedure SetMode(const val: TplTimerMode);
    procedure SetTimeReference(const val: TplTimerTimeReference);
    procedure SetTimeMultiplier(const val: Double);
    function  GetRawReferenceTime: Double;
    procedure RestartASAP;
    procedure Loaded; override;
    procedure OnIdleEvent(Sender: TObject; var Done: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Subscribe(aComponent: TplTimerProgressComponent);
    procedure UnSubscribe(aComponent: TplTimerProgressComponent);
   //Allows to manually trigger a progression.
    procedure Progress;
    //Adjusts CurrentTime if necessary, then returns its value.
    function  GetCurrentTime: Double;
    function  IsBusy: Boolean;
    procedure Reset;
    property  OriginTime: Double read FOriginTime write FOriginTime;
    property  CurrentTime: Double read FCurrentTime write SetCurrentTime;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property TimeReference: TplTimerTimeReference read FTimeReference write SetTimeReference default cmPerformanceCounter;
    property TimeMultiplier: Double read FTimeMultiplier write SetTimeMultiplier stored StoreTimeMultiplier;
    property MaxDeltaTime: Double read FMaxDeltaTime write FMaxDeltaTime;
    property MinDeltaTime: Double read FMinDeltaTime write FMinDeltaTime;
    property FixedDeltaTime: Double read FFixedDeltaTime write FFixedDeltaTime;
    property Mode: TplTimerMode read FMode write SetMode default cmASAP;
    property SleepLength: Integer read FSleepLength write FSleepLength default -1;
    property OnProgress: TplTimerProgressEvent read FOnProgress write FOnProgress;
  end;


TplCustomTimerComponent = class(TplUpdatedComponent)
  private
    FCadencer: TplTimer;
  protected
    procedure SetCadencer(const val: TplTimer);
    property  Cadencer: TplTimer read FCadencer write SetCadencer;
  public
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
  end;


TplTimerComponent = class(TplCustomTimerComponent)
  published
    property Cadencer;
  end;

 procedure QueryPerformanceCounter(var val: Int64);
 function  QueryPerformanceFrequency(var val: Int64): Boolean;

implementation

uses SysUtils;

const
  LM_GLTIMER = LM_INTERFACELAST + 326;
type

  TASAPHandler = class;

  TTimerThread = class(TThread)
  private
    FOwner: TASAPHandler;
    FInterval: Word;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
  end;

  TASAPHandler = class
  private
    FTimerThread: TThread;
    FMutex: TCriticalSection;
  public
    procedure TimerProc;
    procedure Cadence(var Msg: TLMessage); message LM_GLTIMER;
    constructor Create;
    destructor Destroy; override;
  end;

var

  vASAPCadencerList: TList;
  vHandler: TASAPHandler;
  vCounterFrequency: Int64;

procedure RegisterASAPCadencer(aCadencer: TplTimer);
begin
  if aCadencer.Mode = cmASAP then
  begin
    if not Assigned(vASAPCadencerList) then
      vASAPCadencerList := TList.Create;
    if vASAPCadencerList.IndexOf(aCadencer) < 0 then
    begin
      vASAPCadencerList.Add(aCadencer);
      if not Assigned(vHandler) then
        vHandler := TASAPHandler.Create;
    end;
  end
  else if aCadencer.Mode = cmApplicationIdle then
    Application.OnIdle := @aCadencer.OnIdleEvent;
end;

procedure UnRegisterASAPCadencer(aCadencer: TplTimer);
var
  i: Integer;
begin
  if aCadencer.Mode = cmASAP then
  begin
    if Assigned(vASAPCadencerList) then
    begin
      i := vASAPCadencerList.IndexOf(aCadencer);
      if i >= 0 then
        vASAPCadencerList[i] := nil;
    end;
  end
  else if aCadencer.Mode = cmApplicationIdle then
    Application.OnIdle := nil;
end;

//=================== TTimerThread ===============================

constructor TTimerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

procedure TTimerThread.Execute;
var
  lastTick, nextTick, curTick, perfFreq: Int64;
begin
  QueryPerformanceFrequency(perfFreq);
  QueryPerformanceCounter(lastTick);
  nextTick := lastTick + (FInterval * perfFreq) div 1000;
  while not Terminated do
  begin
    FOwner.FMutex.Acquire;
    FOwner.FMutex.Release;
    while not Terminated do
    begin
      QueryPerformanceCounter(lastTick);
      if lastTick >= nextTick then
        break;
      Sleep(1);
    end;
    if not Terminated then
    begin
      // if time elapsed run user-event
      Synchronize(@FOwner.TimerProc);
      QueryPerformanceCounter(curTick);
      nextTick := lastTick + (FInterval * perfFreq) div 1000;
      if nextTick <= curTick then
      begin
        // CPU too slow... delay to avoid monopolizing what's left
        nextTick := curTick + (FInterval * perfFreq) div 1000;
      end;
    end;
  end;
end;

// ================== TASAPHandler ================================

constructor TASAPHandler.Create;
begin
  inherited Create;
  FMutex := TCriticalSection.Create;
  FMutex.Acquire;
  FTimerThread := TTimerThread.Create(False);

  with TTimerThread(FTimerThread) do
  begin
    FOwner := Self;
    FreeOnTerminate := False;
    Priority := tpTimeCritical;
    FInterval := 1;
    FMutex.Release;
  end;

end;

destructor TASAPHandler.Destroy;
begin

  FMutex.Acquire;
//  FTimerThread.Terminate;
  CheckSynchronize;
  // wait & free
  FTimerThread.WaitFor;
  FTimerThread.Free;
  FMutex.Free;

  inherited Destroy;
end;

procedure TASAPHandler.TimerProc;
var
  NewMsg: TLMessage;
begin
  NewMsg.Msg := LM_GLTIMER;
  Cadence(NewMsg);
end;

procedure TASAPHandler.Cadence(var Msg: TLMessage);
var
  i: Integer;
  cad: TplTimer;
begin
  if Assigned(vHandler) and Assigned(vASAPCadencerList)
    and (vASAPCadencerList.Count <> 0) then
    for i := vASAPCadencerList.Count - 1 downto 0 do
    begin
      cad := TplTimer(vASAPCadencerList[i]);
      if Assigned(cad) and (cad.Mode = cmASAP)
        and cad.Enabled and (cad.FProgressing = 0) then
      begin
        if Application.Terminated then
        begin
          // force stop
          cad.Enabled := False;
        end
        else
        begin
          try
            // do stuff
            cad.Progress;
          except
            Application.HandleException(Self);
            // it faulted, stop it
            cad.Enabled := False;
          end
        end;
      end;
    end;
end;

//====================== TplTimer =================================

constructor TplTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeReference := cmPerformanceCounter;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
  FTimeMultiplier := 1;
  FSleepLength := -1;
  Mode := cmASAP;
  Enabled := True;
end;

destructor TplTimer.Destroy;
begin
  Assert(FProgressing = 0);
  UnRegisterASAPCadencer(Self);
  FSubscribedCadenceableComponents.Free;
  FSubscribedCadenceableComponents := nil;
  inherited Destroy;
end;


procedure TplTimer.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if Assigned(FSubscribedCadenceableComponents) then
      FSubscribedCadenceableComponents.Remove(AComponent);
  end;
  inherited;
end;

procedure TplTimer.Loaded;
begin
  inherited Loaded;
  RestartASAP;
end;

procedure TplTimer.OnIdleEvent(Sender: TObject; var Done: Boolean);
begin
  Progress;
  Done := False;
end;

procedure TplTimer.SetEnabled(const val: Boolean);
begin
  if FEnabled <> val then
  begin
    FEnabled := val;
    if not (csDesigning in ComponentState) then
    begin

      if Enabled then
        FOriginTime := FOriginTime + GetRawReferenceTime - downTime
      else
        downTime := GetRawReferenceTime;

       RestartASAP;
    end;
  end;
end;

procedure TplTimer.SetTimeMultiplier(const val: Double);
var
  rawRef: Double;
begin
  if val <> FTimeMultiplier then
  begin
    if val = 0 then
    begin
      lastMultiplier := FTimeMultiplier;
      Enabled := False;
    end
    else
    begin
      rawRef := GetRawReferenceTime;
      if FTimeMultiplier = 0 then
      begin
        Enabled := True;
        FOriginTime := rawRef - (rawRef - FOriginTime) * lastMultiplier / val;
      end
      else
      begin
        FOriginTime := rawRef - (rawRef - FOriginTime) * FTimeMultiplier / val;
      end;
    end;
    FTimeMultiplier := val;
  end;
end;

function TplTimer.StoreTimeMultiplier: Boolean;
begin
  Result := (FTimeMultiplier <> 1);
end;

procedure TplTimer.SetMode(const val: TplTimerMode);
begin
  if FMode <> val then
  begin
    if FMode <> cmManual then
      UnRegisterASAPCadencer(Self);
    FMode := val;
    RestartASAP;
  end;
end;

procedure TplTimer.SetTimeReference(const val: TplTimerTimeReference);
begin
  FTimeReference := val;
end;

procedure TplTimer.RestartASAP;
begin
  if (csLoading in ComponentState) then  exit;
  if (csDesigning in ComponentState) then  exit;

  if (Mode in [cmASAP, cmApplicationIdle])  and Enabled then
      RegisterASAPCadencer(Self)
    else
      UnRegisterASAPCadencer(Self);

end;

procedure TplTimer.Progress;
var
  deltaTime, newTime, totalDelta: Double;
  i: Integer;
  pt: TplTimerProgressTimes;
begin

  if FProgressing < 0 then  Exit;

  if Enabled then
  begin
    if SleepLength >= 0 then  Sleep(SleepLength);

    if Mode = cmASAP then
    begin
      Application.ProcessMessages;

      if (not Assigned(vASAPCadencerList)) or (vASAPCadencerList.IndexOf(Self)<0) then  Exit;
    end;
  end;
  Inc(FProgressing);
  try
    if Enabled then
    begin
      if Enabled then
      begin

        newTime := GetCurrentTime;
        deltaTime := newTime - lastTime;
        if (deltaTime >= MinDeltaTime) and (deltaTime >= FixedDeltaTime) then
        begin
          if FMaxDeltaTime > 0 then
          begin
            if deltaTime > FMaxDeltaTime then
            begin
              FOriginTime := FOriginTime + (deltaTime - FMaxDeltaTime) /
                FTimeMultiplier;
              deltaTime := FMaxDeltaTime;
              newTime := lastTime + deltaTime;
            end;
          end;
          totalDelta := deltaTime;
          if FixedDeltaTime > 0 then  deltaTime := FixedDeltaTime;

          while totalDelta >= deltaTime do
          begin
            lastTime := lastTime + deltaTime;

            //===================================================
           { if Assigned(FOnProgress) and (deltaTime <> 0) then
            begin
              FProgressing := -FProgressing;
              try
               FOnProgress(self, deltaTime, lastTime);
              finally
                FProgressing := -FProgressing;
              end;
            end;    }
            //==================================================
            pt.deltaTime := deltaTime;
            pt.newTime := lastTime;
            i := 0;
            while Assigned(FSubscribedCadenceableComponents) and
              (i <= FSubscribedCadenceableComponents.Count - 1) do
            begin
              TplCustomTimerComponent(FSubscribedCadenceableComponents[i]).DoProgress(pt);
              i := i + 1;
            end;
            if Assigned(FOnProgress) and (not (csDesigning in ComponentState))
              then
              FOnProgress(Self, deltaTime, newTime);
            if deltaTime <= 0 then
              Break;
            totalDelta := totalDelta - deltaTime;
          end;
        end;
      end;
    end;
  finally
    Dec(FProgressing);
  end;
end;


function TplTimer.GetRawReferenceTime: Double;
var
  counter: Int64;
begin
  case FTimeReference of
    cmRTC:
      Result := Now * (3600 * 24);
    cmPerformanceCounter:
      begin
        QueryPerformanceCounter(counter);
        Result := counter / vCounterFrequency;
      end;
    cmExternal:
      Result := FCurrentTime;
  else
    Result := 0;
    Assert(False);
  end;
end;

function TplTimer.GetCurrentTime: Double;
begin
  Result := (GetRawReferenceTime - FOriginTime) * FTimeMultiplier;
  FCurrentTime := Result;
end;

function TplTimer.IsBusy: Boolean;
begin
  Result := (FProgressing <> 0);
end;

procedure TplTimer.Reset;
begin
  lasttime := 0;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
end;

procedure TplTimer.SetCurrentTime(const Value: Double);
begin
  LastTime := Value - (FCurrentTime - LastTime);
  FOriginTime := FOriginTime + (FCurrentTime - Value);
  FCurrentTime := Value;
end;


procedure TplTimer.Subscribe(aComponent: TplTimerProgressComponent);
begin
  if not Assigned(FSubscribedCadenceableComponents) then
    FSubscribedCadenceableComponents := TList.Create;
  if FSubscribedCadenceableComponents.IndexOf(aComponent) < 0 then
  begin
    FSubscribedCadenceableComponents.Add(aComponent);
    aComponent.FreeNotification(Self);
  end;
end;

procedure TplTimer.UnSubscribe(aComponent: TplTimerProgressComponent);
var
  i: Integer;
begin
  if Assigned(FSubscribedCadenceableComponents) then
  begin
    i := FSubscribedCadenceableComponents.IndexOf(aComponent);
    if i >= 0 then
    begin
      FSubscribedCadenceableComponents.Delete(i);
      aComponent.RemoveFreeNotification(Self);
    end;
  end;
end;

// ============================== TplCustomTimerComponent ================================

destructor TplCustomTimerComponent.Destroy;
begin
  Cadencer := nil;
  inherited Destroy;
end;

procedure TplCustomTimerComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FCadencer) then
    Cadencer := nil;
  inherited;
end;

procedure TplCustomTimerComponent.SetCadencer(const val: TplTimer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

procedure TplTimerProgressComponent.DoProgress(const progressTime: TplTimerProgressTimes);
begin
  // nothing
end;

procedure TplUpdatedComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TplUpdatedComponent) then
      (Owner as TplUpdatedComponent).NotifyChange(Self);
end;

//======================================================

{$IFDEF UNIX}
var
  vProgStartSecond: int64;
procedure Init_vProgStartSecond;
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  vProgStartSecond := tz.tv_sec;
end;
{$ENDIF}

function QueryPerformanceFrequency(var val: Int64): Boolean;
{$IFDEF WINDOWS}
begin
  Result := Boolean(Windows.QueryPerformanceFrequency(val));
end;
{$ENDIF}
{$IFDEF UNIX}
begin
  val := 1000000;
  Result := True;
end;
{$ENDIF}


procedure QueryPerformanceCounter(var val: Int64);
{$IFDEF WINDOWS}
begin
  Windows.QueryPerformanceCounter(val);
end;
{$ENDIF}
{$IFDEF UNIX}
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  val := tz.tv_sec - vProgStartSecond;
  val := val * 1000000;
  val := val + tz.tv_usec;
end;
{$ENDIF}

var
  vGLSStartTime : TDateTime;
{$IFDEF WINDOWS}
  vLastTime: TDateTime;
  vDeltaMilliSecond: TDateTime;
{$ENDIF}

function GLSTime: Double;
{$IFDEF WINDOWS}
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := (wHour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
             wMinute * (SecsPerMin * MSecsPerSec) +
             wSecond * MSecsPerSec +
             wMilliSeconds) - vGLSStartTime;
  // Hack to fix time precession
  if Result - vLastTime = 0 then
  begin
    Result := Result + vDeltaMilliSecond;
    vDeltaMilliSecond := vDeltaMilliSecond + 0.1;
  end
  else begin
    vLastTime := Result;
    vDeltaMilliSecond := 0.1;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  Result := tz.tv_sec - vGLSStartTime;
  Result := Result * 1000000;
  Result := Result + tz.tv_usec;
end;
{$ENDIF}


//=====================================================

initialization

  vGLSStartTime := GLSTime;

{$IFDEF UNIX}
  Init_vProgStartSecond;
{$ENDIF}
  RegisterClasses([TplTimer]);
  // Preparation for high resolution timer
  if not QueryPerformanceFrequency(vCounterFrequency) then vCounterFrequency := 0;

finalization
 //if vHandler<>nil then FreeAndNil(vHandler);
 if vASAPCadencerList<>nil then FreeAndNil(vASAPCadencerList);

end.
