
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplNotifierOSUnit;

{$I ExSystem.inc}

interface

uses
{$IFDEF UNIX}
  gtk2, Gtk2Proc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  win32proc,
{$ENDIF}
  ExSystemConsts, ExSystemUtils, Classes, Forms, StdCtrls, ExtCtrls, LCLType, LCLIntf,
  LMessages, Graphics, GraphUtil;

const
{$J+}
  CAutoColorsDefaultGain = 200;
  CHeight = 68;
  CWidth = 280;
  CObjectsLeft = 16;
  CImageHeightWidth = 32;
  CDefaultPause = CLSDefaultHintPause;
  CLeftGain = {$IFDEF UNIX}10{$ELSE}10{$ENDIF};
  CTopGain = {$IFDEF UNIX}10{$ELSE}10{$ENDIF};
{$J-}

type

  { TplNotifyType }

  TplNotifyType = (ntRound, ntRetangle);

  { TplNotifyPosition }

  TplNotifyPosition = (npTopLeft, npTopRight, npBottomRight, npBottomLeft);

  { TplNotifierOS }

  TplNotifierOS = class(THintWindow)
  private
    FCaptionLabel: TLabel;
    FMsgLabel: TLabel;
    FCloseTimer: TTimer;
    FShowTimer: TTimer;
    FImage: TImage;
    FBitmap: TBitmap;
    FIsRetangle: Boolean;
    FIsHintStyle: Boolean;
    procedure DoOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CloseTimerOnTimer(Sender: TObject);
    procedure ShowTimerOnTimer(Sender: TObject);
  protected
    procedure CMMouseEnter(var Message: TLMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MouseLeave;
  public
    constructor Create(const ACaption, AMsg: string; const AGraphic: TGraphic;
      const APause: Integer = CDefaultPause; const AColor: TColor = $00222222;
      const AAutoColorsGain: Integer = CAutoColorsDefaultGain;
      const ACaptionColor: TColor = $FFFBF0;
      const ALSNotifyType: TplNotifyType = ntRound;
      const AOnClick: TNotifyEvent = nil); reintroduce; overload;
    destructor Destroy; override;
    procedure Paint; override;
    class procedure Execute(
      const ACaption, AMsg: string; const AGraphic: TGraphic;
      const ALSNotifyPosition: TplNotifyPosition = npTopRight;
      const APause: Integer = CDefaultPause;
      const ALSNotifyType: TplNotifyType = ntRound;
      const AColor: TColor = $00222222;
      const AAutoColorsGain: Integer = CAutoColorsDefaultGain;
      const ACaptionColor: TColor = $FFFBF0;
      const ALeftGain: Integer = CLeftGain; const ATopGain: Integer = CTopGain;
      const AOnClick: TNotifyEvent = nil);
    class procedure CloseHint;
    property Bitmap: TBitmap read FBitmap write FBitmap;
    property Image: TImage read FImage write FImage;
    property CaptionLabel: TLabel read FCaptionLabel write FCaptionLabel;
    property MsgLabel: TLabel read FMsgLabel write FMsgLabel;
  end;

implementation

var
  _LSNotifierOS: TplNotifierOS = nil;

{ TplNotifierOS }

constructor TplNotifierOS.Create(const ACaption, AMsg: string;
  const AGraphic: TGraphic; const APause: Integer; const AColor: TColor;
  const AAutoColorsGain: Integer; const ACaptionColor: TColor;
  const ALSNotifyType: TplNotifyType; const AOnClick: TNotifyEvent);

  procedure _DrawHorzLines(const ATop: Integer);
  begin
    FBitmap.Canvas.Line(0, ATop, 3, ATop);
    FBitmap.Canvas.Line(ClientWidth, ATop, ClientWidth - 4, ATop);
  end;

  procedure _DrawVertLines(const ALeft: Integer);
  begin
    FBitmap.Canvas.Line(ALeft, 0, ALeft, 3);
    FBitmap.Canvas.Line(ALeft, ClientHeight, ALeft, ClientHeight - 4);
  end;

begin
  inherited Create(nil);
  OnClick := AOnClick;
  if Assigned(AOnClick) then
    FCompStyle := csHintWindow + csForm;
  FIsHintStyle := APause = -1;
  Color := AColor;
  AutoHide := False;
  Height := CHeight;
  Width := CWidth;
  OnClose := @DoOnClose;
  { CaptionLabel }
  FCaptionLabel := TLabel.Create(nil);
  FCaptionLabel.Parent := Self;
  FCaptionLabel.AutoSize := True;
  FCaptionLabel.Caption := ACaption;
  FCaptionLabel.Font.Style := [fsBold];
  if AAutoColorsGain > -1 then
    FCaptionLabel.Font.Color := GetShadowColor(Color, AAutoColorsGain)
  else
    FCaptionLabel.Font.Color := ACaptionColor;
  FCaptionLabel.Left := CObjectsLeft + CImageHeightWidth + 14;
  FCaptionLabel.Top := 13;
  FCaptionLabel.OnClick := AOnClick;
  { MsgLabel }
  FMsgLabel := TLabel.Create(nil);
  FMsgLabel.Parent := Self;
  FMsgLabel.AutoSize := False;
  FMsgLabel.Caption := AMsg;
  FMsgLabel.Font.Color := FCaptionLabel.Font.Color;
  FMsgLabel.Font.Size := 8;
  FMsgLabel.Left := CObjectsLeft + CImageHeightWidth + 14;
  FMsgLabel.Top := FCaptionLabel.Top + FMsgLabel.Height + 4;
  FMsgLabel.Height := 40;
  FMsgLabel.Width := 215;
  FMsgLabel.WordWrap := True;
  FMsgLabel.OnClick := AOnClick;
  { CloseTimer }
  FCloseTimer := TTimer.Create(nil);
  if FIsHintStyle then
    FCloseTimer.Interval := CDefaultPause
  else
    FCloseTimer.Interval := APause;
  FCloseTimer.OnTimer := @CloseTimerOnTimer;
  { ShowTimer }
  FShowTimer := TTimer.Create(nil);
  FShowTimer.Interval := 200;
  FShowTimer.OnTimer := @ShowTimerOnTimer;
  if ALSNotifyType = ntRound then
  begin
    FIsRetangle := False;
    FBitmap := TBitmap.Create;
    FBitmap.SetSize(ClientWidth, ClientHeight);
    FBitmap.Canvas.Pen.Color := clFuchsia;
    _DrawHorzLines(0);
    _DrawHorzLines(ClientHeight - 1);
    _DrawVertLines(0);
    _DrawVertLines(ClientWidth - 1);
    FBitmap.Mask(clFuchsia);
{$IFDEF UNIX}
{$HINTS OFF}
    gtk_widget_shape_combine_mask(PGtkWidget(Handle),
      CreateGdkMaskBitmap(FBitmap.BitmapHandle, FBitmap.MaskHandle), 0, 0);
{$HINTS ON}
{$ENDIF}
{$IFDEF MSWINDOWS}
    SetWindowRgn(Handle, BitmapToRegion(FBitmap.Handle,
      FBitmap.Canvas.Pixels[0, 0]), True);
{$ENDIF}
  end
  else
    FIsRetangle := True;
  { Image }
  FImage := TImage.Create(nil);
  FImage.Parent := Self;
  if Assigned(AGraphic) then
    FImage.Picture.Graphic := AGraphic
  else
    FImage.Picture.Icon.Handle := Application.BigIconHandle;
  FImage.AutoSize := True;
  FImage.Left := CObjectsLeft;
  FImage.Top := (ClientHeight div 2) - (CImageHeightWidth div 2);
  FImage.OnClick := AOnClick;
end;

destructor TplNotifierOS.Destroy;
begin
  if Assigned(FBitmap) then
    FBitmap.Free;
  FMsgLabel.Free;
  FCaptionLabel.Free;
  FImage.Free;
  FCloseTimer.Free;
  FShowTimer.Free;
  inherited Destroy;
end;

procedure TplNotifierOS.Paint;
begin
  if FIsRetangle then
    inherited Paint;
end;

class procedure TplNotifierOS.Execute(const ACaption, AMsg: string;
  const AGraphic: TGraphic; const ALSNotifyPosition: TplNotifyPosition;
  const APause: Integer; const ALSNotifyType: TplNotifyType;
  const AColor: TColor; const AAutoColorsGain: Integer;
  const ACaptionColor: TColor; const ALeftGain: Integer;
  const ATopGain: Integer; const AOnClick: TNotifyEvent);
var
  VRect: TRect;
begin
  if Assigned(_LSNotifierOS) then
    _LSNotifierOS.Close;
  case ALSNotifyPosition of
    npTopLeft:
    begin
      VRect.Left := LSGetWorkAreaRect.Left + ALeftGain;
      VRect.Top := LSGetWorkAreaRect.Top + ATopGain;
    end;
    npTopRight:
    begin
      VRect.Left := (LSGetWorkAreaRect.Right - CWidth) - ALeftGain;
      VRect.Top := LSGetWorkAreaRect.Top + ATopGain;
    end;
    npBottomRight:
    begin
      VRect.Left := (LSGetWorkAreaRect.Right - CWidth) - ALeftGain;
      VRect.Top := (LSGetWorkAreaRect.Bottom - CHeight) - ATopGain;
    end;
    npBottomLeft:
    begin
      VRect.Left := LSGetWorkAreaRect.Left + ALeftGain;
      VRect.Top := (LSGetWorkAreaRect.Bottom - CHeight) - ATopGain;
    end;
  end;
  VRect.Right := VRect.Left + CWidth;
  VRect.Bottom := VRect.Top + CHeight;
  _LSNotifierOS := TplNotifierOS.Create(ACaption, AMsg, AGraphic, APause,
    AColor, AAutoColorsGain, ACaptionColor, ALSNotifyType, AOnClick);
  _LSNotifierOS.ActivateHint(VRect, '');
end;

class procedure TplNotifierOS.CloseHint;
begin
  _LSNotifierOS.Close;
end;

procedure TplNotifierOS.DoOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ReleaseHandle;
  CloseAction := caFree;
  _LSNotifierOS := nil;
end;

procedure TplNotifierOS.CloseTimerOnTimer(Sender: TObject);
begin
  Close;
end;

procedure TplNotifierOS.ShowTimerOnTimer(Sender: TObject);
var
  VMousePoint: TPoint;
  VFormRect: TRect;
begin
  VMousePoint.X := 0;
  VMousePoint.Y := 0;
  if not FIsHintStyle then
  begin
    GetCursorPos(VMousePoint);
    VFormRect.Left := ClientOrigin.X;
    VFormRect.Top := ClientOrigin.Y;
    VFormRect.Right := ClientOrigin.X + ClientWidth;
    VFormRect.Bottom := ClientOrigin.Y + ClientHeight;
    if PtInRect(VFormRect, VMousePoint) then
      Visible := False
    else
      Visible := True;
  end;
end;

procedure TplNotifierOS.CMMouseEnter(var Message: TLMessage);
begin
  inherited;
  if FIsHintStyle then
    FCloseTimer.Enabled := False;
end;

procedure TplNotifierOS.CMMouseLeave(var Message: TLMessage);
begin
  inherited;
  if FIsHintStyle then
    FCloseTimer.Enabled := True;
end;

initialization

finalization
  if Assigned(_LSNotifierOS) then
    _LSNotifierOS.Close;

end.

