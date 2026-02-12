{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                              Project: Optix Neo                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter & wife                                }
{                                                                              }
{******************************************************************************}

unit NeoFlat.Gauge;

interface

uses System.Classes, VCL.Graphics, VCL.Controls, Winapi.Windows, VCL.ExtCtrls;

type
  TFlatGaugeMode = (
                    gmProgressBar,
                    gmMarquee
  );

  TFlatGaugeTextMode = (
                        gtmNone,
                        gtmProgress,
                        gtmCustom
  );

  TFlatMarqueeDirection = (
                          mdLeftToRight,
                          mdRightToLeft
  );

  TFlatGaugeState = (
                      gsNormal,
                      gsError
  );

  TFlatGauge = class(TGraphicControl)
  private
    FBackground       : TColor;
    FBorder           : TColor;
    FForeground       : TColor;
    FBorderWidth      : Integer;

    FMax              : Integer;
    FProgress         : Integer;

    FTextMode         : TFlatGaugeTextMode;
    FText             : String;

    FMode             : TFlatGaugeMode;

    FState            : TFlatGaugeState;

    FMarqueeTimer     : TTimer;
    FMarqueeProgress  : Integer;
    FMarqueeDirection : TFlatMarqueeDirection;

    {@M}
    procedure SetColor(const AIndex : Integer; const AColor : TColor);
    procedure SetInteger(const AIndex, AValue : Integer);

    procedure DrawMarquee(const AClientRect : TRect);
    procedure DrawProgress(const AClientRect : TRect);
    procedure DrawText(const AClientRect : TRect);

    procedure SetMode(const AValue : TFlatGaugeMode);
    procedure SetText(const AValue : String);
    procedure SetTextMode(const AValue : TFlatGaugeTextMode);
    procedure SetState(const AValue : TFlatGaugeState);

    procedure OnTimerMarquee(Sender : TObject);
  protected
    {@M}
    procedure paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Background  : TColor index 0  read FBackground  write SetColor;
    property Border      : TColor index 1  read FBorder      write SetColor;
    property Foreground  : TColor index 2  read FForeground  write SetColor;

    property BorderWidth : Integer index 0 read FBorderWidth write SetInteger;
    property Max         : Integer index 1 read FMax         write SetInteger;
    property Progress    : Integer index 2 read FProgress    write SetInteger;

    property State    : TFlatGaugeState    read FState    write SetState;
    property Mode     : TFlatGaugeMode     read FMode     write SetMode;
    property Text     : String           read FText     write SetText;
    property TextMode : TFlatGaugeTextMode read FTextMode write SetTextMode;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

implementation

uses NeoFlat.Theme, Winapi.GDIPAPI, Winapi.GDIPOBJ, System.SysUtils, System.Math;

{-------------------------------------------------------------------------------
  When Marquee Timer get triggered
-------------------------------------------------------------------------------}
procedure TFlatGauge.OnTimerMarquee(Sender : TObject);
begin
  Inc(FMarqueeProgress);

  if FMarqueeProgress >= 100 then begin
    FMarqueeProgress := 0;

    if FMarqueeDirection = mdLeftToRight then
      FMarqueeDirection := mdRightToLeft
    else
      FMarqueeDirection := mdLeftToRight;
  end;

  ///
  self.Invalidate();
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TFlatGauge.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FBackground       := clBlack;
  FBorder           := MAIN_GRAY;
  FForeground       := MAIN_GRAY;
  FBorderWidth      := 1;
  FMax              := 100;
  FProgress         := 50;

  self.Width        := 300;
  self.Height       := 21;

  Font.Name         := FONT_1;
  Font.Size         := 8;
  Font.Color        := clWhite;

  FText             := '';

  FMode             := gmProgressBar;
  FTextMode         := gtmProgress;
  FState            := gsNormal;

  FMarqueeProgress  := 0;
  FMarqueeDirection := mdLeftToRight;

  self.ShowHint     := True;

  FMarqueeTimer          := TTimer.Create(self);
  FMarqueeTimer.Interval := 10;
  FMarqueeTimer.Enabled  := False;
  FMarqueeTimer.OnTimer  := OnTimerMarquee;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TFlatGauge.Destroy();
begin
  if Assigned(FMarqueeTimer) then
    FreeAndNil(FMarqueeTimer);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Draw Marquee Animation
-------------------------------------------------------------------------------}
procedure TFlatGauge.DrawMarquee(const AClientRect : TRect);
var AGraphics      : TGPGraphics;
    ABrush         : TGPLinearGradientBrush;
    AGPRect        : TGPRect;
    AProgressWidth : Integer;
    AColor1        : LongInt;
    AColor2        : LongInt;

    function GetColor(AColor : TColor) : Longint;
    begin
      result := MakeColor(
                            GetRValue(AColor),
                            GetGValue(AColor),
                            GetBValue(AColor)
      );
    end;

begin
  AGraphics := TGPGraphics.Create(Canvas.Handle);
  try
    AGPRect.X      := AClientRect.Left;
    AGPRect.Y      := AClientRect.Top;
    AGPRect.Width  := AClientRect.Width;
    AGPRect.Height := AClientRect.Height;

    AProgressWidth := ceil((FMarqueeProgress * AClientRect.Width) div 100);

    if FMarqueeDirection = mdLeftToRight then begin
      AColor1 := GetColor(FBackground);
      AColor2 := GetColor(FForeground);
    end else begin
      AColor1 := GetColor(FForeground);
      AColor2 := GetColor(FBackground);
    end;

    AGPRect.Y      := AClientRect.Top;
    AGPRect.Height := AClientRect.Height;

    if FMarqueeDirection = mdLeftToRight then
      AGPRect.X := AClientRect.Left
    else
      AGPRect.X := AClientRect.Right - AProgressWidth;

    AGPRect.Width := AProgressWidth;

    ABrush := TGPLinearGradientBrush.Create(
                                              AGPRect,
                                              AColor1,
                                              AColor2,
                                              LinearGradientModeHorizontal
    );
    try
      AGraphics.FillRectangle(
                                ABrush,
                                AGPRect
      );
    finally
      if Assigned(ABrush) then
        FreeAndNil(ABrush);
    end;
  finally
    if Assigned(AGraphics) then
      FreeAndNil(AGraphics);
  end;
end;

{-------------------------------------------------------------------------------
  Draw Progress Bar
-------------------------------------------------------------------------------}
procedure TFlatGauge.DrawProgress(const AClientRect : TRect);
var ARect  : TRect;
    AWidth : Integer;
begin
  Canvas.Brush.Color := FForeground;

  ARect.Left  := AClientRect.Left;
  ARect.Top   := AClientRect.Top;

  AWidth := ceil((FProgress * AClientRect.Width) div 100);

  if AWidth > AClientRect.Width then
    AWidth := AClientRect.Width;

  ARect.Height := AClientRect.Height;
  ARect.Width  := AWidth;

  Canvas.FillRect(ARect);
end;

{-------------------------------------------------------------------------------
  Draw Text
-------------------------------------------------------------------------------}
procedure TFlatGauge.DrawText(const AClientRect : TRect);
var AText       : String;
    ATextFormat : TTextFormat;
    ATextRect   : TRect;
begin
  if (FTextMode = gtmNone) then
    Exit();
  ///

  case FTextMode of
    gtmProgress : AText := Format('%d%%', [FProgress]);
    gtmCustom   : AText := FText;
  end;

  ATextRect := AClientRect;

  Canvas.Brush.Style := bsClear;

  ATextFormat := [
                    tfCenter,
                    tfEndEllipsis,
                    tfSingleLine,
                    tfVerticalCenter
  ];

  Canvas.TextRect(ATextRect, AText, ATextFormat);
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TFlatGauge.paint();
var ARect       : TRect;
    AClientRect : TRect;
    ABorder     : TColor;
    AFont       : TFont;
begin
  Canvas.Lock();
  try
    AFont := TFont.Create();
    try
      AFont.Assign(Font);
      ///

      AClientRect.Left   := 0;
      AClientRect.Top    := 0;
      AClientRect.Width  := ClientWidth;
      AClientRect.Height := ClientHeight;

      Canvas.Brush.Style := bsSolid;

      case FState of
        gsNormal : begin
          ABorder := FBorder;
        end;

        gsError : begin
          ABorder     := MAIN_RED;
          AFont.Color := ABorder;
        end;
      end;

      Canvas.Font.Assign(AFont);

      {
        Draw Bg
      }
      if FBackground <> clNone then begin
        Canvas.Brush.Color := FBackground;

        Canvas.FillRect(AClientRect);
      end;

      {
        Draw Border
      }
      if (FBorderWidth > 0) and (ABorder <> clNone) then begin
        Canvas.Brush.Color := ABorder;
        ///

        ARect.Left   := 0;
        ARect.Top    := 0;
        ARect.Width  := ClientWidth;
        ARect.Height := FBorderWidth;

        Canvas.FillRect(ARect);

        ARect.Left   := (ClientWidth - FBorderWidth);
        ARect.Top    := 0;
        ARect.Width  := FBorderWidth;
        ARect.Height := ClientHeight;

        Canvas.FillRect(ARect);

        ARect.Left   := 0;
        ARect.Top    := (ClientHeight - FBorderWidth);
        ARect.Width  := ClientWidth;
        ARect.Height := FBorderWidth;

        Canvas.FillRect(ARect);

        ARect.Left   := 0;
        ARect.Top    := 0;
        ARect.Width  := FBorderWidth;
        ARect.Height := ClientHeight;

        Canvas.FillRect(ARect);

        ///
        InflateRect(AClientRect, -FBorderWidth, -FBorderWidth);
      end;


      {
        Draw Marquee or Progress
      }
      case FMode of
        gmProgressBar : self.DrawProgress(AClientRect);
        gmMarquee     : self.DrawMarquee(AClientRect);
      end;

      {
        Draw Text
      }
      if not ((FMode = gmMarquee) and (FTextMode = gtmProgress)) then
        self.DrawText(AClientRect);
    finally
      if Assigned(AFont) then
        FreeAndNil(AFont);
    end;
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TFlatGauge.SetColor(const AIndex : Integer; const AColor : TColor);
begin
  case AIndex of
    0 : FBackground := AColor;
    1 : FBorder     := AColor;
    2 : FForeground := AColor;
  end;

  ///
  self.Invalidate();
end;

procedure TFlatGauge.SetInteger(const AIndex, AValue : Integer);
begin
  case AIndex of
    0 : FBorderWidth := AValue;
    1 : FMax         := AValue;
    2 : FProgress    := AValue;
  end;

  ///
  self.Invalidate();
end;

procedure TFlatGauge.SetMode(const AValue : TFlatGaugeMode);
begin
  if FMode = AValue then
    Exit();

  FMode := AValue;

  FMarqueeTimer.Enabled := (FMode = gmMarquee);

  if FMarqueeTimer.Enabled then begin
    FMarqueeProgress  := 0;
    FMarqueeDirection := mdLeftToRight;
  end;

  ///
  self.Invalidate();
end;

procedure TFlatGauge.SetText(const AValue : String);
begin
  if AValue = FText then
    Exit();

  FText := AValue;

  ///
  self.Invalidate();
end;

procedure TFlatGauge.SetTextMode(const AValue : TFlatGaugeTextMode);
begin
  if AValue = FTextMode then
    Exit();

  FTextMode := AValue;

  ///
  self.Invalidate();
end;

procedure TFlatGauge.SetState(const AValue : TFlatGaugeState);
begin
  if AValue = FState then
    Exit();

  FState := AValue;

  ///
  self.Invalidate();
end;

end.
