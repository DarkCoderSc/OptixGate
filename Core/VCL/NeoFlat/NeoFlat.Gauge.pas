{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                             Project: Optix Gate                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://bsky.app/profile/darkcodersc.bsky.social           }
{                   https://github.com/darkcodersc                             }
{                   License: (!) CHECK README.md (!)                           }
{                                                                              }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{******************************************************************************}

unit NeoFlat.Gauge;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows, Winapi.Messages,

  VCL.Graphics, VCL.Controls, VCL.ExtCtrls;
// ---------------------------------------------------------------------------------------------------------------------

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
    procedure DrawText(AClientRect : TRect);

    procedure SetMode(const AValue : TFlatGaugeMode);
    procedure SetTextMode(const AValue : TFlatGaugeTextMode);
    procedure SetState(const AValue : TFlatGaugeState);

    procedure OnTimerMarquee(Sender : TObject);
  protected
    {@M}
    procedure Paint; override;

    procedure CMTextChanged(var AMessage: TMessage); message CM_TEXTCHANGED;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    property Align;
    property AlignWithMargins;
    property Caption;
    property Margins;
    property Visible;
    property Enabled;
    property Font;

    {@G/S}
    property Background  : TColor  index 0 read FBackground  write SetColor;
    property Border      : TColor  index 1 read FBorder      write SetColor;
    property Foreground  : TColor  index 2 read FForeground  write SetColor;
    property BorderWidth : Integer index 0 read FBorderWidth write SetInteger;
    property Max         : Integer index 1 read FMax         write SetInteger;
    property Progress    : Integer index 2 read FProgress    write SetInteger;

    property State    : TFlatGaugeState    read FState    write SetState;
    property Mode     : TFlatGaugeMode     read FMode     write SetMode;
    property TextMode : TFlatGaugeTextMode read FTextMode write SetTextMode;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Math, System.Types,

  WinApi.GDIPAPI, WinApi.GDIPOBJ,

  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

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
  Invalidate;
end;

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

  Width             := ScaleValue(300);
  Height            := ScaleValue(21);

  Font.Name         := FONT_1;
  Font.Height       := -11;
  Font.Color        := clWhite;

  Caption           := '';

  FMode             := gmProgressBar;
  FTextMode         := gtmProgress;
  FState            := gsNormal;

  FMarqueeProgress  := 0;
  FMarqueeDirection := mdLeftToRight;

  ShowHint := True;

  FMarqueeTimer          := TTimer.Create(self);
  FMarqueeTimer.Interval := 10;
  FMarqueeTimer.Enabled  := False;
  FMarqueeTimer.OnTimer  := OnTimerMarquee;
end;

destructor TFlatGauge.Destroy();
begin
  if Assigned(FMarqueeTimer) then
    FreeAndNil(FMarqueeTimer);

  ///
  inherited Destroy();
end;

procedure TFlatGauge.DrawMarquee(const AClientRect : TRect);

  function GetColor(const AColor : TColor) : Longint;
  begin
    result := MakeColor(GetRValue(AColor), GetGValue(AColor), GetBValue(AColor));
  end;

begin
  var AGraphics := TGPGraphics.Create(Canvas.Handle);
  try
    var AGPRect : TGPRect;

    AGPRect.X      := AClientRect.Left;
    AGPRect.Y      := AClientRect.Top;
    AGPRect.Width  := AClientRect.Width;
    AGPRect.Height := AClientRect.Height;

    var AProgressWidth := ceil((FMarqueeProgress * AClientRect.Width) div 100);

    var AColor1, AColor2 : LongInt;
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

    var ABrush := TGPLinearGradientBrush.Create(AGPRect, AColor1, AColor2, LinearGradientModeHorizontal);
    try
      AGraphics.FillRectangle(ABrush, AGPRect);
    finally
      if Assigned(ABrush) then
        FreeAndNil(ABrush);
    end;
  finally
    if Assigned(AGraphics) then
      FreeAndNil(AGraphics);
  end;
end;

procedure TFlatGauge.DrawProgress(const AClientRect : TRect);
begin
  Canvas.Brush.Color := FForeground;

  var AWidth := ceil((FProgress * AClientRect.Width) div 100);

  if AWidth > AClientRect.Width then
    AWidth := AClientRect.Width;

  var ARect  := TRect.Empty;

  ARect.Left   := AClientRect.Left;
  ARect.Top    := AClientRect.Top;
  ARect.Height := AClientRect.Height;
  ARect.Width  := AWidth;

  Canvas.FillRect(ARect);
end;

procedure TFlatGauge.DrawText(AClientRect : TRect);
begin
  if (FTextMode = gtmNone) then
    Exit;
  ///

  var AText := '';
  case FTextMode of
    gtmProgress : AText := Format('%d%%', [FProgress]);
    gtmCustom   : AText := Caption;
  end;

  Canvas.Brush.Style := bsClear;

  Canvas.TextRect(AClientRect, AText, [tfCenter, tfEndEllipsis, tfSingleLine, tfVerticalCenter]);
end;

procedure TFlatGauge.Paint;
begin
  inherited;
  ///

  Canvas.Lock();
  try
    var AFont := TFont.Create();
    try
      AFont.Assign(Font);
      ///

      var AClientRect    := TRect.Empty;
      AClientRect.Width  := ClientWidth;
      AClientRect.Height := ClientHeight;

      Canvas.Brush.Style := bsSolid;

      var ABorder := clNone;

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

      // Background
      if FBackground <> clNone then begin
        Canvas.Brush.Color := FBackground;

        Canvas.FillRect(AClientRect);
      end;

      // Border
      if (FBorderWidth > 0) and (ABorder <> clNone) then begin
        Canvas.Brush.Color := ABorder;
        ///

        var ARect    := TRect.Empty;
        ARect.Width  := ClientWidth;
        ARect.Height := FBorderWidth;

        Canvas.FillRect(ARect);

        ARect        := TRect.Empty;
        ARect.Left   := (ClientWidth - FBorderWidth);
        ARect.Width  := FBorderWidth;
        ARect.Height := ClientHeight;

        Canvas.FillRect(ARect);

        ARect        := TRect.Empty;
        ARect.Top    := (ClientHeight - FBorderWidth);
        ARect.Width  := ClientWidth;
        ARect.Height := FBorderWidth;

        Canvas.FillRect(ARect);

        ARect        := TRect.Empty;
        ARect.Width  := FBorderWidth;
        ARect.Height := ClientHeight;

        Canvas.FillRect(ARect);

        ///
        InflateRect(AClientRect, -FBorderWidth, -FBorderWidth);
      end;

      // Draw Progress / Marquee
      case FMode of
        gmProgressBar : DrawProgress(AClientRect);
        gmMarquee     : DrawMarquee(AClientRect);
      end;

      // Draw Caption
      if not ((FMode = gmMarquee) and (FTextMode = gtmProgress)) then
        DrawText(AClientRect);
    finally
      if Assigned(AFont) then
        FreeAndNil(AFont);
    end;
  finally
    Canvas.Unlock();
  end;
end;

procedure TFlatGauge.CMTextChanged(var AMessage: TMessage);
begin
  inherited;

  ///
  Invalidate;
end;

procedure TFlatGauge.SetColor(const AIndex : Integer; const AColor : TColor);
begin
  case AIndex of
    0 : FBackground := AColor;
    1 : FBorder     := AColor;
    2 : FForeground := AColor;
  end;

  ///
  Invalidate;
end;

procedure TFlatGauge.SetInteger(const AIndex, AValue : Integer);
begin
  case AIndex of
    0 : FBorderWidth := AValue;
    1 : FMax         := AValue;
    2 : FProgress    := AValue;
  end;

  ///
  Invalidate;
end;

procedure TFlatGauge.SetMode(const AValue : TFlatGaugeMode);
begin
  if FMode = AValue then
    Exit;
  ///

  FMode := AValue;

  FMarqueeTimer.Enabled := (FMode = gmMarquee);

  if FMarqueeTimer.Enabled then begin
    FMarqueeProgress  := 0;
    FMarqueeDirection := mdLeftToRight;
  end;

  ///
  Invalidate;
end;

procedure TFlatGauge.SetTextMode(const AValue : TFlatGaugeTextMode);
begin
  if AValue = FTextMode then
    Exit;
  ///

  FTextMode := AValue;

  ///
  Invalidate;
end;

procedure TFlatGauge.SetState(const AValue : TFlatGaugeState);
begin
  if AValue = FState then
    Exit;
  ///

  FState := AValue;

  ///
  Invalidate;
end;

end.
