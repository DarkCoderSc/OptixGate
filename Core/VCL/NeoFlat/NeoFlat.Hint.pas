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

unit NeoFlat.Hint;

interface

uses System.Classes, VCL.Controls, Winapi.Windows, VCl.Graphics, NeoFlat.Helper, NeoFlat.Types;

type
  TArrowPos = (apBottomRight, apBottomLeft, apTopRight, apTopLeft);

  THintInfo = record
    HintShow       : Boolean;
    HintShortPause : Integer;
    HintPause      : Integer;
    HintHidePause  : Integer;
  end;

  TFlatHint = class(TComponent)
  private
    FActive             : Boolean;

    FOldHintWindowClass : THintWindowClass;
    FOldHintInfo        : THintInfo;

    {@M}
    procedure SetActive(const AValue : Boolean);
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
  published
    {@G/S}
    property Active : Boolean read FActive write SetActive;
  end;

  TFlatHintWindow = class(THintWindow)
  private
    FMetrics  : TFlatMetrics;
    FArrowPos : TArrowPos;
  protected
    {@M}
    procedure CreateParams(var AParams: TCreateParams); override;

    procedure Paint(); override;
  public
    {@M}
    procedure ActivateHint(AHintRect: TRect; const AHint: String); override;

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

  const ARROW_TOP_RIGHT_GLYPH : TMatrixGlyph = [
                                                    [$0, $1, $1, $1, $1, $1, $1, $1],
                                                    [$0, $1, $1, $1, $1, $1, $1, $1],
                                                    [$0, $0, $0, $1, $1, $1, $1, $1],
                                                    [$0, $0, $1, $1, $1, $1, $1, $1],
                                                    [$0, $1, $1, $1, $1, $1, $1, $1],
                                                    [$1, $1, $1, $1, $1, $0, $1, $1],
                                                    [$1, $1, $1, $1, $0, $0, $1, $1],
                                                    [$0, $1, $1, $0, $0, $0, $0, $0]
                                              ];


implementation

uses VCL.Forms, NeoFlat.Theme, System.Types, System.SysUtils;

(* TFlatHint *)

constructor TFlatHint.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FActive := False;

  ZeroMemory(@FOldHintInfo, SizeOf(THintInfo));

  FOldHintWindowClass := nil;
end;

procedure TFlatHint.SetActive(const AValue : Boolean);
begin
  if FActive = AValue then
    Exit();

  FActive := AValue;

  if (csDesigning in ComponentState) then
    Exit();

  if FActive then begin
    FOldHintWindowClass := HintWindowClass;
    HintWindowClass := TFlatHintWindow;

    FOldHintInfo.HintShow       := Application.ShowHint;
    FOldHintInfo.HintShortPause := Application.HintShortPause;
    FOldHintInfo.HintPause      := Application.HintPause;
    FOldHintInfo.HintHidePause  := Application.HintHidePause;

    Application.ShowHint       := FActive;
    Application.HintShortPause := 25;
    Application.HintPause      := 500;
    Application.HintHidePause  := 5000;
  end else begin
    Application.ShowHint       := FOldHintInfo.HintShow;
    Application.HintShortPause := FOldHintInfo.HintShortPause;
    Application.HintPause      := FOldHintInfo.HintPause;
    Application.HintHidePause  := FOldHintInfo.HintHidePause;

    ZeroMemory(@FOldHintInfo, SizeOf(THintInfo));
  end;
end;

(* TFlatHintWindow *)

constructor TFlatHintWindow.Create(AOwner : TComponent);
begin
  inherited;
  ///

  var ACursorPos : TPoint;
  GetCursorPos(ACursorPos);
  var AWinControl := FindVCLWindow(ACursorPos);
  if not Assigned(AWinControl) then
    Exit();
  ///

  FMetrics := TFlatMetrics.Create(AWinControl);
end;

destructor TFlatHintWindow.Destroy();
begin
  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  ///
  inherited;
end;


procedure TFlatHintWindow.CreateParams(var AParams: TCreateParams);
begin
  inherited CreateParams(AParams);
  ///

  AParams.Style := AParams.Style - WS_BORDER;

  AParams.WindowClass.Style := AParams.WindowClass.Style - CS_DROPSHADOW;
end;

procedure TFlatHintWindow.ActivateHint(AHintRect: TRect; const AHint: String);
var ACurWidth       : Integer;
    APoint          : TPoint;
    AHintHeight     : Integer;
    AHintWidth      : Integer;

    ATopLeftRect    : TRect;
    ATopRightRect   : TRect;
    ABottomLeftRect : TRect;
begin
  const HINT_WIDTH = FMetrics.ScaleValue(400);
  ///

  Caption := AHint;

  Canvas.Font.Name  := FONT_1;
  Canvas.Font.Size  := 9;
  Canvas.Font.Color := MAIN_ACCENT;

  AHintRect.Right := AHintRect.Left + HINT_WIDTH - FMetrics.ScaleValue(22);

  DrawText(Canvas.Handle, @AHint[1], Length(AHint), AHintRect, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);

  Inc(AHintRect.Right, FMetrics.ScaleValue(22));
  Inc(AHintRect.Bottom, FMetrics._6);

  ATopLeftRect    := Rect(0, 0, Screen.Width div 2, Screen.Height div 2);
  ATopRightRect   := Rect(Screen.Width div 2, 0, Screen.Width, Screen.Height div 2);
  ABottomLeftRect := Rect(0, Screen.Height div 2, Screen.Width div 2, Screen.Height);

  GetCursorPos(APoint);

  if PtInRect(ATopLeftRect, APoint) then
    FArrowPos := apTopLeft
  else if PtInRect(ATopRightRect, APoint) then
    FArrowPos := apTopRight
  else if PtInRect(ABottomLeftRect, APoint) then
    FArrowPos := apBottomLeft
  else
    FArrowPos := apBottomRight;

  if FArrowPos = apTopLeft then
    ACurWidth := FMetrics._12
  else
    ACurWidth := FMetrics._5;

  AHintHeight := AHintRect.Bottom - AHintRect.Top;
  AHintWidth  := AHintRect.Right - AHintRect.Left;

  case FArrowPos of
    apTopLeft : AHintRect := Rect(
      APoint.x + ACurWidth,
      APoint.y + ACurWidth,
      APoint.x + AHintWidth + ACurWidth,
      APoint.y + AHintHeight + ACurWidth
    );

    apTopRight : AHintRect := Rect(
      APoint.x - AHintWidth - ACurWidth,
      APoint.y + ACurWidth,
      APoint.x - ACurWidth,
      APoint.y + AHintHeight + ACurWidth
    );

    apBottomLeft  : AHintRect := Rect(
      APoint.x + ACurWidth,
      APoint.y - AHintHeight - ACurWidth,
      APoint.x + AHintWidth + ACurWidth,
      APoint.y - ACurWidth
    );

    apBottomRight : AHintRect := Rect(
      APoint.x - AHintWidth - ACurWidth,
      APoint.y - AHintHeight - ACurWidth,
      APoint.x - ACurWidth,
      APoint.y - ACurWidth
    );
  end;

  BoundsRect := AHintRect;

  APoint := ClientToScreen(Point(0, 0));

  ///
  SetWindowPos(Handle, HWND_TOPMOST, APoint.X, APoint.Y, 0, 0, (SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE));
end;

procedure TFlatHintWindow.Paint();
begin
  var AArrowAreaRect := TRect.Empty;
  var ATextRect      := TRect.Empty;

  case FArrowPos of
    apTopLeft, apBottomLeft: begin
      AArrowAreaRect := Rect(
        ClientRect.Left + FMetrics._1,
        ClientRect.Top + FMetrics._1,
        ClientRect.Left + FMetrics._15,
        ClientRect.Bottom - FMetrics._1
      );

      ATextRect := Rect(
        ClientRect.Left + FMetrics._15,
        ClientRect.Top + FMetrics._1,
        ClientRect.Right - FMetrics._1,
        ClientRect.Bottom - FMetrics._1
      );
    end;

    apTopRight, apBottomRight: begin
        AArrowAreaRect := Rect(
          ClientRect.Right - FMetrics._15,
          ClientRect.Top + FMetrics._1,
          ClientRect.Right - FMetrics._1,
          ClientRect.Bottom - FMetrics._1
        );

        ATextRect := Rect(
          ClientRect.Left + FMetrics._1,
          ClientRect.Top + FMetrics._1,
          ClientRect.Right - FMetrics._15,
          ClientRect.Bottom - FMetrics._1
        );
    end;
  end;

  Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;

    // Draw Container Background
    Canvas.Brush.Color := MAIN_ACCENT;
    Canvas.FillRect(ClientRect);

    // Text Background
    Canvas.Brush.Color := LIGHT_GRAY;
    Canvas.FillRect(ATextRect);

    // Arrow Area Background
    Canvas.Brush.Color := MAIN_GRAY;
    Canvas.FillRect(AArrowAreaRect);

    // Draw Glyph (Arrow)
    var AGlyph : TMatrixGlyph;
    ScaleMatrixGlyph(ARROW_TOP_RIGHT_GLYPH, AGlyph, round(FMetrics.ScaleFactor));
    if not IsValidMatrixGlyph(AGlyph) then
      Exit();

    var AArrowPos : TPoint;

    case FArrowPos of
      apTopLeft : begin
        AArrowPos := Point(
          AArrowAreaRect.Left + FMetrics._2, // X
          AArrowAreaRect.Top + FMetrics._2   // Y
        );

        AGlyph := RotateMatrixGlyph(AGlyph, False);
      end;

      apTopRight :
        AArrowPos := Point(
          AArrowAreaRect.Right - FMetrics._3 - Length(AGlyph[0]), // X
          AArrowAreaRect.Top + FMetrics._2                        // Y
        );

      apBottomLeft : begin
        AArrowPos := Point(
          AArrowAreaRect.Left + FMetrics._2,                   // X
          AArrowAreaRect.Bottom - FMetrics._3 - Length(AGlyph) // Y
        );

        AGlyph := RotateMatrixGlyph(AGlyph, True);
        AGlyph := RotateMatrixGlyph(AGlyph, True);
      end;

      apBottomRight : begin
        AArrowPos := Point(
          AArrowAreaRect.Right - FMetrics._3 - Length(AGlyph[0]), // X
          AArrowAreaRect.Bottom - FMetrics._3 - Length(AGlyph)    // Y
        );

        AGlyph := RotateMatrixGlyph(AGlyph, True);
      end;
    end;

    DrawMatrixGlyph(Canvas, AGlyph, AArrowPos, GLYPH_COLOR);

    // Draw Text
    InflateRect(ATextRect, FMetrics.ScaleValue(-3), FMetrics.ScaleValue(-1));

    Canvas.Brush.Color := LIGHT_GRAY;

    ///
    DrawText(Canvas.Handle, PWideChar(Caption), Length(Caption), ATextRect, (DT_WORDBREAK or DT_NOPREFIX));
  finally
    Canvas.Unlock();
  end;
end;

end.
