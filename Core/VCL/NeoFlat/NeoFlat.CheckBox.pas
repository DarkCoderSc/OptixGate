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

unit NeoFlat.CheckBox;

interface

uses Winapi.Windows, VCL.Controls, System.Classes, VCL.Graphics, Winapi.Messages,
     NeoFlat.Theme, NeoFlat.Helper, NeoFlat.Types;

type
  TCheckBoxMode = (cbmCheckBox, cbmRadioBox);

  TControlState = (csNormal, csHover, csActive);

  TFlatCheckBox = class(TCustomControl)
  private
    FMode           : TCheckBoxMode;
    FControlState   : TControlState;

    FMouseHover     : Boolean;

    FOldWindowProc  : TWndMethod;

    FButtonIsDown   : Boolean;

    FChecked        : Boolean;

    FColor          : TColor;
    FHoverColor     : TColor;
    FActiveColor    : TColor;

    FMetrics        : TFlatMetrics;

    FOnStateChanged : TNotifyEvent;

    {@M}
    procedure SetCaption(AValue : String);
    function GetCaption() : String;

    procedure SetMode(AValue : TCheckBoxMode);

    procedure AdjustBound();

    function IsDesigning() : Boolean;

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    procedure OnCustomWindowProc(var AMessage : TMessage);

    procedure SetControlState(AValue : TControlState);
    procedure SetChecked(AValue : Boolean);

    function GetButtonRect() : TRect;
  protected
    {@M}
    procedure Paint(); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Loaded(); override;
  public
    {@M}
    procedure _SetChecked(const AValue : Boolean);

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Caption        : String        read GetCaption      write SetCaption;
    property Mode           : TCheckBoxMode read FMode           write SetMode;
    property Checked        : Boolean       read FChecked        write SetChecked;
    property OnStateChanged : TNotifyEvent  read FOnStateChanged write FOnStateChanged;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

  const CHECKBOX_GLYPH_TEMPLATE : TMatrixGlyph = [
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $1, $0],
                                              [$0, $0, $0, $0, $0, $0, $1, $1, $0],
                                              [$0, $1, $0, $0, $0, $1, $1, $1, $0],
                                              [$0, $1, $1, $0, $1, $1, $1, $0, $0],
                                              [$0, $1, $1, $1, $1, $1, $0, $0, $0],
                                              [$0, $0, $1, $1, $1, $0, $0, $0, $0],
                                              [$0, $0, $0, $1, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

  RADIOBOX_GLYPH_TEMPLATE : TMatrixGlyph = [
                                        [$0, $1, $1, $0],
                                        [$1, $1, $1, $1],
                                        [$1, $1, $1, $1],
                                        [$0, $1, $1, $0]
  ];

implementation

uses System.SysUtils, System.Types;

function TFlatCheckBox.GetButtonRect() : TRect;
begin
  if FMode = cbmCheckBox then begin
    result.Left   := 0;
    result.Top    := (ClientHeight div 2) - FMetrics._6;
    result.Width  := FMetrics._11;
    result.Height := FMetrics._11;
  end else begin
    result.Left   := 0;
    result.Top    := (ClientHeight div 2) - FMetrics._5;
    result.Width  := FMetrics._10;
    result.Height := FMetrics._10;
  end;
end;

procedure TFlatCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  FButtonIsDown := True;
end;

procedure TFlatCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  if FButtonIsDown then begin
    if ptinrect(ClientRect, Point(X, Y)) then begin
      SetChecked(not FChecked);
    end;
  end;

  FButtonIsDown := False;
end;

procedure TFlatCheckBox.OnCustomWindowProc(var AMessage : TMessage);
begin
  FOldWindowProc(AMessage);
  ///

  if (csDesigning in ComponentState) then
    Exit();

  case AMessage.Msg of
    WM_LBUTTONDOWN : begin
      SetControlState(csActive);
    end;

    WM_LBUTTONUP : begin
      FMouseHover := ptinrect(ClientRect, Point(TWMLButtonUp(AMessage).XPos, TWMLButtonUp(AMessage).YPos));
      ///

      if FMouseHover then
        SetControlState(csHover)
      else
        SetControlState(csNormal);
      ///
    end;

    WM_MOUSEMOVE : begin
      FMouseHover := True;
      ///

      if (FControlState = csActive) then
        Exit();

      SetControlState(csHover);
    end;

    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      FMouseHover := False;
      ///

      if (FControlState <> csActive) then
        SetControlState(csNormal);
    end;
  end;
end;

function TFlatCheckBox.IsDesigning() : Boolean;
begin
  result := (csDesigning in ComponentState);
end;

constructor TFlatCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  Font.Height  := -11;
  Font.Name    := FONT_1;
  Font.Color   := MAIN_ACCENT;

  ShowHint := True;

  FMode := cbmCheckBox;

  FControlState := csNormal;
  FMouseHover   := False;
  FChecked      := False;

  FOldWindowProc  := WindowProc;
  WindowProc := OnCustomWindowProc;

  FColor := MAIN_GRAY;
  FButtonIsDown := False;
  FOnStateChanged := nil;

  FHoverColor  := MAIN_GRAY;
  FActiveColor := MAIN_GRAY;

  FMetrics := TFlatMetrics.Create(self);
end;

destructor TFlatCheckBox.Destroy();
begin
  if Assigned(FOldWindowProc) then
    WindowProc := FOldWindowProc;

  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  ///
  inherited Destroy();
end;

procedure TFlatCheckBox.Loaded();
begin
  inherited;
  ///

  if NOT IsDesigning() then
    AdjustBound();
end;

procedure TFlatCheckBox.AdjustBound();
begin
  var AMetrics: TTextMetric;

  var ADC := GetDC(0);
  try
    var ASaveFont := SelectObject(ADC, Font.Handle);

    GetTextMetrics(ADC, AMetrics);
    SelectObject(ADC, ASaveFont);
  finally
    ReleaseDC(0, ADC);
  end;

  Height := (AMetrics.tmHeight + FMetrics._6);
end;

procedure TFlatCheckBox.Paint();
begin
  Canvas.Lock();
  try
    // Draw Background
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FColor;

    Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));

    var AButtonRect : TRect;

    // Draw checkbox / radio
    Canvas.Pen.Style   := psClear;
    Canvas.Brush.Style := bsClear;
    case FMode of
      cbmCheckBox : begin
        // Draw Border
        var ABorder     := clNone;
        var ABackground := clNone;

        case FControlState of
          csNormal : begin
            ABorder     := MAIN_ACCENT;
            ABackground := clNone;
          end;

          csHover : begin
            ABorder     := MAIN_ACCENT;
            ABackground := clNone;
          end;

          csActive : begin
            ABorder     := MAIN_ACCENT;
            ABackground := clNone;
          end;
        end;

        AButtonRect := GetButtonRect();

        if (ABorder <> clNone) then begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := ABorder;

          // Draw Button Border Left
          Canvas.FillRect(Rect(
            AButtonRect.Left,
            AButtonRect.Top,
            AButtonRect.Left + FMetrics._1,
            AButtonRect.Bottom
          ));

          // Draw Button Border Top
          Canvas.FillRect(Rect(
            AButtonRect.Left,
            AButtonRect.Top,
            AButtonRect.Right,
            AButtonRect.Top + FMetrics._1
          ));

          // Draw Button Border Right
          Canvas.FillRect(Rect(
            AButtonRect.Right - FMetrics._1,
            AButtonRect.Top,
            AButtonRect.Right,
            AButtonRect.Bottom
          ));

          // Draw Button Border Bottom
          Canvas.FillRect(Rect(
            AButtonRect.Left,
            AButtonRect.Bottom - FMetrics._1,
            AButtonRect.Right,
            AButtonRect.Bottom
          ));
        end;

        if (ABackground <> clNone) then begin
          InflateRect(AButtonRect, -1, -1);

          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := ABackground;

          Canvas.FillRect(AButtonRect);

          InflateRect(AButtonRect, 1, 1);
        end;

        // Draw Glyph
        if FChecked then begin
          var X := (AButtonRect.Left + FMetrics._1);
          var Y := (AButtonRect.Top + FMetrics._1);

          var AGlyph : TMatrixGlyph;
          ScaleMatrixGlyph(CHECKBOX_GLYPH_TEMPLATE, AGlyph, round(ScaleFactor));

          DrawMatrixGlyph(Canvas, AGlyph, X, Y, MAIN_ACCENT);
        end;
      end;

      cbmRadioBox : begin
        var ABorder     := clNone;
        var ABackground := clNone;

        case FControlState of
          csNormal : begin
            ABorder     := MAIN_ACCENT;
            ABackground := clNone;
          end;

          csHover : begin
            ABorder     := MAIN_ACCENT;
            ABackground := clNone;
          end;

          csActive : begin
            ABorder     := MAIN_ACCENT;
            ABackground := clNone;
          end;
        end;

        if (ABackground <> clNone) then begin
          Canvas.Brush.Color := ABackground;
          Canvas.Brush.Style := bsSolid;
        end;

        if (ABorder <> clNone) then begin
          Canvas.Pen.Color := ABorder;
          Canvas.Pen.Style := psSolid;
        end;

        AButtonRect := GetButtonRect();

        Canvas.Ellipse(AButtonRect);

        // HDPI scaling trick
        var AScaledButtonRect := AButtonRect;
        for var I := 1 to round(FMetrics.ScaleFactor) -1 do begin
          InflateRect(AScaledButtonRect, -1, -1);

          Canvas.Ellipse(AScaledButtonRect);
        end;

        var X := (AButtonRect.Left + FMetrics._3);
        var Y := (AButtonRect.Top + FMetrics._3);

        if FChecked then begin
          var AGlyph : TMatrixGlyph;

          ScaleMatrixGlyph(RADIOBOX_GLYPH_TEMPLATE, AGlyph, round(ScaleFactor));

          DrawMatrixGlyph(Canvas, AGlyph, X, Y, MAIN_ACCENT);
        end;
      end;
    end;

    // Draw caption
    Canvas.Brush.Style := bsClear;

    Canvas.Font.Assign(Font);

    var ALeft := (AButtonRect.Left + AButtonRect.Width + FMetrics._6);
    var ATextRect : TRect;

    ATextRect.Top    := 0;
    ATextRect.Height := ClientHeight - FMetrics._2;
    ATextRect.Left   := ALeft;
    ATextRect.Width  := (ClientWidth - FMetrics._6 - ALeft);

    var ACaption : String := inherited Caption;
    Canvas.TextRect(ATextRect, ACaption, [tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
  finally
    Canvas.Unlock();
  end;
end;

procedure TFlatCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ///

  if NOT IsDesigning() and (csLoading in ComponentState) then
    AdjustBound();
end;

procedure TFlatCheckBox.SetCaption(AValue : String);
begin
  if (AValue = inherited Caption) then
    Exit();
  ///

  inherited Caption := AValue;

  ///
  Invalidate();
end;

function TFlatCheckBox.GetCaption() : String;
begin
  result := inherited Caption;
end;

procedure TFlatCheckBox.SetMode(AValue : TCheckBoxMode);
begin
  if (AValue = FMode) then
    Exit();
  ///

  FMode := AValue;

  ///
  Invalidate();
end;

procedure TFlatCheckBox.SetControlState(AValue : TControlState);
begin
  if (AValue = FControlState) then
    Exit();
  ///

  FControlState := AValue;

  ///
  Invalidate();
end;

procedure TFlatCheckBox.SetChecked(AValue : Boolean);

  procedure UncheckGroupRadio();
  begin
    for var I := 0 to Owner.ComponentCount -1 do begin
      var C := Owner.Components[i];

      if C = self then
        continue;

      if not (C is TFlatCheckBox) then
        continue;

      if (TFlatCheckBox(C).Mode <> cbmRadioBox) then
        continue;

      TFlatCheckBox(C)._SetChecked(False);
    end;
  end;

begin
  if AValue = FChecked then
    Exit();

  if (Mode = cbmRadioBox) then begin
    UncheckGroupRadio();

    AValue := True; // always for radio
  end;

  _SetChecked(AValue);
end;

procedure TFlatCheckBox._SetChecked(const AValue : Boolean);
begin
  FChecked := AValue;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(self);

  ///
  Invalidate();
end;

end.
