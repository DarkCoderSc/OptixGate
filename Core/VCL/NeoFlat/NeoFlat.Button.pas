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

unit NeoFlat.Button;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows, Winapi.Messages,

  VCL.Controls, VCL.Graphics, VCL.ImgList,

  NeoFlat.Theme, NeoFlat.Classes, NeoFlat.Helper, NeoFlat.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOnValueChanged = procedure(Sender : TObject; const ANewValue : Integer) of object;

  TFlatButton = class(TGraphicControl)
  private
    FMetrics         : TFlatMetrics;

    FOldWindowProc   : TWndMethod;

    FButtonState     : TFlatControlStateEx;
    FMouseHover      : Boolean;
    FOldEnabledValue : Boolean;
    FBusy            : Boolean;

    FValue           : Integer;

    FBackground      : TFlatStateColors;
    FOuterBorder     : TFlatStateColors;

    FImageList       : TCustomImageList;
    FImageIndex      : Integer;

    FOnClick         : TNotifyEvent;
    FOnValueChanged  : TOnValueChanged;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);

    procedure SetButtonState(const AState : TFlatControlStateEx);
    procedure SetValue(const AValue : Integer);
    procedure SetBusy(const AValue : Boolean);
    procedure SetImageIndex(const Avalue : Integer);

    procedure DrawText();
    procedure DrawBorders();
    procedure DrawImage();
    procedure DrawBackground();
  protected
    {@M}
    procedure Paint; override;
    procedure SetEnabled(AValue : Boolean); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Caption;
    property Enabled;
    property Font;
    property Visible;
    property Align;
    property ShowHint;

    property Images         : TCustomImageList read FImageList      write FImageList;
    property ImageIndex     : Integer          read FImageIndex     write SetImageIndex;
    property Value          : Integer          read FValue          write SetValue;
    property OnClick        : TNotifyEvent     read FOnClick        write FOnClick;
    property OnValueChanged : TOnValueChanged  read FOnValueChanged write FOnValueChanged;
    property Busy           : Boolean          read FBusy           write SetBusy;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Types, System.SysUtils;
// ---------------------------------------------------------------------------------------------------------------------

(* TFlatButton *)

constructor TFlatButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ShowHint := True;

  FOldWindowProc := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  ControlStyle := ControlStyle;

  FButtonState := csExNormal;
  FMouseHover  := False;

  FOnClick := nil;

  FValue := 0;
  FOnValueChanged := nil;

  FBackground  := TFlatStateColors.Create(self);
  FOuterBorder := TFlatStateColors.Create(self);

  FBackground.Normal   := MAIN_GRAY;
  FBackground.Hover    := MAIN_GRAY;
  FBackground.Focus    := MAIN_GRAY;
  FBackground.Active   := DARKER_GRAY;
  FBackground.Disabled := clNone;

  FOuterBorder.Normal   := MAIN_ACCENT;
  FOuterBorder.Hover    := MAIN_ACCENT;
  FOuterBorder.Focus    := MAIN_ACCENT;
  FOuterBorder.Active   := MAIN_ACCENT;
  FOuterBorder.Disabled := clGray;

  self.Font.Height      := -11;
  self.Font.Name        := FONT_1;
  self.Font.Color       := MAIN_ACCENT;

  FOldEnabledValue      := inherited Enabled;
  FBusy                 := False;

  FImageIndex := -1;
  FImageList  := nil;

  FMetrics := TFlatMetrics.Create(self);
end;

destructor TFlatButton.Destroy();
begin
  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  if Assigned(FBackground) then
    FreeAndNil(FBackground);

  if Assigned(FOuterBorder) then
    FreeAndNil(FOuterBorder);

  if Assigned(FOldWindowProc) then
    self.WindowProc := FOldWindowProc;

  ///
  inherited Destroy();
end;

procedure TFlatButton.DrawText();
begin
  var ATextDownDelta := 0;
  if (FButtonState = csExActive) then
    ATextDownDelta := FMetrics._1;
  ///

  var ALeftMargin := FMetrics._4;

  if Assigned(FImageList) and (FImageIndex > -1) then
    Inc(ALeftMargin, FImageList.Width);

  Canvas.Font.Assign(inherited Font);

  if FButtonState = csExDisabled then
    Canvas.Font.Color := clGray
  else
    Canvas.Font.Color := MAIN_ACCENT;

  var ARect := TRect.Empty;

  ARect.Left   := ALeftMargin;
  ARect.Top    := ATextDownDelta;
  ARect.Width  := ClientWidth - ALeftMargin - FMetrics._4;
  ARect.Height := ClientHeight;

  Canvas.Brush.Style := bsClear;

  var ACaption : String := Caption;
  Canvas.TextRect(ARect, ACaption, [tfEndEllipsis, tfVerticalCenter, tfSingleLine, tfCenter]);
end;

procedure TFlatButton.DrawImage();
begin
  if not Assigned(FImageList) or (FImageIndex <= -1) then
    Exit;
  ///

  DrawGlyph(Canvas, FImageList, FImageIndex, ScaleValue(4), (ClientHeight div 2) - (FImageList.Height div 2), Enabled);
end;

procedure TFlatButton.DrawBorders();
begin
  var AColor := FOuterBorder.GetStateColor(FButtonState);
  if AColor = clNone then
    Exit;

  var ABorderWidth := self.ScaleValue(1);
  ///

  var AOldBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := AColor;

    var ARect := TRect.Empty;

    // Border Top
    ARect.Width  := ClientWidth;
    ARect.Height := ABorderWidth;

    Canvas.FillRect(ARect);
    ARect := TRect.Empty;

    // Border Left
    ARect.Width := ABorderWidth;
    ARect.Height := ClientHeight;

    Canvas.FillRect(ARect);
    ARect := TRect.Empty;

    // Border Right
    ARect.Left   := ClientWidth - ABorderWidth;
    ARect.Height := ClientHeight;
    ARect.Width  := ABorderWidth;

    Canvas.FillRect(ARect);
    ARect := TRect.Empty;

    // Border Bottom
    ARect.Top    := ClientHeight - ABorderWidth;
    ARect.Width  := ClientWidth;
    ARect.Height := ABorderWidth;

    Canvas.FillRect(ARect);
  finally
    Canvas.Brush.Style := AOldBrushStyle;
  end;
end;

procedure TFlatButton.DrawBackground();
begin
  var AColor := FBackground.GetStateColor(FButtonState);
  if AColor = clNone then
    Exit;

  var ARect := ClientRect;
  ///

  var AOldBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := AColor;

    Canvas.FillRect(ARect);
  finally
    Canvas.Brush.Style := AOldBrushStyle;
  end;
end;

procedure TFlatButton.Paint;
begin
  inherited;
  ///

  Canvas.Lock();
  try
    DrawBackground();

    DrawBorders();

    DrawText();

    DrawImage();
  finally
    Canvas.Unlock();
  end;
end;

procedure TFlatButton.OnCustomWindowProc(var AMessage : TMessage);
begin
  FOldWindowProc(AMessage);
  ///

  case AMessage.Msg of
    CM_TEXTCHANGED :
      Invalidate;
  end;

  ///
  if (csDesigning in ComponentState) or (FButtonState = csExDisabled) then
    Exit;
  ///

  case AMessage.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK : begin
      SetButtonState(csExActive);
      ///

    end;

    WM_LBUTTONUP : begin
      FMouseHover := ptinrect(self.ClientRect, Point(
        TWMLButtonUp(AMessage).XPos,
        TWMLButtonUp(AMessage).YPos
      ));

      if FMouseHover then
        SetButtonState(csExHover)
      else
        SetButtonState(csExNormal);
      ///

      if Assigned(FOnClick) and FMouseHover then
        FOnClick(self);
    end;

    WM_MOUSEMOVE : begin
      FMouseHover := True;
      ///

      if (FButtonState = csExActive) then
        Exit;

      SetButtonState(csExHover);
    end;

    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      FMouseHover := False;
      ///

      if (FButtonState <> csExActive) then
        SetButtonState(csExNormal);
    end;
  end;
end;

procedure TFlatButton.SetButtonState(const AState : TFlatControlStateEx);
begin
  if (AState = FButtonState) then
    Exit;
  ///

  FButtonState := AState;

  case FButtonState of
    csExNormal: ;
    csExHover: ;
    csExActive: ;
    csExFocus: ;
    csExDisabled: ;
  end;

  ///
  Invalidate;
end;

procedure TFlatButton.SetValue(const AValue : Integer);
begin
  if FValue = AValue then
    Exit;
  ///

  FValue := AValue;

  if Assigned(FOnValueChanged) then
    FOnValueChanged(self, AValue);
end;

procedure TFlatButton.SetEnabled(AValue : Boolean);
begin
  inherited SetEnabled(AValue);
  ///

  if FBusy then
    FOldEnabledValue := AValue;

  if not AValue then
    FButtonState := csExDisabled
  else
    FButtonState := csExNormal;

  ///
  Invalidate;
end;

procedure TFlatButton.SetBusy(const AValue : Boolean);
begin
  if AValue = FBusy then
    Exit;

  FBusy := AValue;

  if FBusy then begin
    FOldEnabledValue := inherited Enabled;

    inherited Enabled := False;
    FButtonState      := csExDisabled
  end else begin
    inherited Enabled := FOldEnabledValue;
    FButtonState      := csExNormal;
  end;

  ///
  Invalidate;
end;

procedure TFlatButton.SetImageIndex(const AValue : Integer);
begin
  if AValue = FImageIndex then
    Exit;

  FImageIndex := AValue;

  Invalidate;
end;

end.
