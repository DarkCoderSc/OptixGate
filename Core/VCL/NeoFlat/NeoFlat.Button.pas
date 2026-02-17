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

uses Winapi.Windows, System.Classes, VCL.Controls, Winapi.Messages, VCL.Graphics, NeoFlat.Theme,
     NeoFlat.Classes, VCL.ImgList, NeoFlat.Helper;

type
  TFlatButton = class;

  TTextAlign   = (taLeft, taCenter, taRight);

  TOnValueChanged = procedure(Sender : TObject; ANewValue : Integer) of object;

  TFlatButton = class(TGraphicControl)
  private
    FOldWindowProc       : TWndMethod;
    FButtonState         : TFlatControlState;
    FMouseHover          : Boolean;
    FOnClick             : TNotifyEvent;
    FOnValueChanged      : TOnValueChanged;
    FMetrics             : TFlatMetrics;

    FBackground          : TFlatStateColors;
    FOuterBorder         : TFlatStateColors;

    FOldEnabledValue     : Boolean;
    FBusy                : Boolean;

    FImageList           : TCustomImageList;
    FImageIndex          : Integer;

    FValue               : Integer;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);

    procedure SetButtonState(AState : TFlatControlState);

    function GetCaption() : String;
    procedure SetCaption(AValue : String);
    procedure SetValue(AValue : Integer);
    procedure SetBusy(const AValue : Boolean);
    procedure SetImageIndex(Avalue : Integer);

    procedure DrawText();
    procedure DrawBorders();
    procedure DrawImage();
    procedure DrawBackground();
  protected
    {@M}
    procedure Paint(); override;
    procedure SetEnabled(AValue : Boolean); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Enabled;
    property Font;
    property Visible;
    property Align;
    property ShowHint;

    property Images         : TCustomImageList read FImageList      write FImageList;
    property ImageIndex     : Integer          read FImageIndex     write SetImageIndex;
    property Caption        : String           read GetCaption      write SetCaption;
    property Value          : Integer          read FValue          write SetValue;
    property OnClick        : TNotifyEvent     read FOnClick        write FOnClick;
    property OnValueChanged : TOnValueChanged  read FOnValueChanged write FOnValueChanged;
    property Busy           : Boolean          read FBusy           write SetBusy;
  end;

implementation

uses System.IniFiles, System.SysUtils;

(* TFlatButton *)

{ TFlatButton.Create }
constructor TFlatButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ShowHint := True;

  FOldWindowProc := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  ControlStyle := ControlStyle;

  FButtonState := csNormal;
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

{ TFlatButton.Destroy }
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

{ TFlatButton.DrawText }
procedure TFlatButton.DrawText();
var ARect          : TRect;
    ATextFormat    : TTextFormat;
    ACaption       : String;
    ATextDownDelta : Integer;
    ALeftMargin    : Integer;
begin
  ATextDownDelta := 0;
  if (FButtonState = csActive) then
    ATextDownDelta := FMetrics._1;
  ///

  ALeftMargin := FMetrics._4;

  if Assigned(FImageList) and (FImageIndex > -1) then
    Inc(ALeftMargin, FImageList.Width);

  Canvas.Font.Assign(inherited Font);

  if FButtonState = csDisabled then
    Canvas.Font.Color := clGray
  else
    Canvas.Font.Color := MAIN_ACCENT;

  ARect.Left   := ALeftMargin;
  ARect.Top    := (ATextDownDelta);
  ARect.Width  := ClientWidth - ALeftMargin - FMetrics._4;
  ARect.Height := ClientHeight;

  ACaption := inherited Caption;

  Canvas.Brush.Style := bsClear;

  ATextFormat := [tfEndEllipsis, tfVerticalCenter, tfSingleLine, tfCenter];

  Canvas.TextRect(ARect, ACaption, ATextFormat);
end;

{ TFlatButton.DrawImage }
procedure TFlatButton.DrawImage();
begin
  if not Assigned(FImageList) or (FImageIndex <= -1) then
    Exit();

  DrawGlyph(
    Canvas,
    FImageList,
    FImageIndex,
    ScaleValue(4),
    (ClientHeight div 2) - (FImageList.Height div 2),
    Enabled
  );
end;

{ TFlatButton.DrawBorders }
procedure TFlatButton.DrawBorders();
var ARect          : TRect;
    ABorderWidth   : Integer;
    AColor         : TColor;
    AOldBrushStyle : TBrushStyle;
begin
  AColor := FOuterBorder.GetStateColor(FButtonState);
  if AColor = clNone then
    Exit();

  ABorderWidth := self.ScaleValue(1);
  ///

  AOldBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := AColor;

    // Border Top
    ARect.Top    := 0;
    ARect.Left   := 0;
    ARect.Width  := ClientWidth;
    ARect.Height := ABorderWidth;

    Canvas.FillRect(ARect);

    // Border Left
    ARect.Top   := 0;
    ARect.Left  := 0;
    ARect.Width := ABorderWidth;
    ARect.Height := ClientHeight;

    Canvas.FillRect(ARect);

    // Border Right
    ARect.Top    := 0;
    ARect.Left   := ClientWidth - ABorderWidth;
    ARect.Height := ClientHeight;
    ARect.Width  := ABorderWidth;

    Canvas.FillRect(ARect);

    // Border Bottom
    ARect.Top    := ClientHeight - ABorderWidth;
    ARect.Left   := 0;
    ARect.Width  := ClientWidth;
    ARect.Height := ABorderWidth;

    Canvas.FillRect(ARect);
  finally
    Canvas.Brush.Style := AOldBrushStyle;
  end;
end;

{ TFlatButton.DrawBackground }
procedure TFlatButton.DrawBackground();
var ARect          : TRect;
    AColor         : TColor;
    AOldBrushStyle : TBrushStyle;
begin
  AColor := FBackground.GetStateColor(FButtonState);
  if AColor = clNone then
    Exit();

  ARect := ClientRect;
  ///

  AOldBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := AColor;

    Canvas.FillRect(ARect);
  finally
    Canvas.Brush.Style := AOldBrushStyle;
  end;
end;

{ TFlatButton.Paint }
procedure TFlatButton.Paint();
var ARect  : TRect;
begin
  inherited Paint();
  ///

  ARect := ClientRect;

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

{ TFlatButton.OnCustomWindowProc }
procedure TFlatButton.OnCustomWindowProc(var AMessage : TMessage);
var APoint : TPoint;
begin
  FOldWindowProc(AMessage);
  ///

  if (csDesigning in ComponentState) then
    Exit;

  if FButtonState = csDisabled then
    Exit();

  case AMessage.Msg of
    {
      Button Click (Down)
    }
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK : begin
      SetButtonState(csActive);
      ///

    end;

    {
      Button Click (Up)
    }
    WM_LBUTTONUP : begin
      APoint.X := TWMLButtonUp(AMessage).XPos;
      APoint.Y := TWMLButtonUp(AMessage).YPos;

      FMouseHover := ptinrect(self.ClientRect, APoint);

      if FMouseHover then
        SetButtonState(csHover)
      else
        SetButtonState(csNormal);
      ///

      {
        Trigger Event
      }
      if Assigned(FOnClick) and FMouseHover then
        FOnClick(self);
    end;

    {
      Surface Move (Enter)
    }
    WM_MOUSEMOVE : begin
      FMouseHover := True;
      ///

      if (FButtonState = csActive) then
        Exit();

      SetButtonState(csHover);
    end;

    {
      Surface Leave
    }
    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      FMouseHover := False;
      ///

      if (FButtonState <> csActive) then
        SetButtonState(csNormal);
    end;
  end;
end;

{ TFlatButton.SetButtonState }
procedure TFlatButton.SetButtonState(AState : TFlatControlState);
begin
  if (AState = FButtonState) then
    Exit();
  ///

  FButtonState := AState;

  case FButtonState of
    csNormal: ;
    csHover: ;
    csActive: ;
    csFocus: ;
    csDisabled: ;
  end;

  ///
  Invalidate();
end;

{ TFlatButton.GetCaption }
function TFlatButton.GetCaption() : String;
begin
  result := inherited Caption;
end;

{ TFlatButton.SetCaption }
procedure TFlatButton.SetCaption(AValue : String);
begin
  inherited Caption := AValue;

  ///
  Invalidate();
end;

{ TFlatButton.SetValue }
procedure TFlatButton.SetValue(AValue : Integer);
begin
  if FValue = AValue then
    Exit();
  ///

  FValue := AValue;

  if Assigned(FOnValueChanged) then
    FOnValueChanged(self, AValue);
end;

{ TFlatButton.SetEnabled }
procedure TFlatButton.SetEnabled(AValue : Boolean);
begin
  inherited SetEnabled(AValue);
  ///

  if FBusy then
    FOldEnabledValue := AValue;

  if not AValue then
    FButtonState := csDisabled
  else
    FButtonState := csNormal;

  ///
  Invalidate();
end;

{ TFlatButton.SetBusy }
procedure TFlatButton.SetBusy(const AValue : Boolean);
begin
  if AValue = FBusy then
    Exit();

  FBusy := AValue;

  if FBusy then begin
    FOldEnabledValue := inherited Enabled;

    inherited Enabled := False;
    FButtonState      := csDisabled
  end else begin
    inherited Enabled := FOldEnabledValue;
    FButtonState      := csNormal;
  end;

  ///
  Invalidate();
end;

{ TFlatButton.SetImageIndex }
procedure TFlatButton.SetImageIndex(AValue : Integer);
begin
  if AValue = FImageIndex then
    Exit();

  FImageIndex := AValue;

  Invalidate();
end;

end.
