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

unit NeoFlat.CheckBox;

interface

uses Winapi.Windows, VCL.Controls, System.Classes, VCL.Graphics, Winapi.Messages,
     NeoFlat.Theme, NeoFlat.Metrics, NeoFlat.Types;

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

  {
    Glyphs
  }
  const CHECKBOX_GLYPH_TEMPLATE : TByteArrayArray = [
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

  RADIOBOX_GLYPH_TEMPLATE : TByteArrayArray = [
                                        [$0, $1, $1, $0],
                                        [$1, $1, $1, $1],
                                        [$1, $1, $1, $1],
                                        [$0, $1, $1, $0]
  ];

implementation

uses System.SysUtils, NeoFlat.Common;

{ TFlatCheckBox.GetButtonRect }
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

{ TFlatCheckBox.MouseDown }
procedure TFlatCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  FButtonIsDown := True;
end;

{ TFlatCheckBox.MouseUp }
procedure TFlatCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var APoint : TPoint;
begin
  inherited;
  ///

  if FButtonIsDown then begin
    APoint.X := X;
    APoint.Y := Y;

    if ptinrect(self.ClientRect, APoint) then begin
      self.SetChecked(not FChecked);
    end;
  end;

  FButtonIsDown := False;
end;

{ TFlatCheckBox.OnCustomWindowProc }
procedure TFlatCheckBox.OnCustomWindowProc(var AMessage : TMessage);
var APoint : TPoint;
begin
  FOldWindowProc(AMessage);
  ///

  if (csDesigning in ComponentState) then
    Exit;

  case AMessage.Msg of
    {
      Button Click (Down)
    }
    WM_LBUTTONDOWN : begin
      SetControlState(csActive);
      ///

//      if Assigned(FOnClick) then
//        FOnClick(self);
    end;

    {
      Button Click (Up)
    }
    WM_LBUTTONUP : begin
      APoint.X := TWMLButtonUp(AMessage).XPos;
      APoint.Y := TWMLButtonUp(AMessage).YPos;

      FMouseHover := ptinrect(self.ClientRect, APoint);

      if FMouseHover then
        SetControlState(csHover)
      else
        SetControlState(csNormal);
      ///

      {
        Trigger Event
      }
    end;

    {
      Surface Move (Enter)
    }
    WM_MOUSEMOVE : begin
      FMouseHover := True;
      ///

      if (FControlState = csActive) then
        Exit();

      SetControlState(csHover);
    end;

    {
      Surface Leave
    }
    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      FMouseHover := False;
      ///

      if (FControlState <> csActive) then
        SetControlState(csNormal);
    end;
  end;
end;

{ TFlatCheckBox.IsDesigning }
function TFlatCheckBox.IsDesigning() : Boolean;
begin
  result := (csDesigning in ComponentState);
end;


{ TFlatCheckBox.Create }
constructor TFlatCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Font.Height  := -11;
  self.Font.Name    := FONT_1;
  self.Font.Color   := MAIN_ACCENT;

  ShowHint := True;

  FMode := cbmCheckBox;

  FControlState := csNormal;
  FMouseHover   := False;
  FChecked      := False;

  FOldWindowProc  := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  FColor := MAIN_GRAY;
  FButtonIsDown := False;
  FOnStateChanged := nil;

  FHoverColor  := MAIN_GRAY;
  FActiveColor := MAIN_GRAY;

  FMetrics := TFlatMetrics.Create(self);
end;

{ TFlatCheckBox.Destroy }
destructor TFlatCheckBox.Destroy();
begin
  if Assigned(FOldWindowProc) then
    self.WindowProc := FOldWindowProc;

  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  ///
  inherited Destroy();
end;

{ TFlatCheckBox.Loaded }
procedure TFlatCheckBox.Loaded();
begin
  inherited;
  ///

  if NOT self.IsDesigning() then
    AdjustBound();
end;

{ TFlatCheckBox.AdjustBound }
procedure TFlatCheckBox.AdjustBound();
var
  ADC: HDC;
  ASaveFont: HFONT;
  AMetrics: TTextMetric;
begin
  ADC := GetDC(0);
  try
    ASaveFont := SelectObject(ADC, self.Font.Handle);
    GetTextMetrics(ADC, AMetrics);
    SelectObject(ADC, ASaveFont);
  finally
    ReleaseDC(0, ADC);
  end;

  ///
  Height := (AMetrics.tmHeight + FMetrics._6);
end;

{ TFlatCheckBox.Paint }
procedure TFlatCheckBox.Paint();
var ARect       : TRect;
    X, Y        : Integer;
    ALeft       : Integer;
    ACaption    : String;
    ABackground : TColor;
    ABorder     : TColor;
    AGlyph      : TByteArrayArray;

  {
    Draw the CheckBox Glyph in Canvas
  }
  procedure DrawAGlyph(const AGlyphMatrix : TByteArrayArray; const X, Y : Integer; const AGlyphColor : TColor);
  var I, N         : Integer;
      AGlyphWidth  : Integer;
      AGlyphHeight : Integer;
  begin
    AGlyphWidth  := High(AGlyphMatrix[1]) + 1;
    AGlyphHeight := High(AGlyphMatrix) + 1;
    ///

    for I := 0 to AGlyphWidth -1 do begin
      for N := 0 to AGlyphHeight -1 do begin
        if AGlyphMatrix[N][I] <> 0 then begin
          Canvas.Pixels[(X + I), (Y + N)] := AGlyphColor;
        end;
      end;
    end;
  end;

begin
  Canvas.Lock();
  try
    {
      Draw Background
    }
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FColor;

    Canvas.FillRect(Rect(0, 0, self.ClientWidth, self.ClientHeight));

    {
      Draw Glyph
    }
    Canvas.Pen.Style   := psClear;
    Canvas.Brush.Style := bsClear;
    case FMode of
      cbmCheckBox : begin
        {
          Draw CheckBox Border
        }
        ABorder     := clNone;
        ABackground := clNone;

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

        ARect := self.GetButtonRect();

        if (ABorder <> clNone) then begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := ABorder;

          // Draw Button Border Left
          Canvas.FillRect(Rect(
            ARect.Left,
            ARect.Top,
            ARect.Left + FMetrics._1,
            ARect.Bottom
          ));

          // Draw Button Border Top
          Canvas.FillRect(Rect(
            ARect.Left,
            ARect.Top,
            ARect.Right,
            ARect.Top + FMetrics._1
          ));

          // Draw Button Border Right
          Canvas.FillRect(Rect(
            ARect.Right - FMetrics._1,
            ARect.Top,
            ARect.Right,
            ARect.Bottom
          ));

          // Draw Button Border Bottom
          Canvas.FillRect(Rect(
            ARect.Left,
            ARect.Bottom - FMetrics._1,
            ARect.Right,
            ARect.Bottom
          ));
        end;

        if (ABackground <> clNone) then begin
          InflateRect(ARect, -1, -1);

          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := ABackground;

          Canvas.FillRect(ARect);

          InflateRect(ARect, 1, 1);
        end;

        {
          Draw Glyph
        }
        if FChecked then begin
          X := (ARect.Left + FMetrics._1);
          Y := (ARect.Top + FMetrics._1);

          ScaleGlyph(CHECKBOX_GLYPH_TEMPLATE, AGlyph, round(ScaleFactor));

          DrawAGlyph(AGlyph, X, Y, MAIN_ACCENT);
        end;
      end;

      cbmRadioBox : begin
        {
          Draw RadioBox Border
        }

        ABorder     := clNone;
        ABackground := clNone;

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

        ARect := self.GetButtonRect();

        Canvas.Ellipse(ARect);

        X := (ARect.Left + FMetrics._3);
        Y := (ARect.Top + FMetrics._3);

        if FChecked then begin
          ScaleGlyph(RADIOBOX_GLYPH_TEMPLATE, AGlyph, round(ScaleFactor));

          DrawAGlyph(AGlyph, X, Y, MAIN_ACCENT);
        end;
      end;
    end;

    {
      Draw Caption / Text
    }
    Canvas.Brush.Style := bsClear;

    ACaption := inherited Caption;

    Canvas.Font.Assign(inherited Font);

    ALeft := (ARect.Left + ARect.Width + FMetrics._6);

    // Calc Required Height for Text
    ARect.Top    := 0;
    ARect.Height := ClientHeight - FMetrics._2;
    ARect.Left   := ALeft;
    ARect.Width  := (ClientWidth - FMetrics._6 - ALeft);

    // Then Draw Text
    Canvas.TextRect(ARect, ACaption, [tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
  finally
    Canvas.Unlock();
  end;
end;

{ TFlatCheckBox.CMFontChanged }
procedure TFlatCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ///

  if NOT self.IsDesigning() and (csLoading in ComponentState) then
    AdjustBound();
end;

{ TFlatCheckBox.SetCaption }
procedure TFlatCheckBox.SetCaption(AValue : String);
begin
  if (AValue = inherited Caption) then
    Exit();
  ///

  inherited Caption := AValue;

  ///
  Invalidate();
end;

{ TFlatCheckBox.GetCaption }
function TFlatCheckBox.GetCaption() : String;
begin
  result := inherited Caption;
end;

{ TFlatCheckBox.SetMode }
procedure TFlatCheckBox.SetMode(AValue : TCheckBoxMode);
begin
  if (AValue = FMode) then
    Exit();
  ///

  FMode := AValue;

  ///
  Invalidate();
end;

{ TFlatCheckBox.SetControlState }
procedure TFlatCheckBox.SetControlState(AValue : TControlState);
begin
  if (AValue = FControlState) then
    Exit();
  ///

  FControlState := AValue;

  ///
  Invalidate();
end;

{ TFlatCheckBox.SetChecked }
procedure TFlatCheckBox.SetChecked(AValue : Boolean);

  procedure UncheckGroupRadio();
  var I : Integer;
      C : TComponent;
  begin
    for i := 0 to self.Owner.ComponentCount -1 do begin
      C := self.Owner.Components[i];

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

  if (self.Mode = cbmRadioBox) then begin
    UncheckGroupRadio();

    AValue := True; // always for radio
  end;

  _SetChecked(AValue);
end;

{ TFlatCheckBox._SetChecked }
procedure TFlatCheckBox._SetChecked(const AValue : Boolean);
begin
  FChecked := AValue;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(self);

  ///
  Invalidate();
end;

end.
