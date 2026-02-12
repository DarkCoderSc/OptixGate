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

unit NeoFlat.Edit;

interface

uses Winapi.Windows, System.Classes, VCL.Controls, VCL.StdCtrls, Winapi.Messages, VCL.Graphics,
     VCL.Forms, NeoFlat.Classes, NeoFlat.Types;

type
  TFlatEdit = class(TEdit)
  private
    FMouseHover       : Boolean;

    FBackground       : TFlatStateColors;
    FBorder           : TFlatStateColors;
    FShowBorder       : Boolean;

    FEditStatus       : TControlStatus;

    FValidators       : TValidators;


    {@M}
    procedure WMNCPaint(var AMessage: TWMNCPaint);      message WM_NCPAINT;
    procedure CMMouseEnter(var AMessage: TMessage);     message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMessage: TMessage);     message CM_MOUSELEAVE;
    procedure WMSetFocus(var AMessage: TWMSetFocus);    message WM_SETFOCUS;
    procedure WMKillFocus(var AMessage: TWMKillFocus);  message WM_KILLFOCUS;
    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var AMessage: TMessage);    message CM_FONTCHANGED;

    procedure DrawFlatBorder(ARegion : HRGN);

    function IsDesigning() : Boolean;

    procedure AdjustBound();

    procedure SetEditStatus(const AValue : TControlStatus);

    procedure SetEnabled(const AValue : Boolean);
    procedure SetShowBorder(const AValue : Boolean);
    function GetEnabled() : Boolean;
    function GetIsEmpty() : Boolean;

    procedure SetValidators(const AValue : TValidators);
    function GetIsValid() : Boolean;

    procedure DoValidate();
  protected
    {@M}
    procedure Loaded(); override;
    procedure Change(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@G}
    property IsValid : Boolean read GetIsValid;
  published
    {@G/S}
    property Enabled    : Boolean        read GetEnabled  write SetEnabled;
    property Status     : TControlStatus read FEditStatus write SetEditStatus;
    property Validators : TValidators    read FValidators write SetValidators;
    property ShowBorder : Boolean        read FShowBorder write SetShowBorder;

    {@G}
    property IsEmpty : Boolean read GetIsEmpty;
  end;

implementation

uses NeoFlat.Theme, System.SysUtils, NeoFlat.Validators;

{ TFlatEdit.DoValidate }
procedure TFlatEdit.DoValidate();
begin
  if Validate(self.Text, FValidators) then
    self.Status := csNormal
  else
    self.Status := csError;
end;

{ TFlatEdit.AdjustBound }
procedure TFlatEdit.AdjustBound();
var ADC       : HDC;
    ASaveFont : HFONT;
    AMetrics  : TTextMetric;
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
  Height := (AMetrics.tmHeight + self.ScaleValue(6));
end;

{ TFlatEdit.Loaded }
procedure TFlatEdit.Loaded();
begin
  inherited;
  ///

  if NOT self.IsDesigning() then
    AdjustBound();
end;

{ TFlatEdit.Change }
procedure TFlatEdit.Change();
begin
  inherited;
  ///

  if (self.Status = csError) then
    self.DoValidate();
end;

{ TFlatEdit.IsDesigning }
function TFlatEdit.IsDesigning() : Boolean;
begin
  result := (csDesigning in ComponentState);
end;

{ TFlatEdit.Create }
constructor TFlatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Color       := clWhite;
  self.Font.Height := -11;
  self.Font.Color  := clBlack;
  self.Font.Name   := FONT_1;

  FEditStatus := csNormal;

  FShowBorder := True;

  self.ShowHint := True;

  //self.BorderStyle  := bsNone;
  self.ControlStyle := ControlStyle - [csFramed];
  self.Ctl3D        := False;
  self.AutoSize     := False;

  FMouseHover := False;

  FBackground := TFlatStateColors.Create(self);
  FBackground.Normal := clWhite;
  FBackground.Hover  := clWhite;
  FBackground.Focus  := clWhite;

  FBorder := TFlatStateColors.Create(self);
  FBorder.Normal := clBlack;
  FBorder.Hover  := clBlack;
  FBorder.Focus  := clBlack;

  FValidators := [];
end;

{ TFlatEdit.Destroy }
destructor TFlatEdit.Destroy();
begin
  if Assigned(FBackground) then
    FreeAndNil(FBackground);

  if Assigned(FBorder) then
    FreeAndNil(FBorder);

  ///
  inherited Destroy();
end;

{ TFlatEdit.DrawFlatBorder }
procedure TFlatEdit.DrawFlatBorder(ARegion : HRGN);
var ADC           : HDC;
    ARect         : TRect;
    AClientRect   : TRect;
    AColor        : LongInt;
    ABorderWidth  : Cardinal;
    ABrush        : HBRUSH;
begin
  if not FShowBorder then
    Exit();
  ///

  ADC := GetWindowDC(self.Handle);
  try
    if self.Focused then begin
      AColor := ColorToRGB(FBorder.Focus);
    end else begin
      if FMouseHover then begin
        AColor := ColorToRGB(FBorder.Hover);
      end else begin
        AColor := ColorToRGB(FBorder.Normal);
      end;
    end;

    // Override Outer Color if status is <> Normal
    if (FEditStatus <> csNormal) and enabled then begin
      case FEditStatus of
        csError : AColor := MAIN_RED;
      end;
    end;

    ABorderWidth := self.ScaleValue(1);

    ABrush := CreateSolidBrush(AColor);
    try
      GetWindowRect(self.Handle, AClientRect);

      // Border Top
      ARect.Top    := 0;
      ARect.Left   := 0;
      ARect.Width  := AClientRect.Width;
      ARect.Height := ABorderWidth;
      FillRect(ADC, ARect, ABrush);

      // Border Left
      ARect.Top   := 0;
      ARect.Left  := 0;
      ARect.Width := ABorderWidth;
      ARect.Height := AClientRect.Height;
      FillRect(ADC, ARect, ABrush);

      // Border Right
      ARect.Top    := 0;
      ARect.Left   := AClientRect.Width - ABorderWidth;
      ARect.Height := AClientRect.Height;
      ARect.Width  := ABorderWidth;
      FillRect(ADC, ARect, ABrush);

      // Border Bottom
      ARect.Top    := AClientRect.Height - ABorderWidth;
      ARect.Left   := 0;
      ARect.Width  := AClientRect.Width;
      ARect.Height := ABorderWidth;
      FillRect(ADC, ARect, ABrush);
    finally
      DeleteObject(ABrush);
    end;
  finally
    ReleaseDC(self.Handle, ADC);
  end;
end;

{ TFlatEdit.WMNCPaint }
procedure TFlatEdit.WMNCPaint(var AMessage: TWMNCPaint);
begin
  inherited;
  ///

  DrawFlatBorder(AMessage.RGN);
end;

{  }
procedure TFlatEdit.CMMouseEnter(var AMessage: TMessage);
begin
  inherited;
  ///

  if (GetActiveWindow = 0) then
    Exit();
  ///

  FMouseHover := True;

  DrawFlatBorder(0);
end;

{  }
procedure TFlatEdit.CMMouseLeave(var AMessage: TMessage);
begin
  inherited;
  ///

  FMouseHover := false;

  DrawFlatBorder(0);
end;

{  }
procedure TFlatEdit.WMSetFocus(var AMessage: TWMSetFocus);
begin
  inherited;
  if NOT self.IsDesigning() then
    DrawFlatBorder(0);
end;

{  }
procedure TFlatEdit.WMKillFocus(var AMessage: TWMKillFocus);
begin
  inherited;
  ///

  if NOT self.IsDesigning() then
    DrawFlatBorder(0);
end;

{  }
procedure TFlatEdit.CMEnabledChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  DrawFlatBorder(0);
end;

{  }
procedure TFlatEdit.CMFontChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if NOT self.IsDesigning() and (csLoading in ComponentState) then
    AdjustBound();
end;

{  }
function TFlatEdit.GetEnabled() : Boolean;
begin
  result := inherited Enabled;
end;

{  }
function TFlatEdit.GetIsEmpty() : Boolean;
begin
  result := (Length(Trim(self.text)) = 0);
end;

{  }
procedure TFlatEdit.SetEnabled(const AValue : Boolean);
begin
  if AValue = inherited Enabled then
    Exit();

  inherited Enabled := AValue;

  if AValue then
    self.Font.Color := clBlack
  else
    self.Font.Color := clGray;
end;

{  }
procedure TFlatEdit.SetValidators(const AValue : TValidators);
begin
  if AValue = FValidators then
    Exit();
  ///

  FValidators := AValue;

  ///
  Invalidate();
end;

{  }
procedure TFlatEdit.SetEditStatus(const AValue : TControlStatus);
begin
  if AValue = FEditStatus then
    Exit();
  ///

  FEditStatus := AValue;

  ///
  DrawFlatBorder(0);

  Invalidate();
end;

{  }
function TFlatEdit.GetIsValid() : Boolean;
begin
  self.DoValidate();
  ///

  result := (self.Status = csNormal);
end;

{ TFlatEdit.SetShowBorder }
procedure TFlatEdit.SetShowBorder(const AValue : Boolean);
begin
  if AValue = FShowBorder then
    Exit();

  FShowBorder := AValue;

  ///
  Invalidate();
end;

end.
