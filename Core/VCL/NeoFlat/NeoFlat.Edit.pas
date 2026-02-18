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

unit NeoFlat.Edit;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows, Winapi.Messages,

  VCL.Graphics, VCL.Controls, VCL.StdCtrls, VCL.Forms,

  NeoFlat.Classes, NeoFlat.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatEdit = class(TEdit)
  private
    FMouseHover : Boolean;

    FBackground : TFlatStateColors;
    FBorder     : TFlatStateColors;
    FShowBorder : Boolean;

    FEditStatus : TControlStatus;

    FValidators : TValidators;


    {@M}
    procedure WMNCPaint(var AMessage: TWMNCPaint); message WM_NCPAINT;
    procedure CMMouseEnter(var AMessage: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMessage: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var AMessage: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var AMessage: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMEnabledChanged(var AMessage: TMessage);message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var AMessage: TMessage); message CM_FONTCHANGED;

    procedure DrawFlatBorder(ARegion : HRGN);

    function IsDesigning() : Boolean;

    procedure AdjustBound();

    procedure SetEditStatus(const AValue : TControlStatus);
    procedure SetShowBorder(const AValue : Boolean);
    function GetIsEmpty() : Boolean;

    procedure SetValidators(const AValue : TValidators);
    function GetIsValid() : Boolean;

    procedure DoValidate();
  protected
    {@M}
    procedure Loaded(); override;
    procedure Change(); override;

    procedure SetEnabled(AValue : Boolean); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@G}
    property IsValid : Boolean read GetIsValid;
    property IsEmpty : Boolean read GetIsEmpty;
  published
    {@G/S}
    property Status     : TControlStatus read FEditStatus write SetEditStatus;
    property Validators : TValidators    read FValidators write SetValidators;
    property ShowBorder : Boolean        read FShowBorder write SetShowBorder;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Types,

  NeoFlat.Theme, NeoFlat.Validators;
// ---------------------------------------------------------------------------------------------------------------------

procedure TFlatEdit.DoValidate();
begin
  if Validate(Text, FValidators) then
    Status := cStatusNormal
  else
    Status := cStatusError;
end;

procedure TFlatEdit.AdjustBound();
begin
  var AMetrics : TTextMetric;

  var ADC := GetDC(0);
  try
    var ASaveFont := SelectObject(ADC, Font.Handle);

    GetTextMetrics(ADC, AMetrics);
    SelectObject(ADC, ASaveFont);
  finally
    ReleaseDC(0, ADC);
  end;

  ///
  Height := (AMetrics.tmHeight + ScaleValue(6));
end;

procedure TFlatEdit.Loaded();
begin
  inherited;
  ///

  if NOT IsDesigning() then
    AdjustBound();
end;

procedure TFlatEdit.Change();
begin
  inherited;
  ///

  if (Status = cStatusError) then
    DoValidate();
end;

function TFlatEdit.IsDesigning() : Boolean;
begin
  result := (csDesigning in ComponentState);
end;

constructor TFlatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  Color       := clWhite;
  Font.Height := -11;
  Font.Color  := clBlack;
  Font.Name   := FONT_1;

  FEditStatus := cStatusNormal;

  FShowBorder := True;

  ShowHint := True;

  ControlStyle := ControlStyle - [csFramed];
  Ctl3D        := False;
  AutoSize     := False;

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

destructor TFlatEdit.Destroy();
begin
  if Assigned(FBackground) then
    FreeAndNil(FBackground);

  if Assigned(FBorder) then
    FreeAndNil(FBorder);

  ///
  inherited Destroy();
end;

procedure TFlatEdit.DrawFlatBorder(ARegion : HRGN);
begin
  if not FShowBorder then
    Exit;
  ///

  var ADC := GetWindowDC(Handle);
  try
    var AColor : TColor;
    if Focused then begin
      AColor := ColorToRGB(FBorder.Focus);
    end else begin
      if FMouseHover then begin
        AColor := ColorToRGB(FBorder.Hover);
      end else begin
        AColor := ColorToRGB(FBorder.Normal);
      end;
    end;

    // Override Outer Color if status is <> Normal
    if (FEditStatus <> cStatusNormal) and enabled then begin
      case FEditStatus of
        cStatusError : AColor := MAIN_RED;
      end;
    end;

    var ABorderWidth := ScaleValue(1);

    var ABrush := CreateSolidBrush(AColor);
    try
      var AClientRect : TRect;
      GetWindowRect(Handle, AClientRect);

      // Border Top
      var ARect := TRect.Empty;
      ARect.Width  := AClientRect.Width;
      ARect.Height := ABorderWidth;
      FillRect(ADC, ARect, ABrush);

      // Border Left
      ARect := TRect.Empty;
      ARect.Width := ABorderWidth;
      ARect.Height := AClientRect.Height;
      FillRect(ADC, ARect, ABrush);

      // Border Right
      ARect := TRect.Empty;
      ARect.Left   := AClientRect.Width - ABorderWidth;
      ARect.Height := AClientRect.Height;
      ARect.Width  := ABorderWidth;
      FillRect(ADC, ARect, ABrush);

      // Border Bottom
      ARect := TRect.Empty;
      ARect.Top    := AClientRect.Height - ABorderWidth;
      ARect.Width  := AClientRect.Width;
      ARect.Height := ABorderWidth;
      FillRect(ADC, ARect, ABrush);
    finally
      DeleteObject(ABrush);
    end;
  finally
    ReleaseDC(Handle, ADC);
  end;
end;

procedure TFlatEdit.WMNCPaint(var AMessage: TWMNCPaint);
begin
  inherited;
  ///

  DrawFlatBorder(AMessage.RGN);
end;

procedure TFlatEdit.CMMouseEnter(var AMessage: TMessage);
begin
  inherited;
  ///

  if (GetActiveWindow = 0) then
    Exit;
  ///

  FMouseHover := True;

  DrawFlatBorder(0);
end;

procedure TFlatEdit.CMMouseLeave(var AMessage: TMessage);
begin
  inherited;
  ///

  FMouseHover := false;

  DrawFlatBorder(0);
end;

procedure TFlatEdit.WMSetFocus(var AMessage: TWMSetFocus);
begin
  inherited;
  ///

  if NOT IsDesigning() then
    DrawFlatBorder(0);
end;

procedure TFlatEdit.WMKillFocus(var AMessage: TWMKillFocus);
begin
  inherited;
  ///

  if NOT IsDesigning() then
    DrawFlatBorder(0);
end;

procedure TFlatEdit.CMEnabledChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  DrawFlatBorder(0);
end;

procedure TFlatEdit.CMFontChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if NOT IsDesigning() and (csLoading in ComponentState) then
    AdjustBound();
end;

function TFlatEdit.GetIsEmpty() : Boolean;
begin
  result := Length(Trim(Text)) = 0;
end;

procedure TFlatEdit.SetEnabled(AValue : Boolean);
begin
  if AValue = Enabled then
    Exit;
  ///

  inherited SetEnabled(AValue);

  if AValue then
    Font.Color := clBlack
  else
    Font.Color := clGray;
end;

procedure TFlatEdit.SetValidators(const AValue : TValidators);
begin
  if AValue = FValidators then
    Exit;
  ///

  FValidators := AValue;

  ///
  Invalidate;
end;

procedure TFlatEdit.SetEditStatus(const AValue : TControlStatus);
begin
  if AValue = FEditStatus then
    Exit;
  ///

  FEditStatus := AValue;

  ///
  DrawFlatBorder(0);

  Invalidate;
end;

function TFlatEdit.GetIsValid() : Boolean;
begin
  DoValidate();
  ///

  result := (Status = cStatusNormal);
end;

procedure TFlatEdit.SetShowBorder(const AValue : Boolean);
begin
  if AValue = FShowBorder then
    Exit;
  ///

  FShowBorder := AValue;

  ///
  Invalidate;
end;

end.
