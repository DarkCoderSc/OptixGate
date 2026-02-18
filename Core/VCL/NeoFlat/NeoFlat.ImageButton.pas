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

unit NeoFlat.ImageButton;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  WinAPI.Windows, WinAPI.Messages,

  VCL.Graphics, VCL.Controls, VCL.ImgList,

  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOnValueChanged = procedure(Sender : TObject; ANewValue : Integer) of object;

  TFlatImageButton = class(TGraphicControl)
  private
    FBackground     : TColor;
    FImageList      : TCustomImageList;
    FImageIndex     : Integer;
    FMouseIsDown    : Boolean;
    FOnClick        : TNotifyEvent;
    FValue          : Integer;
    FOnValueChanged : TOnValueChanged;

    {@M}
    procedure SetBackground(AValue : TColor);
    procedure SetImageIndex(Avalue : Integer);
    procedure SetValue(AValue : Integer);
  protected
    {@M}
    procedure Paint; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

  published
    property Align;
    property AlignWithMargins;
    property Enabled;
    property Visible;
    property Margins;

    {@G/S}
    property Background     : TColor           read FBackground     write SetBackground;
    property ImageList      : TCustomImageList read FImageList      write FImageList;
    property ImageIndex     : Integer          read FImageIndex     write SetImageIndex;
    property OnClick        : TNotifyEvent     read FOnClick        write FOnClick;
    property Value          : Integer          read FValue          write SetValue;
    property OnValueChanged : TOnValueChanged  read FOnValueChanged write FOnValueChanged;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Types,

  NeoFlat.Helper;
// ---------------------------------------------------------------------------------------------------------------------

constructor TFlatImageButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ShowHint        := True;
  FImageList      := nil;
  FBackground     := MAIN_GRAY;
  Height          := 30;
  Width           := 30;
  FImageIndex     := -1;
  FMouseIsDown    := False;
  FOnClick        := nil;
  FValue          := 0;
  FOnValueChanged := nil;
end;

destructor TFlatImageButton.Destroy();
begin

  ///
  inherited Destroy();
end;

procedure TFlatImageButton.Paint;
begin
  Canvas.Lock();
  try
    // Draw Background
    Canvas.Brush.Color := FBackground;

    Canvas.FillRect(Rect(0, 0, Width, Height));

    // Draw Image
    if Assigned(FImageList) and (FImageIndex > -1) then begin
      var X := (Width div 2) - (FImageList.Width div 2);
      var Y := (Height div 2) - (FImageList.Height div 2);

      if FMouseIsDown then begin
        Inc(X);
        Inc(Y);
      end;

      var AGlyph := TBitmap.Create();
      try
        InitializeBitmap32(AGlyph, FImageList.Width, FImageList.Height);

        FImageList.GetBitmap(FImageIndex, AGlyph);

        if not Enabled then
          FadeBitmap32Opacity(AGlyph, 100);

        ///
        Canvas.Draw(X, Y, AGlyph);
      finally
        if Assigned(AGlyph) then
          FreeAndNil(AGlyph);
      end;
    end;
  finally
    Canvas.Unlock();
  end;
end;

procedure TFlatImageButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  FMouseIsDown := True;

  ///
  Invalidate;
end;

procedure TFlatImageButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  FMouseIsDown := False;

  if ptinrect(ClientRect, Point(X, Y)) and Assigned(FOnClick) then begin
    FOnClick(self);
  end;

  ///
  Invalidate;
end;

procedure TFlatImageButton.SetBackground(AValue : TColor);
begin
  if AValue = FBackground then
    Exit;

  FBackground := AValue;

  ///
  Invalidate;
end;

procedure TFlatImageButton.SetImageIndex(AValue : Integer);
begin
  if AValue = FImageIndex then
    Exit;

  FImageIndex := AValue;

  ///
  Invalidate;
end;

procedure TFlatImageButton.SetValue(AValue : Integer);
begin
  if FValue = AValue then
    Exit;
  ///

  FValue := AValue;

  if Assigned(FOnValueChanged) then
    FOnValueChanged(self, AValue);
end;

end.
