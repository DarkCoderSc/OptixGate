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

unit NeoFlat.PopupMenu;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Winapi.Windows,

  VCL.Menus, VCL.Graphics, VCL.Controls,

  NeoFlat.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatPopupMenu = class(TPopupMenu)
  private
    FMenuBrushHandle : THandle;
    FMetrics         : TFlatMetrics;

    {@M}
    procedure CustomizeMenu(const AMenu : TObject);
    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure InitializeMenu(const AMenuHandle : HMENU);
  protected
    {@M}
    procedure DoPopup(Sender: TObject); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Types,

  NeoFlat.Theme, NeoFlat.CheckBox, NeoFlat.Types;
// ---------------------------------------------------------------------------------------------------------------------

procedure TFlatPopupMenu.InitializeMenu(const AMenuHandle : HMENU);
begin
  var AMenuInfo : TMenuInfo;

  FillChar(AMenuInfo, SizeOf(TMenuInfo), #0);

  AMenuInfo.cbSize  := SizeOf(TMenuInfo);
  AMenuInfo.hbrBack := FMenuBrushHandle;
  AMenuInfo.fMask   := MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;

  ///
  SetMenuInfo(AMenuHandle, AMenuInfo);
end;

procedure TFlatPopupMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
begin
  var M := TMenuItem(Sender);
  ///

  if M.Caption = '-' then
    Height := FMetrics._10
  else
    Height := FMetrics._19;
end;

procedure TFlatPopupMenu.DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  var M := TMenuItem(Sender);
  var ACaption := M.Caption;

  var ASeparator := (ACaption = '-');

  ACanvas.Brush.Style := bsSolid;

  if ASeparator then begin
    ACanvas.Brush.Color := MAIN_GRAY;

    ACanvas.FillRect(ARect);

    var ASeparatorRect := TRect.Empty;

    ASeparatorRect.Left   := ARect.Left + FMetrics._8;
    ASeparatorRect.Top    := ARect.Top + (ARect.Height div 2) - FMetrics._1;
    ASeparatorRect.Height := FMetrics._2;
    ASeparatorRect.Width  := ARect.Width - FMetrics._16;

    ACanvas.Brush.Color := MAIN_ACCENT;

    ACanvas.FillRect(ASeparatorRect);
  end else begin
    var AColor : TColor;
    if Selected and M.Enabled then
      AColor := DARKER_GRAY
    else
      AColor := MAIN_GRAY;

    ACanvas.Brush.Color := AColor;

    var AFontColor : TColor;
    if M.Enabled then
      AFontColor := MAIN_ACCENT
    else
      AFontColor := clGray;

    ACanvas.Font.Color := AFontColor;
    ACanvas.Font.Name  := FONT_1;
    ACanvas.Font.Size  := 9;

    ACanvas.FillRect(ARect);

    Inc(ARect.Left, FMetrics._16);

    ACanvas.TextRect(ARect, ACaption, [tfSingleLine, tfVerticalCenter]);

    if M.Checked then begin
      var AGlyph : TByteArrayArray;

      ScaleMatrixGlyph(CHECKBOX_GLYPH_TEMPLATE, AGlyph, round(FMetrics.ScaleFactor));

      var Y := ARect.Top + ((ARect.Height div 2) - (Length(AGlyph) div 2));
      DrawMatrixGlyph(ACanvas, AGlyph, FMetrics._4, Y, MAIN_ACCENT);
    end;
  end;
end;

constructor TFlatPopupMenu.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  OwnerDraw := True;

  FMetrics := TFlatMetrics.Create(TControl(AOwner));

  FMenuBrushHandle := CreateSolidBrush(ColorToRGB(MAIN_ACCENT));

  InitializeMenu(Handle);
end;

destructor TFlatPopupMenu.Destroy();
begin
  DeleteObject(FMenuBrushHandle);

  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  ///
  inherited Destroy();
end;

procedure TFlatPopupMenu.CustomizeMenu(const AMenu : TObject);

  procedure ApplyCustomization(const AMenuItem : TMenuItem);
  begin
    if not Assigned(AMenuItem.OnDrawItem) then
      AMenuItem.OnDrawItem := DrawItem;

    if not Assigned(AMenuItem.OnMeasureItem) then
      AMenuItem.OnMeasureItem := MeasureItem;

    // Recursive
    if AMenuItem.Count > 0 then begin
      InitializeMenu(AMenuItem.Handle);

      CustomizeMenu(AMenuItem);
    end;
  end;

begin
  if AMenu is TPopupMenu then begin
    for var I := 0 to TPopupMenu(AMenu).Items.Count -1 do
      ApplyCustomization(TPopupMenu(AMenu).Items[I]);
  end else if AMenu is TMenuItem then begin
    for var I := 0 to TMenuItem(AMenu).Count -1 do
      ApplyCustomization(TMenuItem(TMenuItem(AMenu).Items[I]));
  end;
end;

procedure TFlatPopupMenu.DoPopup(Sender: TObject);
begin
  inherited;
  ///

  CustomizeMenu(Self);
end;

end.
