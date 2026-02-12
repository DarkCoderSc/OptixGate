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

unit NeoFlat.PopupMenu;

interface

uses VCL.Menus, System.Classes, VCL.Graphics, Winapi.Windows, System.SysUtils, VCL.Controls,
     NeoFlat.Metrics;

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

uses NeoFlat.Theme;

{ TFlatPopupMenu.InitializeMenu }
procedure TFlatPopupMenu.InitializeMenu(const AMenuHandle : HMENU);
var AMenuInfo  : TMenuInfo;
begin
  FillChar(AMenuInfo, SizeOf(TMenuInfo), #0);

  AMenuInfo.cbSize  := SizeOf(TMenuInfo);
  AMenuInfo.hbrBack := FMenuBrushHandle;
  AMenuInfo.fMask   := MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;

  ///
  SetMenuInfo(AMenuHandle, AMenuInfo);
end;

{ TFlatPopupMenu.MeasureItem }
procedure TFlatPopupMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var M : TMenuItem;
begin
  M := TMenuItem(Sender);
  ///

  if M.Caption = '-' then
    Height := FMetrics._10
  else begin
    Height := FMetrics._19;

    //if (M.MenuIndex = 0) or (M.MenuIndex = self.Items.Count -1) then
    //  Inc(Height, FMetrics._8);
  end;
end;

{ TFlatPopupMenu.DrawItem }
procedure TFlatPopupMenu.DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var AColor         : TColor;
    ACaption       : String;
    ASeparator     : Boolean;
    ASeparatorRect : TRect;
    M              : TMenuItem;
    AFontColor     : TColor;
begin
  M := TMenuItem(Sender);
  ACaption := M.Caption;

  ASeparator := (ACaption = '-');

  ACanvas.Brush.Style := bsSolid;

  if ASeparator then begin
    {
      Draw Separator Menu
    }
    ACanvas.Brush.Color := MAIN_GRAY;

    ACanvas.FillRect(ARect);

    ASeparatorRect.Left   := ARect.Left + FMetrics._8;
    ASeparatorRect.Top    := ARect.Top + (ARect.Height div 2) - FMetrics._1;
    ASeparatorRect.Height := FMetrics._2;
    ASeparatorRect.Width  := ARect.Width - FMetrics._16;

    ACanvas.Brush.Color := MAIN_ACCENT;

    ACanvas.FillRect(ASeparatorRect);
  end else begin
    {
      Draw Menu Item
    }
    if Selected and M.Enabled then
      AColor := DARKER_GRAY
    else
      AColor := MAIN_GRAY;

//    if (M.MenuIndex = 0) or (M.MenuIndex = self.Items.Count -1) then begin
//      ACanvas.Brush.Color := MAIN_GRAY;
//
//      ACanvas.FillRect(ARect);
//
//      if (M.MenuIndex = 0) then
//        ARect.Top := ARect.Top + FMetrics._8
//      else if (M.MenuIndex = self.Items.Count -1) then
//        ARect.Bottom := ARect.Bottom - FMetrics._8
//    end;

    ACanvas.Brush.Color := AColor;

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
  end;
end;

{ TFlatPopupMenu.Create }
constructor TFlatPopupMenu.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.OwnerDraw := True;

  FMetrics := TFlatMetrics.Create(TControl(AOwner));

  FMenuBrushHandle := CreateSolidBrush(ColorToRGB(MAIN_ACCENT));

  self.InitializeMenu(self.Handle);
end;

{ TFlatPopupMenu.Destroy }
destructor TFlatPopupMenu.Destroy();
begin
  DeleteObject(FMenuBrushHandle);

  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  ///
  inherited Destroy();
end;

{ TFlatPopupMenu.CustomizeMenu }
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

{ TFlatPopupMenu.DoPopup }
procedure TFlatPopupMenu.DoPopup(Sender: TObject);
begin
  inherited;
  ///

  CustomizeMenu(Self);
end;

end.
