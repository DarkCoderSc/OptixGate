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
{                   License: GPLv3                                             }
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

unit NeoFlat.Window;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows, Winapi.Messages,

  VCL.Controls, VCL.Forms, VCL.Graphics, VCL.Menus,

  NeoFlat.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOnHandleSystemButton = procedure(Sender : TObject; var AHandled : Boolean) of object;

  TFlatWindow = class;

  TFlatCaptionButton = class
  private
    FRect        : TRect;
    FVisible     : Boolean;
    FOwner       : TFlatWindow;
    FState       : TFlatControlState;

    {@M}
    procedure SetState(AValue : TFlatControlState);
  public
    {@C}
    constructor Create(AOwner : TFlatWindow);

    {@G/S}
    property Rect    : TRect             read FRect    write FRect;
    property Visible : Boolean           read FVisible write FVisible;
    property State   : TFlatControlState read FState   write SetState;
  end;

  TFlatCloseButton     = class(TFlatCaptionButton);
  TFlatMaximizeButton  = class(TFlatCaptionButton);
  TFlatMinimizeButton  = class(TFlatCaptionButton);
  TFlatHamburgerButton = class(TFlatCaptionButton);

  TFlatWindow = class(TComponent)
  private
    FOldWindowProc   : TWndMethod;
    FOwnerForm       : TForm;

    FNonClientCanvas : TCanvas;
    FCanvas          : TCanvas;

    FBorder          : TColor;
    FCaption         : TColor;
    FBackground      : TColor;

    FBorderWidth     : Integer;
    FCaptionHeight   : Integer;

    FShowBorder      : Boolean;

    FCaptionBarRect  : TRect;
    FCaptionRect     : TRect;

    FMaximized       : Boolean;

    FFont            : TFont;
    FCaptionAlign    : TAlignment;

    FBorderStyle     : TFormBorderStyle;

    FCloseButton     : TFlatCloseButton;
    FMaximizeButton  : TFlatMaximizeButton;
    FMinimizeButton  : TFlatMinimizeButton;
    FHamburgerButton : TFlatHamburgerButton;

    FButtonDown      : TFlatCaptionButton;
    FButtonHover     : TFlatCaptionButton;

    FMenuDropdown    : TPopupMenu;

    FOnMaximize      : TOnHandleSystemButton;
    FOnMinimize      : TOnHandleSystemButton;
    FOnRestore       : TOnHandleSystemButton;
    FOnClose         : TOnHandleSystemButton;

    FOldWindowRect   : TRect;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);
    procedure HandleNCCalcSize(var AMessage: TMessage);

    procedure NCPaint;
    procedure DrawCaptionBar(const ACanvas : TCanvas);
    function DrawCaptionButton(const ACanvas : TCanvas;  const AButton : TFlatCaptionButton;
      const AGlyphMatrix : TMatrixGlyph) : TRect;

    function GetCaptionButtonFromCoord(const X, Y : Integer) : TFlatCaptionButton;

    procedure doRestore();
    procedure doMaximize();
    procedure doMinimize();
    procedure doMaximizeRestore();
    procedure doClose();
    procedure DisplayMenuDropDown();

    procedure SetupCaptionBar();

    procedure UpdateForm();
    procedure SetBorderWidth(const AValue : Integer);
    procedure SetCaptionHeight(const AValue : Integer);
    procedure SetColor(const AIndex : Integer; const AValue : TColor);
    procedure SetFont(const AFont : TFont);
    procedure SetCaptionAlign(const AValue : TAlignment);
    procedure SetMenuDropDown(const AValue : TPopupMenu);
    procedure SetShowBorder(const AValue : Boolean);
    procedure SetBorderStyle(const AValue : TFormBorderStyle);
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure Invalidate();
  published
    {@G/S}
    property BorderWidth   : Integer          read FBorderWidth   write SetBorderWidth;
    property CaptionHeight : Integer          read FCaptionHeight write SetCaptionHeight;
    property Font          : TFont            read FFOnt          write SetFont;
    property CaptionAlign  : TAlignment       read FCaptionAlign  write SetCaptionAlign;
    property MenuDropDown  : TPopupMenu       read FMenuDropDown  write SetMenuDropDown;
    property ShowBorder    : Boolean          read FShowBorder    write SetShowBorder;
    property BorderStyle   : TFormBorderStyle read FBorderStyle   write SetBorderStyle;

    property OnMaximize : TOnHandleSystemButton read FOnMaximize write FOnMaximize;
    property OnMinimize : TOnHandleSystemButton read FOnMinimize write FOnMinimize;
    property OnRestore  : TOnHandleSystemButton read FOnRestore  write FOnRestore;
    property OnClose    : TOnHandleSystemButton read FOnClose    write FOnClose;

    property Background : TColor index 0 read FBackground write SetColor;
    property Border     : TColor index 1 read FBorder     write SetColor;
    property Caption    : TColor index 2 read FCaption    write SetColor;
  end;

  const CLOSE_GLYPH : TMatrixGlyph = [
                                        [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                        [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                        [$0, $1, $0, $0, $0, $0, $0, $0, $1, $0],
                                        [$0, $0, $1, $0, $0, $0, $0, $1, $0, $0],
                                        [$0, $0, $0, $1, $0, $0, $1, $0, $0, $0],
                                        [$0, $0, $0, $0, $1, $1, $0, $0, $0, $0],
                                        [$0, $0, $0, $0, $1, $1, $0, $0, $0, $0],
                                        [$0, $0, $0, $0, $1, $1, $0, $0, $0, $0],
                                        [$0, $0, $0, $1, $0, $0, $1, $0, $0, $0],
                                        [$0, $0, $1, $0, $0, $0, $0, $1, $0, $0],
                                        [$0, $1, $0, $0, $0, $0, $0, $0, $1, $0],
                                        [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                        [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

  const MINIMIZE_GLYPH : TMatrixGlyph = [
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1]
];

  const RESTORE_GLYPH : TMatrixGlyph = [
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                          [$0, $0, $0, $1, $1, $1, $1, $1, $1, $0],
                                          [$0, $0, $0, $1, $0, $0, $0, $0, $1, $0],
                                          [$0, $1, $1, $1, $1, $1, $1, $0, $1, $0],
                                          [$0, $1, $0, $0, $0, $0, $1, $0, $1, $0],
                                          [$0, $1, $0, $0, $0, $0, $1, $0, $1, $0],
                                          [$0, $1, $0, $0, $0, $0, $1, $1, $1, $0],
                                          [$0, $1, $0, $0, $0, $0, $1, $0, $0, $0],
                                          [$0, $1, $1, $1, $1, $1, $1, $0, $0, $0],
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];


  const MAXIMIZE_GLYPH : TMatrixGlyph = [
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                            [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                            [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                            [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                            [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                            [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                            [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                            [$0, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

  const HAMBURGER_GLYPH : TMatrixGlyph = [
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Types,

  NeoFlat.Theme, NeoFlat.Helper;
// ---------------------------------------------------------------------------------------------------------------------

(* TFlatCaptionButton *)

constructor TFlatCaptionButton.Create(AOwner : TFlatWindow);
begin
  inherited Create();

  if Assigned(AOwner) then
    FOwner := AOwner;

  FRect        := TRect.Empty;
  FVisible     := True;
  FState       := csNormal;
end;

procedure TFlatCaptionButton.SetState(AValue : TFlatControlState);
begin
  if AValue = FState then
    Exit;
  ///

  FState := AValue;

  ///
  if Assigned(FOwner) then
    FOwner.Invalidate;
end;


(* TFlatWindow *)

procedure TFlatWindow.doRestore();
begin
  if not FMaximized then
    Exit;
  ///

  var AHandled := False;
  if Assigned(FOnRestore) then
    FOnRestore(self, AHandled);

  if AHandled then
    Exit;

  FMaximized := False;

  FShowBorder := True;

  SetWindowPos(
    FOwnerForm.Handle,
    0,
    FOldWindowRect.Left,
    FOldWindowRect.Top,
    FOldWindowRect.Width,
    FOldWindowRect.Height,
    SWP_NOZORDER
  );

  ///
  UpdateForm();
end;

procedure TFlatWindow.doMaximize();
begin
  if FMaximized then
    Exit;
  ///

  var AHandled := False;
  if Assigned(FOnMaximize) then
    FOnMaximize(self, AHandled);

  if AHandled then
    Exit;

  GetWindowRect(FOwnerForm.Handle, FOldWindowRect);

  FShowBorder := False;

  with Screen.MonitorFromRect(FOldWindowRect).workAreaRect do
    SetWindowPos(FOwnerForm.Handle, 0, Left, Top, Width, Height, SWP_NOZORDER);

  FMaximized := True;

  ///
  UpdateForm();
end;

procedure TFlatWindow.doMinimize();
begin
  var AHandled := False;
  if Assigned(FOnMinimize) then
    FOnMinimize(self, AHandled);

  if AHandled then
    Exit;

  if FOwnerForm = Application.MainForm then
    Application.Minimize
  else
    ShowWindow(FOwnerForm.Handle, SW_MINIMIZE);
end;

procedure TFlatWindow.doClose();
begin
  var AHandled := False;
  if Assigned(FOnClose) then
    FOnClose(self, AHandled);

  if AHandled then
    Exit;

  ///
  FOwnerForm.Close();
end;

function TFlatWindow.GetCaptionButtonFromCoord(const X, Y : Integer) : TFlatCaptionButton;
begin
  result := nil;
  ///

  var APoint := Point(X, Y);
  ClientToScreen(FOwnerForm.Handle, APoint);
  Dec(APoint.X, FOwnerForm.BoundsRect.Left);
  Dec(APoint.Y, FOwnerForm.BoundsRect.Top);

  if ptinrect(FCloseButton.Rect, APoint) then
    result := FCloseButton
  else if ptinrect(FMaximizeButton.Rect, APoint) then
    result := FMaximizeButton
  else if ptinrect(FMinimizeButton.Rect, APoint) then
    result := FMinimizeButton
  else if ptinrect(FHamburgerButton.Rect, APoint) then
    result := FHamburgerButton;
end;

procedure TFlatWindow.doMaximizeRestore();
begin
  if FMaximized then
    doRestore()
  else
    doMaximize();
end;

procedure TFlatWindow.DisplayMenuDropDown();
begin
  if not Assigned(FMenuDropDown) then
    Exit;
  ///

  var APoint := Point(0, 0);

  ClientToScreen(FOwnerForm.Handle, APoint);

  FMenuDropDown.Popup(APoint.X, APoint.Y);
end;

procedure TFlatWindow.SetupCaptionBar();
begin
  var AClientRect := TRect.Empty;
  GetClientRect(FOwnerForm.Handle, AClientRect);
  ///

  FHamburgerButton.Visible := Assigned(FMenuDropDown);
  FCloseButton.Visible := biSystemMenu in FOwnerForm.BorderIcons;

  FMinimizeButton.Visible := (biMinimize in FOwnerForm.BorderIcons) and (FBorderStyle <> bsDialog) and
    (FBorderStyle <> bsToolWindow);

  FMaximizeButton.Visible  := (biMaximize in FOwnerForm.BorderIcons) and (FBorderStyle <> bsDialog) and
    (FBorderStyle <> bsToolWindow);

  var ABorderWidth : Integer;
  if FShowBorder then
    ABorderWidth := FOwnerForm.ScaleValue(FBorderWidth)
  else
    ABorderWidth := 0;

  FCaptionBarRect.Left   := AClientRect.Left + ABorderWidth;
  FCaptionBarRect.TOp    := AClientRect.Top + ABorderWidth;
  FCaptionBarRect.Width  := AClientRect.Width;
  FCaptionBarRect.Height := FOwnerForm.ScaleValue(FCaptionHeight);

  FCaptionRect := FCaptionBarRect;
  Inc(FCaptionRect.Left, FOwnerForm.ScaleValue(2));
  Dec(FCaptionRect.Right, FOwnerForm.ScaleValue(8));

  var AButtonWidth := (FCaptionBarRect.Height - FOwnerForm.ScaleValue(8));
  var AButtonY := FCaptionBarRect.Top + ((FCaptionBarRect.Height div 2) - (AButtonWidth div 2));

  var ARect : TRect;

  ARect.Left   := 0;
  ARect.Top    := AButtonY;
  ARect.Width  := 0;
  ARect.Height := AButtonWidth;
  ///

  // Hamburger Button
  if FHamburgerButton.Visible then begin
    ARect.Left  := FOwnerForm.ScaleValue(4);
    ARect.Width := AButtonWidth;

    FHamburgerButton.Rect := ARect;

    /// Reset, since we are moving right
    ARect.Left := 0;
  end;

  // Close Button
  if FCloseButton.Visible then begin
    ARect.Left  := (FCaptionBarRect.Right - FOwnerForm.ScaleValue(4) - AButtonWidth);
    ARect.Width := AButtonWidth;

    FCloseButton.Rect := ARect;
  end;

  // Maximize Restore
  if FMaximizeButton.Visible then begin
    ARect.Left  := (ARect.Left - FOwnerForm.ScaleValue(1)) - AButtonWidth;
    ARect.Width := AButtonWidth;

    FMaximizeButton.Rect := ARect;
  end;

  // Minimize
  if FMinimizeButton.Visible then begin
    ARect.Left  := (ARect.Left - FOwnerForm.ScaleValue(1)) - AButtonWidth;
    ARect.Width := AButtonWidth;

    FMinimizeButton.Rect := ARect;
  end;

  ///
  if FCloseButton.Visible or FMaximizeButton.Visible or FMinimizeButton.Visible then
    Dec(FCaptionRect.Right, (FCaptionBarRect.Width - ARect.Left));

  if FHamburgerButton.Visible then
    Inc(FCaptionRect.Left, AButtonWidth + FOwnerForm.ScaleValue(4));
end;

function TFlatWindow.DrawCaptionButton(const ACanvas : TCanvas; const AButton : TFlatCaptionButton;
  const AGlyphMatrix : TMatrixGlyph) : TRect;
begin
  result := TRect.Empty();
  ///

  if not Assigned(AButton) or not IsValidMatrixGlyph(AGlyphMatrix) then
    Exit;
  ///

  var ADrawBorder := False;
  case AButton.State of
    csHover, csActive :
      ADrawBorder := True;
  end;

  ACanvas.Brush.Color := GLYPH_COLOR;

  // Draw Border
  if ADrawBorder then
    ACanvas.FrameRect(AButton.Rect);

//  ACanvas.MoveTo((AButton.Rect.Left - FOwnerForm.ScaleValue(1)), AButton.Rect.Top);
//  ACanvas.LineTo((AButton.Rect.Left - FOwnerForm.ScaleValue(1)), AButton.Rect.Bottom);

  var X := AButton.Rect.Left + ((FCloseButton.Rect.Width div 2) - (Length(AGlyphMatrix[0]) div 2));
  var Y := AButton.Rect.Top + ((FCloseButton.Rect.Height div 2) - (Length(AGlyphMatrix) div 2));

  DrawMatrixGlyph(ACanvas, AGlyphMatrix, X, Y, GLYPH_COLOR);
end;

procedure TFlatWindow.DrawCaptionBar(const ACanvas : TCanvas);
begin
  if not Assigned(ACanvas) then
    Exit();
  ///

  // Background
  ACanvas.Brush.Color := FCaption;
  ACanvas.FillRect(FCaptionBarRect);

  // System Buttons
  if FCloseButton.Visible then
    DrawCaptionButton(ACanvas, FCloseButton, CLOSE_GLYPH);

  if FMaximizeButton.Visible then begin
    if FMaximized then
      DrawCaptionButton(ACanvas, FMaximizeButton, RESTORE_GLYPH)
    else
      DrawCaptionButton(ACanvas, FMaximizeButton, MAXIMIZE_GLYPH)
  end;

  if FMinimizeButton.Visible then
    DrawCaptionButton(ACanvas, FMinimizeButton, MINIMIZE_GLYPH);

  if FHamburgerButton.Visible then
    DrawCaptionButton(ACanvas, FHamburgerButton, HAMBURGER_GLYPH);

  // Text
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Assign(FFont);
  ACanvas.Font.Height := FOwnerForm.ScaleValue(FFont.Height);

  var ACaption : String := FOwnerForm.Caption;

  var ATextFormat : TTextFormat := [tfVerticalCenter, tfEndEllipsis, tfSingleLine];
  case FCaptionAlign of
    taLeftJustify  : Include(ATextFormat, tfLeft);
    taRightJustify : Include(ATextFormat, tfRight);
    taCenter       : Include(ATextFormat, tfCenter);
  end;

  ///
  ACanvas.TextRect(FCaptionRect, ACaption, ATextFormat);
end;

procedure TFlatWindow.NCPaint;
begin
  if not Assigned(FNonClientCanvas) then
    Exit;
  ///

  var ABorderWidth : Integer;
  if FShowBorder then
    ABorderWidth := FOwnerForm.ScaleValue(FBorderWidth)
  else
    ABorderWidth := 0;

  var ADC := GetWindowDC(FOwnerForm.Handle);
  try
    FNonClientCanvas.Handle := ADC;

    FNonClientCanvas.Brush.Style := bsSolid;

    if ABorderWidth > 0 then begin
      FNonClientCanvas.Brush.Color := FBorder;

      var AClientRect := TRect.Empty;
      GetWindowRect(FOwnerForm.Handle, AClientRect);
      //OffsetRect(AClientRect, -AClientRect.Left, -AClientRect.Top);

      // Draw Borders (using FillRect(<>) instead of FrameRect(<>) because of HDPI glitchs)

      // L
      var ARect    := TRect.Empty;
      ARect.Width  := ABorderWidth;
      ARect.Height := AClientRect.height;
      FNonClientCanvas.FillRect(ARect);

      // T
      ARect := TRect.Empty;
      ARect.Height := ABorderWidth;
      ARect.Width  := AClientRect.Width;
      FNonClientCanvas.FillRect(ARect);

      // R
      ARect := TRect.Empty;
      ARect.Left   := AClientRect.Width - ABorderWidth;
      ARect.Width  := ABorderWidth;
      ARect.Height := AClientRect.Height;
      FNonClientCanvas.FillRect(ARect);

      // B
      ARect := TRect.Empty;
      ARect.Top    := AClientRect.Height - ABorderWidth;
      ARect.Width  := AClientRect.Width;
      ARect.Height := ABorderWidth;
      FNonClientCanvas.FillRect(ARect);
    end;

    // Draw Caption Bar
    DrawCaptionBar(FNonClientCanvas);
  finally
    FNonClientCanvas.Handle := 0;

    ReleaseDC(FOwnerForm.Handle, ADC);
  end;
end;

procedure TFlatWindow.HandleNCCalcSize(var AMessage: TMessage);
begin
  var ABorderWidth : Integer;
  if not FShowBorder then
    ABorderWidth := 0
  else
    ABorderWidth := FOwnerForm.ScaleValue(FBorderWidth);

  if AMessage.WParam <> 0 then begin
    Inc(PNCCalcSizeParams(AMessage.LParam)^.rgrc[0].Left,   ABorderWidth);
    Inc(PNCCalcSizeParams(AMessage.LParam)^.rgrc[0].Top,    ABorderWidth + FOwnerForm.ScaleValue(FCaptionHeight));
    Dec(PNCCalcSizeParams(AMessage.LParam)^.rgrc[0].Right,  ABorderWidth);
    Dec(PNCCalcSizeParams(AMessage.LParam)^.rgrc[0].Bottom, ABorderWidth);
  end else begin
    Inc(PRect(AMessage.LParam)^.Left, ABorderWidth);
    Inc(PRect(AMessage.LParam)^.Top, ABorderWidth + FOwnerForm.ScaleValue(FCaptionHeight));
    Dec(PRect(AMessage.LParam)^.Right, ABorderWidth);
    Dec(PRect(AMessage.LParam)^.Bottom, ABorderWidth);
  end;

  SetupCaptionBar();

  ///
  AMessage.Result := 0;
end;

procedure TFlatWindow.OnCustomWindowProc(var AMessage : TMessage);
begin
  var ADoCallBase := True;
  try
    case AMessage.Msg of
      WM_NCPAINT: begin
        NCPaint;

        ///
        ADoCallBase := False;
      end;

      WM_ERASEBKGND: begin
        var ADC := TWMEraseBkgnd(AMessage).DC;
        var ARect : TRect;
        GetClipBox(ADC, ARect);

        var ABrush := CreateSolidBrush(ColorToRGB(FBackground));
        try
          FillRect(ADC, ARect, ABrush);
        finally
          DeleteObject(ABrush);
        end;

        AMessage.Result := 1;

        ///
        ADoCallBase := False;
      end;

      WM_WINDOWPOSCHANGED: begin
        if (PWindowPos(AMessage.LParam)^.flags and SWP_NOSIZE) = 0 then
          Invalidate();
      end;

      WM_NCCALCSIZE: begin
        HandleNCCalcSize(AMessage);

        ADoCallBase := False;
      end;

      WM_SETTEXT, CM_TEXTCHANGED, CM_STYLECHANGED, WM_DPICHANGED, WM_NCACTIVATE, WM_SETTINGCHANGE, WM_THEMECHANGED:
        Invalidate();
    end;

    ///
    if (csDesigning in ComponentState) then
      Exit;
    ///

    var AButton : TFlatCaptionButton;

    case AMessage.Msg of
      WM_NCLBUTTONDBLCLK:
        if FMaximizeButton.Visible then
          doMaximizeRestore();

      WM_LBUTTONDOWN : begin
        AButton := GetCaptionButtonFromCoord(TWMLButtonDown(AMessage).XPos, TWMLButtonDown(AMessage).YPos);
        if Assigned(AButton) and AButton.Visible then begin
          AButton.State := csActive;

          ///
          FButtonDown := AButton;
        end;
      end;

       WM_LBUTTONUP : begin
        AButton := GetCaptionButtonFromCoord(TWMLButtonUp(AMessage).XPos, TWMLButtonUp(AMessage).YPos);
        if Assigned(AButton) and AButton.Visible then begin
          AButton.State := csHover;

          if (AButton = FButtonDown) then begin
            if AButton is TFlatCloseButton then
              DoClose()
            else if AButton is TFlatMaximizeButton then
              doMaximizeRestore()
            else if AButton is TFlatMinimizeButton then
              DoMinimize()
            else if AButton is TFlatHamburgerButton then
              DisplayMenuDropDown();
          end;
        end;

        if Assigned(FButtonDown) then begin
          FButtonDown.State := csNormal;

          FButtonDown := nil;
        end;
      end;

      WM_MOUSEMOVE : begin
        if Assigned(FButtonDown) then
          Exit;
        ///

        AButton := GetCaptionButtonFromCoord(TWMMouseMove(AMessage).XPos, TWMMouseMove(AMessage).YPos);
        if Assigned(AButton) and AButton.Visible then begin
          if (AButton <> FButtonHover) then begin
            if Assigned(FButtonHover) then
              FButtonHover.State := csNormal;

            FButtonHover := AButton;
          end;

          if (AButton.State <> csActive) then begin
            AButton.State := csHover;
          end;
        end else begin
          if Assigned(FButtonHover) then begin
            FButtonHover.State := csNormal;

            FButtonHover := nil;
          end;
        end;
      end;

      WM_NCMOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
        if Assigned(FButtonDown) then
          Exit;
        ///

        if Assigned(FButtonHover) then begin
          if (FButtonHover.State <> csActive) then
            FButtonHover.State := csNormal;
        end;
      end;

      WM_NCHITTEST : begin
        var APoint := Point(TWMNCHitTest(AMessage).XPos, TWMNCHitTest(AMessage).YPos);

        var AWindowRect := TRect.Empty;
        GetWindowRect(FOwnerForm.Handle, AWindowRect);

        var AEdgeL := APoint.X - AWindowRect.Left;
        var AEdgeT := APoint.Y - AWindowRect.Top;
        var AEdgeR := AWindowRect.Right - APoint.X;
        var AEdgeB := AWindowRect.Bottom - APoint.Y;

        var AResizeArea := FOwnerForm.ScaleValue(8);

        var ASizeable := (FBorderStyle = bsSizeable) or (FBorderStyle = bsSizeToolWin);

        if ASizeable and (AEdgeT < AResizeArea) and (AEdgeL < AResizeArea) then
          AMessage.Result := HTTOPLEFT
        else if ASizeable and (AEdgeT < AResizeArea) and (AEdgeR < AResizeArea) then
          AMessage.Result := HTTOPRIGHT
        else if ASizeable and (AEdgeB < AResizeArea) and (AEdgeL < AResizeArea) then
          AMessage.Result := HTBOTTOMLEFT
        else if ASizeable and (AEdgeB < AResizeArea) and (AEdgeR < AResizeArea) then
          AMessage.Result := HTBOTTOMRIGHT
        else if ASizeable and (AEdgeT < AResizeArea) then
          AMessage.Result := HTTOP
        else if ASizeable and (AEdgeB < AResizeArea) then
          AMessage.Result := HTBOTTOM
        else if ASizeable and (AEdgeL < AResizeArea) then
          AMessage.Result := HTLEFT
        else if ASizeable and (AEdgeR < AResizeArea) then
          AMessage.Result := HTRIGHT
        else begin
          var AClientPoint := Point(APoint.X - AWindowRect.Left, APoint.Y - AWindowRect.Top);
          if ptinrect(FCaptionRect, AClientPoint) then
            AMessage.Result := HTCAPTION
          else
            AMessage.Result := HTCLIENT;
        end;

        ///
        ADoCallBase := False;
      end;
    end;
  finally
    if ADoCallBase then
      FOldWindowProc(AMessage);
  end;
end;

constructor TFlatWindow.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FNonClientCanvas := TCanvas.Create();
  FCanvas          := TCanvas.Create();

  FCloseButton     := TFlatCloseButton.Create(self);
  FMaximizeButton  := TFlatMaximizeButton.Create(self);
  FMinimizeButton  := TFlatMinimizeButton.Create(self);
  FHamburgerButton := TFlatHamburgerButton.Create(self);
  FHamburgerButton.Visible := False;

  FCaptionBarRect := TRect.Empty;
  FCaptionRect    := TRect.Empty;

  FMaximized := False;

  FOwnerForm := nil;

  FOnMaximize := nil;
  FOnMinimize := nil;
  FOnRestore  := nil;
  FOnClose    := nil;

  FButtonDown  := nil;
  FButtonHover := nil;

  if NOT Assigned(AOwner) then
    Exit;

  if NOT (AOwner is TForm) then
    Exit;
  ///

  FOwnerForm := TForm(AOwner);
  FOwnerForm.DoubleBuffered := True;

//  SetClassLong(FOwnerForm.Handle, GCL_STYLE, GetClassLong(FOwnerForm.Handle, GCL_STYLE) or CS_HREDRAW or CS_VREDRAW);
//  SetWindowLong(FOwnerForm.Handle, GWL_STYLE, GetWindowLong(FOwnerForm.Handle, GWL_STYLE) or WS_CLIPCHILDREN);

  FBorderWidth   := 2;
  FShowBorder    := True;
  FBackground    := MAIN_GRAY;
  FBorder        := MAIN_ACCENT;
  FCaption       := FBorder;
  FCaptionHeight := 25;
  FBorderStyle   := bsSizeable;

  FFont        := TFont.Create();
  FFont.Name   := FONT_1;
  FFont.Height := -11;
  FFont.Color  := clWhite;

  FOwnerForm.DoubleBuffered := True;
  FOwnerForm.BorderStyle    := bsNone;
  FOwnerForm.Font.Name      := FONT_1;

  FOldWindowProc := FOwnerForm.WindowProc;
  FOwnerForm.WindowProc := OnCustomWindowProc;

  if not (csLoading in ComponentState) then
    UpdateForm();
end;

destructor TFlatWindow.Destroy();
begin
  if Assigned(FOldWindowProc) then
    FOwnerForm.WindowProc := FOldWindowProc;

  if Assigned(FCloseButton) then
    FreeAndNil(FCloseButton);

  if Assigned(FMaximizeButton) then
    FreeAndNil(FMaximizeButton);

  if Assigned(FMinimizeButton) then
    FreeAndNil(FMinimizeButton);

  if Assigned(FHamburgerButton) then
    FreeAndNil(FHamburgerButton);

  if Assigned(FFont) then
    FreeAndNil(FFont);

  if Assigned(FNonClientCanvas) then
    FreeAndNil(FNonClientCanvas);

  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);

  ///
  inherited Destroy();
end;

procedure TFlatWindow.UpdateForm();
begin
  if not Assigned(FOwnerForm) or not FOwnerForm.HandleAllocated then
    Exit();
  ///

  SetupCaptionBar();

  SetWindowPos(FOwnerForm.Handle, 0, 0, 0, 0, 0,
    SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);

  RedrawWindow(FOwnerForm.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TFlatWindow.Invalidate;
begin
  if not Assigned(FOwnerForm) then
    Exit;
  ///

  SetupCaptionBar();

  RedrawWindow(FOwnerForm.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TFlatWindow.SetBorderWidth(const AValue : Integer);
begin
  if AValue = FBorderWidth then
    Exit;
  ///

  FBorderWidth := AValue;

  UpdateForm();
end;

procedure TFlatWindow.SetColor(const AIndex : Integer; const AValue : TColor);
begin
  if not Assigned(FOwnerForm) then
    Exit;
  ///

  case AIndex of
    0 : FBackground := AValue;
    1 : FBorder     := AValue;
    2 : FCaption    := AValue;
  end;

  ///
  UpdateForm();
end;

procedure TFlatWindow.SetFont(const AFont : TFont);
begin
  FFont.Assign(AFont);

  ///
  UpdateForm();
end;

procedure TFlatWindow.SetCaptionAlign(const AValue : TAlignment);
begin
  if AValue = FCaptionAlign then
    Exit;

  FCaptionAlign := AValue;

  ///
  UpdateForm();
end;

procedure TFlatWindow.SetMenuDropDown(const AValue : TPopupMenu);
begin
  FMenuDropDown := AValue;

  ///
  UpdateForm();
end;

procedure TFlatWindow.SetShowBorder(const AValue : Boolean);
begin
  if AValue = FShowBorder then
    Exit;
  ///

  FShowBorder := AValue;

  ///
  UpdateForm();
end;

procedure TFlatWindow.SetBorderStyle(const AValue : TFormBorderStyle);
begin
  if AValue = FBorderStyle then
    Exit;
  ///

  FBorderStyle := AValue;

  ///
  UpdateForm();
end;

procedure TFlatWindow.SetCaptionHeight(const AValue : Integer);
begin
  if AValue = FCaptionHeight then
    Exit;
  ///

  FCaptionHeight := AValue;

  ///
  UpdateForm();
end;

end.
