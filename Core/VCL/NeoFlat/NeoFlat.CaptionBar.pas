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

unit NeoFlat.CaptionBar;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils, System.UITypes,

  Winapi.Windows, Winapi.Messages,

  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Menus,

  NeoFlat.Form, NeoFlat.Theme, NeoFlat.Helper, NeoFlat.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatCaptionBar = class;

  TCaptionButtonState = (
    cbsNormal,
    cbsHover,
    cbsActive
  );

  TOnHandleSystemButton = procedure(Sender : TObject; var AHandled : Boolean) of object;

  TCaptionButton = class
  private
    FRect        : TRect;
    FVisible     : Boolean;
    FOwner       : TFlatCaptionBar;
    FState       : TCaptionButtonState;

    {@M}
    procedure SetState(AValue : TCaptionButtonState);
  public
    {@C}
    constructor Create(AOwner : TFlatCaptionBar);

    {@G/S}
    property Rect    : TRect               read FRect    write FRect;
    property Visible : Boolean             read FVisible write FVisible;
    property State   : TCaptionButtonState read FState   write SetState;
  end;

  TCloseButton     = class(TCaptionButton);
  TMaximizeButton  = class(TCaptionButton);
  TMinimizeButton  = class(TCaptionButton);
  TDockButton      = class(TCaptionButton);
  THamburgerButton = class(TCaptionButton);

  TFlatCaptionBar = class(TGraphicControl)
  private
    FOldWindowProc   : TWndMethod;

    FOwnerForm       : TForm;

    FOldBoundRect    : TRect;

    FForm            : TFlatForm;

    FCloseButton     : TCloseButton;
    FMaximizeButton  : TMaximizeButton;
    FMinimizeButton  : TMinimizeButton;
    FHamburgerButton : THamburgerButton;

    FButtonDown      : TCaptionButton;
    FButtonHover     : TCaptionButton;

    FMaximized       : Boolean;

    FBorderIcons     : TBorderIcons;

    FTransparent     : Boolean;

    FCaptionRect     : TRect;

    FSubCaption      : String;

    FTextCenter      : Boolean;

    FMenuDropdown    : TPopupMenu;

    FOnMaximize      : TOnHandleSystemButton;
    FOnMinimize      : TOnHandleSystemButton;
    FOnRestore       : TOnHandleSystemButton;
    FOnClose         : TOnHandleSystemButton;
    FOnClickCaption  : TNotifyEvent;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);
    function GetCaptionButtonFromCoord(const X, Y : Integer) : TCaptionButton;

    function DrawCaptionButtons() : Integer;

    procedure doRestore();
    procedure doMaximize();
    procedure doMinimize();
    procedure doMaximizeRestore();
    procedure doClose();
    procedure DisplayMenuDropDown();

    procedure SetTransparent(const AValue : Boolean);
    procedure SetBorderIcons(const AValue : TBorderIcons);
    procedure SetSubCaption(const AValue : String);
    procedure SetTextCenter(const AValue : Boolean);
    procedure SetMenuDropDown(const AValue : TPopupMenu);

    procedure PrepareCaptionButtons();
  protected
    {@M}
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure Maximize();

    {@G}
    property Maximized    : Boolean read FMaximized;
    property OldBoundRect : TRect   read FOldBoundRect;
  published
    property Align;
    property AlignWithMargins;
    property Caption;
    property Enabled;
    property Font;
    property Margins;
    property Visible;

    {@G/S}
    property BorderIcons  : TBorderIcons read FBordericons  write SetBorderIcons;
    property Form         : TFlatForm    read FForm         write FForm;
    property SubCaption   : String       read FSubCaption   write SetSubCaption;
    property Transparent  : Boolean      read FTransparent  write SetTransparent;
    property TextCenter   : Boolean      read FTextCenter   write SetTextCenter;
    property MenuDropDown : TPopupMenu   read FMenuDropDown write SetMenuDropDown;

    property OnMaximize     : TOnHandleSystemButton read FOnMaximize     write FOnMaximize;
    property OnMinimize     : TOnHandleSystemButton read FOnMinimize     write FOnMinimize;
    property OnRestore      : TOnHandleSystemButton read FOnRestore      write FOnRestore;
    property OnClose        : TOnHandleSystemButton read FOnClose        write FOnClose;
    property OnClickCaption : TNotifyEvent          read FOnClickCaption write FOnClickCaption;
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
  System.Types;
// ---------------------------------------------------------------------------------------------------------------------

(* TFlatCaptionBar *)

procedure TFlatCaptionBar.Maximize();
begin
  if (biMaximize in BorderIcons) and (not FMaximized) then
    doMaximize();
end;

procedure TFlatCaptionBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  if not Assigned(FOnClickCaption) then
    Exit;
end;

procedure TFlatCaptionBar.doRestore();
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

  if Assigned(FForm) then
    FForm.ShowBorder := True;

  FOwnerForm.BoundsRect := FOldBoundRect;

  ///
  Invalidate;
end;

procedure TFlatCaptionBar.doMaximize();
begin
  if FMaximized then
    Exit;
  ///

  var AHandled := False;
  if Assigned(FOnMaximize) then
    FOnMaximize(self, AHandled);

  if AHandled then
    Exit;

  FOldBoundRect := FOwnerForm.BoundsRect;

  if Assigned(FForm) then
    FForm.ShowBorder := False;

  with Screen.MonitorFromRect(FOwnerForm.BoundsRect).workAreaRect do begin
    FOwnerForm.SetBounds(Left, Top, Right - Left, Bottom - Top);
  end;

  FMaximized := True;

  ///
  Invalidate;
end;

procedure TFlatCaptionBar.doMinimize();
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

procedure TFlatCaptionBar.doClose();
begin
  var AHandled := False;
  if Assigned(FOnClose) then
    FOnClose(self, AHandled);

  if AHandled then
    Exit;

  FOwnerForm.Close();
end;

constructor TFlatCaptionBar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOldWindowProc := WindowProc;
  WindowProc := OnCustomWindowProc;

  Align  := alTop;
  ClientHeight := ScaleValue(25);

  FOwnerForm := nil;

  FForm := nil;

  FButtonDown := nil;
  FButtonHover := nil;

  FTransparent := False;

  FMaximized := False;
  FTextCenter := False;

  Font.Name   := FONT_1;
  Font.Color  := clWhite;
  Font.Height := -11;

  FSubCaption := '';

  if Assigned(AOwner) then begin
     var AObject : TComponent := self;
     while true do begin
      AObject := AObject.Owner;

      if (AObject = nil) then
        break;

      if (AObject is TForm) then begin
        FOwnerForm := TForm(AObject);

        break;
      end;
    end;
  end;

  FOnMaximize     := nil;
  FOnMinimize     := nil;
  FOnRestore      := nil;
  FOnClose        := nil;
  FOnClickCaption := nil;

  FCloseButton     := TCloseButton.Create(self);
  FMaximizeButton  := TMaximizeButton.Create(self);
  FMinimizeButton  := TMinimizeButton.Create(self);

  FHamburgerButton := THamburgerButton.Create(self);
  FHamburgerButton.Visible := False;

  BorderIcons := [biSystemMenu,biMinimize,biMaximize];
end;

destructor TFlatCaptionBar.Destroy();
begin
  if Assigned(FOldWindowProc) then
    WindowProc := FOldWindowProc;

  if Assigned(FCloseButton) then
    FreeAndNil(FCloseButton);

  if Assigned(FMaximizeButton) then
    FreeAndNil(FMaximizeButton);

  if Assigned(FMinimizeButton) then
    FreeAndNil(FMinimizeButton);

  if Assigned(FHamburgerButton) then
    FreeAndNil(FHamburgerButton);

  ///
  inherited Destroy();
end;

procedure TFlatCaptionBar.PrepareCaptionButtons();
begin
  var AButtonWidth := (ClientHeight - ScaleValue(8)); // Minus two borders pixels
  var AButtonY := (ClientHeight div 2) - (AButtonWidth div 2);

  var ARect : TRect;

  ARect.Left   := 0;
  ARect.Top    := AButtonY;
  ARect.Width  := 0;
  ARect.Height := AButtonWidth;
  ///

  // Hamburger Button
  if FHamburgerButton.Visible then begin
    ARect.Left  := ScaleValue(4);
    ARect.Width := AButtonWidth;

    FHamburgerButton.Rect := ARect;

    /// Reset, since we are moving right
    ARect.Left := 0;
  end;

  // Close Button
  if FCloseButton.Visible then begin
    ARect.Left  := (ClientWidth - ScaleValue(4) - AButtonWidth);
    ARect.Width := AButtonWidth;

    FCloseButton.Rect := ARect;
  end;

  // Maximize Restore
  if FMaximizeButton.Visible then begin
    ARect.Left  := (ARect.Left - ScaleValue(1)) - AButtonWidth;
    ARect.Width := AButtonWidth;

    FMaximizeButton.Rect := ARect;
  end;

  // Minimize
  if FMinimizeButton.Visible then begin
    ARect.Left  := (ARect.Left - ScaleValue(1)) - AButtonWidth;
    ARect.Width := AButtonWidth;

    FMinimizeButton.Rect := ARect;
  end;

  ///
  if FCloseButton.Visible or FMaximizeButton.Visible or
     FMinimizeButton.Visible then
    Dec(FCaptionRect.Right, (Width - ARect.Left));

  if FHamburgerButton.Visible then
    Inc(FCaptionRect.Left, AButtonWidth + ScaleValue(4));
end;

function TFlatCaptionBar.DrawCaptionButtons() : Integer;

    function DrawAButton(AButton : TCaptionButton; const AGlyphMatrix : TMatrixGlyph) : TRect;
    begin
      result := TRect.Empty();
      ///

      if not Assigned(AButton) or not IsValidMatrixGlyph(AGlyphMatrix) then
        Exit;
      ///

      var ADrawBorder := False;
      case AButton.State of
        cbsHover, cbsActive :
          ADrawBorder := True;
      end;

      Canvas.Brush.Color := GLYPH_COLOR;

      // Draw Border
      if ADrawBorder then
        Canvas.FrameRect(AButton.Rect);

      // Draw Button Glyph
      Canvas.MoveTo((AButton.Rect.Left - ScaleValue(1)), AButton.Rect.Top);
      Canvas.LineTo((AButton.Rect.Left - ScaleValue(1)), AButton.Rect.Bottom);

      var X := AButton.Rect.Left + ((FCloseButton.Rect.Width div 2) - (Length(AGlyphMatrix[0]) div 2));
      var Y := AButton.Rect.Top + ((FCloseButton.Rect.Height div 2) - (Length(AGlyphMatrix) div 2));

      DrawMatrixGlyph(Canvas, AGlyphMatrix, X, Y, GLYPH_COLOR);
    end;

begin
  result := 0;
  ///

  PrepareCaptionButtons();
  ///

  Canvas.Brush.Style := bsSolid;

  // Draw Close Button
  if FCloseButton.Visible then
    DrawAButton(FCloseButton, CLOSE_GLYPH);

  // Draw Maximize / Restore Button
  if FMaximizeButton.Visible then begin
    if FMaximized then
      DrawAButton(FMaximizeButton, RESTORE_GLYPH)
    else
      DrawAButton(FMaximizeButton, MAXIMIZE_GLYPH)
  end;

  // Draw Minimize Button
  if FMinimizeButton.Visible then
    DrawAButton(FMinimizeButton, MINIMIZE_GLYPH);

  // Draw Hamburger Menu Button
  if FHamburgerButton.Visible then
    DrawAButton(FHamburgerButton, HAMBURGER_GLYPH);
end;

procedure TFlatCaptionBar.Paint;
begin
  inherited;
  ///

  FCaptionRect := Rect(
    ScaleValue(8),
    0,
    (ClientWidth - ScaleValue(16)),
    ClientHeight
  );
  ///

  Canvas.Lock();
  try
    // Prepare Canvas
    Canvas.Pen.Width := 0;
    Canvas.Pen.Color := clNone;

    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clNone;

    Canvas.Font.Assign(inherited Font);

    Canvas.Brush.Style := bsSolid;

    var ARect := TRect.Empty;
    ARect.Height := ClientHeight;
    ARect.Width  := ClientWidth;

    // Draw Background
    Canvas.Brush.Color := MAIN_ACCENT;
    Canvas.FillRect(ARect);

    // Draw System Buttons (Caption Buttons)
    DrawCaptionButtons();

    // Draw Caption Text
    Canvas.Brush.Style := bsClear;

    var ACaption : String := Caption;

    if FSubCaption <> '' then
      ACaption := Format('%s / %s', [ACaption, FSubCaption]);

    var ATextFormat : TTextFormat := [tfLeft, tfVerticalCenter, tfEndEllipsis, tfSingleLine];
    if FTextCenter then
      ATextFormat := ATextFormat + [tfCenter];

    ///
    Canvas.TextRect(FCaptionRect, ACaption, ATextFormat);
  finally
    Canvas.Unlock();
  end;
end;

function TFlatCaptionbar.GetCaptionButtonFromCoord(const X, Y : Integer) : TCaptionButton;
begin
  result := nil;
  ///

  if ptinrect(FCloseButton.Rect, Point(X, Y)) then
    result := FCloseButton
  else if ptinrect(FMaximizeButton.Rect, Point(X, Y)) then
    result := FMaximizeButton
  else if ptinrect(FMinimizeButton.Rect, Point(X, Y)) then
    result := FMinimizeButton
  else if ptinrect(FHamburgerButton.Rect, Point(X, Y)) then
    result := FHamburgerButton;
end;

procedure TFlatCaptionBar.doMaximizeRestore();
begin
  if not (biMaximize in FBorderIcons) then
    Exit;
  ///

  if FMaximized then
    doRestore()
  else
    doMaximize();
end;

procedure TFlatCaptionBar.DisplayMenuDropDown();
begin
  if not Assigned(FMenuDropDown) then
    Exit;
  ///

  var APoint := ClientToScreen(
    Point(
      FHamburgerButton.Rect.Left,
      FHamburgerButton.Rect.Top + FHamburgerButton.Rect.Height
    )
  );

  FMenuDropDown.Popup(APoint.X, APoint.Y);
end;

procedure TFlatCaptionBar.OnCustomWindowProc(var AMessage : TMessage);
begin
  FOldWindowProc(AMessage);
  ///

  case AMessage.Msg of
    CM_TEXTCHANGED : begin
      if Assigned(FOwnerForm) then
        FOwnerForm.Caption := Caption;

      ///
      Invalidate;
    end;
  end;

  ///
  if (csDesigning in ComponentState) then
    Exit;
  ///

  var AButton : TCaptionButton;

  case AMessage.Msg of
    WM_LBUTTONDBLCLK : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonDown(AMessage).XPos, TWMLButtonDown(AMessage).YPos);
      if not Assigned(AButton) then
        doMaximizeRestore();
    end;

    WM_LBUTTONDOWN : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonDown(AMessage).XPos, TWMLButtonDown(AMessage).YPos);
      if Assigned(AButton) then begin
        AButton.State := cbsActive;

        FButtonDown := AButton;
      end;
      ///

      if (NOT Assigned(AButton)) and Assigned(FOwnerForm) then begin
        ReleaseCapture();
        SendMessage(FOwnerForm.Handle, WM_SYSCOMMAND, $F012, 0);
      end;
    end;

    WM_LBUTTONUP : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonUp(AMessage).XPos, TWMLButtonUp(AMessage).YPos);
      if Assigned(AButton) then begin
        AButton.State := cbsHover;

        if (AButton = FButtonDown) then begin
          if AButton is TCloseButton then
            DoClose()
          else if AButton is TMaximizeButton then
            doMaximizeRestore()
          else if AButton is TMinimizeButton then
            DoMinimize()
          else if AButton is THamburgerButton then
            DisplayMenuDropDown();
        end;
      end;

      if Assigned(FButtonDown) then begin
        FButtonDown.State := cbsNormal;

        FButtonDown := nil;
      end;
    end;

    WM_MOUSEMOVE : begin
      if Assigned(FButtonDown) then
        Exit;
      ///

      AButton := GetCaptionButtonFromCoord(TWMMouseMove(AMessage).XPos, TWMMouseMove(AMessage).YPos);
      if Assigned(AButton) then begin
        if (AButton <> FButtonHover) then begin

          if Assigned(FButtonHover) then
            FButtonHover.State := cbsNormal;

          FButtonHover := AButton;
        end;

        if (AButton.State <> cbsActive) then begin
          AButton.State := cbsHover;
        end;
      end else begin
        if Assigned(FButtonHover) then begin
          FButtonHover.State := cbsNormal;

          FButtonHover := nil;
        end;
      end;
    end;

    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      if Assigned(FButtonDown) then
        Exit;
      ///

      if Assigned(FButtonHover) then begin
        if (FButtonHover.State <> cbsActive) then
          FButtonHover.State := cbsNormal;
      end;
    end;
  end;
end;

procedure TFlatCaptionBar.SetSubCaption(const AValue : String);
begin
  if AValue = FSubCaption then
    Exit;
  ///

  FSubCaption := AValue;

  ///
  Invalidate;
end;

procedure TFlatCaptionBar.SetBorderIcons(const AValue : TBorderIcons);
begin
  FBorderIcons := AValue;

  FCloseButton.Visible    := (biSystemMenu in FBorderIcons);
  FMaximizeButton.Visible := (biMaximize in FBorderIcons);
  FMinimizeButton.Visible := (biMinimize in FBorderIcons);

  ///
  Invalidate;
end;

procedure TFlatCaptionBar.SetTransparent(const AValue : Boolean);
begin
  if AValue = FTransparent then
    Exit;
  ///

  FTransparent := AValue;

  ///
  Invalidate;
end;

procedure TFlatCaptionBar.SetTextCenter(const AValue : Boolean);
begin
  if AValue = FTextCenter then
    Exit;
  ///

  FTextCenter := AValue;

  ///
  Invalidate;
end;

procedure TFlatCaptionBar.SetMenuDropDown(const AValue : TPopupMenu);
begin
  FMenuDropDown := AValue;

  FHamburgerButton.Visible := Assigned(FMenuDropDown);

  ///
  Invalidate;
end;

(* TCaptionButton *)

constructor TCaptionButton.Create(AOwner : TFlatCaptionBar);
begin
  inherited Create();

  if Assigned(AOwner) then
    FOwner := AOwner;

  FRect        := TRect.Empty;
  FVisible     := True;
  FState       := cbsNormal;
end;

procedure TCaptionButton.SetState(AValue : TCaptionButtonState);
begin
  if AValue = FState then
    Exit;
  ///

  FState := AValue;

  ///
  if Assigned(FOwner) then
    FOwner.Invalidate;
end;

end.
