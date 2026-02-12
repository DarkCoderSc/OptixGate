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

unit NeoFlat.CaptionBar;

interface

uses Winapi.Windows, System.Classes, VCL.Graphics, VCL.Controls, Winapi.Messages,
     VCL.Forms, System.SysUtils, System.UITypes, NeoFlat.Form, NeoFlat.Theme,
     VCL.Menus;

type
  TFlatCaptionBar = class;

  TByteArrayArray = array of array of Byte;

  TCaptionButtonState = (cbsNormal, cbsHover, cbsActive);

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
    FDockButton      : TDockButton;
    FHamburgerButton : THamburgerButton;

    FButtonDown      : TCaptionButton;
    FButtonHover     : TCaptionButton;

    FCollapsed       : Boolean;
    FOwnerOldClientH : Integer;
    FOldConstraintH  : Integer;
    FOldConstraintW  : Integer;

    FCollapsible     : Boolean;

    FMaximized       : Boolean;

    FBorderIcons     : TBorderIcons;

    FTransparent     : Boolean;

    FCaptionRect     : TRect;

    FSubCaption      : String;

    FTextCenter      : Boolean;

    FDockable        : Boolean;

    FMenuDropdown    : TPopupMenu;

    FOnMaximize      : TOnHandleSystemButton;
    FOnMinimize      : TOnHandleSystemButton;
    FOnRestore       : TOnHandleSystemButton;
    FOnClose         : TOnHandleSystemButton;
    FOnDock          : TNotifyEvent;
    FOnClickCaption  : TNotifyEvent;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);
    function GetCaptionButtonFromCoord(X, Y : Integer) : TCaptionButton;

    function DrawCaptionButtons() : Integer;

    procedure doRestore();
    procedure doMaximize();
    procedure doMinimize();
    procedure doMaximizeRestore();
    procedure doCollapseRestore();
    procedure doClose();
    procedure doDock();
    procedure DisplayMenuDropDown();

    procedure SetCaption(AValue : String);
    function GetCaption() : String;
    procedure SetDockable(const AValue : Boolean);
    procedure SetTransparent(const AValue : Boolean);
    procedure SetBorderIcons(AValue : TBorderIcons);
    procedure SetSubCaption(const AValue : String);
    procedure SetTextCenter(const AValue : Boolean);
    procedure SetMenuDropDown(const AValue : TPopupMenu);

    procedure PrepareCaptionButtons();
  protected
    {@M}
    procedure Paint(); override;
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
    {@G/S}
    property Caption      : String       read GetCaption    write SetCaption;
    property BorderIcons  : TBorderIcons read FBordericons  write SetBorderIcons;
    property Form         : TFlatForm    read FForm         write FForm;
    property SubCaption   : String       read FSubCaption   write SetSubCaption;
    property Dockable     : Boolean      read FDockable     write SetDockable;
    property Transparent  : Boolean      read FTransparent  write SetTransparent;
    property Collapsible  : Boolean      read FCollapsible  write FCollapsible;
    property TextCenter   : Boolean      read FTextCenter   write SetTextCenter;
    property MenuDropDown : TPopupMenu   read FMenuDropDown write SetMenuDropDown;

    property OnMaximize     : TOnHandleSystemButton read FOnMaximize     write FOnMaximize;
    property OnMinimize     : TOnHandleSystemButton read FOnMinimize     write FOnMinimize;
    property OnRestore      : TOnHandleSystemButton read FOnRestore      write FOnRestore;
    property OnClose        : TOnHandleSystemButton read FOnClose        write FOnClose;
    property OnDock         : TNotifyEvent          read FOnDock         write FOnDock;
    property OnClickCaption : TNotifyEvent          read FOnClickCaption write FOnClickCaption;

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
  const CLOSE_GLYPH : TByteArrayArray = [
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

  const MINIMIZE_GLYPH : TByteArrayArray = [
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

  const RESTORE_GLYPH : TByteArrayArray = [
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


  const MAXIMIZE_GLYPH : TByteArrayArray = [
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

  const DOCK_GLYPH : TByteArrayArray = [
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $1, $1, $1, $1, $1, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $1, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $1, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $1, $1, $1, $1, $1, $1, $1, $1],
                                              [$0, $0, $0, $1, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $1, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $1, $1, $1, $1, $1, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

  const HAMBURGER_GLYPH : TByteArrayArray = [
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

uses System.Types;

{ TFlatCaption.Maximize }
procedure TFlatCaptionBar.Maximize();
begin
  if (biMaximize in self.BorderIcons) and (not FMaximized) then
    self.doMaximize();
end;

{ TFlatCaption.MouseMove
  : Spy on mouse position over the control }
procedure TFlatCaptionBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  if not Assigned(FOnClickCaption) then
    Exit();
end;

{ TFlatCaptionBar.doRestore }
procedure TFlatCaptionBar.doRestore();
var AHandled : Boolean;
begin
  if not FMaximized then
    Exit();
  ///

  AHandled := False;
  if Assigned(FOnRestore) then
    FOnRestore(self, AHandled);

  FMaximized := False;

  if Assigned(FForm) then
    FForm.ShowBorder := True;

  FOwnerForm.BoundsRect := FOldBoundRect;

  ///
  Invalidate();
end;

{ TFlatCaptionBar.doMaximize }
procedure TFlatCaptionBar.doMaximize();
var AHandled : Boolean;
begin
  if FMaximized then
    Exit;
  ///

  AHandled := False;
  if Assigned(FOnMaximize) then
    FOnMaximize(self, AHandled);

  FOldBoundRect := FOwnerForm.BoundsRect;

  if Assigned(FForm) then
    FForm.ShowBorder := False;

  // Fit window to the correct screen monitor (depending on the position of our window)
  with Screen.MonitorFromRect(FOwnerForm.BoundsRect).workAreaRect do begin
    FOwnerForm.SetBounds(Left, Top, Right - Left, Bottom - Top);
  end;

  FMaximized := True;

  ///
  Invalidate();
end;

{ TFlatCaptionBar.doMinimize }
procedure TFlatCaptionBar.doMinimize();
var AHandled : Boolean;
begin
  AHandled := False;
  if Assigned(FOnMinimize) then
    FOnMinimize(self, AHandled);

  if AHandled then
    Exit();

  if FOwnerForm = Application.MainForm then
    Application.Minimize
  else
    ShowWindow(FOwnerForm.Handle, SW_MINIMIZE);
end;

{ TFlatCaptionBar.doClose }
procedure TFlatCaptionBar.doClose();
var AHandled : Boolean;
begin
  AHandled := False;
  if Assigned(FOnClose) then
    FOnClose(self, AHandled);

  if AHandled then
    Exit();

  FOwnerForm.Close();
end;

{ TFlatCaptionBar.doDock }
procedure TFlatCaptionBar.doDock();
begin
  if Assigned(FOnDOck) then
    FOnDock(self);
end;

{ TFlatCaptionBar.Create }
constructor TFlatCaptionBar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOldWindowProc := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  self.Align  := alTop;
  ClientHeight := self.ScaleValue(25);

  FOwnerForm := nil;

  FForm := nil;

  FButtonDown := nil;
  FButtonHover := nil;

  FTransparent := False;

  FCollapsed   := False;
  FCollapsible := True;

  FMaximized := False;
  FTextCenter := False;

  FOldConstraintH := 0;
  FOldConstraintW := 0;

  self.Font.Name   := FONT_1;
  self.Font.Color  := clWhite;
  self.Font.Height := -11;

  FSubCaption := '';

  // Get Owner Form
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
  FOnDock         := nil;
  FOnClickCaption := nil;

  // Create Caption Buttons Classes
  FCloseButton     := TCloseButton.Create(self);
  FMaximizeButton  := TMaximizeButton.Create(self);
  FMinimizeButton  := TMinimizeButton.Create(self);
  FDockButton      := TDockButton.Create(self);

  FHamburgerButton := THamburgerButton.Create(self);
  FHamburgerButton.Visible := False;
end;


{ TFlatCaptionBar.Destroy }
destructor TFlatCaptionBar.Destroy();
begin
  if Assigned(FOldWindowProc) then
    self.WindowProc := FOldWindowProc;

  if Assigned(FCloseButton) then
    FreeAndNil(FCloseButton);

  if Assigned(FMaximizeButton) then
    FreeAndNil(FMaximizeButton);

  if Assigned(FMinimizeButton) then
    FreeAndNil(FMinimizeButton);

  if Assigned(FDockButton) then
    FreeAndNil(FDockButton);

  if Assigned(FHamburgerButton) then
    FreeAndNil(FHamburgerButton);

  ///
  inherited Destroy();
end;

{ TFlatCaptionBar.PrepareCaptionButtons }
procedure TFlatCaptionBar.PrepareCaptionButtons();
begin
  var AButtonWidth := (self.ClientHeight - self.ScaleValue(8)); // Minus two borders pixels
  var AButtonY := (self.ClientHeight div 2) - (AButtonWidth div 2);

  var ARect : TRect;

  ARect.Left   := 0;
  ARect.Top    := AButtonY;
  ARect.Width  := 0;
  ARect.Height := AButtonWidth;
  ///

  // Hamburger Button
  if FHamburgerButton.Visible then begin
    ARect.Left  := self.ScaleValue(4);
    ARect.Width := AButtonWidth;

    FHamburgerButton.Rect := ARect;

    /// Reset, since we are moving right
    ARect.Left := 0;
  end;

  // Close Button
  if FCloseButton.Visible then begin
    ARect.Left  := (self.ClientWidth - self.ScaleValue(4) - AButtonWidth);
    ARect.Width := AButtonWidth;

    FCloseButton.Rect := ARect;
  end;

  // Maximize Restore
  if FMaximizeButton.Visible then begin
    ARect.Left  := (ARect.Left - self.ScaleValue(1)) - AButtonWidth;
    ARect.Width := AButtonWidth;

    FMaximizeButton.Rect := ARect;
  end;

  // Minimize
  if FMinimizeButton.Visible then begin
    ARect.Left  := (ARect.Left - self.ScaleValue(1)) - AButtonWidth;
    ARect.Width := AButtonWidth;

    FMinimizeButton.Rect := ARect;
  end;

  // Dock Button
  if FDockable then begin
    ARect.Left := (ARect.Left - self.ScaleValue(1)) - AButtonWidth;
    ARect.Width := AButtonWidth;

    FDockButton.Rect := ARect;
  end;

  ///
  if FCloseButton.Visible or FMaximizeButton.Visible or
     FMinimizeButton.Visible or FDockable then
    Dec(FCaptionRect.Right, (Width - ARect.Left));

  if FHamburgerButton.Visible then
    Inc(FCaptionRect.Left, AButtonWidth + self.ScaleValue(4));
end;

{ TFlatCaptionBar.DrawCaptionButtons }
function TFlatCaptionBar.DrawCaptionButtons() : Integer;

    { _.DrawAButton }
    function DrawAButton(AButton : TCaptionButton; const AGlyphMatrix : TByteArrayArray) : TRect;
    var I, N         : Integer;
        X, Y         : Integer;
        AGlyphWidth  : Integer;
        AGlyphHeight : Integer;
        AGlyphYDelta : Integer;
        ADrawBorder  : Boolean;
    begin
      result := TRect.Empty();
      ///

      if NOT Assigned(AButton) then
        Exit();
      ///

      AGlyphYDelta := 0;
      ADrawBorder := False;
      case AButton.State of
        cbsHover : begin
          ADrawBorder := True;
        end;

        cbsActive : begin
          AGlyphYDelta := self.ScaleValue(1);

          ADrawBorder := True;
        end;
      end;

      Canvas.Brush.Color := clWhite;

      // Draw Border
      if ADrawBorder then
        Canvas.FrameRect(AButton.Rect);

      // Draw Button Glyph
      Canvas.MoveTo((AButton.Rect.Left - self.ScaleValue(1)), AButton.Rect.Top);
      Canvas.LineTo((AButton.Rect.Left - self.ScaleValue(1)), AButton.Rect.Bottom);

      AGlyphWidth  := High(AGlyphMatrix[1]) + 1;
      AGlyphHeight := High(AGlyphMatrix) + 1;

      X := AButton.Rect.Left + ((FCloseButton.Rect.Width div 2) - (AGlyphWidth div 2));
      Y := AButton.Rect.Top + ((FCloseButton.Rect.Height div 2) - (AGlyphHeight div 2));

      for I := 0 to AGlyphWidth -1 do begin
        for N := 0 to AGlyphHeight -1 do begin
          if AGlyphMatrix[N][I] <> 0 then
            Canvas.Pixels[(X + I), (Y + N + AGlyphYDelta)] := clWhite;
        end;
      end;
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

  // Draw Dock Button
  if FDockable then
    DrawAButton(FDockButton, DOCK_GLYPH);

  // Draw Hamburger Menu Button
  if FHamburgerButton.Visible then
    DrawAButton(FHamburgerButton, HAMBURGER_GLYPH);
end;

{ TFlatCaptionBar.Paint }
procedure TFlatCaptionBar.Paint();
var ARect       : TRect;
    ACaption    : String;
    ATextFormat : TTextFormat;
begin
  FCaptionRect := Rect(
    self.ScaleValue(8),
    0,
    (ClientWidth - self.ScaleValue(16)),
    ClientHeight
  );
  ///

  Canvas.Lock();
  try
    // Prepare Canvas
    self.Canvas.Pen.Width := 0;
    self.Canvas.Pen.Color := clNone;

    self.Canvas.Brush.Style := bsSolid;
    self.Canvas.Brush.Color := clNone;

    Canvas.Font.Assign(inherited Font);

    Canvas.Brush.Style := bsSolid;

    ARect.Left   := 0;
    ARect.Top    := 0;
    ARect.Height := ClientHeight;
    ARect.Width  := ClientWidth;

    // Draw Background
    Canvas.Brush.Color := MAIN_ACCENT;
    Canvas.FillRect(ARect);

    // Draw System Buttons (Caption Buttons)
    DrawCaptionButtons();

    // Draw Caption Text
    Canvas.Brush.Style := bsClear;

    ACaption := inherited Caption;

    if FSubCaption <> '' then
      ACaption := Format('%s / %s', [ACaption, FSubCaption]);

    ATextFormat := [tfLeft, tfVerticalCenter, tfEndEllipsis, tfSingleLine];
    if FTextCenter then
      ATextFormat := ATextFormat + [tfCenter];

    Canvas.TextRect(FCaptionRect, ACaption, ATextFormat);
  finally
    Canvas.Unlock();
  end;
end;

{ TFlatCaptionbar.GetCaptionButtonFromCoord }
function TFlatCaptionbar.GetCaptionButtonFromCoord(X, Y : Integer) : TCaptionButton;
begin
  result := nil;
  ///

  if ptinrect(FCloseButton.Rect, Point(X, Y)) then
    result := FCloseButton
  else if ptinrect(FMaximizeButton.Rect, Point(X, Y)) then
    result := FMaximizeButton
  else if ptinrect(FMinimizeButton.Rect, Point(X, Y)) then
    result := FMinimizeButton
  else if ptinrect(FDockButton.Rect, Point(X, Y)) then
    result := FDockButton
  else if ptinrect(FHamburgerButton.Rect, Point(X, Y)) then
    result := FHamburgerButton;
end;

{ TFlatCaptionBar.doCollapseRestore }
procedure TFlatCaptionBar.doCollapseRestore();
begin
  if FMaximized or not FCollapsible then
    Exit();
  ///

  FCollapsed := not FCollapsed;

  if Assigned(FForm) then
    FForm.Resizable := not FCollapsed;
  ///

  if FCollapsed then begin
    FOwnerOldClientH := FOwnerForm.ClientHeight;
    FOldConstraintH  := FOwnerForm.Constraints.MinHeight;
    FOldConstraintW  := FOwnerForm.Constraints.MinWidth;

    FOwnerForm.Constraints.MinHeight := 0;
    FOwnerForm.Constraints.MinWidth  := 0;

    FOwnerForm.ClientHeight := self.ClientHeight;
  end else begin
    FOwnerForm.ClientHeight := FOwnerOldClientH;

    FOwnerForm.Constraints.MinHeight := FOldConstraintH;
    FOwnerForm.Constraints.MinWidth  := FOldConstraintW;
  end;
end;

{ TFlatCaptionBar.doMaximizeRestore }
procedure TFlatCaptionBar.doMaximizeRestore();
begin
  if (not (biMaximize in FBorderIcons)) or (FCollapsed) then
    Exit();
  ///

  if FMaximized then
    self.doRestore()
  else
    self.doMaximize();
end;

{ TFlatCaptionBar.DisplayMenuDropDown }
procedure TFlatCaptionBar.DisplayMenuDropDown();
begin
  if not Assigned(FMenuDropDown) then
    Exit();
  ///

  var APoint := self.ClientToScreen(
    Point(
      FHamburgerButton.Rect.Left,
      FHamburgerButton.Rect.Top + FHamburgerButton.Rect.Height
    )
  );

  FMenuDropDown.Popup(APoint.X, APoint.Y);
end;

{ TFlatCaptionBar.OnCustomWindowProc }
procedure TFlatCaptionBar.OnCustomWindowProc(var AMessage : TMessage);
var AButton : TCaptionButton;
begin
  FOldWindowProc(AMessage);
  ///

  if (csDesigning in ComponentState) then
    Exit;

  case AMessage.Msg of
    // Handle mouse double click
    WM_LBUTTONDBLCLK : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonDown(AMessage).XPos, TWMLButtonDown(AMessage).YPos);
      if not Assigned(AButton) then
        self.doMaximizeRestore();
    end;

    // On mouse left button down
    WM_LBUTTONDOWN : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonDown(AMessage).XPos, TWMLButtonDown(AMessage).YPos);
      if Assigned(AButton) then begin
        AButton.State := cbsActive;

        FButtonDown := AButton;
      end;
      ///

      // Move Owner Form
      if (NOT Assigned(AButton)) and Assigned(FOwnerForm) then begin
        ReleaseCapture();
        SendMessage(FOwnerForm.Handle, WM_SYSCOMMAND, $F012, 0);
      end;
    end;

    // Button Click (Up)
    WM_LBUTTONUP : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonUp(AMessage).XPos, TWMLButtonUp(AMessage).YPos);
      if Assigned(AButton) then begin
        AButton.State := cbsHover;

        if (AButton = FButtonDown) then begin
          if AButton is TCloseButton then
            self.DoClose()
          else if AButton is TMaximizeButton then
            self.doMaximizeRestore()
          else if AButton is TMinimizeButton then
            self.DoMinimize()
          else if AButton is TDockButton then
            self.DoDock()
          else if AButton is THamburgerButton then
            self.DisplayMenuDropDown();
        end;
      end;

      if Assigned(FButtonDown) then begin
        FButtonDown.State := cbsNormal;

        FButtonDown := nil;
      end;
    end;

    // Surface Move (Enter)
    WM_MOUSEMOVE : begin
      if Assigned(FButtonDown) then
        Exit();
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

    // Surface Leave
    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      if Assigned(FButtonDown) then
        Exit();
      ///

      if Assigned(FButtonHover) then begin
        if (FButtonHover.State <> cbsActive) then
          FButtonHover.State := cbsNormal;
      end;
    end;

    // Right Mouse Click
    WM_RBUTTONDOWN : begin
      self.doCollapseRestore();
    end;
  end;
end;

{ TFlatCaptionBar.SetCaption }
procedure TFlatCaptionBar.SetCaption(AValue : String);
begin
  if AValue = inherited Caption then
    Exit();

  inherited Caption := AValue;

  FOwnerForm.Caption := AValue;

  ///
  Invalidate();
end;

{ TFlatCaptionBar.SetSubCaption }
procedure TFlatCaptionBar.SetSubCaption(const AValue : String);
begin
  if AValue = FSubCaption then
    Exit();

  FSubCaption := AValue;

  ///
  Invalidate();
end;

{ TFlatCaptionBar.GetCaption }
function TFlatCaptionBar.GetCaption() : String;
begin
  result := inherited Caption;
end;

{ TFlatCaptionBar.SetBorderIcons }
procedure TFlatCaptionBar.SetBorderIcons(AValue : TBorderIcons);
begin
  if (AValue = FBorderIcons) then
    Exit();

  FBorderIcons := AValue;

  // Update Border Icons visibility
  self.FCloseButton.Visible    := (biSystemMenu in FBorderIcons);
  self.FMaximizeButton.Visible := (biMaximize in FBorderIcons);
  self.FMinimizeButton.Visible := (biMinimize in FBorderIcons);

  ///
  Invalidate();
end;

{ TFlatCaptionBar.SetDockable }
procedure TFlatCaptionBar.SetDockable(const AValue : Boolean);
begin
  if AValue = FDockable then
    Exit();

  FDockable := AValue;

  ///
  Invalidate();
end;

{ TFlatCaptionBar.SetTransparent }
procedure TFlatCaptionBar.SetTransparent(const AValue : Boolean);
begin
  if AValue = FTransparent then
    Exit();

  FTransparent := AValue;

  ///
  Invalidate();
end;

{ TFlatCaptionBar.SetTransparent }
procedure TFlatCaptionBar.SetTextCenter(const AValue : Boolean);
begin
  if AValue = FTextCenter then
    Exit();

  FTextCenter := AValue;

  ///
  Invalidate();
end;

{ TFlatCaptionBar.SetMenuDropDown }
procedure TFlatCaptionBar.SetMenuDropDown(const AValue : TPopupMenu);
begin
  FMenuDropDown := AValue;

  FHamburgerButton.Visible := Assigned(FMenuDropDown);

  ///
  Invalidate();
end;

(* TCaptionButton.Create *)

{ TCaptionButton.Create }
constructor TCaptionButton.Create(AOwner : TFlatCaptionBar);
begin
  inherited Create();

  if Assigned(AOwner) then
    FOwner := AOwner;

  FRect        := TRect.Empty;
  FVisible     := True;
  FState       := cbsNormal;
end;

{ TCaptionButton.SetState }
procedure TCaptionButton.SetState(AValue : TCaptionButtonState);
begin
  if AValue = FState then
    Exit();

  FState := AValue;

  ///
  if Assigned(FOwner) then
    FOwner.Invalidate();
end;

end.
