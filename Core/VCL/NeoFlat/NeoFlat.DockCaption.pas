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

unit NeoFlat.DockCaption;

interface

uses System.Classes, VCL.Controls, VCL.ImgList, VCL.Graphics, WinAPI.Windows;

type
  TDockStatusEvent = procedure(Sender : TObject; ADocked : Boolean) of object;

  TFlatDockCaption = class(TGraphicControl)
  private
    FImageList          : TCustomImageList;
    FDockImageIndex     : Integer;
    FUndockImageIndex   : Integer;
    FCaption            : String;
    FDocked             : Boolean;

    FButtonRect         : TRect;

    FOnButtonClick      : TNotifyEvent;
    FOnDockStatusChange : TDockStatusEvent;

    {@M}
    procedure SetImageIndex(const AIndex : Integer; AValue : Integer);
    procedure SetCaption(const AValue : String);
    procedure SetDocked(const AValue : Boolean);
    procedure CalcSize();
  protected
    {@M}
    procedure Paint(); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@S}
    property DefaultDocked : Boolean write FDocked;

    {@G/S}
    property ImageList          : TCustomImageList read FImageList          write FImageList;
    property Caption            : String           read FCaption            write SetCaption;
    property Docked             : Boolean          read FDocked             write SetDocked;
    property OnButtonClick      : TNotifyEvent     read FOnButtonClick      write FOnButtonClick;
    property OnDockStatusChange : TDockStatusEvent read FOnDockStatusChange write FOnDockStatusChange;

    property DockImageIndex   : Integer index 0 read FDockImageIndex   write SetImageIndex;
    property UndockImageIndex : Integer index 1 read FUndockImageIndex write SetImageIndex;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

implementation

uses NeoFlat.Theme;

{-------------------------------------------------------------------------------
  Detect Mouse Click
-------------------------------------------------------------------------------}
procedure TFlatDockCaption.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var APoint : TPoint;
begin
  APoint.X := X;
  APoint.Y := Y;

  if ptinrect(FButtonRect, APoint) then begin
    self.Docked := not self.Docked;

    if Assigned(FOnButtonClick) then
      FOnButtonClick(self);
  end;
end;

{-------------------------------------------------------------------------------
  Calc Sizes
-------------------------------------------------------------------------------}
procedure TFlatDockCaption.CalcSize();
begin
  FButtonRect := TRect.Empty;

  if Assigned(FImageList) then begin
    FButtonRect.Left   := (ClientWidth - 4 - FImageList.Width);
    FButtonRect.Top    := (ClientHeight div 2) - (FImageList.Height div 2);
    FButtonRect.Width  := FImageList.Width;
    FButtonRect.Height := FImageList.Height;
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TFlatDockCaption.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FCaption            := self.ClassName;
  FImageList          := nil;
  FDockImageIndex     := -1;
  FUndockImageIndex   := -1;

  Height              := 22;
  self.Font.Height    := -11;
  self.Font.Name      := FONT_1;
  self.Font.Color     := clWhite;
  align               := alTop;
  AlignWithMargins    := True;

  self.Margins.Left   := 2;
  self.Margins.Top    := 2;
  self.Margins.Right  := 2;
  self.Margins.Bottom := 2;

  FDocked             := True;

  FButtonRect         := TRect.Empty;
  FOnButtonClick      := nil;
  FOnDockStatusChange := nil;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TFlatDockCaption.Destroy();
begin


  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TFlatDockCaption.Paint();
var AGlyphWidth : Integer;
    ATextRect   : TRect;
    X, Y        : Integer;
    ADrawGlyph  : Boolean;
    AImageIndex : Integer;
begin
  AGlyphWidth := 0;
  AImageIndex := -1;
  ///

  self.CalcSize();

  if Assigned(FImageList) then begin
    if FDocked then
      AImageIndex := FUndockImageIndex
    else
      AImageIndex := FDockImageIndex;
  end;

  ADrawGlyph  := (AImageIndex > -1);

  if ADrawGlyph then
    AGlyphWidth := (FImageList.Width + 4);
  ///

  ATextRect.Left   := 4;
  ATextRect.Top    := 0;
  ATextRect.Height := ClientHeight;
  ATextRect.Width  := (ClientWidth - AGlyphWidth - 8);

  Canvas.Lock();
  try
    Canvas.Font.Assign(Font);
    ///

    Canvas.Brush.Style := bsClear;

    {
      Draw Caption
    }
    Canvas.TextRect(ATextRect, FCaption, [
                                            tfLeft,
                                            tfVerticalCenter,
                                            tfEndEllipsis,
                                            tfSingleLine
    ]);

    {
      Draw Underline
    }
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Font.Color;

    Y := ClientHeight -4;
    X := 0;

    Canvas.MoveTo(X, Y);
    Canvas.LineTo(ClientWidth, Y);

    {
      Draw Glyph
    }
    if ADrawGlyph  then
      FImageList.Draw(Canvas, FButtonRect.Left, FButtonRect.Top, AImageIndex);
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TFlatDockCaption.SetCaption(const AValue : String);
begin
  if AValue = FCaption then
    Exit();
  ///

  FCaption := AValue;

  ///
  Invalidate();
end;

procedure TFlatDockCaption.SetImageIndex(const AIndex : Integer; AValue : Integer);
begin
  case AIndex of
    0 : FDockImageIndex   := AValue;
    1 : FUndockImageIndex := AValue;
  end;

  ///
  Invalidate();
end;

procedure TFlatDockCaption.SetDocked(const AValue : Boolean);
begin
  if AValue = FDocked then
    Exit();
  ///

  FDocked := AValue;

  if Assigned(FOnDockStatusChange) then
    FOnDockStatusChange(self, FDocked);

  ///
  Invalidate();
end;

end.
