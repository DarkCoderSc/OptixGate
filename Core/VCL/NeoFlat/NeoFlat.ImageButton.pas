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

unit NeoFlat.ImageButton;

interface

uses WinAPI.Windows, System.Classes, VCL.Graphics, VCL.Controls, VCL.ImgList,
     WinAPI.Messages, NeoFlat.Theme;

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
    procedure Paint(); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
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

uses NeoFlat.Common, System.SysUtils;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
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

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TFlatImageButton.Destroy();
begin

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TFlatImageButton.Paint();
var X, Y   : Integer;
    AGlyph : TBitmap;
begin
  Canvas.Lock();
  try
    {
      Draw Background
    }
    Canvas.Brush.Color := FBackground;

    Canvas.FillRect(Rect(0, 0, Width, Height));

    {
      Draw Image
    }
    if Assigned(FImageList) and (FImageIndex > -1) then begin
      X := (self.Width div 2) - (FImageList.Width div 2);
      Y := (self.Height div 2) - (FImageList.Height div 2);

      if FMouseIsDown then begin
        Inc(X);
        Inc(Y);
      end;

      AGlyph := TBitmap.Create();
      try
        InitializeBitmap32(AGlyph, FImageList.Width, FImageList.Height);

        FImageList.GetBitmap(FImageIndex, AGlyph);

        if not inherited Enabled then
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

{-------------------------------------------------------------------------------
  Mouse Movement Control
-------------------------------------------------------------------------------}

procedure TFlatImageButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  FMouseIsDown := True;

  Invalidate();
end;

procedure TFlatImageButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var APoint : TPoint;
begin
  inherited;
  ///

  FMouseIsDown := False;

  APoint.X := X;
  APoint.Y := Y;

  if ptinrect(self.ClientRect, APoint) and Assigned(FOnClick) then begin
    FOnClick(self);
  end;

  Invalidate();
end;

procedure TFlatImageButton.MouseMove(Shift: TShiftState; X, Y: Integer);
//var APoint : TPoint;
begin
  inherited;
  ///

//  APoint.X := X;
//  APoint.X := Y;
//
//  FMouseIsHover := ptinrect(self.ClientRect, APoint);
end;

procedure TFlatImageButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  ///

end;

procedure TFlatImageButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  ///

end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TFlatImageButton.SetBackground(AValue : TColor);
begin
  if AValue = FBackground then
    Exit();

  FBackground := AValue;

  Invalidate();
end;

procedure TFlatImageButton.SetImageIndex(AValue : Integer);
begin
  if AValue = FImageIndex then
    Exit();

  FImageIndex := AValue;

  Invalidate();
end;

procedure TFlatImageButton.SetValue(AValue : Integer);
begin
  if FValue = AValue then
    Exit();
  ///

  FValue := AValue;

  if Assigned(FOnValueChanged) then
    FOnValueChanged(self, AValue);
end;

end.
