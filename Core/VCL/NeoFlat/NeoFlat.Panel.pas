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

unit NeoFlat.Panel;

interface

uses Winapi.Windows, System.Classes, VCL.Controls, VCL.Graphics, NeoFlat.Theme;

type
  TFlatPanel = class(TCustomControl)
  private
    FBorderTop    : Integer;
    FBorderLeft   : Integer;
    FBorderRight  : Integer;
    FBorderBottom : Integer;

    FColor        : TColor;
    FBorderColor  : TColor;

    {@M}
    procedure SetBorder(AIndex : Integer; AValue : Integer);
    procedure SetColor(AIndex : Integer; AValue : TColor);
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property BorderTop    : Integer index 0 read FBorderTop    write SetBorder;
    property BorderLeft   : Integer index 1 read FBorderLeft   write SetBorder;
    property BorderRight  : Integer index 2 read FBorderRight  write SetBorder;
    property BorderBottom : Integer index 3 read FBorderBottom write SetBorder;

    property Color        : TColor  index 0 read FColor        write SetColor;
    property BorderColor  : TColor  index 1 read FBorderColor  write SetColor;

    property Align;
    property Cursor;
    property Caption;
    property Font;
    property ParentFont;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property TabOrder;
    property TabStop;
    property Hint;
    property HelpContext;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Padding;
  end;

implementation

uses NeoFlat.Classes;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TFlatPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  FBorderTop    := 0;
  FBorderLeft   := 0;
  FBorderRight  := 0;
  FBorderBottom := 0;

  Font.Height  := -11;
  Font.Name    := FONT_1;
  Font.Color   := MAIN_ACCENT;

  ControlStyle  := ControlStyle + [csAcceptsControls, csOpaque];

  DoubleBuffered := True;

  FColor := MAIN_GRAY;
  FBorderColor := clBlack;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TFlatPanel.Destroy();
begin

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TFlatPanel.Paint();
var ARect         : TRect;

    ABorderTop    : Integer;
    ABorderLeft   : Integer;
    ABorderRight  : Integer;
    ABorderBottom : Integer;
begin
  ABorderTop    := self.ScaleValue(FBorderTop);
  ABorderLeft   := self.ScaleValue(FBorderLeft);
  ABorderRight  := self.ScaleValue(FBorderRight);
  ABorderBottom := self.ScaleValue(FBorderBottom);

  Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;

    {
      Draw Background
    }
    Canvas.Brush.Color := FColor;
    Canvas.FillRect(self.ClientRect);

    {
      Draw Borders
    }
    Canvas.Brush.Color := FBorderColor;
    ///

    if (ABorderTop > 0) then begin
      ARect.Left   := 0;
      ARect.Top    := 0;
      ARect.Width  := ClientWidth;
      ARect.Height := ABorderTop;

      Canvas.FillRect(ARect);
    end;

    if (ABorderRight > 0) then begin
      ARect.Left   := (ClientWidth - ABorderRight);
      ARect.Top    := 0;
      ARect.Width  := ABorderRight;
      ARect.Height := ClientHeight;

      Canvas.FillRect(ARect);
    end;

    if (ABorderBottom > 0) then begin
      ARect.Left   := 0;
      ARect.Top    := (ClientHeight - ABorderBottom);
      ARect.Width  := ClientWidth;
      ARect.Height := ABorderBottom;

      Canvas.FillRect(ARect);
    end;

    if (ABorderLeft > 0) then begin
      ARect.Left   := 0;
      ARect.Top    := 0;
      ARect.Width  := ABorderLeft;
      ARect.Height := ClientHeight;

      Canvas.FillRect(ARect);
    end;
  finally
    Canvas.UnLock();
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TFlatPanel.SetBorder(AIndex : Integer; AValue : Integer);
begin
  case AIndex of
    0 : FBorderTop    := AValue;
    1 : FBorderLeft   := AValue;
    2 : FBorderRight  := AValue;
    3 : FBorderBottom := AValue;
  end;

  ///
  Invalidate();
end;

procedure TFlatPanel.SetColor(AIndex : Integer; AValue : TColor);
begin
  case AIndex of
    0 : FColor       := AValue;
    1 : FBorderColor := AValue;
  end;

  ///
  Invalidate();
end;

end.
