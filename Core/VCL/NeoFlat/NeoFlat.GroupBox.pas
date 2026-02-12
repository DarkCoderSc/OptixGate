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

unit NeoFlat.GroupBox;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, VCL.Forms, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.ExtCtrls;

type
  TFlatGroupBox = class(TCustomControl)
  private
    FBorderColor : TColor;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure SetColors(const Index: Integer; const Value: TColor);
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWmNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Cursor;
    property Caption;
    property Font;
    property ParentFont;
    property Color;
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
    property ColorBorder: TColor index 0 read FBorderColor write SetColors default $008396A0;
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
  end;

implementation

uses NeoFlat.Theme, System.Types;

constructor TFlatGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  ControlStyle := ControlStyle + [
                                    csAcceptsControls,
                                    csOpaque
  ];

  self.DoubleBuffered := True;

  FBorderColor    := clGray;
  self.Font.Name  := FONT_1;
  self.Font.Color := clGray;
  self.Color      := clBlack;

  ///
  SetBounds(0, 0, 185, 105);
end;

procedure TFlatGroupBox.Paint;
var ABorderRect : TRect;
    ATextBounds : TRect;
    ATextHeight : Integer;
    ATextWidth  : Integer;
    AFormat     : Cardinal;
begin
  ABorderRect := ClientRect;

  AFormat := DT_TOP  or
             DT_LEFT or
             DT_SINGLELINE;

  Canvas.Lock();
  try
    Canvas.Font.Assign(self.Font);

    ATextHeight := Canvas.textHeight(Caption);
    ATextWidth := Canvas.textWidth(Caption);

    ATextBounds := Rect(ClientRect.Left + 10, ClientRect.Top, ClientRect.Left + 10 + ATextWidth,
        ClientRect.Top + ATextHeight);

    ATextBounds := Rect(ClientRect.Left + 10, ClientRect.Top, ClientRect.Right - 10, ClientRect.Top + ATextHeight);

    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(ClientRect);

    Canvas.Pen.Color := FBorderColor;

    Canvas.Polyline([Point(ClientRect.Left + 5, ClientRect.Top + (ATextHeight div 2)),
      Point(ClientRect.Left, ClientRect.Top + (ATextHeight div 2)), Point(ClientRect.Left,
        ClientRect.Bottom - 1), Point(ClientRect.Right - 1, ClientRect.Bottom - 1), Point(ClientRect.Right - 1,
        ClientRect.Top + (ATextHeight div 2)), Point(ClientRect.Left + 12 + ATextWidth,
        ClientRect.Top + (ATextHeight div 2))]);

    Canvas.Brush.Style := bsClear;

    if not Enabled then begin
      OffsetRect(ATextBounds, 1, 1);

      Canvas.Font.Color := clBtnHighlight;

      DrawText(Canvas.Handle, PWideChar(Caption), Length(Caption), ATextBounds, AFormat);

      OffsetRect(ATextBounds, -1, -1);

      Canvas.Font.Color := clBtnShadow;

      DrawText(Canvas.Handle, PWideChar(Caption), Length(Caption), ATextBounds, AFormat);
    end else
      DrawText(Canvas.Handle, PWideChar(Caption), Length(Caption), ATextBounds, AFormat);

    Canvas.CopyRect(ClientRect, Canvas, ClientRect);
  finally
    Canvas.Unlock();
  end;
end;

procedure TFlatGroupBox.CMTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TFlatGroupBox.SetColors(const Index: Integer; const Value: TColor);
begin
  case Index of
    0:
      FBorderColor := Value;
  end;
  Invalidate;
end;

procedure TFlatGroupBox.CMParentColorChanged(var Message: TWmNoParams);
begin
  inherited;
  ///

  Invalidate;
end;

procedure TFlatGroupBox.CMSysColorChange(var Message: TMessage);
begin
  Invalidate;
end;

procedure TFlatGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus;

      Result := 1;
    end;
end;

procedure TFlatGroupBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;


end.
