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

unit NeoFlat.GroupBox;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Classes,

  Winapi.Windows, Winapi.Messages,

  VCL.Forms, VCL.Graphics, VCL.Controls, VCL.ExtCtrls,

  NeoFlat.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatGroupBox = class(TCustomControl)
  private
    FBorderColor : TColor;
    FMetrics     : TFlatMetrics;

    {@M}
    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var AMessage: TWmNoParams); message CM_TEXTCHANGED;
    procedure SetColors(const AIndex: Integer; const AValue: TColor);
    procedure CMSysColorChange(var AMessage: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var AMessage: TWmNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMDialogChar(var AMessage: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    {@M}
    procedure Paint; override;
  public
    {@C}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property Align;
    property Cursor;
    property Caption;
    property Font;
    property ParentFont;
    property Color;
    property Padding;
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

    property ColorBorder: TColor index 0 read FBorderColor write SetColors default clBlack;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Types,

  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

constructor TFlatGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  FMetrics := TFlatMetrics.Create(self);

  ControlStyle := ControlStyle + [
    csAcceptsControls,
    csOpaque
  ];

  DoubleBuffered := True;

  Color        := MAIN_GRAY;
  FBorderColor := MAIN_ACCENT;
  Font.Name    := FONT_1;
  Font.Color   := MAIN_ACCENT;
  Font.Height  := -11;

  ///
  SetBounds(0, 0, FMetrics.ScaleValue(185), FMetrics.ScaleValue(105));
end;

destructor TFlatGroupBox.Destroy();
begin
  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  ///
  inherited Destroy();
end;

procedure TFlatGroupBox.Paint;
begin
  var AFormat := DT_TOP or DT_LEFT or DT_SINGLELINE;

  Canvas.Lock();
  try
    Canvas.Font.Assign(Font);

    var ATextHeight := Canvas.TextHeight(Caption);
    var ATextWidth := Canvas.TextWidth(Caption);

    var ATextBounds := Rect(
      ClientRect.Left + FMetrics._10,
      ClientRect.Top,
      ClientRect.Right - FMetrics._10,
      ClientRect.Top + ATextHeight
    );

    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);

    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Width := 1;

    var AMidTextY := ClientRect.Top + (ATextHeight div 2);

    // (!) Using Canvas.FillRect(<>) is the most reliable approach for proper HDPI rendering (preferable to using
    // Polyline or MoveTo / LineTo for drawing thick borders)

    Canvas.Brush.Color := MAIN_ACCENT;
    Canvas.Brush.Style := bsSolid;

    // Left vertical
    Canvas.FillRect(Rect(ClientRect.Left, AMidTextY, ClientRect.Left + FMetrics._1, ClientRect.Bottom));

    // Bottom horizontal
    Canvas.FillRect(Rect(ClientRect.Left, ClientRect.Bottom - FMetrics._1, ClientRect.Right, ClientRect.Bottom));

    // Right vertical
    Canvas.FillRect(Rect(ClientRect.Right - FMetrics._1, AMidTextY, ClientRect.Right, ClientRect.Bottom));

    // --- [TEXT] ----------------

    // Top left segment
    Canvas.FillRect(
      Rect(
        ClientRect.Left + FMetrics._5,
        AMidTextY,
        ClientRect.Left + FMetrics._5 + (ClientRect.Left - (ClientRect.Left + FMetrics._5)),
        AMidTextY + FMetrics._1
      )
    );

    // Top right segment
    Canvas.FillRect(Rect(
      ClientRect.Left + FMetrics._12 + ATextWidth,
      AMidTextY,
      ClientRect.Right,
      AMidTextY + FMetrics._1
    ));

    Canvas.Brush.Style := bsClear;

    if not Enabled then begin
      OffsetRect(ATextBounds, FMetrics._1, FMetrics._1);

      Canvas.Font.Color := clBtnHighlight;

      DrawText(Canvas.Handle, PWideChar(Caption), Length(Caption), ATextBounds, AFormat);

      OffsetRect(ATextBounds, -FMetrics._1, -FMetrics._1);

      Canvas.Font.Color := clBtnShadow;

      DrawText(Canvas.Handle, PWideChar(Caption), Length(Caption), ATextBounds, AFormat);
    end else
      DrawText(Canvas.Handle, PWideChar(Caption), Length(Caption), ATextBounds, AFormat);

    Canvas.CopyRect(ClientRect, Canvas, ClientRect);
  finally
    Canvas.Unlock();
  end;
end;

procedure TFlatGroupBox.CMTextChanged(var AMessage: TWmNoParams);
begin
  inherited;
  ///

  Invalidate;
end;

procedure TFlatGroupBox.SetColors(const AIndex: Integer; const AValue: TColor);
begin
  case AIndex of
    0: FBorderColor := AValue;
  end;

  ///
  Invalidate;
end;

procedure TFlatGroupBox.CMParentColorChanged(var AMessage: TWmNoParams);
begin
  inherited;
  ///

  Invalidate;
end;

procedure TFlatGroupBox.CMSysColorChange(var AMessage: TMessage);
begin
  Invalidate;
end;

procedure TFlatGroupBox.CMDialogChar(var AMessage: TCMDialogChar);
begin
  with AMessage do
    if IsAccel(AMessage.CharCode, Caption) and CanFocus then begin
      SetFocus();

      Result := 1;
    end;
end;

procedure TFlatGroupBox.CMEnabledChanged(var AMessage: TMessage);
begin
  inherited;

  ///
  Invalidate;
end;

end.
