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

unit NeoFlat.Panel;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows,

  VCL.Controls, VCL.Graphics,

  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

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
    procedure Paint; override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
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

    {@G/S}
    property BorderTop    : Integer index 0 read FBorderTop    write SetBorder;
    property BorderLeft   : Integer index 1 read FBorderLeft   write SetBorder;
    property BorderRight  : Integer index 2 read FBorderRight  write SetBorder;
    property BorderBottom : Integer index 3 read FBorderBottom write SetBorder;

    property Color        : TColor  index 0 read FColor        write SetColor;
    property BorderColor  : TColor  index 1 read FBorderColor  write SetColor;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Types,

  NeoFlat.Classes;
// ---------------------------------------------------------------------------------------------------------------------

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

destructor TFlatPanel.Destroy();
begin

  ///
  inherited Destroy();
end;

procedure TFlatPanel.Paint;
begin
  var ABorderTop    := ScaleValue(FBorderTop);
  var ABorderLeft   := ScaleValue(FBorderLeft);
  var ABorderRight  := ScaleValue(FBorderRight);
  var ABorderBottom := ScaleValue(FBorderBottom);

  Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;

    // Draw Background
    Canvas.Brush.Color := FColor;
    Canvas.FillRect(ClientRect);

    // Draw BOrders
    Canvas.Brush.Color := FBorderColor;
    ///

    var ARect := TRect.Empty;

    if (ABorderTop > 0) then begin
      ARect.Width  := ClientWidth;
      ARect.Height := ABorderTop;

      Canvas.FillRect(ARect);
    end;

    if (ABorderRight > 0) then begin
      ARect        := TRect.Empty;
      ARect.Left   := (ClientWidth - ABorderRight);
      ARect.Width  := ABorderRight;
      ARect.Height := ClientHeight;

      Canvas.FillRect(ARect);
    end;

    if (ABorderBottom > 0) then begin
      ARect        := TRect.Empty;
      ARect.Top    := (ClientHeight - ABorderBottom);
      ARect.Width  := ClientWidth;
      ARect.Height := ABorderBottom;

      Canvas.FillRect(ARect);
    end;

    if (ABorderLeft > 0) then begin
      ARect        := TRect.Empty;
      ARect.Width  := ABorderLeft;
      ARect.Height := ClientHeight;

      Canvas.FillRect(ARect);
    end;
  finally
    Canvas.UnLock();
  end;
end;

procedure TFlatPanel.SetBorder(AIndex : Integer; AValue : Integer);
begin
  case AIndex of
    0 : FBorderTop    := AValue;
    1 : FBorderLeft   := AValue;
    2 : FBorderRight  := AValue;
    3 : FBorderBottom := AValue;
  end;

  ///
  Invalidate;
end;

procedure TFlatPanel.SetColor(AIndex : Integer; AValue : TColor);
begin
  case AIndex of
    0 : FColor       := AValue;
    1 : FBorderColor := AValue;
  end;

  ///
  Invalidate;
end;

end.
