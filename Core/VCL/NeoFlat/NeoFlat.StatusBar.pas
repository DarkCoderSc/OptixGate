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

unit NeoFlat.StatusBar;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows,

  VCL.Controls, VCL.Forms, VCL.Graphics,

  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

type
  TStatusPanel = class(TCollectionItem)
  private
    FText      : String;
    FWidth     : Integer;
    FHint      : String;
    FAlignment : TAlignment;
    FRect      : TRect;

    {@M}
    procedure Invalidate;

    procedure SetText(const AValue : String);
    procedure SetWidth(const AValue : Integer);
    procedure SetAlignment(const AValue : TAlignment);
  public
    {@C}
    constructor Create(ACollection : TCollection); override;
    destructor Destroy(); override;

    {@G/S}
    property Rect : TRect read FRect write FRect;
  published
    {@G/S}
    property Text      : String     read FText      write SetText;
    property Width     : Integer    read FWidth     write SetWidth;
    property Hint      : String     read FHint      write FHint;
    property Alignment : TAlignment read FAlignment write SetAlignment;
  end;

  TStatusPanels = class(TOwnedCollection)
  private
    {@M}
    procedure Invalidate;
  protected
    {@M}
    function GetItem(Index: Integer): TStatusPanel;
    procedure SetItem(Index: Integer; Value: TStatusPanel);
  public
    {@C}
    constructor Create(AOwner: TPersistent);

    {@M}
    function Add: TStatusPanel;

    {@G/S}
    property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
  end;

  TFlatStatusBar = class(TCustomControl)
  private
    FPanels : TStatusPanels;

    {@M}
    procedure CMHintShow(var AMessage: TCMHintShow); message CM_HINTSHOW;

    procedure RefreshPanelsRect();
    function GetPanelByPoint(APoint : TPoint) : TStatusPanel;
  protected
    {@M}
    procedure Paint; override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    property Font;

    {@G/S}
    property Panels : TStatusPanels read FPanels write FPanels;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Types;
// ---------------------------------------------------------------------------------------------------------------------

(* TFlatStatusBar *)

function TFlatStatusBar.GetPanelByPoint(APoint : TPoint) : TStatusPanel;
begin
  result := nil;
  ///

  for var I := 0 to Panels.Count -1 do begin
    var APanelItem := Panels.Items[i];
    ///

    if ptinrect(APanelItem.Rect, APoint) then
      Exit(APanelItem);
  end;
end;

procedure TFlatStatusBar.RefreshPanelsRect();
begin
  var ARect    := TRect.Empty;
  ARect.Top    := 1;
  ARect.Height := (ClientHeight - 2);

  for var I := 0 to Panels.Count -1 do begin
    var APanelItem := Panels.Items[i];
    ///

    ARect.Left  := (ARect.Left + ARect.Width + 1);

    if I = (Panels.Count -1) then
      ARect.Width := (ClientWidth - ARect.Left - 1)
    else
      ARect.Width := Panels[i].Width;

    ///
    APanelItem.Rect := ARect;
  end;
end;

constructor TFlatStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  Align := alBottom;

  FPanels := TStatusPanels.Create(self);

  Font.Name   := FONT_1;
  Font.Color  := clBlack;
  Font.Height := -11;

  ShowHint := True;

  Height := 19;
end;

destructor TFlatStatusBar.Destroy();
begin
  if Assigned(FPanels) then
    FreeAndNil(FPanels);

  ///
  inherited Destroy();
end;

procedure TFlatStatusBar.Paint;
begin
  Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Assign(Font);
    ///

    // Draw Background
    var ARect    := TRect.Empty;
    ARect.Width  := ClientWidth;
    ARect.Height := ClientHeight;

    Canvas.Brush.Color := clBlack;

    Canvas.FillRect(ARect);

    // Draw Panels
    Canvas.Brush.Color := MAIN_GRAY;

    RefreshPanelsRect();

    for var I := 0 to (Panels.Count -1) do begin
      ARect := Panels.Items[i].Rect;
      ///

      // Draw Panel Backgrouind
      Canvas.FillRect(ARect);

      // Draw Panel Caption
      var ACaption := Panels[I].Text;

      var ATextRect    := TRect.Empty;
      ATextRect.Top    := ARect.Top;
      ATextRect.Height := ARect.Height;
      ATextRect.Left   := (ARect.Left + 2);
      ATextRect.Width  := (ARect.Width - 4);

      var ATextFormat : TTextFormat := [tfEndEllipsis, tfSingleLine, tfVerticalCenter];

      case Panels[I].Alignment of
        taLeftJustify  : ATextFormat := ATextFormat + [tfLeft];
        taRightJustify : ATextFormat := ATextFormat + [tfRight];
        taCenter       : ATextFormat := ATextFormat + [tfCenter];
      end;

      ///
      Canvas.TextRect(ATextRect, ACaption, ATextFormat);
    end;
  finally
    Canvas.Unlock();
  end;
end;

procedure TFlatStatusBar.CMHintShow(var AMessage: TCMHintShow);
begin
  var APanelItem := GetPanelByPoint(AMessage.HintInfo.CursorPos);

  if Assigned(APanelItem) then begin
    AMessage.HintInfo.HintStr := APanelItem.Hint;
    AMessage.HintInfo.CursorRect := APanelItem.Rect;
  end;

  ///
  inherited;
end;

(* TStatusPanel *)

constructor TStatusPanel.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  ///

  FText      := '';
  FWidth     := 50;
  FHint      := '';
  FAlignment := taLeftJustify;
  FRect      := TRect.Empty;

  ///
  Invalidate;
end;

destructor TStatusPanel.Destroy();
begin
  inherited Destroy();
  ///

  Invalidate;
end;

procedure TStatusPanel.Invalidate;
begin
  if not Assigned(GetOwner()) then
    Exit();
  ///

  if Assigned(TStatusPanels(GetOwner())) then
    TFlatStatusBar(TStatusPanels(GetOwner()).GetOwner).Invalidate;
end;

procedure TStatusPanel.SetText(const AValue : String);
begin
  if (FText = AValue) then
    Exit;
  ///

  FText := AValue;

  ///
  Invalidate;
end;

procedure TStatusPanel.SetWidth(const AValue : Integer);
begin
  if (FWidth = AValue) then
    Exit;
  ///

  FWidth := AValue;

  ///
  Invalidate;
end;

procedure TStatusPanel.SetAlignment(const AValue : TAlignment);
begin
  if (AValue = FAlignment) then
    Exit;
  ///

  FAlignment := AValue;

  ///
  Invalidate;
end;

(* TStatusPanels *)

function TStatusPanels.Add: TStatusPanel;
begin
  Result := TStatusPanel(inherited Add);
end;

constructor TStatusPanels.create(AOwner : TPersistent);
begin
  inherited Create(AOwner, TStatusPanel);
  ///

end;

function TStatusPanels.GetItem(Index: Integer): TStatusPanel;
begin
  Result := TStatusPanel(inherited GetItem(Index));
end;

procedure TStatusPanels.SetItem(Index: Integer; Value: TStatusPanel);
begin
  inherited SetItem(Index, Value);
  ///

  Invalidate;
end;

procedure TStatusPanels.Invalidate;
begin
  if Assigned(GetOwner()) then
    TFlatStatusBar(GetOwner()).Invalidate;
end;

end.
