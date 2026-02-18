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

unit NeoFlat.ComboBox;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  WinAPI.Messages, WinAPI.Windows,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics,

  NeoFlat.Types, NeoFlat.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatComboBox = class(TCustomComboBox)
  private
    FArrowColor       : TColor;
    FBorderColor      : TColor;
    FButtonBackground : TColor;

    FButtonWidth      : Integer;

    FMetrics          : TFlatMetrics;

    FValidators       : TValidators;

    FStatus           : TControlStatus;

    {@M}
    procedure WMPaint(var AMessage: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var AMessage: TMessage); message WM_NCPAINT;
    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
    procedure CMFontChanged(var AMessage: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var AMessage: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var AMessage: TWMNoParams); message CM_PARENTCOLORCHANGED;

    procedure RedrawBorders();
    procedure PaintBorder();

    procedure PaintButton();
    function GetButtonRect(): TRect;

    function GetIsValid() : Boolean;
    procedure SetStatus(const AStatus : TControlStatus);
    procedure DoValidate();
    procedure SetValidators(const AValue : TValidators);
  protected
    {@M}
    procedure ComboWndProc(var AMessage: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure Change; override;

    procedure SetEnabled(AValue : Boolean); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    function HasSelectedItem() : Boolean;

    {@G}
    property IsValid : Boolean read GetIsValid;
  published
    property Align;
    property Enabled;
    property Style;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Font;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property ItemIndex;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;

    {@G/S}
    property Status     : TControlStatus read FStatus     write SetStatus;
    property Validators : TValidators    read FValidators write SetValidators;

  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math, System.Types, System.SysUtils,

  NeoFlat.Theme, NeoFlat.Validators;
// ---------------------------------------------------------------------------------------------------------------------

procedure TFlatComboBox.Change();
begin
  inherited;
  ///

  if (Status = cStatusError) then
    DoValidate();
end;

procedure TFlatComboBox.DoValidate();
begin
  if Validate(Text, FValidators) then
    Status := cStatusNormal
  else
    Status := cStatusError;
end;

constructor TFlatComboBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ControlStyle := ControlStyle - [csOpaque];

  FArrowColor       := MAIN_ACCENT;
  FBorderColor      := MAIN_ACCENT;
  Color             := clWhite;
  FButtonBackground := RGB(204, 191, 190);

  Font.Color  := MAIN_ACCENT;
  Font.Name   := FONT_1;
  Font.Height := -11;

  FMetrics := TFlatMetrics.Create(self);

  FValidators := [];

  DoubleBuffered := True;

  FStatus := cStatusNormal;

  ShowHint := True;

  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
end;

destructor TFlatComboBox.Destroy();
begin
  if Assigned(FMetrics) then
    FreeAndNil(FMetrics);

  ///
  inherited Destroy();
end;

procedure TFlatComboBox.ComboWndProc(var AMessage: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  inherited;
  ///

  if (ComboWnd <> EditHandle) then
    Exit;
end;

procedure TFlatComboBox.RedrawBorders();
begin
  PaintBorder();
  ///

  if Style <> csSimple then
    PaintButton();
end;

procedure TFlatComboBox.PaintBorder();
begin
  var ADC := GetWindowDC(Handle);
  try
    var ABorderColor : TColor;
    if (Status = cStatusError) and Enabled then
      ABorderColor := MAIN_RED
    else
      ABorderColor := FBorderColor;

    var ABrush := CreateSolidBrush(ColorToRGB(ABorderColor));
    try
      var AClientRect : TRect;
      GetWindowRect(Handle, AClientRect);

      // Border Top
      var ARect := TRect.Empty;
      ARect.Width  := AClientRect.Width;
      ARect.Height := FMetrics._1;

      FillRect(ADC, ARect, ABrush);

      // Border Left
      ARect := TRect.Empty;
      ARect.Width := FMetrics._1;
      ARect.Height := AClientRect.Height;

      FillRect(ADC, ARect, ABrush);

      // Border Bottom
      ARect := TRect.Empty;
      ARect.Top    := AClientRect.Height - FMetrics._1;
      ARect.Width  := AClientRect.Width;
      ARect.Height := FMetrics._1;

      FillRect(ADC, ARect, ABrush);
    finally
      DeleteObject(ABrush);
    end;
  finally
    ReleaseDC(Handle, ADC);
  end;
end;

function TFlatComboBox.GetButtonRect(): TRect;
begin
  GetWindowRect(Handle, Result);
  OffsetRect(Result, -Result.Left, -Result.Top);

  Inc(Result.Left, ClientWidth - FButtonWidth);
  OffsetRect(Result, -1, 0);
end;

procedure TFlatComboBox.PaintButton();
begin
  var ARect := GetButtonRect();
  InflateRect(ARect, 1, 0);

  var AArrowColor  : TColor;
  var ABorderColor : TColor;
  if (Status = cStatusError) and Enabled then begin
    AArrowColor  := MAIN_RED;
    ABorderColor := MAIN_RED;
  end else begin
    AArrowColor  := FArrowColor;
    ABorderColor := FBorderColor;
  end;

  // Draw Button Background
  Canvas.Brush.Color := FButtonBackground;
  Canvas.FillRect(ARect);

  // Draw Button Border Left
  Canvas.Brush.Color := ABorderColor;
  Canvas.FillRect(Rect(
    ARect.Left,
    ARect.Top,
    ARect.Left + FMetrics._1,
    ARect.Bottom
  ));

  // Draw Button Border Top
  Canvas.FillRect(Rect(
    ARect.Left,
    ARect.Top,
    ARect.Right,
    FMetrics._1
  ));

  // Draw Button Border Right
  Canvas.FillRect(Rect(
    ARect.Right - FMetrics._1,
    ARect.Top,
    ARect.Right,
    ARect.Bottom
  ));

  // Draw Button Border Bottom
  Canvas.FillRect(Rect(
    ARect.Left,
    ARect.Bottom - FMetrics._1,
    ARect.Right,
    ARect.Bottom
  ));

  var X := (ARect.Right - ARect.Left) div 2 - FMetrics._6 + ARect.Left;

  var Y : Integer;
  if DroppedDown then
    Y := (ARect.Bottom - ARect.Top) div 2 - FMetrics._1 + ARect.Top
  else
    Y := (ARect.Bottom - ARect.Top) div 2 - FMetrics._1 + ARect.Top;

  if Enabled then begin
    canvas.Brush.Color := AArrowColor;
    canvas.Pen.Color   := AArrowColor;

    if DroppedDown then
      canvas.Polygon([
        Point(x + FMetrics._4, y + FMetrics._2),
        Point(x + FMetrics._8, y + FMetrics._2),
        Point(x + FMetrics._6, y)
      ])
    else
      canvas.Polygon([
        Point(x + FMetrics._4, y),
        Point(x + FMetrics._8, y),
        Point(x + FMetrics._6, y + FMetrics._2)
      ]);
  end else begin
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color   := clWhite;

    Inc(x);
    Inc(y);

    if DroppedDown then
      canvas.Polygon([
        Point(x + FMetrics._4, y + FMetrics._2),
        Point(x + FMetrics._8, y + FMetrics._2),
        Point(x + FMetrics._6, y)
      ])
    else
      canvas.Polygon([
        Point(x + FMetrics._4, y),
        Point(x + FMetrics._8, y),
        Point(x + FMetrics._6, y + FMetrics._2)
      ]);

    Dec(x);
    Dec(y);

    canvas.Brush.Color := clGray;
    canvas.Pen.Color   := clGray;

    if DroppedDown then
      canvas.Polygon([
        Point(x + FMetrics._4, y + FMetrics._2),
        Point(x + FMetrics._8, y + FMetrics._2),
        Point(x + FMetrics._6, y)
      ])
    else
      canvas.Polygon([
        Point(x + FMetrics._4, y),
        Point(x + FMetrics._8, y),
        Point(x + FMetrics._6, y + FMetrics._2)
      ]);
  end;

  ExcludeClipRect(
                    Canvas.Handle,
                    (ClientWidth - FButtonWidth),
                    0,
                    ClientWidth,
                    ClientHeight
  );
end;

procedure TFlatComboBox.CMSysColorChange (var AMessage: TMessage);
begin
  Invalidate;
end;

procedure TFlatComboBox.CMParentColorChanged(var AMessage: TWMNoParams);
begin
  Invalidate;
end;

procedure TFlatComboBox.CMEnabledChanged(var AMessage : TMessage);
begin
  inherited;
  ///

  Invalidate;
end;

procedure TFlatComboBox.CNCommand(var AMessage: TWMCommand);
begin
  inherited;
  ///

  if (AMessage.NotifyCode in [CBN_CLOSEUP]) then begin
    var ARect := GetButtonRect;
    Dec(ARect.Left, FMetrics._2);

    InvalidateRect(Handle, @ARect, FALSE);
  end;
end;

procedure TFlatComboBox.WMPaint(var AMessage: TWMPaint);

  function RectInRect(R1, R2: TRect): Boolean;
  begin
    Result := IntersectRect(R1, R1, R2);
  end;

begin
  var APaintStruct : TPaintStruct;
  var DC := BeginPaint(Handle, APaintStruct);
  try
    var ARect := APaintStruct.rcPaint;
    if ARect.Right > Width - FButtonWidth - FMetrics._4 then
      ARect.Right := Width - FButtonWidth - FMetrics._4;

    FillRect(DC, ARect, Brush.Handle);
    if RectInRect(GetButtonRect, APaintStruct.rcPaint) then
      PaintButton();

    ExcludeClipRect(DC, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);

    ///
    PaintWindow(DC);
  finally
    EndPaint(Handle, APaintStruct);
  end;
  RedrawBorders();

  ///
  AMessage.Result := 0;
end;

procedure TFlatComboBox.WMNCPaint(var AMessage: TMessage);
begin
  inherited;
  ///

  RedrawBorders();
end;

procedure TFlatComboBox.CMFontChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  RecreateWnd();
end;

procedure TFlatComboBox.SetEnabled(AValue : Boolean);
begin
  if AValue = Enabled then
    Exit;
  ///

  inherited SetEnabled(AValue);

  if AValue then
    Font.Color := clWhite
  else
    Font.Color := clGray;
end;

procedure TFlatComboBox.SetStatus(const AStatus : TControlStatus);
begin
  if AStatus = FStatus then
    Exit;
  ///

  FStatus := AStatus;

  ///
  Invalidate;
end;

procedure TFlatComboBox.SetValidators(const AValue : TValidators);
begin
  if AValue = FValidators then
    Exit;
  ///

  FValidators := AValue;

  ///
  Invalidate;
end;

function TFlatComboBox.HasSelectedItem() : Boolean;
begin
  result := (ItemIndex >= 0) and (ItemIndex <= Items.count -1);
end;

function TFlatComboBox.GetIsValid() : Boolean;
begin
  DoValidate();
  ///

  result := (Status = cStatusNormal);
end;

end.
