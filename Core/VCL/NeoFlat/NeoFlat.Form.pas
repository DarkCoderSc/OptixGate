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
unit NeoFlat.Form;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows, Winapi.Messages,

  VCL.Controls, VCL.Forms, VCL.Graphics;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatForm = class(TComponent)
  private
    FOldWindowProc : TWndMethod;
    FOwnerForm     : TForm;
    FResizable     : Boolean;
    FShowBorder    : Boolean;
    FColor         : TColor;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);
    procedure SetShowBorder(const AValue : Boolean);
    procedure SetColor(const AValue : TColor);
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Resizable  : Boolean read FResizable  write FResizable;
    property ShowBorder : Boolean read FShowBorder write SetShowBorder;
    property Color      : TColor  read FColor      write SetColor;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

procedure TFlatForm.OnCustomWindowProc(var AMessage : TMessage);
begin
  var ADoCallBase := True;
  try
    if (csDesigning in ComponentState) then
      Exit;
    ///

    case AMessage.Msg of
      // Form On Show
      WM_SHOWWINDOW : begin
        if AMessage.WParam = 1 then
          // Fix scaling issue when using HDPI. The Scale Factor is not available
          // Before owner form is shown, so we refresh this property to apply correct
          // border size (scaled)
          SetShowBorder(FShowBorder);
      end;

      // Form Border Resize
      WM_NCHITTEST : begin
        if NOT FResizable then
          Exit;
        ///

        var AFrameEdges := Rect(8, 8, 8, 8);

        // Define corners
        var ARect : TRect;

        ARect.Left    := TWMNCHitTest(AMessage).XPos  - FOwnerForm.BoundsRect.Left;
        ARect.Top    := TWMNCHitTest(AMessage).YPos  - FOwnerForm.BoundsRect.Top;
        ARect.Bottom := FOwnerForm.BoundsRect.Bottom - TWMNCHitTest(AMessage).YPos;
        ARect.Right  := FOwnerForm.BoundsRect.Right  - TWMNCHitTest(AMessage).XPos;

        if (ARect.Top < AFrameEdges.Top) and (ARect.Left < AFrameEdges.Left) then
          TWMNCHitTest(AMessage).Result := HTTOPLEFT
        else if (ARect.Top < AFrameEdges.Top) and (ARect.Right < AFrameEdges.Right) then
          TWMNCHitTest(AMessage).Result := HTTOPRIGHT
        else if (ARect.Bottom < AFrameEdges.Bottom) and (ARect.Left < AFrameEdges.Left) then
          TWMNCHitTest(AMessage).Result := HTBOTTOMLEFT
        else if (ARect.Bottom < AFrameEdges.Bottom) and (ARect.Right < AFrameEdges.Right) then
          TWMNCHitTest(AMessage).Result := HTBOTTOMRIGHT
        else if (ARect.Top < AFrameEdges.Top) then
          TWMNCHitTest(AMessage).Result := HTTOP
        else if (ARect.Left < AFrameEdges.Left) then
          TWMNCHitTest(AMessage).Result := HTLEFT
        else if (ARect.Bottom < AFrameEdges.Bottom) then
          TWMNCHitTest(AMessage).Result := HTBOTTOM
        else if (ARect.Right < AFrameEdges.Right) then
          TWMNCHitTest(AMessage).Result := HTRIGHT
        else
          TWMNCHitTest(AMessage).Result := HTCLIENT;

        ///
        ADoCallBase := False;
      end;
    end;
  finally
    if ADoCallBase then
      FOldWindowProc(AMessage);
  end;
end;

constructor TFlatForm.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOwnerForm := nil;

  if NOT Assigned(AOwner) then
    Exit;

  if NOT (AOwner is TForm) then
    Exit;
  ///

  FOwnerForm := TForm(AOwner);

  SetColor(MAIN_ACCENT);

  FOwnerForm.DoubleBuffered := True;
  FOwnerForm.BorderStyle    := bsNone;
  FOwnerForm.Font.Name      := FONT_1;

  SetShowBorder(True);

  FOldWindowProc := FOwnerForm.WindowProc;
  FOwnerForm.WindowProc := OnCustomWindowProc;

  FResizable := True;
end;

destructor TFlatForm.Destroy();
begin
  if Assigned(FOldWindowProc) then
    FOwnerForm.WindowProc := FOldWindowProc;

  ///
  inherited Destroy();
end;

procedure TFlatForm.SetShowBorder(const AValue : Boolean);
begin
  if not Assigned(FOwnerForm) then
    Exit;
  ///

  if AValue then
    FOwnerForm.BorderWidth := FOwnerForm.ScaleValue(2)
  else
    FOwnerForm.BorderWidth := 0;

  FShowBorder := AValue;
end;

procedure TFlatForm.SetColor(const AValue : TColor);
begin
  if not Assigned(FOwnerForm) then
    Exit;
  ///

  FColor           := AValue;
  FOwnerForm.Color := FColor;
end;

end.
