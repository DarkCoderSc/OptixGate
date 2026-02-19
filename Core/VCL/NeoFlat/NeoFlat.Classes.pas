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

unit NeoFlat.Classes;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  VCL.Graphics, VCL.Controls,

  NeoFlat.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatStateColors = class(TPersistent)
  private
    FOwner    : TControl;

    FNormal   : TColor;
    FHover    : TColor;
    FActive   : TColor;
    FFocus    : TColor;
    FDisabled : TColor;

    {@M}
    procedure SetColor(const AIndex : Integer; const AColor : TColor);
  public
    {@C}
    constructor Create(AOwner : TControl); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;
    function GetStateColor(const AState : TFlatControlStateEx) : TColor;
  published
    {@G/S}
    property Normal   : TColor index 0 read FNormal   write SetColor;
    property Hover    : TColor index 1 read FHover    write SetColor;
    property Focus    : TColor index 2 read FFocus    write SetColor;
    property Active   : TColor index 3 read FActive   write SetColor;
    property Disabled : TColor index 4 read FDisabled write SetColor;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  WinAPI.Windows,

  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

(* TFlatStateColors *)

constructor TFlatStateColors.Create(AOwner : TControl);
begin
  inherited Create();
  ///

  FOwner    := AOwner;

  FNormal   := clNone;
  FHover    := clNone;
  FFocus    := clNone;
  FActive   := clNone;
  FDisabled := clNone;
end;

function TFlatStateColors.GetStateColor(const AState : TFlatControlStateEx) : TColor;
begin
  case AState of
    csExNormal   : result := FNormal;
    csExHover    : result := FHover;
    csExActive   : result := FActive;
    csExFocus    : result := FFocus;
    csExDisabled : result := FDisabled;

    else
      result := clNone;
  end;
end;

procedure TFlatStateColors.Assign(ASource : TPersistent);
begin
  if ASource is TFlatStateColors then begin
    FNormal   := TFlatStateColors(ASource).Normal;
    FHover    := TFlatStateColors(ASource).Hover;
    FFocus    := TFlatStateColors(ASource).Focus;
    FActive   := TFlatStateColors(ASource).Active;
    FDisabled := TFlatStateColors(ASource).Disabled;
  end else
    inherited;
end;

procedure TFlatStateColors.SetColor(const AIndex : Integer; const AColor : TColor);
begin
  case AIndex of
    {Normal}
    0 : begin
      FNormal := AColor;
    end;

    {Hover}
    1 : begin
      FHover := AColor;
    end;

    {Focus}
    2 : begin
      FFocus := AColor;
    end;

    {Active}
    3 : begin
      FActive := AColor;
    end;

    {Disabled}
    4 : begin
      FDisabled := AColor;
    end;
  end;

  ///
  FOwner.Invalidate;
end;

end.
