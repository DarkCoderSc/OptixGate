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

unit NeoFlat.Classes;

interface

uses System.Classes, VCL.Graphics, VCL.Controls;

type
  TFlatControlState = (
                      csNormal,
                      csHover,
                      csActive,
                      csFocus,
                      csDisabled
  );

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
    constructor Create(AOwner : TControl);
    destructor Destroy(); override;

    {@M}
    procedure Assign(ASource : TPersistent); override;
  published
    {@M}
    function GetStateColor(AState : TFlatControlState) : TColor;

    {@G/S}
    property Normal   : TColor index 0 read FNormal   write SetColor;
    property Hover    : TColor index 1 read FHover    write SetColor;
    property Focus    : TColor index 2 read FFocus    write SetColor;
    property Active   : TColor index 3 read FActive   write SetColor;
    property Disabled : TColor index 4 read FDisabled write SetColor;
  end;

implementation

uses NeoFlat.Theme, System.SysUtils, WinAPI.Windows;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    TFlatStateColors


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
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

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TFlatStateColors.Destroy();
begin

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Get State Color
-------------------------------------------------------------------------------}
function TFlatStateColors.GetStateColor(AState : TFlatControlState) : TColor;
begin
  case AState of
    csNormal   : result := FNormal;
    csHover    : result := FHover;
    csActive   : result := FActive;
    csFocus    : result := FFocus;
    csDisabled : result := FDisabled;

    else
      result := clNone;
  end;
end;

{-------------------------------------------------------------------------------
  ___assign
-------------------------------------------------------------------------------}
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

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

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
  FOwner.Invalidate();
end;

end.
