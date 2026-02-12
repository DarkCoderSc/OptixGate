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

unit NeoFlat.PaintScene;

interface

uses System.Classes, VCL.Controls, VCL.Graphics;

type
  TOnPaint = procedure(Sender : TObject; ACanvas : TCanvas) of object;

  TFlatPaintScene = class(TCustomControl)
  private
    FOnPaint : TOnPaint;
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property OnPaint : TOnPaint read FOnPaint write FOnPaint;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

implementation

{ TFlatPaintScene.Create }

constructor TFlatPaintScene.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ControlStyle  := ControlStyle + [csAcceptsControls, csOpaque];

  FOnPaint := nil;
end;

{ TFlatPaintScene.Destroy }

destructor TFlatPaintScene.Destroy();
begin

  ///
  inherited Destroy();
end;

{ TFlatPaintScene.Paint }

procedure TFlatPaintScene.Paint();
begin
  Canvas.Lock();
  try
    if Assigned(FOnPaint) then
      FOnPaint(self, Canvas);
  finally
    Canvas.Unlock();
  end;
end;

end.
