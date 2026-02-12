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

unit NeoFlat.Metrics;

interface

uses VCL.Controls;

type
  TFlatMetrics = class
  private
    FControl : TControl;

    {@M}
    function GetMetric(const AIndex : Integer) : Integer;
  public
    {@C}
    constructor Create(const AControl : TControl);

    {@G}
    property _1  : Integer index 1  read GetMetric;
    property _2  : Integer index 2  read GetMetric;
    property _3  : Integer index 3  read GetMetric;
    property _4  : Integer index 4  read GetMetric;
    property _5  : Integer index 5  read GetMetric;
    property _6  : Integer index 6  read GetMetric;
    property _7  : Integer index 7  read GetMetric;
    property _8  : Integer index 8  read GetMetric;
    property _9  : Integer index 9  read GetMetric;
    property _10 : Integer index 10 read GetMetric;
    property _11 : Integer index 11 read GetMetric;
    property _12 : Integer index 12 read GetMetric;
    property _13 : Integer index 13 read GetMetric;
    property _14 : Integer index 14 read GetMetric;
    property _15 : Integer index 15 read GetMetric;
    property _16 : Integer index 16 read GetMetric;
    property _17 : Integer index 17 read GetMetric;
    property _18 : Integer index 18 read GetMetric;
    property _19 : Integer index 19 read GetMetric;
    property _20 : Integer index 20 read GetMetric;
  end;

implementation

{ TFlatMetrics.GetMetric }
function TFlatMetrics.GetMetric(const AIndex : Integer) : Integer;
begin
  if not Assigned(FControl) then
    result := AIndex
  else
    result := FControl.ScaleValue(AIndex);
end;

{ TFlatMetrics.Create }
constructor TFlatMetrics.Create(const AControl : TControl);
begin
  inherited Create();
  ///

  FControl := AControl;
end;

end.
