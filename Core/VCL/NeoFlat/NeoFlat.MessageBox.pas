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

unit NeoFlat.MessageBox;

interface

uses
  System.Classes, VCL.Forms;

type
  TFlatMessageBox = class(TComponent)
  private
    {@M}
  public
    {@M}
    function MessageBox(AOwner : TForm; const AMessage, ATitle : String; const AFlags : Integer = 0) : Integer;
    function InputQuery(AOwner : TForm; const ADescription, ATitle : String) : String;
  end;

implementation

uses NeoFlat.MessageBoxForm, NeoFlat.InputQueryForm, System.SysUtils;

{-------------------------------------------------------------------------------
  Spawn MessageBox
-------------------------------------------------------------------------------}
function TFlatMessageBox.MessageBox(AOwner : TForm; const AMessage, ATitle : String; const AFlags : Integer = 0) : Integer;
var AMessageBox : TSub7FormMessageBox;
begin
  AMessageBox := TSub7FormMessageBox.Create(AOwner);
  try
    AMessageBox.MessageBox(AMessage, ATitle, AFlags);

    AMessageBox.ShowModal();

    ///
    result := AMessageBox.Result;
  finally
    FreeAndNil(AMessageBox);
  end;
end;

{-------------------------------------------------------------------------------
  Spawn InputQuery
-------------------------------------------------------------------------------}
function TFlatMessageBox.InputQuery(AOwner : TForm; const ADescription, ATitle : String) : String;
var AInputQuery : TSub7FormInputQuery;
begin
  AInputQuery := TSub7FormInputQuery.Create(AOwner);
  try
    AInputQuery.CaptionBar.Caption   := ATitle;
    AInputQuery.LabelMessage.Caption := ADescription;

    AInputQuery.ShowModal();

    ///
    result := Trim(AInputQuery.Result);
  finally
    FreeAndNil(AInputQuery);
  end;
end;


end.
