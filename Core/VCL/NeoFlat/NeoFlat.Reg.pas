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

unit NeoFlat.Reg;

interface

uses System.Classes, NeoFlat.Button, NeoFlat.CaptionBar, NeoFlat.Edit, NeoFlat.Form, 
     NeoFlat.CheckBox, NeoFlat.Panel,
     NeoFlat.StatusBar, NeoFlat.TreeView, NeoFlat.ImageButton, NeoFlat.ComboBox, NeoFlat.THeme, NeoFlat.MessageBox,
     NeoFlat.Hint, NeoFlat.OptionDialog, NeoFlat.Gauge, NeoFlat.PopupMenu, NeoFlat.DockCaption, NeoFlat.GroupBox,
     NeoFlat.SettingHandler, NeoFlat.ScrollingCredit, NeoFlat.Timer, NeoFlat.PaintScene;

procedure register;

implementation

{-------------------------------------------------------------------------------
  ___Register
-------------------------------------------------------------------------------}
procedure register;
begin
  RegisterComponents('NeoFlat', [
                              TFlatButton,
                              TFlatCaptionbar,
                              TFlatEdit,
                              TFlatForm,
                              TFlatCheckBox,
                              TFlatPanel,
                              TFlatStatusBar,
                              TFlatVirtualStringTree,
                              TFlatImageButton,
                              TFlatComboBox,
                              TFlatMessageBox,
                              TFlatHint,
                              TFlatOptionDialog,
                              TFlatGauge,
                              TFlatPopupMenu,
                              TFlatDockCaption,
                              TFlatGroupBox,
                              TFlatSettingHandler,
                              TFlatScrollingCredit,
                              TFlatTimer,
                              TFlatPaintScene
  ]);
end;

end.
