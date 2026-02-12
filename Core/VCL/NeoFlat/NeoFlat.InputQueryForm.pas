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

unit NeoFlat.InputQueryForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NeoFlat.Form, NeoFlat.Button, Vcl.StdCtrls,
  Vcl.ExtCtrls, NeoFlat.Panel, NeoFlat.CaptionBar, Vcl.Imaging.pngimage, NeoFlat.Edit;

type
  TSub7FormInputQuery = class(TForm)
    CaptionBar: TFlatCaptionBar;
    PanelClient: TFlatPanel;
    ImageIcon: TImage;
    LabelMessage: TLabel;
    PanelFooter: TFlatPanel;
    ButtonRight: TFlatButton;
    ButtonLeft: TFlatButton;
    SubSevenForms: TFlatForm;
    EditResponse: TFlatEdit;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonLeftClick(Sender: TObject);
    procedure ButtonRightClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FResult : String;

    {@M}
    procedure DoResize();
  public
    {@G}
    property Result : String read FResult;
  end;

var
  Sub7FormInputQuery: TSub7FormInputQuery;

implementation

uses VCL.Consts;

{$R *.dfm}

procedure TSub7FormInputQuery.ButtonLeftClick(Sender: TObject);
begin
  self.Close();
end;

procedure TSub7FormInputQuery.ButtonRightClick(Sender: TObject);
begin
  FResult := EditResponse.text;

  self.Close();
end;

procedure TSub7FormInputQuery.DoResize();
var AHeight : Integer;
const START_TOP = 16;
begin
  self.Constraints.MinHeight := 0;
  self.Constraints.MinWidth  := 0;

  ImageIcon.Left    := 16;

  LabelMessage.Left     := ImageIcon.Left + ImageIcon.Width + 16;
  LabelMessage.Top      := START_TOP;
  LabelMessage.Width    := PanelClient.Width - LabelMessage.Left - 16;

  AHeight               := LabelMessage.top + LabelMessage.Height + 8 + CaptionBar.Height;

  EditResponse.top      := LabelMessage.top + LabelMessage.Height + 8;
  EditResponse.left     := LabelMessage.Left;
  EditResponse.Width    := LabelMessage.Width;

  ImageIcon.Top         := START_TOP + (((LabelMessage.top + LabelMessage.BoundsRect.Bottom) div 2) - (Icon.Height div 2));

  Inc(AHeight, PanelFooter.Height + EditResponse.Height + 8);

  if AHeight < 130 then
    AHeight := 130;

  ClientHeight := AHeight;

  ButtonLeft.Left  := (PanelFooter.Width div 2) - 4 - ButtonLeft.Width;
  ButtonRight.Left := ButtonLeft.Left + ButtonLeft.Width + 8;

  ButtonRight.Top := (PanelFooter.Height div 2) - (ButtonRight.Height div 2);
  ButtonLeft.Top  := ButtonRight.Top;

  self.Constraints.MinHeight := ClientHeight;
  self.Constraints.MinWidth  := ClientWidth;
end;

procedure TSub7FormInputQuery.FormCreate(Sender: TObject);
begin
  FResult := '';

  ButtonLeft.Caption  := SMsgDlgCancel;
  ButtonRight.Caption := SMsgDlgConfirm;
end;

procedure TSub7FormInputQuery.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27 : self.Close;
    13 : ButtonRightClick(ButtonRight);
  end;
end;

procedure TSub7FormInputQuery.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TSub7FormInputQuery.FormShow(Sender: TObject);
begin
  DoResize();
end;

end.
