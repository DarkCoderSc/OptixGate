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
{                   License: GPL v3                                            }
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
{                                                                              }
{  Authorship (No AI):                                                         }
{  -------------------                                                         }
{   All code contained in this unit was written and developed by the author    }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{******************************************************************************}



unit uFormGenerateNewCertificate;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  System.Skia, Vcl.Skia, NeoFlat.Edit, NeoFlat.Panel, NeoFlat.Button, NeoFlat.Window;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFormGenerateNewCertificate = class(TForm)
    PanelClient: TFlatPanel;
    Label2: TLabel;
    EditCN: TFlatEdit;
    PanelLeft: TFlatPanel;
    EditC: TFlatEdit;
    Label1: TLabel;
    Label3: TLabel;
    EditO: TFlatEdit;
    SkSvg1: TSkSvg;
    FlatWindow1: TFlatWindow;
    PanelBottom: TFlatPanel;
    ButtonGenerate: TFlatButton;
    ButtonCancel: TFlatButton;
    procedure ButtonGenerateClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    {@M}
    procedure DoResize();
  end;

var
  FormGenerateNewCertificate: TFormGenerateNewCertificate;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  Optix.Constants, Optix.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TFormGenerateNewCertificate.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormGenerateNewCertificate.ButtonGenerateClick(Sender: TObject);
begin
  var AErrorDialog := TOptixErrorDialog.Create(self);
  try
    if String.IsNullOrWhiteSpace(EditC.Text) then
      AErrorDialog.Add('You must specify a country.');

    if String.IsNullOrWhiteSpace(EditO.Text) then
      AErrorDialog.Add('You must specify an organization name.');

    if String.IsNullOrWhiteSpace(EditCN.Text) then
      AErrorDialog.Add('You must specify a common name.');

    if AErrorDialog.ShowErrors() then
      Exit();
  finally
    FreeAndNil(AErrorDialog);
  end;

  ///
  ModalResult := mrOk;
end;

procedure TFormGenerateNewCertificate.DoResize();
begin
  ButtonGenerate.Top  := (PanelBottom.Height div 2) - (ButtonGenerate.Height div 2);
  ButtonCancel.Top    := ButtonGenerate.Top;

  ButtonGenerate.Left := PanelBottom.Width - ButtonGenerate.Width - ScaleValue(8);
  ButtonCancel.Left   := ButtonGenerate.Left - ButtonGenerate.Width - ScaleValue(8);
end;

procedure TFormGenerateNewCertificate.FormCreate(Sender: TObject);
begin
  {$IFDEF CLIENT_GUI}
  FlatWindow1.Caption    := clRed;
  FlatWindow1.Background := clWhite;
  PanelClient.Color      := FlatWindow1.Background;
  PanelLeft.Color        := FlatWindow1.Background;
  PanelBottom.Color      := FlatWindow1.Background;
  {$ENDIF}
end;

procedure TFormGenerateNewCertificate.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    13 : ButtonGenerateClick(ButtonGenerate);
    27 : ModalResult := mrCancel;
  end;
end;

procedure TFormGenerateNewCertificate.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormGenerateNewCertificate.FormShow(Sender: TObject);
begin
  DoResize();
end;

end.
