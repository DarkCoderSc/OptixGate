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
{                                                                              }
{******************************************************************************}

unit uFormGenerateNewCertificate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls;

type
  TFormGenerateNewCertificate = class(TForm)
    PanelBottom: TPanel;
    ButtonGenerate: TButton;
    ButtonCancel: TButton;
    PanelClient: TPanel;
    Label2: TLabel;
    EditCN: TEdit;
    PanelLeft: TPanel;
    Image: TVirtualImage;
    EditC: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    EditO: TEdit;
    procedure ButtonGenerateClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FCanceled : Boolean;

    {@M}
    procedure DoResize();
  public
    {@G}
    property Canceled : Boolean read FCanceled;
  end;

var
  FormGenerateNewCertificate: TFormGenerateNewCertificate;

implementation

uses uFormMain, Optix.Constants;

{$R *.dfm}

procedure TFormGenerateNewCertificate.ButtonCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TFormGenerateNewCertificate.ButtonGenerateClick(Sender: TObject);
begin
  if String.IsNullOrWhiteSpace(EditC.Text) then begin
    EditC.SetFocus();

    raise Exception.Create('You must specify a country.');
  end;

  if String.IsNullOrWhiteSpace(EditO.Text) then begin
    EditO.SetFocus();

    raise Exception.Create('You must specify an organization name.');
  end;

  if String.IsNullOrWhiteSpace(EditCN.Text) then begin
    EditCN.SetFocus();

    raise Exception.Create('You must specify a common name.');
  end;

  FCanceled := False;

  Close();
end;

procedure TFormGenerateNewCertificate.DoResize();
begin
  ButtonGenerate.Top := (PanelBottom.Height div 2) - (ButtonGenerate.Height div 2);
  ButtonCancel.Top  := ButtonGenerate.Top;

  ButtonGenerate.Left := PanelBottom.Width - ButtonGenerate.Width - 8;
  ButtonCancel.Left  := ButtonGenerate.Left - ButtonGenerate.Width - 8;
end;

procedure TFormGenerateNewCertificate.FormCreate(Sender: TObject);
begin
  FCanceled := True;

  Image.ImageIndex := IMAGE_CERTIFICATE;
end;

procedure TFormGenerateNewCertificate.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    13 : ButtonGenerateClick(ButtonGenerate);
    27 : ButtonCancelClick(ButtonCancel);
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
