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

unit uFormSelectCertificate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, NeoFlat.ComboBox, NeoFlat.Panel, NeoFlat.Button,
  NeoFlat.Window;

type
  TFormSelectCertificate = class(TForm)
    PanelMain: TFlatPanel;
    LabelCertificate: TLabel;
    ComboCertificate: TFlatComboBox;
    FlatWindow1: TFlatWindow;
    PanelBottom: TFlatPanel;
    ButtonValidate: TFlatButton;
    ButtonCancel: TFlatButton;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonValidateClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    FDefaultIndex : Integer;

    {@M}
    procedure DoResize();
  public
    {@C}
    constructor Create(AOwner : TComponent; const APreSelectCertificate : String = ''); reintroduce;
  end;

var
  FormSelectCertificate: TFormSelectCertificate;

implementation

uses Optix.Helper,

     uFormCertificatesStore;

{$R *.dfm}

procedure TFormSelectCertificate.DoResize();
begin
  ButtonValidate.Top := (PanelBottom.Height div 2) - (ButtonValidate.Height div 2);
  ButtonCancel.Top  := ButtonValidate.Top;

  ButtonValidate.Left := PanelBottom.Width - ButtonValidate.Width - 8;
  ButtonCancel.Left  := ButtonValidate.Left - ButtonValidate.Width - 8;
end;

procedure TFormSelectCertificate.FormCreate(Sender: TObject);
begin
  {$IFDEF CLIENT_GUI}
  FlatWindow1.Caption    := clRed;
  FlatWindow1.Background := clWhite;
  PanelMain.Color        := FlatWindow1.Background;
  PanelBottom.Color      := FlatWindow1.Background;
  {$ENDIF}
end;

procedure TFormSelectCertificate.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    13 : ButtonValidateClick(ButtonValidate);
    27 : ModalResult := mrCancel;
  end;
end;

procedure TFormSelectCertificate.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormSelectCertificate.FormShow(Sender: TObject);
begin
  DoResize();
end;

procedure TFormSelectCertificate.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel
end;

procedure TFormSelectCertificate.ButtonValidateClick(Sender: TObject);
begin
  if ComboCertificate.ItemIndex = -1 then
    TOptixHelper.Error(self.Handle, 'You must select an existing certificate (via its fingerprint).')
  else if ComboCertificate.ItemIndex = FDefaultIndex then
    ModalResult := mrCancel // Nothing changed
  else
    ModalResult := mrOk;
end;

constructor TFormSelectCertificate.Create(AOwner : TComponent; const APreSelectCertificate : String = '');
begin
  inherited Create(AOwner);
  ///

  FDefaultIndex := -1;

  ComboCertificate.Clear();

  var AFingerprints := FormCertificatesStore.GetCertificatesFingerprints();
  try
    for var AFingerprint in AFingerprints do begin
      var AIndex := ComboCertificate.Items.Add(AFingerprint);
      if (APreSelectCertificate <> '') and (String.Compare(APreSelectCertificate, AFingerprint, True) = 0) then
        FDefaultIndex := AIndex;
    end;
  finally
    FreeAndNil(AFingerprints);
  end;

  ///
  ComboCertificate.ItemIndex := FDefaultIndex;
end;

end.
