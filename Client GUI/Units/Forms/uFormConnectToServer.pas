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

unit uFormConnectToServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Mask, Vcl.Samples.Spin,
  Generics.Collections;

type
  TFormConnectToServer = class(TForm)
    PanelLeft: TPanel;
    Image: TVirtualImage;
    PanelClient: TPanel;
    Label1: TLabel;
    EditServerAddress: TEdit;
    Label2: TLabel;
    SpinPort: TSpinEdit;
    PanelBottom: TPanel;
    ButtonConnect: TButton;
    ButtonCancel: TButton;
    LabelCertificate: TLabel;
    ComboCertificate: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinPortChange(Sender: TObject);
  private
    FCanceled : Boolean;

    {@M}
    procedure DoResize();
  public
    {$IFDEF USETLS}
    {@C}
    constructor Create(AOwner : TComponent; const ACertificatesFingerprints : TList<String>); overload;
    {$ENDIF}

    {@G}
    property Canceled : Boolean read FCanceled;
  end;

var
  FormConnectToServer: TFormConnectToServer;

implementation

uses uFormMain;

{$R *.dfm}

procedure TFormConnectToServer.ButtonCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TFormConnectToServer.ButtonConnectClick(Sender: TObject);
begin
  if String.IsNullOrWhiteSpace(EditServerAddress.Text) then begin
    EditServerAddress.SetFocus();

    raise Exception.Create('You must specify a server hostname.');
  end;

  if ComboCertificate.Visible and (ComboCertificate.ItemIndex = -1) then
    raise Exception.Create(
      'You must select an existing certificate (via its fingerprint) for the server to start listening.'
    );

  FCanceled := False;

  Close();
end;

procedure TFormConnectToServer.DoResize();
begin
  ButtonConnect.Top := (PanelBottom.Height div 2) - (ButtonConnect.Height div 2);
  ButtonCancel.Top  := ButtonConnect.Top;

  ButtonConnect.Left := PanelBottom.Width - ButtonConnect.Width - 8;
  ButtonCancel.Left  := ButtonConnect.Left - ButtonConnect.Width - 8;

  var ANewHeight := PanelBottom.Height;

  if not LabelCertificate.Visible then
    Inc(ANewHeight, SpinPort.Top + SpinPort.Height + 8)
  else
    Inc(ANewHeight, ComboCertificate.Top + ComboCertificate.Height + 8);

  ClientHeight := ANewHeight;
end;

procedure TFormConnectToServer.FormCreate(Sender: TObject);
begin
  FCanceled := True;

  {$IFNDEF USETLS}
  LabelCertificate.Visible := False;
  ComboCertificate.Visible := False;
  {$ENDIF}
end;

procedure TFormConnectToServer.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    13 : ButtonConnectClick(ButtonConnect);
    27 : ButtonCancelClick(ButtonCancel);
  end;
end;

procedure TFormConnectToServer.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormConnectToServer.FormShow(Sender: TObject);
begin
  DoResize();

  {$IFDEF USETLS}
  ComboCertificate.ItemIndex := 0;
  {$ENDIF}
end;

procedure TFormConnectToServer.SpinPortChange(Sender: TObject);
begin
  if TSpinEdit(Sender).Value < 0 then
    TSpinEdit(Sender).Value := 0
  else if TSpinEdit(Sender).Value > 65535 then
    TSpinEdit(Sender).Value := 65535;
end;

{$IFDEF USETLS}
{ TFormConnectToServer.Create }
constructor TFormConnectToServer.Create(AOwner : TComponent; const ACertificatesFingerprints : TList<String>);
begin
  inherited Create(AOwner);
  ///

  ComboCertificate.Clear();

  if not Assigned(ACertificatesFingerprints) then
    Exit();

  for var AFingerprint in ACertificatesFingerprints do
    ComboCertificate.Items.Add(AFingerprint);
end;
{$ENDIF}

end.
