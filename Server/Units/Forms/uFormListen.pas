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

unit uFormListen;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,

  uFormServers;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFormListen = class(TForm)
    PanelBottom: TPanel;
    ButtonConnect: TButton;
    ButtonCancel: TButton;
    PanelClient: TPanel;
    Label1: TLabel;
    SpinPort: TSpinEdit;
    PanelLeft: TPanel;
    Image: TVirtualImage;
    LabelCertificate: TLabel;
    ComboCertificate: TComboBox;
    CheckBoxAutoStart: TCheckBox;
    Label3: TLabel;
    ComboIpVersion: TComboBox;
    GroupBox1: TGroupBox;
    RadioBindAll: TRadioButton;
    RadioBindLocal: TRadioButton;
    RadioBindCustom: TRadioButton;
    EditServerBindAddress: TEdit;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure SpinPortChange(Sender: TObject);
    procedure RadioBindCustomClick(Sender: TObject);
    procedure RadioBindLocalClick(Sender: TObject);
    procedure RadioBindAllClick(Sender: TObject);
  private
    FCanceled : Boolean;

    {@M}
    procedure DoResize();
  public
    {@M}
    function GetServerConfiguration() : TServerConfiguration;

    {@G}
    property Canceled : Boolean read FCanceled;

    {$IFDEF USETLS}
    {@C}
    constructor Create(AOwner : TComponent; const ACertificatesFingerprints : TList<String>); reintroduce;
    {$ENDIF}
  end;

var
  FormListen: TFormListen;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  Optix.Helper, Optix.Sockets.Helper

  {$IFDEF USETLS}, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

function TFormListen.GetServerConfiguration() : TServerConfiguration;
begin
  result.Address   := EditServerBindAddress.Text;
  result.Port      := SpinPort.Value;
  result.Version   := TIpVersion(ComboIpVersion.ItemIndex);

  if RadioBindCustom.Checked then
    result.Address := EditServerBindAddress.Text
  else begin
    case result.Version of
      ipv4 : begin
        if RadioBindLocal.Checked then
          result.Address := '127.0.0.1'
        else
          result.Address := '0.0.0.0';
      end;

      ipv6 : begin
        if RadioBindLocal.checked then
          result.Address := '::1'
        else
          result.Address := '::';
      end;
    end;
  end;

  result.AutoStart := CheckBoxAutoStart.Checked;

  {$IFDEF USETLS}
  result.CertificateFingerprint := ComboCertificate.Text;
  {$ENDIF}
end;

procedure TFormListen.RadioBindAllClick(Sender: TObject);
begin
  EditServerBindAddress.Enabled := False;
end;

procedure TFormListen.RadioBindCustomClick(Sender: TObject);
begin
  EditServerBindAddress.Enabled := TRadioButton(Sender).Checked;
end;

procedure TFormListen.RadioBindLocalClick(Sender: TObject);
begin
  EditServerBindAddress.Enabled := False;
end;

procedure TFormListen.DoResize();
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

procedure TFormListen.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    13 : ButtonConnectClick(ButtonConnect);
    27 : ButtonCancelClick(ButtonCancel);
  end;
end;

procedure TFormListen.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormListen.FormCreate(Sender: TObject);
begin
  FCanceled := True;

  ComboIpVersion.ItemIndex := 0;

  {$IFNDEF USETLS}
  LabelCertificate.Visible := False;
  ComboCertificate.Visible := False;
  {$ENDIF}
end;

procedure TFormListen.FormShow(Sender: TObject);
begin
  {$IFDEF USETLS}
  ComboCertificate.ItemIndex := 0;
  {$ENDIF}

  DoResize();
end;

procedure TFormListen.SpinPortChange(Sender: TObject);
begin
  if TSpinEdit(Sender).Value < 0 then
    TSpinEdit(Sender).Value := 0
  else if TSpinEdit(Sender).Value > 65535 then
    TSpinEdit(Sender).Value := 65535;
end;

procedure TFormListen.ButtonCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TFormListen.ButtonConnectClick(Sender: TObject);
begin
  if ComboCertificate.Visible and (ComboCertificate.ItemIndex = -1) then
    raise Exception.Create(
      'You must select an existing certificate (via its fingerprint) for the server to start listening.'
    );

  FCanceled := False;

  Close();
end;

{$IFDEF USETLS}
{ TFormListen.Create }
constructor TFormListen.Create(AOwner : TComponent; const ACertificatesFingerprints : TList<String>);
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
