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



unit uFormListen;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,

  uFormServers, System.Skia, Vcl.Skia, NeoFlat.GroupBox, NeoFlat.CheckBox, NeoFlat.ComboBox, NeoFlat.Edit,
  NeoFlat.Button, NeoFlat.Panel, NeoFlat.Window;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFormListen = class(TForm)
    PanelBottom: TFlatPanel;
    ButtonConnect: TFlatButton;
    ButtonCancel: TFlatButton;
    PanelClient: TFlatPanel;
    Label1: TLabel;
    EditPort: TFlatEdit;
    LabelCertificate: TLabel;
    ComboCertificate: TFlatComboBox;
    CheckBoxAutoStart: TFlatCheckBox;
    Label3: TLabel;
    ComboIpVersion: TFlatComboBox;
    GroupBox1: TFlatGroupBox;
    RadioBindAll: TFlatCheckBox;
    RadioBindLocal: TFlatCheckBox;
    RadioBindCustom: TFlatCheckBox;
    EditServerBindAddress: TFlatEdit;
    FlatWindow1: TFlatWindow;
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure EditPortChange(Sender: TObject);
    procedure RadioBindAllClick(Sender: TObject);
    procedure RadioBindCustomStateChanged(Sender: TObject);
    procedure RadioBindLocalStateChanged(Sender: TObject);
  private
    {@M}
    procedure DoResize();
  public
    {@C}
    constructor Create(AOwner : TComponent); override;

    {@M}
    function GetServerConfiguration() : TServerConfiguration;
  end;

var
  FormListen: TFormListen;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  OptixCore.Sockets.Helper, Optix.Helper

  {$IFDEF USETLS}, Optix.DebugCertificate,

  uFormCertificatesStore{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

function TFormListen.GetServerConfiguration() : TServerConfiguration;
begin
  var APort := StrToInt(EditPort.Text);
  if APort < 0 then
    APort := 0
  else if APort > High(Word) then
    APort := High(Word);
  ///

  result.Address   := EditServerBindAddress.Text;
  result.Port      := APort;
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

procedure TFormListen.RadioBindCustomStateChanged(Sender: TObject);
begin
  EditServerBindAddress.Enabled := TFlatCheckBox(Sender).Checked;
end;

procedure TFormListen.RadioBindLocalStateChanged(Sender: TObject);
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
    Inc(ANewHeight, EditPort.Top + EditPort.Height + 8)
  else
    Inc(ANewHeight, ComboCertificate.Top + ComboCertificate.Height + 8);

  ClientHeight := ANewHeight;
end;

procedure TFormListen.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    13 : ButtonConnectClick(ButtonConnect);
    27 : ModalResult := mrCancel;
  end;
end;

procedure TFormListen.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormListen.FormCreate(Sender: TObject);
begin
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

procedure TFormListen.EditPortChange(Sender: TObject);
begin
  if TSpinEdit(Sender).Value < 0 then
    TSpinEdit(Sender).Value := 0
  else if TSpinEdit(Sender).Value > 65535 then
    TSpinEdit(Sender).Value := 65535;
end;

procedure TFormListen.ButtonConnectClick(Sender: TObject);
begin
  var AConfiguration := GetServerConfiguration();
  ///

  var AErrorDialog := TOptixErrorDialog.Create(self);
  try
    if ComboCertificate.Visible and (ComboCertificate.ItemIndex = -1) then
      AErrorDialog.Add(
        'You must select an existing certificate (via its fingerprint) for the server to start listening.'
      );

    if FormServers.ServerPortExists(AConfiguration.Port, AConfiguration.Version) then
      AErrorDialog.Add(
        'The specified port is already present in the server list. A port can only be bound and listened to by one ' +
        'server instance at a time.'
      );

    if AErrorDialog.ShowErrors then
      Exit();
  finally
    FreeAndNil(AErrorDialog);
  end;

  ///
  ModalResult := mrOk;
end;

constructor TFormListen.Create(AOwner : TComponent);
begin
  inherited;
  ///

  {$IFDEF USETLS}
    ComboCertificate.Clear();

    var AFingerprints := FormCertificatesStore.GetCertificatesFingerprints();
    try
      for var AFingerprint in AFingerprints do
        ComboCertificate.Items.Add(AFingerprint);
    finally
      FreeAndNil(AFingerprints);
    end;
  {$ENDIF}
end;

end.
