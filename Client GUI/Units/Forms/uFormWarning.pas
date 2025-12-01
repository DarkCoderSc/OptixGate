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

// TODO: Improve this beta form.

unit uFormWarning;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.ComCtrls;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFormWarning = class(TForm)
    PanelFooter: TPanel;
    PanelHeader: TPanel;
    ImageWarning: TVirtualImage;
    ButtonAcceptTheRisk: TButton;
    ButtonCancel: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    Timer: TTimer;
    PanelBody: TPanel;
    PanelAcceptSentence: TPanel;
    LabelAccept: TLabel;
    EditAccept: TEdit;
    RichAgreement: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonAcceptTheRiskClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FValidated : Boolean;

    {@M}
    procedure DoResize();
  public
    {@G}
    property Validated : Boolean read FValidated;
  end;

var
  FormWarning: TFormWarning;

const
  MAGIC_SENTENCE = 'I understand and accept the risk and accept the full agreement and disclaimer';

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain, Optix.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}
{$R control_agreement.res}

procedure TFormWarning.DoResize();
begin
  ButtonAcceptTheRisk.Left := PanelFooter.Width - ButtonAcceptTheRisk.Width - ScaleValue(8);
  ButtonCancel.Left        := ButtonAcceptTheRisk.Left - ButtonAcceptTheRisk.Width - ScaleValue(4);

  ButtonAcceptTheRisk.Top  := (PanelFooter.Height div 2) - (ButtonAcceptTheRisk.Height div 2);
  ButtonCancel.Top         := ButtonAcceptTheRisk.Top;

  LabelAccept.Top := (PanelAcceptSentence.Height div 2) - ((LabelAccept.Height + EditAccept.Height + ScaleValue(4)) div 2);
  EditAccept.Top  := LabelAccept.Top + LabelAccept.Height + ScaleValue(4);

  LabelAccept.Left := ScaleValue(8);
  EditAccept.Left  := LabelAccept.Left;

  EditAccept.Width := PanelAcceptSentence.Width - (ScaleValue(8) * 2);
end;

procedure TFormWarning.ButtonAcceptTheRiskClick(Sender: TObject);
begin
  if String.Compare(Trim(EditAccept.Text), MAGIC_SENTENCE, True) = 0 then begin
    FValidated := True;

    Close();
  end;
end;

procedure TFormWarning.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormWarning.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer.Enabled := False;
end;

procedure TFormWarning.FormCreate(Sender: TObject);
begin
  FValidated := False;

  DoResize();
end;

procedure TFormWarning.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormWarning.FormShow(Sender: TObject);
begin
  DoResize();
  ///
  try
    RichAgreement.Text := ReadResourceString('CONTROL_AGREEMENT');

    RichAgreement.SelStart := Length(RichAgreement.Text);

    RichAgreement.SelAttributes.Color := $0000D0D0;

    RichAgreement.SelText := #13#10 +
      'To continue, you are required to type the complete acknowledgment statement exactly as ' +
      Format('written: "%s"', [MAGIC_SENTENCE]);

    RichAgreement.SelStart := 0;
    RichAgreement.SetFocus;
  except
    Application.Terminate;
  end;

  ///
  Timer.Enabled := True;
end;

procedure TFormWarning.TimerTimer(Sender: TObject);
begin
  ButtonAcceptTheRisk.Enabled := String.Compare(Trim(EditAccept.Text), MAGIC_SENTENCE, True) = 0;

  ///

  if PanelAcceptSentence.Visible then
    Exit();

  var AScrollInfo : TScrollInfo;
  AScrollInfo.cbSize := SizeOf(TScrollInfo);
  AScrollInfo.fMask := SIF_ALL;

  var ASuccess : Boolean;

  if GetScrollInfo(RichAgreement.Handle, SB_VERT, AScrollInfo) then
    ASuccess := AScrollInfo.nPos + Integer(AScrollInfo.nPage) >= AScrollInfo.nMax
  else
    ASuccess := True;

  ///
  PanelAcceptSentence.Visible := ASuccess;
end;

end.
