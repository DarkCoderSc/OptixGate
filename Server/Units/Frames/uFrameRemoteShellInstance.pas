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


unit uFrameRemoteShellInstance;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,

  OMultiPanel,

  __uBaseFormControl__;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFrameRemoteShellInstance = class(TFrame)
    Shell: TRichEdit;
    PanelCommand: TPanel;
    Command: TRichEdit;
    ButtonSend: TButton;
    OMultiPanel: TOMultiPanel;
    procedure ShellLinkClick(Sender: TCustomRichEdit; const URL: string; Button: TMouseButton);
    procedure ButtonSendClick(Sender: TObject);
    procedure CommandKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CommandChange(Sender: TObject);
  private
    FClosed      : Boolean;
    FInstanceId  : TGUID;
    FControlForm : TBaseFormControl;

    {@M}
    procedure SendCommandLine();
  public
    {@M}
    procedure AddOutput(const AOutput : String);
    procedure Close();

    {@C}
    constructor Create(const AOwner : TComponent; const AControlForm : TBaseFormControl; const AInstanceId : TGUID); reintroduce;
    destructor Destroy(); override;

    {@G}
    property InstanceId : TGUID read FInstanceId;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain, uControlFormRemoteShell,

  Optix.Func.Commands, Optix.Helper, Optix.Constants, Optix.Func.Commands.Shell;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

destructor TFrameRemoteShellInstance.Destroy();
begin

  ///
  inherited Destroy();
end;

procedure TFrameRemoteShellInstance.Close();
begin
  FClosed            := True;
  ButtonSend.Enabled := False;
  Command.Enabled    := False;

  Shell.SelAttributes.Color := COLOR_TEXT_WARNING;
  Shell.SelText := #13#10 + 'Shell Instance Closed...';

  Shell.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFrameRemoteShellInstance.SendCommandLine();
begin
  if not Assigned(FControlForm) or FClosed then
    Exit();
  ///

  FControlForm.SendCommand(TOptixStdinShellInstance.Create(FInstanceId, Command.Text));

  ///
  Command.Clear();
  ButtonSend.Enabled := False;
end;

procedure TFrameRemoteShellInstance.AddOutput(const AOutput : String);
begin
  Shell.SelStart := Length(Shell.Text);
  Shell.SelLength := 0;

  Shell.SelText := AOutput;

  Shell.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFrameRemoteShellInstance.ButtonSendClick(Sender: TObject);
begin
  SendCommandLine();
end;

procedure TFrameRemoteShellInstance.CommandChange(Sender: TObject);
begin
  if FClosed then
    Exit();
  ///

  ButtonSend.Enabled := Length(Trim(TRichEdit(Sender).Text)) > 0;
end;

procedure TFrameRemoteShellInstance.CommandKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (*((not (ssShift in Shift)) and *) (Key = 13) and ButtonSend.Enabled then
    SendCommandLine();
end;

constructor TFrameRemoteShellInstance.Create(const AOwner : TComponent; const AControlForm : TBaseFormControl; const AInstanceId : TGUID);
begin
  inherited Create(AOwner);
  ///

  FControlForm := AControlForm;
  FInstanceId  := AInstanceId;
  FClosed      := False;
end;

procedure TFrameRemoteShellInstance.ShellLinkClick(Sender: TCustomRichEdit; const URL: string; Button: TMouseButton);
begin
  case Button of
    TMouseButton.mbLeft : Open(Url);

    TMouseButton.mbRight : ;
    TMouseButton.mbMiddle : ;
  end;
end;

end.
