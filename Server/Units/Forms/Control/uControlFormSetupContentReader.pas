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



unit uControlFormSetupContentReader;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  __uBaseFormControl__, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls;
// ---------------------------------------------------------------------------------------------------------------------

type
  TControlFormSetupContentReader = class(TBaseFormControl)
    PanelClient: TPanel;
    Label1: TLabel;
    PanelLeft: TPanel;
    Image: TVirtualImage;
    LabelPageSize: TLabel;
    EditPath: TEdit;
    SpinPageSize: TSpinEdit;
    PanelBottom: TPanel;
    ButtonStart: TButton;
    ButtonCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    {@M}
    procedure DoResize();
  public
    { Public declarations }
  end;

var
  ControlFormSetupContentReader: TControlFormSetupContentReader;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.Func.Commands.ContentReader, Optix.FileSystem.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormSetupContentReader.ButtonCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TControlFormSetupContentReader.ButtonStartClick(Sender: TObject);
begin
  if String.IsNullOrWhiteSpace(EditPath.Text) then begin
    EditPath.SetFocus;

    ///
    raise Exception.Create('You must enter a valid file path.');
  end;

  StreamFileContent(EditPath.Text, SpinPageSize.Value);

  ///
  Close();
end;

procedure TControlFormSetupContentReader.DoResize();
begin
  ButtonStart.Top := (PanelBottom.Height div 2) - (ButtonStart.Height div 2);
  ButtonCancel.Top  := ButtonStart.Top;

  ButtonStart.Left := PanelBottom.Width - ButtonStart.Width - 8;
  ButtonCancel.Left  := ButtonStart.Left - ButtonStart.Width - 8;

  var ANewHeight := PanelBottom.Height;

  Inc(ANewHeight, SpinPageSize.Top + SpinPageSize.Height + 8);

  ClientHeight := ANewHeight;
end;

procedure TControlFormSetupContentReader.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TControlFormSetupContentReader.FormCreate(Sender: TObject);
begin
  LabelPageSize.Caption := Format(LabelPageSize.Caption, [
    TContentReader.MIN_PAGE_SIZE,
    TContentReader.MAX_PAGE_SIZE
  ]);

  ///
  SpinPageSize.MinValue := TContentReader.MIN_PAGE_SIZE;
  SpinPageSize.MaxValue := TContentReader.MAX_PAGE_SIZE;
end;

procedure TControlFormSetupContentReader.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    13 : ButtonStartClick(ButtonStart);
    27 : ButtonCancelClick(ButtonCancel);
  end;
end;

procedure TControlFormSetupContentReader.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TControlFormSetupContentReader.FormShow(Sender: TObject);
begin
  EditPath.SetFocus;

  DoResize();
end;

end.
