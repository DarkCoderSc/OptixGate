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
{                   License: Apache License 2.0                                }
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

unit uFormAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TFormAbout = class(TForm)
    ImageLogo: TVirtualImage;
    LabelName: TLabel;
    LabelDarkCoderSc: TLabel;
    ButtonClose: TButton;
    LabelDisclaimer: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    {@M}
    procedure DoResize();
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses uFormMain;

{$R *.dfm}

procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  self.Close();
end;

procedure TFormAbout.DoResize();
begin
  ImageLogo.Top  := self.ScaleValue(8);
  ImageLogo.Left := (ClientWidth div 2) - (ImageLogo.Width div 2);

  LabelName.Top  := ImageLogo.Top + ImageLogo.Height + self.ScaleValue(8);
  LabelName.Left := (ClientWidth div 2) - (LabelName.Width div 2);

  LabelDarkCoderSc.Top  := LabelName.Top + LabelName.Height + self.ScaleValue(4);
  LabelDarkCoderSc.Left := (ClientWidth div 2) - (labelDarkCoderSc.Width div 2);

  LabelDisclaimer.Top   := labelDarkCoderSc.Top + labelDarkCoderSc.Height + self.ScaleValue(8);
  LabelDisclaimer.Left  := self.ScaleValue(8);
  LabelDisclaimer.Width := ClientWidth - (LabelDisclaimer.Left * 2);

  self.LabelDisclaimer.AutoSize := False;
  self.LabelDisclaimer.AutoSize := True;

  ButtonClose.Top  := LabelDisclaimer.Top + LabelDisclaimer.Height + self.ScaleValue(8);
  ButtonClose.Left := (ClientWidth div 2) - (ButtonClose.Width div 2);

  ClientHeight := ButtonClose.Top + ButtonClose.Height + self.ScaleValue(8);
end;

procedure TFormAbout.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  DoResize();
end;

end.
