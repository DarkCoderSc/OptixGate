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



unit uFormAbout;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Variants, System.Classes, System.SysUtils,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.BaseImageCollection, Vcl.ImageCollection,

  NeoFlat.Window, NeoFlat.Panel, NeoFlat.Button, Vcl.Imaging.pngimage, Vcl.ExtCtrls;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFormAbout = class(TForm)
    ImageBanner: TVirtualImage;
    ImageCollection: TImageCollection;
    FlatWindow1: TFlatWindow;
    PanelDisclaimer: TFlatPanel;
    Disclaimer: TRichEdit;
    ShapeBanner: TShape;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageBannerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FFirstShow : Boolean;

    {@M}
    procedure DoResize();
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  Optix.Helper, Optix.Constants;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TFormAbout.DoResize();
begin
  ImageBanner.Top   := 0;
  ImageBanner.Left  := 0;

  ShapeBanner.Top   := ImageBanner.Top + ImageBanner.Height;
  ShapeBanner.Left  := 0;
  ShapeBanner.Width := ClientWidth;

  PanelDisclaimer.Top   := ShapeBanner.Top + ShapeBanner.Height + ScaleValue(4);
  PanelDisclaimer.Left  := ScaleValue(4);
  PanelDisclaimer.Width := ClientWidth - (ScaleValue(4) * 2);

  ClientHeight := PanelDisclaimer.Top + PanelDisclaimer.Height + ScaleValue(4);
  ClientWidth  := ImageBanner.Width;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  FFirstShow := True;

  {$IFDEF CLIENT_GUI}
  FlatWindow1.Caption    := clRed;
  FlatWindow1.Background := clWhite;
  {$ENDIF}
end;

procedure TFormAbout.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  if FFirstShow then begin
    Disclaimer.Text := TOptixHelper.TryReadResourceString('DISCLAIMER');

    ///
    FFirstShow := False;
  end;

  DoResize();
end;

procedure TFormAbout.ImageBannerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture();
  SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
end;

end.
