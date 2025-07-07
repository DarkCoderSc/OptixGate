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

unit __uBaseFormControl__;

interface

uses VCL.Forms, VCL.Controls, System.Classes, Winapi.Messages, XSuperObject,
     Optix.Func.Commands;

type
  TBaseFormControl = class(TForm)
  private
    FGUID            : TGUID;
    FUserIdentifier  : String;
    FOriginalCaption : String;
  protected
    {@M}
    procedure SendCommand(const ACommand : TOptixCommand);

    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;

    procedure RefreshCaption(); virtual;
  public
    {@G}
    property GUID : TGUID read FGUID;

    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); virtual; abstract;

    {@C}
    constructor Create(AOwner : TComponent); overload; override;
    constructor Create(AOwner : TComponent; const AUserIdentifier : String); overload;
    destructor Destroy(); override;
  end;

implementation

uses System.SysUtils, Winapi.Windows, uFormMain;

{ TBaseFormControl.Create }
constructor TBaseFormControl.Create(AOwner : TComponent);
begin
  inherited;
  ///

  FGUID            := TGUID.NewGuid();
  FOriginalCaption := self.Caption; // Default
  FUserIdentifier  := '';
end;

{ TBaseFormControl.Create }
constructor TBaseFormControl.Create(AOwner : TComponent; const AUserIdentifier : String);
begin
  Create(AOwner);
  ///

  FUserIdentifier := AUserIdentifier;
end;

{ TBaseFormControl.Destroy }
destructor TBaseFormControl.Destroy();
begin

  ///
  inherited;
end;

{ TBaseFormControl.SendCommand }
procedure TBaseFormControl.SendCommand(const ACommand : TOptixCommand);
begin
  ACommand.WindowGUID := FGUID;

  ///
  FormMain.SendCommand(self, ACommand);
end;

{ TBaseFormControl.CMVisibleChanged }
procedure TBaseFormControl.CMVisibleChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if self.Visible then
    RefreshCaption();
end;

{ TBaseFormControl.RefreshCaption }
procedure TBaseFormControl.RefreshCaption();
begin
  if String.IsNullOrEmpty(FUserIdentifier) then
    Caption := FOriginalCaption
  else
    Caption := Format('%s (%s)', [
      FOriginalCaption,
      FUserIdentifier
    ]);
end;

end.
