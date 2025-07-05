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

program OptixGate;

uses
  Vcl.Forms,
  Optix.Exceptions in '..\Shared\Optix.Exceptions.pas',
  Optix.Sockets.Helper in '..\Shared\Optix.Sockets.Helper.pas',
  Optix.Protocol.Packet in '..\Shared\Optix.Protocol.Packet.pas' {* External Libraries *},
  Optix.Sockets.Exceptions in '..\Shared\Optix.Sockets.Exceptions.pas',
  Optix.Interfaces in '..\Shared\Optix.Interfaces.pas' {* External Libraries *},
  Optix.Thread in '..\Shared\Optix.Thread.pas' {* Server Units *},
  Optix.Protocol.Client.Handler in '..\Shared\Optix.Protocol.Client.Handler.pas',
  Optix.Func.SessionInformation in '..\Shared\Functions\Optix.Func.SessionInformation.pas',
  Optix.Func.Commands in '..\Shared\Functions\Optix.Func.Commands.pas',
  Optix.Func.Response in '..\Shared\Functions\Optix.Func.Response.pas',
  Optix.System.Helper in '..\Shared\Optix.System.Helper.pas',
  XSuperJSON in '..\Shared\XSuperJSON.pas',
  Optix.InformationGathering.Helper in '..\Shared\Optix.InformationGathering.Helper.pas',
  Optix.WinApiEx in '..\Shared\Optix.WinApiEx.pas',
  Optix.InformationGathering.Process in '..\Shared\Optix.InformationGathering.Process.pas',
  Optix.Protocol.Network.Server in 'Units\Threads\Optix.Protocol.Network.Server.pas',
  Optix.Protocol.SessionHandler in 'Units\Threads\Optix.Protocol.SessionHandler.pas',
  Optix.Protocol.Sockets.Client in 'Units\Threads\Optix.Protocol.Sockets.Client.pas',
  __uBaseFormControl__ in 'Units\Forms\__uBaseFormControl__.pas',
  Optix.Helper in 'Units\Optix.Helper.pas',
  Optix.VCL.Helper in 'Units\Optix.VCL.Helper.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  Vcl.Themes,
  Vcl.Styles,
  Optix.Constants in 'Units\Optix.Constants.pas',
  uFormAbout in 'Units\Forms\uFormAbout.pas' {FormAbout},
  uFormProcessManager in 'Units\Forms\uFormProcessManager.pas' {FormProcessManager},
  Optix.Func.Enum.Process in '..\Shared\Functions\Optix.Func.Enum.Process.pas';

{$R *.res}

begin
  IsMultiThread := True;
  ///

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Aqua Light Slate');
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  // Application.CreateForm(TFormProcessManager, FormProcessManager);
  Application.Run;
end.
