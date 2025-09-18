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

program Client_OpenSSL;

{$WARN DUPLICATE_CTOR_DTOR OFF}

{$IFDEF DEBUG}
  {$APPTYPE GUI}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}

uses
  System.SysUtils,
  Optix.OpenSSL.Headers in '..\Shared\OpenSSL\Optix.OpenSSL.Headers.pas',
  Optix.OpenSSL.Helper in '..\Shared\OpenSSL\Optix.OpenSSL.Helper.pas',
  Optix.OpenSSL.Exceptions in '..\Shared\OpenSSL\Optix.OpenSSL.Exceptions.pas',
  Optix.OpenSSL.Context in '..\Shared\OpenSSL\Optix.OpenSSL.Context.pas',
  Optix.OpenSSL.Handler in '..\Shared\OpenSSL\Optix.OpenSSL.Handler.pas',
  Optix.DebugCertificate in 'Units\Optix.DebugCertificate.pas',
  Optix.Exceptions in '..\Shared\Optix.Exceptions.pas',
  Optix.Sockets.Helper in '..\Shared\Optix.Sockets.Helper.pas',
  Optix.Protocol.Packet in '..\Shared\Optix.Protocol.Packet.pas',
  Optix.Sockets.Exceptions in '..\Shared\Optix.Sockets.Exceptions.pas',
  Optix.Func.Commands in '..\Shared\Functions\Optix.Func.Commands.pas',
  Optix.Interfaces in '..\Shared\Optix.Interfaces.pas',
  Optix.Thread in '..\Shared\Optix.Thread.pas',
  Optix.Protocol.Client.Handler in '..\Shared\Optix.Protocol.Client.Handler.pas',
  Optix.InformationGathering.Helper in '..\Shared\Optix.InformationGathering.Helper.pas',
  Optix.Process.Helper in '..\Shared\Optix.Process.Helper.pas',
  Optix.Func.SessionInformation in '..\Shared\Functions\Optix.Func.SessionInformation.pas',
  Optix.Func.Enum.Process in '..\Shared\Functions\Optix.Func.Enum.Process.pas',
  Optix.Protocol.SessionHandler in 'Units\Threads\Optix.Protocol.SessionHandler.pas',
  Optix.Protocol.Client in 'Units\Threads\Optix.Protocol.Client.pas',
  XSuperJSON in '..\Shared\XSuperJSON.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  Optix.WinApiEx in '..\Shared\Optix.WinApiEx.pas',
  Optix.System.Helper in '..\Shared\Optix.System.Helper.pas',
  Optix.Shared.Types in '..\Shared\Optix.Shared.Types.pas',
  Optix.Actions.Process in 'Units\Actions\Optix.Actions.Process.pas',
  Optix.Func.LogNotifier in '..\Shared\Functions\Optix.Func.LogNotifier.pas',
  Optix.Func.Enum.FileSystem in '..\Shared\Functions\Optix.Func.Enum.FileSystem.pas',
  Optix.Shared.Classes in '..\Shared\Optix.Shared.Classes.pas',
  Optix.FileSystem.Helper in '..\Shared\Optix.FileSystem.Helper.pas',
  Optix.Protocol.Preflight in '..\Shared\Optix.Protocol.Preflight.pas',
  Optix.Protocol.Exceptions in '..\Shared\Optix.Protocol.Exceptions.pas',
  Optix.Protocol.Worker.FileTransfer in 'Units\Threads\Optix.Protocol.Worker.FileTransfer.pas',
  Optix.Shared.Protocol.FileTransfer in '..\Shared\Optix.Shared.Protocol.FileTransfer.pas',
  Optix.Task.ProcessDump in '..\Shared\Tasks\Optix.Task.ProcessDump.pas',
  Optix.Task in '..\Shared\Tasks\Optix.Task.pas',
  Optix.Actions.ProcessHandler in 'Units\Actions\Optix.Actions.ProcessHandler.pas',
  Optix.Func.Shell in '..\Shared\Functions\Optix.Func.Shell.pas',
  Optix.Client.Entrypoint in 'Units\Optix.Client.Entrypoint.pas',
  Optix.Func.Commands.ActionResponse in '..\Shared\Functions\Optix.Func.Commands.ActionResponse.pas';

begin
  IsMultiThread := True;
  try
	  {$IFNDEF USETLS}
    'The USETLS compiler directive is missing from the project options. Please define it in the respective build '
    'configuration by navigating to Project > Options > Delphi Compiler > Conditional defines, and adding USETLS.'
    {$ENDIF}

    ClientEntrypoint();
  except
    {$IFNDEF DEBUG}
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
    {$ENDIF}
  end;
end.
