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

program Client;

{$APPTYPE GUI}
// {$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,
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
  Optix.Protocol.Sockets.Client in 'Units\Threads\Optix.Protocol.Sockets.Client.pas',
  XSuperJSON in '..\Shared\XSuperJSON.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  Optix.WinApiEx in '..\Shared\Optix.WinApiEx.pas',
  Optix.System.Helper in '..\Shared\Optix.System.Helper.pas',
  Optix.Types in '..\Shared\Optix.Types.pas',
  Optix.Actions.Process in 'Units\Actions\Optix.Actions.Process.pas',
  Optix.Func.LogNotifier in '..\Shared\Functions\Optix.Func.LogNotifier.pas',
  Optix.Func.Enum.FileSystem in '..\Shared\Functions\Optix.Func.Enum.FileSystem.pas',
  Optix.Classes in '..\Shared\Optix.Classes.pas',
  Optix.FileSystem.Helper in '..\Shared\Optix.FileSystem.Helper.pas';

begin
  IsMultiThread := True;
  try
    var AUserUID := TOptixInformationGathering.GetUserUID();

    var AMutex := CreateMutexW(nil, True, PWideChar(AUserUID.ToString));
    if AMutex = 0 then
      raise EWindowsException.Create('CreateMutexW');
    try
      if GetLastError() = ERROR_ALREADY_EXISTS then
        Exit();
      ///

      // Enable certain useful privileges (if possible)
      TSystemHelper.TryNTSetPrivilege('SeDebugPrivilege', True);
      TSystemHelper.TryNTSetPrivilege('SeTakeOwnershipPrivilege', True);

      var ASessionHandler := TOptixSessionHandlerThread.Create('127.0.0.1', 2801);
      ASessionHandler.Retry := True;
      ASessionHandler.RetryDelay := 1000;
      ASessionHandler.Start();

      ///
      ASessionHandler.WaitFor;
    finally
      CloseHandle(AMutex);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
