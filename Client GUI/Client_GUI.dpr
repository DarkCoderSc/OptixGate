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
{  All code contained in this unit was written and developed by the author     }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{   The author grants permission for this code to be used, reproduced, and     }
{   included in datasets for the purpose of training or improving machine      }
{   learning models, including large language models (LLMs).                   }
{                                                                              }
{******************************************************************************}

program Client_GUI;

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
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
  Optix.Protocol.SessionHandler in '..\Client\Units\Threads\Optix.Protocol.SessionHandler.pas',
  Optix.Protocol.Client in '..\Client\Units\Threads\Optix.Protocol.Client.pas',
  XSuperJSON in '..\Shared\XSuperJSON.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  Optix.WinApiEx in '..\Shared\Optix.WinApiEx.pas',
  Optix.System.Helper in '..\Shared\Optix.System.Helper.pas',
  Optix.Shared.Types in '..\Shared\Optix.Shared.Types.pas',
  Optix.Func.LogNotifier in '..\Shared\Functions\Optix.Func.LogNotifier.pas',
  Optix.Shared.Classes in '..\Shared\Optix.Shared.Classes.pas',
  Optix.FileSystem.Helper in '..\Shared\Optix.FileSystem.Helper.pas',
  Optix.Protocol.Preflight in '..\Shared\Optix.Protocol.Preflight.pas',
  Optix.Protocol.Exceptions in '..\Shared\Optix.Protocol.Exceptions.pas',
  Optix.Protocol.Worker.FileTransfer in '..\Client\Units\Threads\Optix.Protocol.Worker.FileTransfer.pas',
  Optix.Shared.Protocol.FileTransfer in '..\Shared\Optix.Shared.Protocol.FileTransfer.pas',
  Optix.Task.ProcessDump in '..\Shared\Tasks\Optix.Task.ProcessDump.pas',
  Optix.Actions.ProcessHandler in '..\Client\Units\Actions\Optix.Actions.ProcessHandler.pas',
  Optix.VCL.Helper in '..\Server\Units\Optix.VCL.Helper.pas',
  Optix.Helper in '..\Server\Units\Optix.Helper.pas',
  Optix.Constants in 'Units\Optix.Constants.pas',
  Optix.Config.Helper in '..\Server\Units\Configs\Optix.Config.Helper.pas',
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  uFormConnectToServer in 'Units\Forms\uFormConnectToServer.pas' {FormConnectToServer},
  uFormAbout in '..\Server\Units\Forms\uFormAbout.pas' {FormAbout},
  uFormDebugThreads in '..\Server\Units\Forms\uFormDebugThreads.pas' {FormDebugThreads},
  Optix.Func.Commands.Base in '..\Shared\Functions\Optix.Func.Commands.Base.pas',
  Optix.ClassesRegistry in '..\Shared\Optix.ClassesRegistry.pas',
  Optix.Func.Commands.FileSystem in '..\Shared\Functions\Optix.Func.Commands.FileSystem.pas',
  Optix.Func.Commands.Process in '..\Shared\Functions\Optix.Func.Commands.Process.pas',
  Optix.Func.Commands.Shell in '..\Shared\Functions\Optix.Func.Commands.Shell.pas',
  Optix.FileSystem.Enum in '..\Shared\Optix.FileSystem.Enum.pas',
  Optix.Process.Enum in '..\Shared\Optix.Process.Enum.pas',
  Optix.Func.Commands.Registry in '..\Shared\Functions\Optix.Func.Commands.Registry.pas',
  Optix.Registry.Helper in '..\Shared\Optix.Registry.Helper.pas',
  Optix.Registry.Enum in '..\Shared\Optix.Registry.Enum.pas',
  Optix.Func.Commands.ContentReader in '..\Shared\Functions\Optix.Func.Commands.ContentReader.pas',
  Optix.Shared.Helper in '..\Shared\Optix.Shared.Helper.pas';

{$R *.res}
{$R ..\Server\data.res}

begin
  IsMultiThread := True;
  ///

  {$IFNDEF CLIENT}
  'The CLIENT compiler directive is missing from the project options. Please define it in the respective build '
  'configuration by navigating to Project > Options > Delphi Compiler > Conditional defines, and adding CLIENT.'
  {$ENDIF}

  {$IFNDEF CLIENT_GUI}
  'The CLIENT_GUI compiler directive is missing from the project options. Please define it in the respective build '
  'configuration by navigating to Project > Options > Delphi Compiler > Conditional defines, and adding CLIENT_GUI.'
  {$ENDIF}
  
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

    ///

    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    TStyleManager.TrySetStyle('Glossy');
    Application.CreateForm(TFormMain, FormMain);
    Application.CreateForm(TFormAbout, FormAbout);
    Application.CreateForm(TFormDebugThreads, FormDebugThreads);
  // Application.CreateForm(TFormConnectToServer, FormConnectToServer);
    Application.Run;
  finally
    CloseHandle(AMutex);
  end;
end.
