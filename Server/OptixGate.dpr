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

program OptixGate;

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  XSuperObject in '..\ExtLibraries\Delphi Libraries\XSuperObject.pas',
  XSuperJSON in '..\ExtLibraries\Delphi Libraries\XSuperJSON.pas',
  OptixCore.Exceptions in '..\Core\OptixCore.Exceptions.pas',
  OptixCore.Sockets.Helper in '..\Core\Network\OptixCore.Sockets.Helper.pas',
  OptixCore.Protocol.Packet in '..\Core\Network\OptixCore.Protocol.Packet.pas',
  OptixCore.Sockets.Exceptions in '..\Core\Network\OptixCore.Sockets.Exceptions.pas',
  OptixCore.Interfaces in '..\Core\OptixCore.Interfaces.pas',
  OptixCore.Thread in '..\Core\OptixCore.Thread.pas',
  OptixCore.Protocol.Client.Handler in '..\Core\Network\OptixCore.Protocol.Client.Handler.pas',
  OptixCore.SessionInformation in '..\Core\Commands\OptixCore.SessionInformation.pas',
  OptixCore.Commands in '..\Core\Commands\OptixCore.Commands.pas',
  OptixCore.System.Helper in '..\Core\System\OptixCore.System.Helper.pas',
  OptixCore.Types in '..\Core\OptixCore.Types.pas',
  OptixCore.System.InformationGathering in '..\Core\System\OptixCore.System.InformationGathering.pas',
  OptixCore.WinApiEx in '..\Core\OptixCore.WinApiEx.pas',
  OptixCore.LogNotifier in '..\Core\Commands\OptixCore.LogNotifier.pas',
  OptixCore.System.Process in '..\Core\System\OptixCore.System.Process.pas',
  OptixCore.System.FileSystem in '..\Core\System\OptixCore.System.FileSystem.pas',
  OptixCore.Classes in '..\Core\OptixCore.Classes.pas',
  OptixCore.Protocol.Preflight in '..\Core\Network\OptixCore.Protocol.Preflight.pas',
  OptixCore.Protocol.Exceptions in '..\Core\Network\OptixCore.Protocol.Exceptions.pas',
  OptixCore.Protocol.FileTransfer in '..\Core\Network\OptixCore.Protocol.FileTransfer.pas',
  OptixCore.Task.ProcessDump in '..\Core\Tasks\OptixCore.Task.ProcessDump.pas',
  Optix.Protocol.Worker.FileTransfer in 'Units\Threads\Optix.Protocol.Worker.FileTransfer.pas',
  Optix.Protocol.Server in 'Units\Threads\Optix.Protocol.Server.pas',
  Optix.Protocol.SessionHandler in 'Units\Threads\Optix.Protocol.SessionHandler.pas',
  Optix.Protocol.Client in 'Units\Threads\Optix.Protocol.Client.pas',
  Optix.Helper in 'Units\Optix.Helper.pas',
  Optix.Config.Helper in 'Units\Configs\Optix.Config.Helper.pas',
  __uBaseFormControl__ in 'Units\Forms\Control\__uBaseFormControl__.pas',
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  Optix.Constants in 'Units\Optix.Constants.pas',
  uFormAbout in 'Units\Forms\uFormAbout.pas' {FormAbout},
  uControlFormProcessManager in 'Units\Forms\Control\uControlFormProcessManager.pas' {ControlFormProcessManager},
  uControlFormLogs in 'Units\Forms\Control\uControlFormLogs.pas' {ControlFormLogs},
  uControlFormFileManager in 'Units\Forms\Control\uControlFormFileManager.pas' {ControlFormFileManager},
  uControlFormControlForms in 'Units\Forms\Control\uControlFormControlForms.pas' {ControlFormControlForms},
  uControlFormTransfers in 'Units\Forms\Control\uControlFormTransfers.pas' {ControlFormTransfers},
  uFormDebugThreads in 'Units\Forms\uFormDebugThreads.pas' {FormDebugThreads},
  uControlFormTasks in 'Units\Forms\Control\uControlFormTasks.pas' {ControlFormTasks},
  uControlFormDumpProcess in 'Units\Forms\Dialogs\Control\uControlFormDumpProcess.pas' {ControlFormDumpProcess},
  uControlFormRemoteShell in 'Units\Forms\Control\uControlFormRemoteShell.pas' {ControlFormRemoteShell},
  uFrameRemoteShellInstance in 'Units\Frames\uFrameRemoteShellInstance.pas' {FrameRemoteShellInstance: TFrame},
  uFormListen in 'Units\Forms\uFormListen.pas' {FormListen},
  uFormServers in 'Units\Forms\uFormServers.pas' {FormServers},
  Optix.Config.Servers in 'Units\Configs\Optix.Config.Servers.pas',
  OptixCore.Commands.Base in '..\Core\Commands\OptixCore.Commands.Base.pas',
  OptixCore.ClassesRegistry in '..\Core\OptixCore.ClassesRegistry.pas',
  OptixCore.Commands.FileSystem in '..\Core\Commands\OptixCore.Commands.FileSystem.pas',
  OptixCore.Commands.Process in '..\Core\Commands\OptixCore.Commands.Process.pas',
  OptixCore.Commands.Shell in '..\Core\Commands\OptixCore.Commands.Shell.pas',
  OptixCore.Commands.Registry in '..\Core\Commands\OptixCore.Commands.Registry.pas',
  OptixCore.System.Registry in '..\Core\System\OptixCore.System.Registry.pas',
  uControlFormRegistryManager in 'Units\Forms\Control\uControlFormRegistryManager.pas' {ControlFormRegistryManager},
  OptixCore.Commands.ContentReader in '..\Core\Commands\OptixCore.Commands.ContentReader.pas',
  uControlFormContentReader in 'Units\Forms\Control\uControlFormContentReader.pas' {ControlFormContentReader},
  uControlFormSetupContentReader in 'Units\Forms\Control\uControlFormSetupContentReader.pas' {ControlFormSetupContentReader},
  uFrameHexEditor in 'Units\Frames\Components\uFrameHexEditor.pas' {FrameHexEditor: TFrame},
  uControlFormRegistryEditor in 'Units\Forms\Control\uControlFormRegistryEditor.pas' {ControlFormRegistryEditor},
  OptixCore.Helper in '..\Core\OptixCore.Helper.pas';

{$R *.res}
{$R data.res}

begin
  IsMultiThread := True;
  ///

  {$IFNDEF SERVER}
  'The SERVER compiler directive is missing from the project options. Please define it in the respective build '
  'configuration by navigating to Project > Options > Delphi Compiler > Conditional defines, and adding SERVER.'
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormDebugThreads, FormDebugThreads);
  Application.CreateForm(TFormServers, FormServers);

  ///
  Application.Run;
end.
