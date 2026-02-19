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

program Client_GUI_OpenSSL;

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  XSuperObject in '..\ExtLibraries\Delphi Libraries\XSuperObject.pas',
  XSuperJSON in '..\ExtLibraries\Delphi Libraries\XSuperJSON.pas',
  OptixCore.Exceptions in '..\Core\OptixCore.Exceptions.pas',
  OptixCore.Sockets.Helper in '..\Core\Network\OptixCore.Sockets.Helper.pas',
  OptixCore.Protocol.Packet in '..\Core\Network\OptixCore.Protocol.Packet.pas',
  OptixCore.Sockets.Exceptions in '..\Core\Network\OptixCore.Sockets.Exceptions.pas',
  OptixCore.Commands in '..\Core\Commands\OptixCore.Commands.pas',
  OptixCore.Interfaces in '..\Core\OptixCore.Interfaces.pas',
  OptixCore.Thread in '..\Core\OptixCore.Thread.pas',
  OptixCore.Protocol.Client.Handler in '..\Core\Network\OptixCore.Protocol.Client.Handler.pas',
  OptixCore.System.InformationGathering in '..\Core\System\OptixCore.System.InformationGathering.pas',
  OptixCore.System.Process in '..\Core\System\OptixCore.System.Process.pas',
  OptixCore.SessionInformation in '..\Core\Commands\OptixCore.SessionInformation.pas',
  Optix.Protocol.SessionHandler in 'Units\Threads\Optix.Protocol.SessionHandler.pas',
  Optix.Protocol.Client in 'Units\Threads\Optix.Protocol.Client.pas',
  OptixCore.WinApiEx in '..\Core\OptixCore.WinApiEx.pas',
  OptixCore.System.Helper in '..\Core\System\OptixCore.System.Helper.pas',
  OptixCore.Types in '..\Core\OptixCore.Types.pas',
  OptixCore.LogNotifier in '..\Core\Commands\OptixCore.LogNotifier.pas',
  OptixCore.Classes in '..\Core\OptixCore.Classes.pas',
  OptixCore.System.FileSystem in '..\Core\System\OptixCore.System.FileSystem.pas',
  OptixCore.Protocol.Preflight in '..\Core\Network\OptixCore.Protocol.Preflight.pas',
  OptixCore.Protocol.Exceptions in '..\Core\Network\OptixCore.Protocol.Exceptions.pas',
  Optix.Protocol.Worker.FileTransfer in 'Units\Threads\Optix.Protocol.Worker.FileTransfer.pas',
  OptixCore.Protocol.FileTransfer in '..\Core\Network\OptixCore.Protocol.FileTransfer.pas',
  OptixCore.Task.ProcessDump in '..\Core\Tasks\OptixCore.Task.ProcessDump.pas',
  Optix.Actions.ProcessHandler in 'Units\Actions\Optix.Actions.ProcessHandler.pas',
  Optix.Helper in '..\Server\Units\Optix.Helper.pas',
  Optix.Constants in '..\Server\Units\Optix.Constants.pas',
  OptixCore.OpenSSL.Headers in '..\Core\Network\OpenSSL\OptixCore.OpenSSL.Headers.pas',
  OptixCore.OpenSSL.Helper in '..\Core\Network\OpenSSL\OptixCore.OpenSSL.Helper.pas',
  OptixCore.OpenSSL.Exceptions in '..\Core\Network\OpenSSL\OptixCore.OpenSSL.Exceptions.pas',
  OptixCore.OpenSSL.Context in '..\Core\Network\OpenSSL\OptixCore.OpenSSL.Context.pas',
  OptixCore.OpenSSL.Handler in '..\Core\Network\OpenSSL\OptixCore.OpenSSL.Handler.pas',
  Optix.DebugCertificate in 'Units\Optix.DebugCertificate.pas',
  Optix.Config.CertificatesStore in '..\Server\Units\Configs\Optix.Config.CertificatesStore.pas',
  Optix.Config.Helper in '..\Server\Units\Configs\Optix.Config.Helper.pas',
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  uFormConnectToServer in 'Units\Forms\uFormConnectToServer.pas' {FormConnectToServer},
  uFormAbout in '..\Server\Units\Forms\uFormAbout.pas' {FormAbout},
  uFormDebugThreads in '..\Server\Units\Forms\uFormDebugThreads.pas' {FormDebugThreads},
  uFormCertificatesStore in '..\Server\Units\Forms\uFormCertificatesStore.pas' {FormCertificatesStore},
  uFormGenerateNewCertificate in '..\Server\Units\Forms\uFormGenerateNewCertificate.pas' {FormGenerateNewCertificate},
  uFormTrustedCertificates in '..\Server\Units\Forms\uFormTrustedCertificates.pas' {FormTrustedCertificates},
  Optix.Config.TrustedCertificatesStore in '..\Server\Units\Configs\Optix.Config.TrustedCertificatesStore.pas',
  OptixCore.Commands.Base in '..\Core\Commands\OptixCore.Commands.Base.pas',
  OptixCore.ClassesRegistry in '..\Core\OptixCore.ClassesRegistry.pas',
  OptixCore.Commands.FileSystem in '..\Core\Commands\OptixCore.Commands.FileSystem.pas',
  OptixCore.Commands.Process in '..\Core\Commands\OptixCore.Commands.Process.pas',
  OptixCore.Commands.Shell in '..\Core\Commands\OptixCore.Commands.Shell.pas',
  OptixCore.Commands.Registry in '..\Core\Commands\OptixCore.Commands.Registry.pas',
  OptixCore.System.Registry in '..\Core\System\OptixCore.System.Registry.pas',
  OptixCore.Commands.ContentReader in '..\Core\Commands\OptixCore.Commands.ContentReader.pas',
  OptixCore.Helper in '..\Core\OptixCore.Helper.pas',
  uFormWarning in 'Units\Forms\uFormWarning.pas' {FormWarning},
  uFormSelectCertificate in '..\Server\Units\Forms\uFormSelectCertificate.pas' {FormSelectCertificate};

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

  {$IFNDEF USETLS}
  'The USETLS compiler directive is missing from the project options. Please define it in the respective build '
  'configuration by navigating to Project > Options > Delphi Compiler > Conditional defines, and adding USETLS.'
  {$ENDIF}

  var AUserUID := TOptixInformationGathering.GetUserUID('+OpenSSL');
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
    Application.CreateForm(TFormMain, FormMain);
    Application.CreateForm(TFormAbout, FormAbout);
    Application.CreateForm(TFormDebugThreads, FormDebugThreads);
    Application.CreateForm(TFormCertificatesStore, FormCertificatesStore);
    Application.CreateForm(TFormTrustedCertificates, FormTrustedCertificates);

    Application.Run;
  finally
    CloseHandle(AMutex);
  end;
end.
