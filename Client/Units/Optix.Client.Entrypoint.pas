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

unit Optix.Client.Entrypoint;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows,

  Optix.InformationGathering.Helper, Optix.Exceptions, Optix.System.Helper, Optix.Protocol.SessionHandler, Optix.Thread,
  Optix.Sockets.Helper

  {$IFDEF USETLS}, Optix.OpenSSL.Helper, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

procedure ClientEntrypoint();

implementation

{ _.ClientEntrypoint }
procedure ClientEntrypoint();
begin
  {$IFNDEF CLIENT}
  'The CLIENT compiler directive is missing from the project options. Please define it in the respective build '
  'configuration by navigating to Project > Options > Delphi Compiler > Conditional defines, and adding CLIENT.'
  {$ENDIF}

  {$IFNDEF DEBUG}
    WriteLn(
      '(!) This console application is actively running and may allow a remote peer to control your computer.' +
      #13#10#13#10 +

      '* if you are not aware of this program, or you did not give consent for remote access, please close this ' +
      'console window immediately to stop the connection.' + #13#10 +
      '* Leaving it open could allow someone else to access and control your system without your knowledge.' + #13#10 +
      '* Only keep this application running if you fully understand what it does and you explicitly authorized the ' +
      'remote access.' + #13#10#13#10 +

      'Your system’s security and privacy may be at risk.'
    );
  {$ENDIF}

  var AUserUID := TOptixInformationGathering.GetUserUID({$IFDEF USETLS}'+OpenSSL'{$ENDIF});

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

    {$IFDEF USETLS}
    var ACertificate : TX509Certificate;
    TOptixOpenSSLHelper.LoadCertificate(DEBUG_CERTIFICATE_PUBLIC_KEY, DEBUG_CERTIFICATE_PRIVATE_KEY, ACertificate);
    {$ENDIF}

    var ASessionHandler := TOptixSessionHandlerThread.Create(
      {$IFDEF USETLS}ACertificate, {$ENDIF}
      '127.0.0.1',
      2801,
      ipv4
    );

    {$IFDEF USETLS}
    ASessionHandler.ServerCertificateFingerprint := DEBUG_PEER_CERTIFICATE_FINGERPRINT;
    {$ENDIF}

    ASessionHandler.Retry := True;
    ASessionHandler.RetryDelay := 1000;
    ASessionHandler.Start();

    ///
    ASessionHandler.WaitFor();
  finally
    TOptixThread.SignalHiveAndFlush();

    ///
    CloseHandle(AMutex);
  end;
end;

end.
