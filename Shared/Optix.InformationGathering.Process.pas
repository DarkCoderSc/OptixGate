{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                              Project: Optix Neo                              }
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

unit Optix.InformationGathering.Process;

interface

uses Winapi.Windows, System.Classes;

type
  TElevatedStatus = (
    esUnknown,
    esLimited,
    esElevated
  );

  TProcessInformation = class
  public
    {@M}
    class function IsElevated(AProcessHandle : THandle = 0) : TElevatedStatus; static;
    class function TryGetIsElevated(AProcessHandle : THandle = 0) : TElevatedStatus; static;
  end;

  function ElevatedStatusToString(const AValue : TElevatedStatus) : String;

implementation

uses Optix.Exceptions;

(* Local *)

{ _.ElevatedStatusToString }
function ElevatedStatusToString(const AValue : TElevatedStatus) : String;
begin
  result := 'Unknown';
  ///

  case AValue of
    esLimited  : result := 'Limited';
    esElevated : result := 'Elevated';
  end;
end;

(* TProcessInformation *)

{ TProcessInformation.IsElevated }
class function TProcessInformation.IsElevated(AProcessHandle : THandle = 0) : TElevatedStatus;
var AToken        : THandle;
    ATokenInfo    : TTokenElevation;
    AReturnLength : DWORD;
begin
  result := esUnknown;
  ///

  if AProcessHandle = 0 then begin
    AProcessHandle := GetCurrentProcess();
    if AProcessHandle = 0 then
      raise EWindowsException.Create('GetCurrentProcess');
  end;

  if not OpenProcessToken(AProcessHandle, TOKEN_QUERY, AToken) then
    raise EWindowsException.Create('OpenProcessToken');

  if not GetTokenInformation(AToken, TokenElevation, @ATokenInfo, SizeOf(TTokenElevation), AReturnLength) then
    raise EWindowsException.Create('GetTokenInformation');

  ///
  if ATokenInfo.TokenIsElevated <> 0 then
    result := esElevated
  else
    result := esLimited;
end;

{ TProcessInformation.TryGetIsElevated }
class function TProcessInformation.TryGetIsElevated(AProcessHandle : THandle = 0) : TElevatedStatus;
begin
  result := esUnknown;
  try
    result := IsElevated(AProcessHandle);
  except
    on E : EWindowsException do begin
      // Ignore, we just try but we can log
    end;
  end;
end;

end.
