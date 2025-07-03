{******************************************************************************}
{                                                                              }
{            __________.__                                                     }
{            \______   \  |_________  ____________ ____   ____                 }
{             |     ___/  |  \_  __ \/  _ \___   // __ \ /    \                }
{             |    |   |   Y  \  | \(  <_> )    /\  ___/|   |  \               }
{             |____|   |___|  /__|   \____/_____ \\___  >___|  /               }
{             \/                  \/    \/     \/                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://www.phrozen.io/                                    }
{                   https://github.com/PhrozenIO                               }
{                   License: Private                                           }
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
