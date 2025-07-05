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

unit Optix.System.Helper;

interface

uses Winapi.Windows;

type
  TSystemHelper = class
  public
    {@M}
    class function FileTimeToDateTime(const AFileTime: TFileTime) : TDateTime; static;
    class function TryFileTimeToDateTime(const AFileTime: TFileTime) : TDateTime; static;
    class procedure NTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
    class procedure TryNTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
  end;

implementation

uses Optix.Exceptions, System.SysUtils;

{ TSystemHelper.NTSetPrivilege }
class procedure TSystemHelper.NTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
begin
  var hToken : THandle;
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
    raise EWindowsException.Create('OpenProcessToken');

  try
    var ATokenPrivilege : TOKEN_PRIVILEGES;

    if not LookupPrivilegeValue(nil, PChar(APrivilegeName), ATokenPrivilege.Privileges[0].Luid) then
      raise EWindowsException.Create('LookupPrivilegeValue');

    ATokenPrivilege.PrivilegeCount := 1;

    case AEnabled of
      True  : ATokenPrivilege.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED;
      False : ATokenPrivilege.Privileges[0].Attributes  := 0;
    end;

    if not AdjustTokenPrivileges(
                                  hToken,
                                  False,
                                  ATokenPrivilege,
                                  SizeOf(TOKEN_PRIVILEGES),
                                  PTokenPrivileges(nil)^,
                                  PDWORD(nil)^
    ) then
      raise EWindowsException.Create('AdjustTokenPrivileges');
  finally
    CloseHandle(hToken);
  end;
end;

{ TSystemHelper.TryNTSetPrivilege }
class procedure TSystemHelper.TryNTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
begin
  try
    NTSetPrivilege(APrivilegeName, AEnabled);
  except

  end;
end;

{ TSystemHelper.FileTimeToDateTime }
class function TSystemHelper.FileTimeToDateTime(const AFileTime: TFileTime): TDateTime;
begin
  var ALocalFileTime: TFileTime;
  if not FileTimeToLocalFileTime(AFileTime, ALocalFileTime) then
    raise EWindowsException.Create('FileTimeToLocalFileTime');

  var ASystemTime : TSystemTime;
  if not FileTimeToSystemTime(AFileTime, ASystemTime) then
    raise EWindowsException.Create('FileTimeToSystemTime');

  ///
  Result := SystemTimeToDateTime(ASystemTime);
end;

{ TSystemHelper.TryFileTimeToDateTime }
class function TSystemHelper.TryFileTimeToDateTime(const AFileTime: TFileTime): TDateTime;
begin
  try
    result := FileTimeToDateTime(AFileTime);
  except
    result := Now;
  end;
end;

end.
