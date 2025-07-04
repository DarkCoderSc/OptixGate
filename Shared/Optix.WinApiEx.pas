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

unit Optix.WinApiEx;

interface

uses Winapi.Windows;

type
  (* Netapi32.dll *)
  NET_API_STATUS = DWORD;

  {$A8}
  WKSTA_INFO_100 = record
    wki100_platform_id  : DWORD;
    wki100_computername : LPWSTR;
    wki100_langroup     : LPWSTR;
    wki100_ver_major    : DWORD;
    wki100_ver_minor    : DWORD;
  end;
  TWkstaInfo100 = WKSTA_INFO_100;
  PWkstaInfo100 = ^TWkstaInfo100;
  {$A4}


  DOMAIN_CONTROLLER_INFO = record
    DomainControllerName        : LPWSTR;
    DomainControllerAddress     : LPWSTR;
    DomainControllerAddressType : ULONG;
    DomainGuid                  : TGUID;
    DomainName                  : LPWSTR;
    DnsForestName               : LPWSTR;
    Flags                       : ULONG;
    DcSiteName                  : LPWSTR;
    ClientSiteName              : LPWSTR;
  end;
  TDomainControllerInfo = DOMAIN_CONTROLLER_INFO;
  PDomainControllerInfo = ^TDomainControllerInfo;

const
  NERR_Success                  = 0;

  DS_DIRECTORY_SERVICE_REQUIRED = $00000010;

  SECURITY_NT_AUTHORITY : TSIDIdentifierAuthority = (
    Value: (0, 0, 0, 0, 0, 5)
  );

  DOMAIN_ALIAS_RID_ADMINS       = $00000220;
  SECURITY_BUILTIN_DOMAIN_RID   = $00000020;

(* Netapi32.dll *)
function DsGetDcNameW(
  ComputerName         : LPCWSTR;
  DomainName           : LPCWSTR;
  GUID                 : PGUID;
  SiteName             : LPCWSTR;
  Flags                : ULONG;
  out DomainControllerInfo : PDomainControllerInfo
) : NET_API_STATUS; stdcall; external 'Netapi32.dll' Name 'DsGetDcNameW';

function NetWkstaGetInfo(
  servername : LPWSTR;
  level      : DWORD;
  out bufptr : Pointer
) : NET_API_STATUS; stdcall; external 'Netapi32.dll' Name 'NetWkstaGetInfo';

function NetApiBufferFree(
  Buffer : Pointer
) : NET_API_STATUS; stdcall; external 'Netapi32.dll';

(* Advapi32.dll *)

function CheckTokenMembership(
  TokenHandle  : THandle;
  SIdToCheck   : PSID;
  var IsMember : Boolean
): BOOL; stdcall; external 'Advapi32.dll';

implementation

end.
