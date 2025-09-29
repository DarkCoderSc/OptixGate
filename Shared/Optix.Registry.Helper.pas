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

unit Optix.Registry.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  Winapi.Windows;
// ---------------------------------------------------------------------------------------------------------------------

type
  TRegistryKeyPermission = (
    rkpRead,
    rkpWrite,
    rkpExecute,
    rkpQueryValue,
    rkpSetValue,
    rkpCreateSubKey,
    rkpEnumerateSubKeys,
    rkpNotify,
    rkpDelete
  );

  TRegistryKeyPermissions = set of TRegistryKeyPermission;

  TRegistryHelper = class
    public
      class function GetRegistryACLString(const AHive : HKEY; const AKeyPath : String = '') : String;
      class function TryGetFileACLString(const AHive : HKEY; const AKeyPath : String) : String;
      class procedure GetCurrentUserRegistryKeyAccess(const AHive : HKEY; const AKeyPath : String;
        var ARegistryKeyPermissions : TRegistryKeyPermissions);
      class procedure TryGetCurrentUserRegistryKeyAccess(const AHive : HKEY; const AKeyPath : String;
        var ARegistryKeyPermissions : TRegistryKeyPermissions);
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.Exceptions, Optix.WinApiEx, Optix.System.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{ TRegistryHelper.GetRegistryACLString }
class function TRegistryHelper.GetRegistryACLString(const AHive : HKEY; const AKeyPath : String = '') : String;
begin
  var ASecurityDescriptorSize := DWORD(0);

  var ptrSecurityDescriptor := PSecurityDescriptor(nil);
  var hToken := THandle(0);
  ///

  var AKeyHandle := HKEY(0);
  try
    var AResult := RegOpenKeyExW(AHive, PWideChar(AKeyPath), 0, READ_CONTROL, AKeyHandle);
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegOpenKeyW', AResult);
    ///

    var AFlags := OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION;

    AResult := RegGetKeySecurity(AKeyHandle, AFlags, nil, ASecurityDescriptorSize);
    if (AResult <> ERROR_SUCCESS) and (AResult <> 122) then
      raise EWindowsException.Create('GetNamedSecurityInfoW', AResult);

    GetMem(ptrSecurityDescriptor, ASecurityDescriptorSize);

    AResult := RegGetKeySecurity(AKeyHandle, AFlags, ptrSecurityDescriptor, ASecurityDescriptorSize);
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('GetNamedSecurityInfoW', AResult);

    var pSSDL : LPWSTR := nil;
    if not ConvertSecurityDescriptorToStringSecurityDescriptorW(
      ptrSecurityDescriptor,
      SDDL_REVISION_1,
      AFlags,
      pSSDL,
      nil
    ) then
      raise EWindowsException.Create('ConvertSecurityDescriptorToStringSecurityDescriptorW');

    ///
    result := string(pSSDL);
  finally
    if AKeyHandle <> 0 then
      RegCloseKey(AKeyHandle);

    if Assigned(ptrSecurityDescriptor) then
      FreeMem(ptrSecurityDescriptor, ASecurityDescriptorSize);

    if hToken <> 0 then
      CloseHandle(hToken);
  end;
end;

{ TRegistryHelper.TryGetFileACLString }
class function TRegistryHelper.TryGetFileACLString(const AHive : HKEY; const AKeyPath : String) : String;
begin
  try
    result := GetRegistryACLString(AHive, AKeyPath);
  except
    result := '';
  end;
end;

{ TRegistryHelper.GetCurrentUserRegistryKeyAccess }
class procedure TRegistryHelper.GetCurrentUserRegistryKeyAccess(const AHive : HKEY; const AKeyPath : String;
 var ARegistryKeyPermissions : TRegistryKeyPermissions);
begin
  ARegistryKeyPermissions := [];
  ///

  var AImpersonated := False;

  var ASecurityDescriptorSize := DWORD(0);

  var ptrSecurityDescriptor := PSecurityDescriptor(nil);
  var hToken := THandle(0);
  ///

  var AKeyHandle := HKEY(0);
  try
    var AResult := RegOpenKeyExW(AHive, PWideChar(AKeyPath), 0, READ_CONTROL, AKeyHandle);
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegOpenKeyW', AResult);
    ///

    AImpersonated := ImpersonateSelf(SecurityImpersonation);

    if not OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, False, hToken) then
      raise EWindowsException.Create('OpenProcessToken');
    ///

    var AFlags := OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION;

    AResult := RegGetKeySecurity(AKeyHandle, AFlags, nil, ASecurityDescriptorSize);
    if (AResult <> ERROR_SUCCESS) and (AResult <> 122) then
      raise EWindowsException.Create('GetNamedSecurityInfoW', AResult);

    GetMem(ptrSecurityDescriptor, ASecurityDescriptorSize);

    AResult := RegGetKeySecurity(AKeyHandle, AFlags, ptrSecurityDescriptor, ASecurityDescriptorSize);
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('GetNamedSecurityInfoW', AResult);

    ///
    if TSystemHelper.AccessCheck(KEY_READ, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpRead);

    if TSystemHelper.AccessCheck(KEY_WRITE, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpWrite);

    if TSystemHelper.AccessCheck(KEY_EXECUTE, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpExecute);

    if TSystemHelper.AccessCheck(KEY_QUERY_VALUE, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpQueryValue);

    if TSystemHelper.AccessCheck(KEY_SET_VALUE, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpSetValue);

    if TSystemHelper.AccessCheck(KEY_CREATE_SUB_KEY, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpCreateSubKey);

    if TSystemHelper.AccessCheck(KEY_ENUMERATE_SUB_KEYS, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpEnumerateSubKeys);

    if TSystemHelper.AccessCheck(KEY_NOTIFY, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpNotify);

    if TSystemHelper.AccessCheck(_DELETE, hToken, ptrSecurityDescriptor) then
      Include(ARegistryKeyPermissions, rkpDelete);
  finally
    if AKeyHandle <> 0 then
      RegCloseKey(AKeyHandle);

    if Assigned(ptrSecurityDescriptor) then
      FreeMem(ptrSecurityDescriptor, ASecurityDescriptorSize);

    if hToken <> 0 then
      CloseHandle(hToken);

    if AImpersonated then
      RevertToSelf();
  end;
end;

{ TRegistryHelper.TryGetCurrentUserRegistryKeyAccess }
class procedure TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(const AHive : HKEY; const AKeyPath : String;
 var ARegistryKeyPermissions : TRegistryKeyPermissions);
begin
  try
    GetCurrentUserRegistryKeyAccess(AHive, AKeyPath, ARegistryKeyPermissions);
  except
    ARegistryKeyPermissions := [];
  end;
end;

end.
