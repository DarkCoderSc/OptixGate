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

unit Optix.Registry.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  Generics.Collections,

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
  private
    class var FRegistryHives : TDictionary<String, HKEY>;
  public
    {@C}
    class constructor Create();
    class destructor Destroy();

    {@M}
    class procedure ExtractKeyPathInformation(const AFullPath: String; var AHive: HKEY; var APath: String); static;
    class function GetRegistryACLString(const AKeyFullPath : String = '') : String; static;
    class function TryGetFileACLString(const AKeyFullPath : String) : String; static;
    class procedure GetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
      var ARegistryKeyPermissions : TRegistryKeyPermissions); static;
    class procedure TryGetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
      var ARegistryKeyPermissions : TRegistryKeyPermissions); static;
    class function HiveToString(const AHive : HKEY) : String; static;
    class function ExpandHiveShortName(const AKeyFullPath : String) : String; static;
    class procedure CheckRegistryPath(const AKeyFullPath : String); static;

    {@G}
    class property RegistryHives : TDictionary<String, HKEY> read FRegistryHives;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Optix.Exceptions, Optix.WinApiEx, Optix.System.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{ TRegistryHelper.Create }
class constructor TRegistryHelper.Create();
begin
  FRegistryHives := TDictionary<String, HKEY>.Create();
  ///

  FRegistryHives.Add('HKEY_CLASSES_ROOT', HKEY_CLASSES_ROOT);
  FRegistryHives.Add('HKEY_CURRENT_USER', HKEY_CURRENT_USER);
  FRegistryHives.Add('HKEY_LOCAL_MACHINE', HKEY_LOCAL_MACHINE);
  FRegistryHives.Add('HKEY_USERS', HKEY_USERS);
  FRegistryHives.Add('HKEY_PERFORMANCE_DATA', HKEY_PERFORMANCE_DATA);
  FRegistryHives.Add('HKEY_CURRENT_CONFIG', HKEY_CURRENT_CONFIG);
  FRegistryHives.Add('HKEY_DYN_DATA', HKEY_DYN_DATA);
end;

{ TRegistryHelper.Destroy }
class destructor TRegistryHelper.Destroy();
begin
  if Assigned(FRegistryHives) then
    FreeAndNil(FRegistryHives);
end;

{ TRegistryHelper.ExtractKeyPathInformation }
class procedure TRegistryHelper.ExtractKeyPathInformation(const AFullPath: String; var AHive: HKEY; var APath: String);
begin
  AHive := 0;
  APath := '';

  var APosSlash := Pos('\', AFullPath);
  var AHiveName := '';

  if APosSlash > 0 then begin
    AHiveName := Copy(AFullPath, 1, APosSlash - 1);

    APath := ExcludeTrailingPathDelimiter(Copy(AFullPath, APosSlash + 1, MaxInt));
  end else
    AHiveName := AFullPath;

  ///
  if not Assigned(FRegistryHives) or not FRegistryHives.TryGetValue(AHiveName, AHive) then
    raise Exception.Create(Format('"%s" is not a valid registry hive.', [AHiveName]));
end;

{ TRegistryHelper.GetRegistryACLString }
class function TRegistryHelper.GetRegistryACLString(const AKeyFullPath : String = '') : String;
begin
  result := '';
  ///

  var AHive : HKEY;
  var AKeyPath : String;

  ExtractKeyPathInformation(AKeyFullPath, AHive, AKeyPath);

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
class function TRegistryHelper.TryGetFileACLString(const AKeyFullPath : String) : String;
begin
  try
    result := GetRegistryACLString(AKeyFullPath);
  except
    result := '';
  end;
end;

{ TRegistryHelper.GetCurrentUserRegistryKeyAccess }
class procedure TRegistryHelper.GetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
 var ARegistryKeyPermissions : TRegistryKeyPermissions);
begin
  ARegistryKeyPermissions := [];
  ///

  var AHive : HKEY;
  var AKeyPath := '';

  ExtractKeyPathInformation(AKeyFullPath, AHive, AKeyPath);

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
class procedure TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
 var ARegistryKeyPermissions : TRegistryKeyPermissions);
begin
  try
    GetCurrentUserRegistryKeyAccess(AKeyFullPath, ARegistryKeyPermissions);
  except
    ARegistryKeyPermissions := [];
  end;
end;

{ TRegistryHelper.HiveToString }
class function TRegistryHelper.HiveToString(const AHive : HKEY) : String;
begin
  result := '';
  ///

  for var APair in FRegistryHives do
    if APair.Value = AHive then
      result := APair.Key;
end;

class function TRegistryHelper.ExpandHiveShortName(const AKeyFullPath : String) : String;
const
  HIVES_MAP : array[0..7-1, 0..2-1] of String = (
    ('HKCR\', 'HKEY_CLASSES_ROOT\'),
    ('HKCU\', 'HKEY_CURRENT_USER\'),
    ('HKLM\', 'HKEY_LOCAL_MACHINE\'),
    ('HKU\',  'HKEY_USERS\'),
    ('HKPD\', 'HKEY_PERFORMANCE_DATA\'),
    ('HKCC\', 'HKEY_CURRENT_CONFIG\'),
    ('HKDD\', 'HKEY_DYN_DATA\')
  );
begin
  result := AKeyFullPath;
  ///

  for var i := 0 to High(HIVES_MAP) do begin
    if AKeyFullPath.StartsWith(HIVES_MAP[i, 0], True) then begin
      Result := HIVES_MAP[i, 1] + Copy(AKeyFullPath, Length(HIVES_MAP[i, 0]) + 1, MaxInt);

      Break;
    end;
  end;
end;

{ TRegistryHelper.CheckRegistryPath }
class procedure TRegistryHelper.CheckRegistryPath(const AKeyFullPath : String);
begin
  var AHive : HKEY;
  var AKeyPath := '';

  ExtractKeyPathInformation(AKeyFullPath, AHive, AKeyPath);

  var AKeyHandle : HKEY;

  var AResult := RegOpenKeyExW(AHive, PWideChar(AKeyPath), 0, READ_CONTROL, AKeyHandle);
  if AResult <> ERROR_SUCCESS then
    raise EWindowsException.Create('RegOpenKeyW', AResult);

  RegCloseKey(AKeyHandle);
end;

end.
