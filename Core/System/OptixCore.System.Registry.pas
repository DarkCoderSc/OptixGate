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



unit OptixCore.System.Registry;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  Winapi.Windows,

  OptixCore.Classes;
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
    const
      REG_MSZ_LINE_SEP = '{/\$/\@/\0\n/%\//\!/\}';

    {@C}
    class constructor Create();
    class destructor Destroy();

    {@M}
    class procedure ExtractKeyPathInformation(const AFullPath: String; out AHive: HKEY; out APath: String); static;
    class function GetRegistryACLString(const AKeyFullPath : String = '') : String; static;
    class function TryGetFileACLString(const AKeyFullPath : String) : String; static;
    class procedure GetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
      out ARegistryKeyPermissions : TRegistryKeyPermissions); static;
    class procedure TryGetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
      out ARegistryKeyPermissions : TRegistryKeyPermissions); static;
    class function HiveToString(const AHive : HKEY) : String; static;
    class function ExpandHiveShortName(const AKeyFullPath : String) : String; static;
    class function OpenRegistryKey(const ARegistryHive : HKEY; const AKeyPath : String;
      const ADesiredAccess : REGSAM) : HKEY; overload; static;
    class procedure SplitRegKeyFullPath(AKeyFullPath: String; out ARootKeyPath, AKeyName: String); static;
    class function OpenRegistryKey(const AKeyFullPath : String; const ADesiredAccess : REGSAM) : HKEY; overload; static;
    class procedure CheckRegistryPath(const AKeyFullPath : String); static;
    class procedure CreateSubKey(ANewKeyFullPath : String); overload; static;
    class procedure CreateSubKey(const AKeyFullPath, ANewKeyName : String); overload; static;
    class procedure DeleteKey(const AKeyFullPath : String); static;
    class procedure SetValue(const hOpenedKey : HKEY; const AName : String; const AValueKind : DWORD;
      const pData : Pointer; const ADataSize : UInt64); overload; static;
    class procedure SetValue(const AKeyFullPath, AName : String; const AValueKind : DWORD;
      const pData : Pointer; const ADataSize : UInt64); overload; static;
    class function ValueKindToString(const AValueKind : DWORD) : String; static;
    class procedure ReadValue(const hOpenedKey : HKEY; const AValueName : String; out AValueType : DWORD;
      out pData : Pointer; out ADataSize : DWORD); overload; static;
    class procedure ReadValue(const AFullKeyPath : String; const AValueName : String; out AValueType : DWORD;
      out pData : Pointer; out ADataSize : DWORD); overload; static;
    class procedure RenameKey(const hOpenedKey : HKEY; const ASubKeyName, ANewKeyName : String); overload; static;
    class procedure RenameKey(const AFullKeyPath, ASubKeyName, ANewKeyName : String); overload; static;
    class procedure DeleteValue(const hOpenedKey : HKEY; const AValueName : String); overload; static;
    class procedure DeleteValue(const AFullKeyPath : String; const AValueName : String); overload; static;
    class procedure RenameValue(const AFullKeyPath, AValueName, ANewValueName : String); static;

    {@G}
    class property RegistryHives : TDictionary<String, HKEY> read FRegistryHives;
  end;

  TRegistryKeyInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FACL_SSDL : String;

    [OptixSerializableAttribute]
    FPermissions : TRegistryKeyPermissions;
  public
    {@C}
    constructor Create(const AName : String; const APath : String = ''); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Name : String read FName write FName;

    property ACL_SSDL    : String                  read FACL_SSDL;
    property Permissions : TRegistryKeyPermissions read FPermissions;
  end;

  TRegistryValueInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FValue : TOptixMemoryObject;

    [OptixSerializableAttribute]
    FType : DWORD;

    {@M}
    function GetIsDefault() : Boolean;
  public
    {@C}
    constructor Create(const AName : String; const AType : DWORD; const pData : Pointer;
      const ADataSize : DWORD); overload;
    destructor Destroy(); override;

    {@M}
    function ToString() : String; override;
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Value     : TOptixMemoryObject read FValue;
    property _Type     : DWORD              read FType;
    property IsDefault : Boolean            read GetIsDefault;

    {@G/S}
    property Name : String read FName write FName;
  end;

  TOptixEnumRegistry = class
  public
    class procedure Enum(const AKeyFullPath : String; var AKeys : TObjectList<TRegistryKeyInformation>;
      var AValues : TObjectList<TRegistryValueInformation>); static;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.IOUtils, System.StrUtils,

  OptixCore.Exceptions, OptixCore.WinApiEx, OptixCore.System.Helper, OptixCore.Helper;
// ---------------------------------------------------------------------------------------------------------------------

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
class destructor TRegistryHelper.Destroy();
begin
  if Assigned(FRegistryHives) then
    FreeAndNil(FRegistryHives);
end;

class procedure TRegistryHelper.ExtractKeyPathInformation(const AFullPath: String; out AHive: HKEY; out APath: String);
begin
  AHive := 0;

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

class function TRegistryHelper.TryGetFileACLString(const AKeyFullPath : String) : String;
begin
  try
    result := GetRegistryACLString(AKeyFullPath);
  except
    result := '';
  end;
end;

class procedure TRegistryHelper.GetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
 out ARegistryKeyPermissions : TRegistryKeyPermissions);
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

class procedure TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(const AKeyFullPath : String;
 out ARegistryKeyPermissions : TRegistryKeyPermissions);
begin
  try
    GetCurrentUserRegistryKeyAccess(AKeyFullPath, ARegistryKeyPermissions);
  except
    ARegistryKeyPermissions := [];
  end;
end;

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

class function TRegistryHelper.OpenRegistryKey(const ARegistryHive : HKEY; const AKeyPath : String;
  const ADesiredAccess : REGSAM) : HKEY;
begin
  var AResult := RegOpenKeyExW(ARegistryHive, PWideChar(AKeyPath), 0, ADesiredAccess, result);
  if AResult <> ERROR_SUCCESS then
    raise EWindowsException.Create('RegOpenKeyW', AResult);
end;

class procedure TRegistryHelper.SplitRegKeyFullPath(AKeyFullPath: String; out ARootKeyPath, AKeyName: String);
begin
  if (AKeyFullPath <> '') and (AKeyFullPath[Length(AKeyFullPath)] = '\') then
    SetLength(AKeyFullPath, Length(AKeyFullPath) - 1);
  ///

  var ADirectories := SplitString(AKeyFullPath, '\');

  if Length(ADirectories) = 1 then
    ARootKeyPath := ADirectories[0]
  else if Length(ADirectories) > 1 then begin
    ARootKeyPath := String.Join('\', Copy(ADirectories, 0, High(ADirectories)));
    AKeyName := ADirectories[High(ADirectories)];
  end;
end;

class function TRegistryHelper.OpenRegistryKey(const AKeyFullPath : String; const ADesiredAccess : REGSAM) : HKEY;
begin
  var AHive : HKEY;
  var AKeyPath := '';
  ///

  ExtractKeyPathInformation(AKeyFullPath, AHive, AKeyPath);

  ///
  result := OpenRegistryKey(AHive, AKeyPath, ADesiredAccess);
end;

class procedure TRegistryHelper.CheckRegistryPath(const AKeyFullPath : String);
begin
  var AKeyHandle := OpenRegistryKey(AKeyFullPath, READ_CONTROL);

  ///
  RegCloseKey(AKeyHandle);
end;

class procedure TRegistryHelper.CreateSubKey(const AKeyFullPath, ANewKeyName : String);
begin
  var AKeyHandle := OpenRegistryKey(AKeyFullPath, KEY_CREATE_SUB_KEY);
  try
    var ANewKeyHandle : HKEY;

    var AResult := RegCreateKeyW(AKeyHandle, PWideChar(ANewKeyName), ANewKeyHandle);
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegCreateKeyW', AResult);

    ///
    RegCloseKey(ANewKeyHandle);
  finally
    RegCloseKey(AKeyHandle);
  end;
end;

class procedure TRegistryHelper.CreateSubKey(ANewKeyFullPath : String);
begin
  var ARootKeyPath := '';
  var AKeyName := '';

  SplitRegKeyFullPath(ANewKeyFullPath, ARootKeyPath, AKeyName);

  ///
  CreateSubKey(ARootKeyPath, AKeyName);
end;

class procedure TRegistryHelper.DeleteKey(const AKeyFullPath : String);
begin
  var ARootKeyPath := '';
  var AKeyName := '';

  SplitRegKeyFullPath(AKeyFullPath, ARootKeyPath, AKeyName);

  var AKeyHandle := OpenRegistryKey(ARootKeyPath, _DELETE);
  try
    var AResult := RegDeleteTreeW(AKeyHandle, PWideChar(AKeyName));
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegDeleteKeyW', AResult);
  finally
    RegCloseKey(AKeyHandle);
  end;
end;

class procedure TRegistryHelper.SetValue(const hOpenedKey : HKEY; const AName : String; const AValueKind : DWORD;
  const pData : Pointer; const ADataSize : UInt64);
begin
  var AResult := RegSetValueExW(
      hOpenedKey,
      PWideChar(AName),
      0,
      AValueKind,
      pData,
      ADataSize
    );
  if AResult <> ERROR_SUCCESS then
    raise EWindowsException.Create('RegSetValueExW', AResult);
end;
  
class procedure TRegistryHelper.SetValue(const AKeyFullPath, AName : String; const AValueKind : DWORD;
  const pData : Pointer; const ADataSize : UInt64);
begin
  var AKeyHandle := OpenRegistryKey(AKeyFullPath, KEY_SET_VALUE);
  try
    SetValue(AKeyHandle, AName, AValueKind, pData, ADataSize);  
  finally
    RegCloseKey(AKeyHandle);
  end;
end;

class function TRegistryHelper.ValueKindToString(const AValueKind : DWORD) : String;
begin
  case AValueKind of
    REG_SZ       : result := 'String (SZ)';
    REG_MULTI_SZ : result := 'Multi Line String (MSZ)';
    REG_DWORD    : result := 'DWORD';
    REG_QWORD    : result := 'QWORD';
    REG_BINARY   : result := 'Binary';
    else
      result := 'None';
  end;
end;

class procedure TRegistryHelper.ReadValue(const hOpenedKey : HKEY; const AValueName : String;
  out AValueType : DWORD; out pData : Pointer; out ADataSize : DWORD);
begin
  pData := nil;
  ADataSize := 0;
  ///

  var ARet := RegGetValueW(hOpenedKey, nil, PWideChar(AValueName), RRF_RT_ANY, AValueType, nil, ADataSize);
  if (ARet <> ERROR_SUCCESS) and (ARet <> ERROR_MORE_DATA) then
    Exit();
  ///

  GetMem(pData, ADataSize * SizeOf(WideChar));
  try
    ARet := RegGetValueW(
      hOpenedKey,
      nil,
      PWideChar(AValueName),
      RRF_RT_ANY,
      AValueType,
      pData,
      ADataSize
    );
    if ARet <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegGetValueW');
  except
    FreeMem(pData, ADataSize);
  end;
end;

class procedure TRegistryHelper.ReadValue(const AFullKeyPath : String; const AValueName : String;
  out AValueType : DWORD; out pData : Pointer; out ADataSize : DWORD);
begin
  var AKeyHandle := OpenRegistryKey(AFullKeyPath, KEY_QUERY_VALUE);
  try
    ReadValue(AKeyHandle, AValueName, AValueType, pData, ADataSize);
  finally
    RegCloseKey(AKeyHandle);
  end;
end;

class procedure TRegistryHelper.RenameKey(const hOpenedKey : HKEY; const ASubKeyName, ANewKeyName : String);
begin
  var ARet := RegRenameKey(hOpenedKey, PWideChar(ASubKeyName), PWideChar(ANewKeyName));
  if ARet <> ERROR_SUCCESS then
    raise EWindowsException.Create('RegRenameKey', ARet);
end;

class procedure TRegistryHelper.RenameKey(const AFullKeyPath, ASubKeyName, ANewKeyName : String);
begin
  var AKeyHandle := OpenRegistryKey(AFullKeyPath, KEY_WRITE);
  try
    RenameKey(AKeyHandle, ASubKeyName, ANewKeyName);
  finally
    RegCloseKey(AKeyHandle);
  end;
end;

class procedure TRegistryHelper.DeleteValue(const hOpenedKey : HKEY; const AValueName : String);
begin
  var ARet := RegDeleteValueW(hOpenedKey, PWideChar(AValueName));
  if ARet <> ERROR_SUCCESS then
    raise EWindowsException.Create('RegDeleteValueW');
end;

class procedure TRegistryHelper.DeleteValue(const AFullKeyPath : String; const AValueName : String);
begin
  var AKeyHandle := OpenRegistryKey(AFullKeyPath, KEY_SET_VALUE);
  try
    DeleteValue(AKeyHandle, AValueName);
  finally
    RegCloseKey(AKeyHandle);
  end;
end;

class procedure TRegistryHelper.RenameValue(const AFullKeyPath, AValueName, ANewValueName : String);
begin
  var pData := nil;
  var ADataSize : DWORD;
  ///
  
  var AKeyHandle := OpenRegistryKey(AFullKeyPath, KEY_QUERY_VALUE or KEY_SET_VALUE);
  try
    var AValueType : DWORD;        

    ReadValue(AKeyHandle, AValueName, AValueType, pData, ADataSize);
    
    SetValue(AKeyHandle, ANewValueName, AValueType, pData, ADataSize);

    ///
    DeleteValue(AKeyHandle, AValueName);
  finally
    if Assigned(pData) then
      FreeMem(pData, ADataSize);
    
    ///
    RegCloseKey(AKeyHandle);
  end;
end;

(* TRegistryKeyInformation *)

constructor TRegistryKeyInformation.Create(const AName : String; const APath : String = '');
begin
  inherited Create();
  ///

  FName     := AName;
  FACL_SSDL := TRegistryHelper.TryGetFileACLString(APath);

  TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(APath, FPermissions);
end;

procedure TRegistryKeyInformation.Assign(ASource : TPersistent);
begin
  if ASource is TRegistryKeyInformation then begin
    FName        := TRegistryKeyInformation(ASource).FName;
    FACL_SSDL    := TRegistryKeyInformation(ASource).FACL_SSDL;
    FPermissions := TRegistryKeyInformation(ASource).FPermissions;
  end else
    inherited;
end;

(* TRegistryValueInformation *)

constructor TRegistryValueInformation.Create(const AName : String; const AType : DWORD; const pData : Pointer;
  const ADataSize : DWORD);
begin
  inherited Create();
  ///

  FName  := AName;
  FType  := AType;

  FValue := TOptixMemoryObject.Create();
  FValue.CopyFrom(pData, ADataSize);
end;

destructor TRegistryValueInformation.Destroy();
begin
  if Assigned(FValue) then
    FreeAndNil(FValue);

  ///
  inherited;
end;

procedure TRegistryValueInformation.Assign(ASource : TPersistent);
begin
  if ASource is TRegistryValueInformation then begin
    FName := TRegistryValueInformation(ASource).FName;
    FType := TRegistryValueInformation(ASource).FType;

    if Assigned(TRegistryValueInformation(ASource).FValue) then begin
      FValue := TOptixMemoryObject.Create();
      FValue.CopyFrom(TRegistryValueInformation(ASource).FValue);
    end else
      FValue := nil;
  end else
    inherited;
end;

function TRegistryValueInformation.GetIsDefault() : Boolean;
begin
  result := FName.IsEmpty();
end;

function TRegistryValueInformation.ToString() : String;
begin
  if not Assigned(FValue) or not FValue.HasData then
    Exit('');
  ///

  case FType of
    REG_SZ, REG_EXPAND_SZ:
      result := TMemoryUtils.MemoryToString(FValue.Address, FValue.Size);

    REG_MULTI_SZ:
      result := TMemoryUtils.MemoryMultiStringToString(FValue.Address, FValue.Size);

    REG_DWORD:
      result := Format('0x%.8X (%d)', [PDWORD(FValue.Address)^, PDWORD(FValue.Address)^]);

    REG_QWORD:
      result := Format('0x%.16X (%u)', [PUInt64(FValue.Address)^, PUInt64(FValue.Address)^]);

    REG_BINARY: begin
      var AStringBuilder := TStringBuilder.Create(FValue.Size * 3 (* 1 Byte = 2 (Hex) + 1 (Space) *));
      try
        for var n := 0 to FValue.Size -1 do
          AStringBuilder.AppendFormat('%.2X ', [PByte(NativeUInt(FValue.Address) + n)^]);

        ///
        result := AStringBuilder.ToString.TrimRight;
      finally
        if Assigned(AStringBuilder) then
          FreeAndNil(AStringBuilder);
      end;
    end;

    else
      result := '<could not read>';
  end;
end;

(* TOptixEnumRegistry *)

class procedure TOptixEnumRegistry.Enum(const AKeyFullPath : String; var AKeys : TObjectList<TRegistryKeyInformation>;
  var AValues : TObjectList<TRegistryValueInformation>);
begin
  var hOpenedKey : HKEY;
  var ACloseKey := False;

  if not Assigned(AKeys) then
    AKeys := TObjectList<TRegistryKeyInformation>.Create(True)
  else
    AKeys.Clear();

  if not Assigned(AValues) then
    AValues := TObjectList<TRegistryValueInformation>.Create(True)
  else
    AValues.Clear();

  var AHive : HKEY;
  var AKeyPath := '';
  TRegistryHelper.ExtractKeyPathInformation(AKeyFullPath, AHive, AKeyPath);

  var ADefaultValueExists := False;

  if not String.IsNullOrWhiteSpace(AKeyPath) then begin
    var ARet := RegOpenKeyEx(
      AHive,
      PWideChar(AKeyPath),
      0,
      KEY_READ,
      hOpenedKey
    );

    if ARet <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegOpenKeyEx', ARet);

    ACloseKey := True;
  end else
    hOpenedKey := AHive;
  try
    var ASubKeysCount : DWORD;
    var AMaxKeyNameLength : DWORD;

    var AValuesCount : DWORD;
    var AMaxValueNameLength : DWORD;

    var ARet := RegQueryInfoKeyW(
        hOpenedKey,
        nil,
        nil,
        nil,
        @ASubKeysCount,
        @AMaxKeyNameLength,
        nil,
        @AValuesCount,
        @AMaxValueNameLength,
        nil,
        nil,
        nil
    );

    if ARet <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegQueryInfoKeyW', ARet);

    if (ASubKeysCount = 0) and (AValuesCount = 0) then
      Exit();

    // Include terminating NULL characters
    Inc(AMaxKeyNameLength);
    Inc(AMaxValueNameLength);

    var ABufferSize : DWORD;
    if AMaxKeyNameLength > AMaxValueNameLength then
      ABufferSize := AMaxKeyNameLength * SizeOf(WideChar)
    else
      ABufferSize := AMaxValueNameLength * SizeOf(WideChar);

    var pNameBuffer : PWideChar;
    GetMem(pNameBuffer, ABufferSize);
    try
      // Enumerate SubKeys
      var AKeyLength : DWORD;
      if ASubKeysCount > 0 then begin
        for var I := 0 to ASubKeysCount -1 do begin
          ZeroMemory(pNameBuffer, AMaxKeyNameLength * SizeOf(WideChar));

          AKeyLength := AMaxKeyNameLength;

          ARet := RegEnumKeyExW(hOpenedKey, I, pNameBuffer, AKeyLength, nil, nil, nil, nil);
          if ARet <> ERROR_SUCCESS then
            continue;

          var AKeyName := String(pNameBuffer);

          ///
          AKeys.Add(TRegistryKeyInformation.Create(AKeyName, IncludeTrailingPathDelimiter(AKeyFullPath) + AKeyName));
        end;
      end;

      if AValuesCount > 0 then begin
        var AValueNameLength : DWORD;
        var AValueName : String;
        for var  I := 0 to AValuesCount -1 do begin
          ZeroMemory(pNameBuffer, AMaxKeyNameLength * SizeOf(WideChar));

          AValueNameLength := AMaxValueNameLength;

          ARet := RegEnumValueW(hOpenedKey, I, pNameBuffer, AValueNameLength, nil, nil, nil, nil);
          if ARet <> ERROR_SUCCESS then
            continue;

          ///
          AValueName := String(pNameBuffer);
          if AValueName.IsEmpty then
            ADefaultValueExists := True;
          try
            var AValueKind    : DWORD;
            var ADataAsString : String;
            var pData         : Pointer;
            var ADataSize     : DWORD;

            TRegistryHelper.ReadValue(hOpenedKey, AValueName, AValueKind, pData, ADataSize);
            try
              AValues.Add(TRegistryValueInformation.Create(AValueName, AValueKind, pData, ADataSize));
            finally
              if Assigned(pData) then
                FreeMem(pData, ADataSize);
            end;
          except
          end;
        end;
      end;
    finally
      FreeMem(pNameBuffer, ABufferSize);
    end;
  finally
    if not ADefaultValueExists then
        AValues.Add(TRegistryValueInformation.Create('', REG_SZ, nil, 0));
    ///

    if ACloseKey then
      RegCloseKey(hOpenedKey);
  end;
end;

end.
