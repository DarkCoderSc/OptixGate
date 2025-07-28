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

unit Optix.InformationGathering.Helper;

interface

uses Winapi.Windows, System.Classes, System.Hash, Optix.WinApiEx;

type
  TOptixInformationGathering = class
  public
    class function ComputerName() : string; static;
    class function TryGetComputerName() : string; static;
    class function UserName() : string; static;
    class function TryGetUserName() : string; static;
    class function CurrentProcessArchitecture() : TProcessorArchitecture; static;
    class function GetUserSidByType(const AUserName : String; ASidType : TSidNameUse = SidTypeUser) : String; static;
    class function GetWindowsDirectory() : string; static;
    class function GetHardDriveSerial() : String; static;
    class function GetUserUID() : TGUID; static;
    class function GetCurrentUserSid() : String; static;
    class function TryGetCurrentUserSid() : String; static;
    class function GetLangroup() : String; static;
    class function GetDomainName() : String; static;
    class function IsCurrentUserInAdminGroup() : Boolean; static;
    class function TryIsCurrentUserInAdminGroup() : Boolean; static;
    class function GetWindowsArchitecture() : TProcessorArchitecture; static;
  end;

  function ProcessArchitectureToString(const AValue : TProcessorArchitecture) : String;

implementation

uses Optix.Exceptions, System.SysUtils, Optix.Process.Helper;

(* Local *)

{ _.ProcessArchitectureToString }
function ProcessArchitectureToString(const AValue : TProcessorArchitecture) : String;
begin
  result := '';
  ///

  case AValue of
    pa86_32 : result := 'x32';
    pa86_64 : result := 'x64';
  end;
end;

(* TOptixInformationGathering *)

{ TOptixInformationGathering.CurrentProcessArchitecture }
class function TOptixInformationGathering.CurrentProcessArchitecture() : TProcessorArchitecture;
begin
  {$IFDEF WIN32}
    result := pa86_32;
  {$ELSE IF WIN64}
    result := pa86_64;
  {$ELSE}
    result := paUnknown;
  {$ENDIF}
end;

{ TOptixInformationGathering.UserName }
class function TOptixInformationGathering.UserName() : string;
var ABufferLen : DWORD;
    ABuffer    : array[0..255 -1] of WideChar;
begin
  result := '';
  ///

  ABufferLen := Length(ABuffer);

  if GetUserName(ABuffer, ABufferLen) then
    SetString(result, ABuffer, ABufferLen)
  else
    raise EWindowsException.Create('GetUserName');
end;

{ TOptixInformationGathering.TryUserName}
class function TOptixInformationGathering.TryGetUserName() : String;
begin
  result := '';
  try
    result := UserName();
  except
    on E : EWindowsException do begin
      // Ignore, we just try but we can log
    end;
  end;
end;

{ TOptixInformationGathering.ComputerName }
class function TOptixInformationGathering.ComputerName() : string;
var ABufferLen : DWORD;
    ABuffer    : array[0..255 -1] of WideChar;
begin
  result := '';
  ///

  ABufferLen := Length(ABuffer);

  if GetComputerName(ABuffer, ABufferLen) then
    SetString(result, ABuffer, ABufferLen)
  else
    raise EWindowsException.Create('GetComputerName');
end;

{ TOptixInformationGathering.TryGetComputerName }
class function TOptixInformationGathering.TryGetComputerName() : String;
begin
  result := '';
  try
    result := ComputerName();
  except
    on E : EWindowsException do begin
      // Ignore, we just try but we can log
    end;
  end;
end;

{ TOptixInformationGathering.GetUserSidByType }
class function TOptixInformationGathering.GetUserSidByType(const AUserName : String; ASidType : TSidNameUse = SidTypeUser) : String;
var ptrSID         : PSID;
    ASidSize       : Cardinal;
    ARefDomainSize : Cardinal;
    ASidNameUse    : SID_NAME_USE;
    ARefDomain     : String;
    ARet           : Boolean;
    ASid           : PWideChar;
begin
  result := '';
  ///

  ASidSize       := 0;
  ARefDomainSize := 0;

  ASidNameUse := Cardinal(ASidType);

  LookupAccountNameW(nil, PWideChar(AUserName), nil, ASidSize, nil, ARefDomainSize, ASidNameUse);

  GetMem(ptrSID, ASidSize);
  try
    SetLength(ARefDomain, ARefDomainSize);

    ARet := LookupAccountNameW(
                                  nil,
                                  PWideChar(AUserName),
                                  ptrSID,
                                  ASidSize,
                                  PWideChar(ARefDomain),
                                  ARefDomainSize,
                                  ASidNameUse
    );

    if ARet then begin
      ConvertSidToStringSidW(ptrSID, ASid);
      try
        result := String(ASid);
      finally
        LocalFree(ASid);
      end;
    end;
  finally
    FreeMem(ptrSID, ASidSize);
  end;
end;

{ TOptixInformationGathering.GetCurrentUserSid() }
class function TOptixInformationGathering.GetCurrentUserSid() : String;
begin
  result := GetUserSidByType(TOptixInformationGathering.UserName);
end;

{ TOptixInformationGathering.TryGetCurrentUserSid }
class function TOptixInformationGathering.TryGetCurrentUserSid() : String;
begin
  try
    result := TOptixInformationGathering.GetCurrentUserSid();
  except

  end;
end;

{ TOptixInformationGathering.GetWindowsDirectory }
class function TOptixInformationGathering.GetWindowsDirectory() : string;
var ALen  : Cardinal;
begin
  SetLength(result, MAX_PATH);

  ALen := WinAPI.Windows.GetWindowsDirectory(@result[1], MAX_PATH);

  SetLength(result, ALen);
  if ALen > MAX_PATH then
    WinAPI.Windows.GetWindowsDirectory(@result[1], ALen);

  ///
  result := IncludeTrailingPathDelimiter(result);
end;

{ TOptixInformationGathering.GetHardDriveSerial }
class function TOptixInformationGathering.GetHardDriveSerial() : String;
begin
  result := '';
  ///

  var hFile := CreateFileW('\\.\PhysicalDrive0', 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if hFile = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('CreateFileW');
  try
    var AStoragePropertyQuery : TStoragePropertyQuery;
    ZeroMemory(@AStoragePropertyQuery, SizeOf(TStoragePropertyQuery));

    AStoragePropertyQuery.PropertyId := StorageDeviceProperty;
    AStoragePropertyQuery.QueryType := PropertyStandardQuery;

    var AStorageDescriptorHeader : TStorageDescriptorHeader;
    var ABytesReturned : DWORD;

    if not DeviceIoControl(
      hFile,
      IOCTL_STORAGE_QUERY_PROPERTY,
      @AStoragePropertyQuery,
      SizeOf(TStoragePropertyQuery),
      @AStorageDescriptorHeader,
      SizeOf(TStorageDescriptorHeader),
      ABytesReturned,
      nil
    ) then
      raise EWindowsException.Create('DeviceIoControl(1)');

    var pBuffer : Pointer;
    GetMem(pBuffer, AStorageDescriptorHeader.Size);
    try
      if not DeviceIoControl(
        hFile,
        IOCTL_STORAGE_QUERY_PROPERTY,
        @AStoragePropertyQuery,
        SizeOf(TStoragePropertyQuery),
        pBuffer,
        AStorageDescriptorHeader.Size,
        ABytesReturned,
        nil
      ) then
        raise EWindowsException.Create('DeviceIoControl(2)');

     ///
     result := String(PAnsiChar(NativeUInt(pBuffer) + PStorageDeviceDescriptor(pBuffer)^.SerialNumberOffset));
    finally
      FreeMem(pBuffer, AStorageDescriptorHeader.Size);
    end;
  finally
    CloseHandle(hFile);
  end;
end;

{ TOptixInformationGathering.GetUserUID }
class function TOptixInformationGathering.GetUserUID() : TGUID;
begin
  var A128BitHash := THashMD5.GetHashBytes(
    GetHardDriveSerial +                                  // Uniqueness in machine level
    GetCurrentUserSid +                                   // Uniqueness in user level
    IntToStr(Cardinal(TProcessHelper.IsElevated()))       // Uniqueness in elevation level
  );

  Move(A128BitHash[0], result, SizeOf(TGUID));
end;

{ TOptixInformationGathering.GetLangroup }
class function TOptixInformationGathering.GetLangroup() : String;
begin
  result := '';
  ///

  var pWkstaInfo : PWkstaInfo100;

  if NetWkstaGetInfo(nil, 100, Pointer(pWkstaInfo)) = NERR_Success then begin
    result := string(pWkstaInfo.wki100_langroup);

    ///
    NetApiBufferFree(pWkstaInfo);
  end;
end;

{ TOptixInformationGathering.GetDomainName }
class function TOptixInformationGathering.GetDomainName() : String;
begin
  result := '';
  ///

  var pDcInfo : PDomainControllerInfo;

  if DsGetDcNameW(nil, nil, nil, nil, DS_DIRECTORY_SERVICE_REQUIRED, pDcInfo) = ERROR_SUCCESS then begin
    result := pDcInfo^.DomainName;

    ///
    NetApiBufferFree(pDcInfo);
  end;
end;

{ TOptixInformationGathering.IsCurrentUserInAdminGroup }
class function TOptixInformationGathering.IsCurrentUserInAdminGroup() : Boolean;
begin
  result := False;
  ///

  var hToken : THandle := 0;
  var pAdminSID : PSID := nil;
  try
    if not OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, hToken) then
      raise EWindowsException.Create('OpenProcessToken');

    var AReturnLength : DWORD;

    GetTokenInformation(hToken, TokenGroups, nil, 0, AReturnLength);
    if GetLastError() <> ERROR_INSUFFICIENT_BUFFER then
      raise EWindowsException.Create('GetTokenInformation(1)');

    var pTokenInfo : PTokenGroups;
    GetMem(pTokenInfo, AReturnLength);
    try
      if not GetTokenInformation(hToken, TokenGroups, pTokenInfo, AReturnLength, AReturnLength) then
        raise EWindowsException.Create('GetTokenInformation(2)');

        if not AllocateAndInitializeSid(
            SECURITY_NT_AUTHORITY,
            2,
            SECURITY_BUILTIN_DOMAIN_RID,
            DOMAIN_ALIAS_RID_ADMINS,
            0,
            0,
            0,
            0,
            0,
            0,
            pAdminSID
      ) then
        raise EWindowsException.Create('AllocateAndInitializeSid');

      {$R-}
      for var I := 0 to pTokenInfo^.GroupCount -1 do begin
        if EqualSID(pAdminSID, pTokenInfo^.Groups[I].Sid) then begin
          result := True;

          break
        end;
      end;
      {$R+}
    finally
      FreeMem(pTokenInfo, AReturnLength);
    end;
  finally
    if Assigned(pAdminSID) then
      FreeSid(pAdminSID);

    if hToken <> 0 then
      CloseHandle(hToken);
  end;
end;

{ TOptixInformationGathering.TryIsCurrentUserInAdminGroup }
class function TOptixInformationGathering.TryIsCurrentUserInAdminGroup() : Boolean;
begin
  result := False;
  try
    result := IsCurrentUserInAdminGroup();
  except

  end;
end;

{ TOptixInformationGathering.GetWindowsArchitecture }
class function TOptixInformationGathering.GetWindowsArchitecture() : TProcessorArchitecture;
begin
  var ASystemInfo : TSystemInfo;
  GetNativeSystemInfo(ASystemInfo);

  case ASystemInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL : result := pa86_32;
    PROCESSOR_ARCHITECTURE_AMD64 : result := pa86_64;
    PROCESSOR_ARCHITECTURE_ARM   : result := paARM;
    PROCESSOR_ARCHITECTURE_ARM64 : result := paARM64;
    else
      result := paUnknown;
  end;
end;

end.
