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

unit Optix.FileSystem.Helper;

interface

uses System.Classes;

type
  TDriveType = (
    dtUnknown,
    dtNoRootDir,
    dtRemovable,
    dtFixed,
    dtRemote,
    dtCDROM,
    dtRAMDisk
  );

  TFileAccess = (
    faRead,
    faWrite,
    faExecute
  );
  TFileAccessAttributes = set of TFileAccess;

  TFileSystemHelper = class
  public
    class function GetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean; static;
    class function TryGetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean; static;
    class function GetFileACLString(const AFileName : String) : String; static;
    class function TryGetFileACLString(const AFileName : String) : String; static;
    class procedure GetCurrentUserFileAccess(const AFileName : String; var ARead, AWrite, AExecute : Boolean); overload; static;
    class function GetCurrentUserFileAccess(const AFileName : String) : TFileAccessAttributes; overload; static;
    class procedure TryGetCurrentUserFileAccess(const AFileName : String; var ARead, AWrite, AExecute : Boolean); overload; static;
    class function TryGetCurrentUserFileAccess(const AFileName : String) : TFileAccessAttributes; overload; static;
    class function GetFileSize(const AFileName : String) : Int64; static;
    class function TryGetFileSize(const AFileName : String) : Int64; static;
    class function GetFileTypeDescription(const AFileName: String): String; static;
    class procedure GetFileTime(const AFileName : String; var ACreate, ALastModified, ALastAccess : TDateTime); static;
    class function TryGetFileTime(const AFileName : String; var ACreate, ALastModified, ALastAccess : TDateTime) : Boolean; static;
    class function UniqueFileName(const AFileName : String) : String; static;
  end;

  function DriveTypeToString(const AValue : TDriveType) : String;
  function FileAccessAttributesToString(const AValue : TFileAccessAttributes) : String;

implementation

uses System.SysUtils, Winapi.AccCtrl, Winapi.AclAPI, Winapi.Windows, Optix.Exceptions, Optix.WinApiEx, Winapi.ShellAPI,
     Optix.System.Helper, System.IOUtils;

(* Local *)

{ _.ElevatedStatusToString }
function DriveTypeToString(const AValue : TDriveType) : String;
begin
  result := 'Unknown';
  ///

  case AValue of
    dtUnknown   : result := 'Unknown';
    dtNoRootDir : result := 'No Root Dir';
    dtRemovable : result := 'Removable';
    dtFixed     : result := 'Fixed';
    dtRemote    : result := 'Network';
    dtCDROM     : result := 'CD-ROM';
    dtRAMDisk   : result := 'RAM Disk';
  end;
end;

{ _.FileAccessAttributesToString }
function FileAccessAttributesToString(const AValue : TFileAccessAttributes) : String;
begin
  result := '';
  ///

  if faRead in AValue then
    result := 'R';

  if faWrite in AValue then
    result := result + 'W';

  if faExecute in AValue then
    result := result + 'E';

  if String.IsNullOrEmpty(result) then
    result := 'No Access';
end;

(* TFileSystemHelper *)

{ TFileSystemHelper.GetDriveInformation }
class function TFileSystemHelper.GetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean;
begin
  var AOldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    ADriveLetter := IncludeTrailingPathDelimiter(ExtractFileDrive(ADriveLetter));
    ///

    var ADummy        : DWORD;
    var ABufferName   : array[0..MAX_PATH-1] of WideChar;
    var ABufferFormat : array[0..MAX_PATH-1] of WideChar;

    FillChar(ABufferName, MAX_PATH, #0);
    FillChar(ABufferFormat, MAX_PATH, #0);

    result := GetVolumeInformation(
                                    PWideChar(ADriveLetter),
                                    ABufferName,
                                    MAX_PATH,
                                    nil,
                                    ADummy,
                                    ADummy,
                                    ABufferFormat,
                                    MAX_PATH
    );

    {
      Conv to String
    }
    AName   := String(ABufferName);
    AFormat := String(ABufferFormat);

    {
      Get Drive Type
    }
    case GetDriveType(PWideChar(ADriveLetter)) of
      1 : ADriveType := dtNoRootDir; // DRIVE_NO_ROOT_DIR
      2 : ADriveType := dtRemovable; // DRIVE_REMOVABLE
      3 : ADriveType := dtFixed;     // DRIVE_FIXED
      4 : ADriveType := dtRemote;    // DRIVE_REMOTE
      5 : ADriveType := dtCDROM;     // DRIVE_CDROM
      6 : ADriveType := dtRAMDisk;   // DRIVE_RAMDISK
      else
        ADriveType := dtUnknown;
    end;
  finally
    SetErrorMode(AOldErrorMode);
  end;
end;

{ TFileSystemHelper.TryGetDriveInformation }
class function TFileSystemHelper.TryGetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String; var ADriveType : TDriveType) : Boolean;
begin
  AName      := '';
  AFormat    := '';
  ADriveType := dtUnknown;
  ///
  try
    result := GetDriveInformation(ADriveLetter, AName, AFormat, ADriveType);
  except
    result := False;
  end;
end;

{ TFileSystemHelper.GetFileACLString }
class function TFileSystemHelper.GetFileACLString(const AFileName : String) : String;
begin
  var ptrSecurityDescriptor : PSecurityDescriptor := nil;
  var pFileACL_SSDL : LPWSTR := nil;
  try
    var ASecurityInformation := OWNER_SECURITY_INFORMATION or 
                                GROUP_SECURITY_INFORMATION or 
                                DACL_SECURITY_INFORMATION;
    
    var AResult := GetNamedSecurityInfoW(
      PWideChar(AFileName),
      SE_FILE_OBJECT,
      ASecurityInformation,
      nil,
      nil,
      nil,
      nil,
      @ptrSecurityDescriptor
    );
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('GetNamedSecurityInfoW');

    if not ConvertSecurityDescriptorToStringSecurityDescriptorW(
      ptrSecurityDescriptor,
      SDDL_REVISION_1,
      ASecurityInformation,
      pFileACL_SSDL,
      nil
    ) then
      raise EWindowsException.Create('ConvertSecurityDescriptorToStringSecurityDescriptorW');

    ///
    result := string(pFileACL_SSDL);
  finally
    if Assigned(pFileACL_SSDL) then
      LocalFree(pFileACL_SSDL);

    if Assigned(ptrSecurityDescriptor) then
      LocalFree(ptrSecurityDescriptor);
  end;
end;

{ TFileSystemHelper.TryGetFileACLString }
class function TFileSystemHelper.TryGetFileACLString(const AFileName : String) : String;
begin
  try
    result := GetFileACLString(AFileName);
  except
    result := '';
  end;
end;

{ TFileSystemHelper.GetCurrentUserFileAccess }
class procedure TFileSystemHelper.GetCurrentUserFileAccess(const AFileName : String; var ARead, AWrite, AExecute : Boolean);
var AMapping              : TGenericMapping;
    ptrSecurityDescriptor : PSecurityDescriptor;
    hToken                : THandle;

  function AccessCheck(ADesiredAccess : DWORD) : Boolean;
  begin
    MapGenericMask(ADesiredAccess, AMapping);

    var APrivilegeSet : TPrivilegeSet;
    var APrivilegeSetSize := DWORD(SizeOf(TPrivilegeSet));

    var AGrantedAccess := DWORD(0);
    var AStatus : BOOL;

    if not Winapi.Windows.AccessCheck(
      ptrSecurityDescriptor,
      hToken,
      ADesiredAccess,
      AMapping,
      APrivilegeSet,
      APrivilegeSetSize,
      AGrantedAccess,
      AStatus
    ) then 
      result := False
    else
      result := AStatus;
  end;

begin
  var AImpersonated := False;
  
  ptrSecurityDescriptor := nil;  
  hToken := 0;
  ///

  AMapping.GenericRead    := FILE_GENERIC_READ;
  AMapping.GenericWrite   := FILE_GENERIC_WRITE;
  AMapping.GenericExecute := FILE_GENERIC_EXECUTE;
  AMapping.GenericAll     := FILE_ALL_ACCESS;
  try
    AImpersonated := ImpersonateSelf(SecurityImpersonation);
    
    if not OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, False, hToken) then
      raise EWindowsException.Create('OpenProcessToken');
    ///

    var AResult := GetNamedSecurityInfoW(
      PWideChar(AFileName),
      SE_FILE_OBJECT,
      (
        OWNER_SECURITY_INFORMATION or 
        GROUP_SECURITY_INFORMATION or 
        DACL_SECURITY_INFORMATION
      ),
      nil,
      nil,
      nil,
      nil,
      @ptrSecurityDescriptor
    );
    if AResult <> ERROR_SUCCESS then
      raise EWindowsException.Create('GetNamedSecurityInfoW');

    ///
    if AccessCheck(FILE_ALL_ACCESS) then begin
      ARead    := True;
      AWrite   := True;
      AExecute := True;
    end else begin
      ARead    := AccessCheck(FILE_GENERIC_READ);
      AWrite   := AccessCheck(FILE_GENERIC_WRITE);
      AExecute := AccessCheck(FILE_GENERIC_EXECUTE);
    end;
  finally
    if Assigned(ptrSecurityDescriptor) then
      LocalFree(ptrSecurityDescriptor);

    if hToken <> 0 then
      CloseHandle(hToken);

    if AImpersonated then
      RevertToSelf();
  end;
end;

{ TFileSystemHelper.GetCurrentUserFileAccess }
class function TFileSystemHelper.GetCurrentUserFileAccess(const AFileName : String) : TFileAccessAttributes;
begin
  var ARead, AWrite, AExecute : Boolean;

  result := [];

  GetCurrentUserFileAccess(AFileName, ARead, AWrite, AExecute);

  if ARead then
    Include(result, faRead);

  if AWrite then
    Include(result, faWrite);

  if AExecute then
    Include(result, faExecute);
end;

{ TFileSystemHelper.TryGetCurrentUserFileAccess }
class procedure TFileSystemHelper.TryGetCurrentUserFileAccess(const AFileName : String; var ARead, AWrite, AExecute : Boolean);
begin
  try
    GetCurrentUserFileAccess(AFileName, ARead, AWrite, AExecute);
  except
    ARead    := False;
    AWrite   := False;
    AExecute := False;
  end;
end;

{ TFileSystemHelper.TryGetCurrentUserFileAccess }
class function TFileSystemHelper.TryGetCurrentUserFileAccess(const AFileName : String) : TFileAccessAttributes;
begin
  try
    result := GetCurrentUserFileAccess(AFileName);
  except
    result := [];
  end;
end;

{ TFileSystemHelper.GetFileSize }
class function TFileSystemHelper.GetFileSize(const AFileName : String) : Int64;
begin
  var AFileInfo : TWin32FileAttributeData;

  if NOT GetFileAttributesEx(PWideChar(AFileName), GetFileExInfoStandard, @AFileInfo) then
    raise EWindowsException.Create('GetFileAttributesEx');

  ///
  result := Int64(AFileInfo.nFileSizeLow) or Int64(AFileInfo.nFileSizeHigh shl 32);
end;

{ TFileSystemHelper.GetFileSize }
class function TFileSystemHelper.TryGetFileSize(const AFileName : String) : Int64; 
begin
  try
    result := GetFileSize(AFileName);
  except
    result := 0;
  end;
end;

{ TFileSystemHelper.GetFileTypeDescription }
class function TFileSystemHelper.GetFileTypeDescription(const AFileName: String): String;
begin
  var AShFileInfo: TSHFileInfoW;
  
  ZeroMemory(@AShFileInfo, SizeOf(TSHFileInfoW));
  
  if SHGetFileInfoW(
    PWideChar(AFileName),
    0,
    AShFileInfo,
    SizeOf(TSHFileInfoW),
    SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES
  ) <> 0 then
    result := AShFileInfo.szTypeName
  else
    result := '';
end;

{ TFileSystem.Helper.GetFileTime }
class procedure TFileSystemHelper.GetFileTime(const AFileName : String; var ACreate, ALastModified, ALastAccess : TDateTime);
begin
  var hFile := CreateFileW(
    PWideChar(AFileName),
    GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS,
    0
  );
  if hFile = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('GetFileTime');
  try
    var AFtCreate, AFtLastAccess, AFtLastModified : TFileTime;
    
    if not Winapi.Windows.GetFileTime(hFile, @AFtCreate, @AFtLastAccess, @AFtLastModified) then
      raise EWindowsException.Create('GetFileTime');

    ACreate       := TSystemHelper.TryFileTimeToDateTime(AFtCreate);
    ALastModified := TSystemHelper.TryFileTimeToDateTime(AFtLastModified);
    ALastAccess   := TSystemHelper.TryFileTimeToDateTime(AFtLastAccess);
  finally
    CloseHandle(hFile);
  end;
end;

{ TFileSystem.Helper.TryGetFileTime }
class function TFileSystemHelper.TryGetFileTime(const AFileName : String; var ACreate, ALastModified, ALastAccess : TDateTime) : Boolean;
begin
  try
    GetFileTime(AFileName, ACreate, ALastModified, ALastAccess);

    ///
    result := True;
  except
    result := False;
  end;
end;

{ TFileSystemHelper.UniqueFileName }
class function TFileSystemHelper.UniqueFileName(const AFileName : String) : String;
begin
  if not FileExists(AFileName) then
    Exit(AFileName);

  var i := 1;
  repeat
    result := Format('%s%s(%d)%s', [
      IncludeTrailingPathDelimiter(ExtractFilePath(AFileName)),
      TPath.GetFileNameWithoutExtension(AFileName),
      i,
      TPath.GetExtension(AFileName)
    ]);

    ///
    Inc(i);
  until (NOT FileExists(result));
end;

end.
