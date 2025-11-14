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

unit Optix.FileSystem.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes;
// ---------------------------------------------------------------------------------------------------------------------

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
  type
    TTraversedDirectoryCallback = reference to procedure(const ADirectoryName : String; const AAbsolutePath : String);
  public
    class function GetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String;
      var ADriveType : TDriveType) : Boolean; static;
    class function TryGetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String;
      var ADriveType : TDriveType) : Boolean; static;
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
    class function ExpandPath(const APath : String) : String; static;
    class procedure TraverseDirectories(const APath : String;
      const ATraversedDirectoryFunc : TTraversedDirectoryCallback); static;
    class function GetFullPathName(const APath : String) : String; static;
    class procedure PathExists(const APath : String);
  end;

  TContentReader = class
  private
    FPageSize     : UInt64;
    FFileHandle   : THandle;
    FFileSize     : UInt64;
    FFilePath     : String;

    {@M}
    function GetPageCount() : UInt64;
    procedure SetPageSize(AValue : UInt64);
  public
    const
      MIN_PAGE_SIZE = 128;
      MAX_PAGE_SIZE = 409600;
  public
    {@C}
    constructor Create(const AFilePath : String; const APageSize : UInt64);
    destructor Destroy(); override;

    {@M}
    procedure ReadPage(APageNumber : UInt64; var pBuffer : Pointer; var ABufferSize : UInt64);

    {@G}
    property FileSize  : UInt64  read FFileSize;
    property PageCount : UInt64  read GetPageCount;
    property FilePath  : String  read FFilePath;

    {@S}
    property PageSize : UInt64 read FPageSize write SetPageSize;
  end;

  function DriveTypeToString(const AValue : TDriveType) : String;
  function AccessSetToString(const AValue : TFileAccessAttributes) : String;
  function StringToAccessSet(const AValue : String) : TFileAccessAttributes;
  function AccessSetToReadableString(const AValue : TFileAccessAttributes) : String;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.IOUtils, System.StrUtils, System.Math,

  Winapi.AccCtrl, Winapi.AclAPI, Winapi.Windows, Winapi.ShellAPI,

  Optix.Exceptions, Optix.WinApiEx, Optix.System.Helper;
// ---------------------------------------------------------------------------------------------------------------------

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

{ _.AccessSetToString }
function AccessSetToString(const AValue : TFileAccessAttributes) : String;
begin
  SetLength(result, 3);
  ///

  result[1] := IfThen(faRead in AValue, 'R', '_')[1];
  result[2] := IfThen(faWrite in AValue, 'W', '_')[1];
  result[3] := IfThen(faExecute in AValue, 'E', '_')[1];
end;

{ _.StringToAccessSet }
function StringToAccessSet(const AValue : String) : TFileAccessAttributes;
begin
  result := [];
  ///

  if Length(AValue) <> 3 then
    Exit();

  if Copy(AValue, 1, 1) = 'R' then
    Include(result, faRead);

  if Copy(AValue, 2, 1) = 'W' then
    Include(result, faWrite);

  if Copy(AValue, 3, 1) = 'E' then
    Include(result, faExecute);
end;

{ _.FileAccessAttributesToString }
function AccessSetToReadableString(const AValue : TFileAccessAttributes) : String;
begin
  if AValue <> [] then
    result := AccessSetToString(AValue)
  else
    result := 'No Access';
end;

(* TFileSystemHelper *)

{ TFileSystemHelper.GetDriveInformation }
class function TFileSystemHelper.GetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String;
 var ADriveType : TDriveType) : Boolean;
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
class function TFileSystemHelper.TryGetDriveInformation(ADriveLetter : String; var AName : String; var AFormat : String;
 var ADriveType : TDriveType) : Boolean;
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
      raise EWindowsException.Create('GetNamedSecurityInfoW', AResult);

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
class procedure TFileSystemHelper.GetCurrentUserFileAccess(const AFileName : String;
 var ARead, AWrite, AExecute : Boolean);
begin
  var AImpersonated := False;
  
  var ptrSecurityDescriptor := PSecurityDescriptor(nil);
  var hToken := THandle(0);
  ///

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
      raise EWindowsException.Create('GetNamedSecurityInfoW', AResult);

    ///
    if TSystemHelper.AccessCheck(FILE_ALL_ACCESS, hToken, ptrSecurityDescriptor) then begin
      ARead    := True;
      AWrite   := True;
      AExecute := True;
    end else begin
      ARead    := TSystemHelper.AccessCheck(FILE_GENERIC_READ, hToken, ptrSecurityDescriptor);
      AWrite   := TSystemHelper.AccessCheck(FILE_GENERIC_WRITE, hToken, ptrSecurityDescriptor);
      AExecute := TSystemHelper.AccessCheck(FILE_GENERIC_EXECUTE, hToken, ptrSecurityDescriptor);
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
class procedure TFileSystemHelper.TryGetCurrentUserFileAccess(const AFileName : String;
 var ARead, AWrite, AExecute : Boolean);
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
class procedure TFileSystemHelper.GetFileTime(const AFileName : String;
 var ACreate, ALastModified, ALastAccess : TDateTime);
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
class function TFileSystemHelper.TryGetFileTime(const AFileName : String;
 var ACreate, ALastModified, ALastAccess : TDateTime) : Boolean;
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

{ TFileSystemHelper.ExpandPath }
class function TFileSystemHelper.ExpandPath(const APath : String) : String;
begin
  var APathLength := ExpandEnvironmentStrings(PWideChar(APath), nil, 0);
  if APathLength = 0 then
    Exit(APath);
  ///

  SetLength(Result, APathLength - 1);

  if ExpandEnvironmentStrings(PWideChar(APath), PWideChar(result), APathLength) = 0 then
    result := APath;

  ///
  result := IncludeTrailingPathDelimiter(result);
end;

{ TFileSystemHelper.TraverseDirectories }
class procedure TFileSystemHelper.TraverseDirectories(const APath : String;
  const ATraversedDirectoryFunc : TTraversedDirectoryCallback);
begin
  var ADirectories := APath.Split(['\'], TStringSplitOptions.ExcludeEmpty);
  ///

  var ACurrentPath := '';
  for var ADirectory in ADirectories do begin
    ACurrentPath := TSystemHelper.IncludeTrailingPathDelimiterIfNotEmpty(ACurrentPath) + ADirectory;

    ///
    ATraversedDirectoryFunc(ADirectory, ACurrentPath);
  end;
end;

{ TFileSystemHelper.GetFullPathName }
class function TFileSystemHelper.GetFullPathName(const APath : String) : String;
begin
  result := '';
  ///

  var pDummy : PWideChar;

  var ARequiredLength := Winapi.Windows.GetFullPathNameW(PWideChar(APath), 0, nil, pDummy);
  if ARequiredLength = 0 then
    raise EWindowsException.Create('GetFullPathNameW(0)');
  ///

  Inc(ARequiredLength);

  var pBuffer : PWideChar;
  GetMem(pBuffer, ARequiredLength * SizeOf(WideChar));
  try
    if Winapi.Windows.GetFullPathNameW(PWideChar(APath), ARequiredLength, pBuffer, pDummy) = 0 then
      raise EWindowsException.Create('GetFullPathNameW(1)');

    ///
    result := String(pBuffer);
  finally
    FreeMem(pBuffer, ARequiredLength * SizeOf(WideChar));
  end;
end;

{ TFileSystemHelper.PathExists }
class procedure TFileSystemHelper.PathExists(const APath : String);
begin
  if GetFileAttributesW(PWideChar(APath)) = INVALID_FILE_ATTRIBUTES then
    raise EWindowsException.Create('GetFileAttributesW');
end;

(* TContentReader *)

{ TContentReader.Create }
constructor TContentReader.Create(const AFilePath : String; const APageSize : UInt64);
begin
  inherited Create();
  ///

  FFileSize := 0;
  FFilePath := AFilePath;

  FFileHandle := CreateFileW(
    PWideChar(FFilePath),
    GENERIC_READ,
    FILE_SHARE_READ,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('CreateFileW');
  ///

  FFileSize := TFileSystemHelper.GetFileSize(FFilePath);
  SetPageSize(APageSize);
end;

{ TContentReader.Destroy }
destructor TContentReader.Destroy();
begin
  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);

  ///
  inherited Destroy();
end;

{ TContentReader.GetPageCount }
function TContentReader.GetPageCount() : UInt64;
begin
  result := 0;
  ///

  if FFileHandle = INVALID_HANDLE_VALUE then
    Exit();

  ///
  result := ceil(FFileSize / FPageSize);
end;

{ TContentReader.SetPageSize }
procedure TContentReader.SetPageSize(AValue : UInt64);
begin
  if AValue < MIN_PAGE_SIZE then
    AValue := MIN_PAGE_SIZE
  else if AValue > MAX_PAGE_SIZE then
    AValue := MAX_PAGE_SIZE;

  if (FFileSize > 0) and (AValue > FFileSize) then
    AValue := FFileSize;

  ///
  FPageSize := AValue;
end;

{ TContentReader.ReadPage }
procedure TContentReader.ReadPage(APageNumber : UInt64; var pBuffer : Pointer; var ABufferSize : UInt64);
begin
  pBuffer := nil;
  ABufferSize := 0;
  ///

  var APageCount := GetPageCount();
  ///

  if APageNumber > APageCount  then
    APageNumber := APageCount;

  var AOffset := UInt64(FPageSize * APageNumber);
  var AHighOffset := DWORD((AOffset shr 32) and $FFFFFFFF);

  if SetFilePointer(FFileHandle, DWORD(AOffset and $FFFFFFFF), @AHighOffset, FILE_BEGIN) = INVALID_SET_FILE_POINTER then
    raise EWindowsException.Create('SetFilePointer');

  GetMem(pBuffer, FPageSize);

  var ABytesRead : DWORD;
  if not ReadFile(FFileHandle, PByte(pBuffer)^, FPageSize, ABytesRead, nil) then
    raise EWindowsException.Create('ReadFile');

  ABufferSize := ABytesRead;
  if ABufferSize < FPageSize then
    ReallocMem(pBuffer, ABufferSize);
end;

end.
