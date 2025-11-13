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

unit Optix.FileSystem.Enum;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  XSuperObject,

  Optix.Protocol.Packet, Optix.Shared.Classes, Optix.FileSystem.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  // Folder Information (Simplified) -----------------------------------------------------------------------------------
  TSimpleFolderInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FPath : String;

    [OptixSerializableAttribute]
    FAccess : TFileAccessAttributes;

    [OptixSerializableAttribute]
    FIsRoot : Boolean;
  public
    {@C}
    constructor Create(const AName, APath : String; const AAccess : TFileAccessAttributes;
      const AIsRoot : Boolean); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Name   : String                read FName;
    property Path   : String                read FPath;
    property Access : TFileAccessAttributes read FAccess;
    property IsRoot : Boolean               read FIsRoot;
  end;

  // Drives ------------------------------------------------------------------------------------------------------------
  TDriveInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FLetter : String;

    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FFormat : String;

    [OptixSerializableAttribute]
    FType : TDriveType;

    [OptixSerializableAttribute]
    FTotalSize : Int64;

    [OptixSerializableAttribute]
    FFreeSize : Int64;

    {@G}
    function GetUsedPercentage() : Byte;
    function GetUsedSize() : Int64;
  public
    {@C}
    constructor Create(const ADrive : String; const AIndex : Integer); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Letter         : String     read FLetter;
    property Name           : String     read FName;
    property Format         : String     read FFormat;
    property DriveType      : TDriveType read FType;
    property TotalSize      : Int64      read FTotalSize;
    property FreeSize       : Int64      read FFreeSize;
    property UsedSize       : Int64      read GetUsedSize;
    property UsedPercentage : Byte       read GetUsedPercentage;
  end;

  TOptixEnumDrives = class
  public
    class procedure Enum(var AList : TObjectList<TDriveInformation>); static;
  end;

  // Files -------------------------------------------------------------------------------------------------------------
  TFileInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FPath : String;

    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FIsDirectory : Boolean;

    [OptixSerializableAttribute]
    FACL_SSDL : String;

    [OptixSerializableAttribute]
    FAccess : TFileAccessAttributes;

    [OptixSerializableAttribute]
    FTypeDescription : String;

    [OptixSerializableAttribute]
    FSize : Int64;

    [OptixSerializableAttribute]
    FDateAreValid : Boolean;

    [OptixSerializableAttribute]
    FCreatedDate : TDateTime;

    [OptixSerializableAttribute]
    FLastModifiedDate : TDateTime;

    [OptixSerializableAttribute]
    FLastAccessDate : TDateTime;
  public
    {@C}
    constructor Create(const APath : String; const AIsDirectory : Boolean); overload;

    {@M}
    function GetFileTypeDescription() : String;
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Path             : String                read FPath;
    property Name             : String                read FName;
    property IsDirectory      : Boolean               read FIsDirectory;
    property ACL_SSDL         : String                read FACL_SSDL;
    property Access           : TFileAccessAttributes read FAccess;
    property TypeDescription  : String                read GetFileTypeDescription;
    property Size             : Int64                 read FSize;
    property DateAreValid     : Boolean               read FDateAreValid;
    property CreatedDate      : TDateTime             read FCreatedDate;
    property LastModifiedDate : TDateTime             read FLastModifiedDate;
    property LastAccessDate   : TDateTime             read FLastAccessDate;
  end;

  TOptixEnumFiles = class
  public
    class procedure Enum(const APath : String; var AList : TObjectList<TFileInformation>; var AIsRoot : Boolean;
      var AAccess : TFileAccessAttributes); static;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows,

  Optix.Exceptions;
// ---------------------------------------------------------------------------------------------------------------------

(***********************************************************************************************************************

  TSimpleFolderInformation

***********************************************************************************************************************)

{ TSimpleFolderInformation.Create }
constructor TSimpleFolderInformation.Create(const AName, APath : String; const AAccess : TFileAccessAttributes;
  const AIsRoot : Boolean);
begin
  inherited Create();
  ///

  FName   := AName;
  FPath   := IncludeTrailingPathDelimiter(APath);
  FAccess := AAccess;
  FIsRoot := AIsRoot;
end;

{ TSimpleFolderInformation.Assign }
procedure TSimpleFolderInformation.Assign(ASource : TPersistent);
begin
  if ASource is TSimpleFolderInformation then begin
    FName   := TSimpleFolderInformation(ASource).FName;
    FPath   := TSimpleFolderInformation(ASource).FPath;
    FAccess := TSimpleFolderInformation(ASource).FAccess;
    FIsRoot := TSimpleFolderInformation(ASource).FIsRoot;
  end else
    inherited;
end;

(***********************************************************************************************************************

  TDriveInformation

***********************************************************************************************************************)

{ TDriveInformation.Assign }
procedure TDriveInformation.Assign(ASource : TPersistent);
begin
  if ASource is TDriveInformation then begin
    FLetter    := TDriveInformation(ASource).FLetter;
    FName      := TDriveInformation(ASource).FName;
    FFormat    := TDriveInformation(ASource).FFormat;
    FType      := TDriveInformation(ASource).FType;
    FTotalSize := TDriveInformation(ASource).FTotalSize;
    FFreeSize  := TDriveInformation(ASource).FFreeSize;
  end else
    inherited;
end;

{ TDriveInformation.Create }
constructor TDriveInformation.Create(const ADrive : String; const AIndex : Integer);
begin
  FLetter := ADrive;

  TFileSystemHelper.TryGetDriveInformation(FLetter, FName, FFormat, FType);

  try
    FTotalSize := DiskSize(AIndex);
    FFreeSize  := DiskFree(AIndex);
  except
    FTotalSize := 0;
    FFreeSize  := 0;
  end;
end;

{ TDriveInformation.UsedPercentage }
function TDriveInformation.GetUsedPercentage() : Byte;
begin
  if (FTotalSize <= 0) or (FFreeSize <= 0) then
    Exit(0);
  ///

  result := (GetUsedSize() * 100) div FTotalSize;
end;

{ TDriveInformation.UsedSize }
function TDriveInformation.GetUsedSize() : Int64;
begin
  if FTotalSize <= 0 then
    Exit(0);
  ///

  result := FTotalSize - FFreeSize;
end;

(***********************************************************************************************************************

  TOptixEnumDrives

***********************************************************************************************************************)

{ TOptixEnumDrives.Enum }
class procedure TOptixEnumDrives.Enum(var AList : TObjectList<TDriveInformation>);
begin
  if not Assigned(AList) then
    AList := TObjectList<TDriveInformation>.Create(True)
  else
    AList.Clear();
  ///

  {$I-}
  var ALogicalDrives := GetLogicalDrives();

  var AIndex := 0;
  for var ALetter : Char in ['a'..'z'] do begin
    if (ALogicalDrives and (1 shl AIndex)) = 0 then begin
      Inc(AIndex);

      continue;
    end;
    ///

    ///
    Inc(AIndex);

    var ADrive := Format('%s:', [UpperCase(ALetter)]);

    AList.Add(TDriveInformation.Create(ADrive, AIndex));
  end;
  {$I+}
end;

(***********************************************************************************************************************

  TFileInformation

***********************************************************************************************************************)

{ TFileInformation.GetFileTypeDescription }
function TFileInformation.GetFileTypeDescription() : String;
begin
  if FIsDirectory then
    result := 'Directory'
  else
    result := FTypeDescription;
end;

{ TFileInformation.Assign }
procedure TFileInformation.Assign(ASource : TPersistent);
begin
  if ASource is TFileInformation then begin
    FPath             := TFileInformation(ASource).FPath;
    FName             := TFileInformation(ASource).FName;
    FIsDirectory      := TFileInformation(ASource).FIsDirectory;
    FACL_SSDL         := TFileInformation(ASource).FACL_SSDL;
    FAccess           := TFileInformation(ASource).FAccess;
    FTypeDescription  := TFileInformation(ASource).FTypeDescription;
    FSize             := TFileInformation(ASource).FSize;
    FDateAreValid     := TFileInformation(ASource).FDateAreValid;
    FCreatedDate      := TFileInformation(ASource).FCreatedDate;
    FLastModifiedDate := TFileInformation(ASource).FLastModifiedDate;
    FLastAccessDate   := TFileInformation(ASource).FLastAccessDate;
  end else
    inherited;
end;

{ TFileInformation.Create }
constructor TFileInformation.Create(const APath : String; const AIsDirectory : Boolean);
begin
  FPath         := APath;
  FName         := ExtractFileName(APath);
  FIsDirectory  := AIsDirectory;
  FACL_SSDL     := TFileSystemHelper.TryGetFileACLString(APath);
  FAccess       := TFileSystemHelper.TryGetCurrentUserFileAccess(APath);
  FDateAreValid := TFileSystemHelper.TryGetFileTime(APath, FCreatedDate, FLastModifiedDate, FLastAccessDate);

  if not FIsDirectory then begin
    FTypeDescription := TFileSystemHelper.GetFileTypeDescription(APath);
    FSize            := TFileSystemHelper.TryGetFileSize(APath);
  end else begin
    FTypeDescription := '';
    FSize            := 0;
  end;
end;

(***********************************************************************************************************************

  TOptixEnumFiles

***********************************************************************************************************************)

{ TOptixEnumFiles.Enum }
class procedure TOptixEnumFiles.Enum(const APath : String; var AList : TObjectList<TFileInformation>;
  var AIsRoot : Boolean; var AAccess : TFileAccessAttributes);
begin
  if not Assigned(AList) then
    AList := TObjectList<TFileInformation>.Create(True)
  else
    AList.Clear();
  ///

  if String.IsNullOrEmpty(APath) then
    Exit();

  var ASearchParameter := Format('%s*.*', [APath]);

  var AWin32FindData : TWin32FindDataW;

  var hSearch := FindFirstFileW(PWideChar(ASearchParameter), AWin32FindData);
  if hSearch = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('FindFirstFileW')
  else if hSearch = ERROR_FILE_NOT_FOUND then
    raise Exception.Create(Format('No files found so far in the directory: `%s`', [APath]));
  try
    AIsRoot := True;
    AAccess := TFileSystemHelper.TryGetCurrentUserFileAccess(APath);
    repeat
      var AFileName := String(AWin32FindData.cFileName);
      if AFileName = '.' then
        continue;
      ///

      if AFileName = '..' then
        AIsRoot := False;

      AList.Add(
        TFileInformation.Create(
          APath + AFileName,
          (AWin32FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY)
        )
      );
    until FindNextFileW(hSearch, AWin32FindData) = False;
  finally
    FindClose(hSearch);
  end;
end;

end.
