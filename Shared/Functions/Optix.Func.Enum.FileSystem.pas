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

unit Optix.Func.Enum.FileSystem;

interface

uses Optix.Protocol.Packet, Generics.Collections, XSuperObject, System.Classes,
     Optix.Classes, Optix.FileSystem.Helper;

type
  // Drives --------------------------------------------------------------------
  TDriveInformation = class(TEnumerableItem)
  private
    FLetter    : String;
    FName      : String;
    FFormat    : String;
    FType      : TDriveType;
    FTotalSize : UInt64;
    FFreeSize  : UInt64;

    {@G}
    function GetUsedPercentage() : Byte;
    function GetUsedSize() : UInt64;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const ADrive : String; const AIndex : Integer); overload;

    {@M}
    function Serialize() : ISuperObject; override;
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Letter         : String     read FLetter;
    property Name           : String     read FName;
    property Format         : String     read FFormat;
    property DriveType      : TDriveType read FType;
    property TotalSize      : UInt64     read FTotalSize;
    property FreeSize       : UInt64     read FFreeSize;
    property UsedSize       : UInt64     read GetUsedSize;
    property UsedPercentage : Byte       read GetUsedPercentage;
  end;

  TDriveList = class(TOptixPacket)
  private
    FList : TObjectList<TDriveInformation>;
  protected
    {@M}
    procedure Refresh();
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
    procedure BeforeCreate(); override;
  public
    {@C}
    constructor Create(const AWindowGUID : TGUID); override;
    destructor Destroy(); override;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property List : TObjectList<TDriveInformation> read FList;
  end;

  // Files ---------------------------------------------------------------------
  TFileInformation = class(TEnumerableItem)
  protected
    FName             : String;
    FIsDirectory      : Boolean;
    FACL_SSDL         : String;
    FAccess           : TFileAccessAttributes;
    FTypeDescription  : String;
    FSize             : Int64;
    FDateAreValid     : Boolean;
    FCreatedDate      : TDateTime;
    FLastModifiedDate : TDateTime;
    FLastAccessDate   : TDateTime;

    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const AFilePath : String; const AIsDirectory : Boolean); overload;

    {@M}
    function Serialize() : ISuperObject; override;
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Name             : String                read FName;
    property IsDirectory      : Boolean               read FIsDirectory;
    property ACL_SSDL         : String                read FACL_SSDL;
    property Access           : TFileAccessAttributes read FAccess;
    property TypeDescription  : String                read FTypeDescription;
    property Size             : Int64                 read FSize;
    property DateAreValid     : Boolean               read FDateAreValid;
    property CreatedDate      : TDateTime             read FCreatedDate;
    property LastModifiedDate : TDateTime             read FLastModifiedDate;
    property LastAccessDate   : TDateTime             read FLastAccessDate;
  end;

  TFileList = class(TOptixPacket)
  private
    FPath   : String;
    FIsRoot : Boolean;
    FList   : TObjectList<TFileInformation>;
  protected
    {@M}
    procedure Refresh();
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
    procedure BeforeCreate(); override;
  public
    {@C}
    constructor Create(const AWindowGUID : TGUID; const APath : String); overload;
    destructor Destroy(); override;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property List   : TObjectList<TFileInformation> read FList;
    property Path   : String                        read FPath;
    property IsRoot : Boolean                       read FIsRoot;
  end;

implementation

uses System.SysUtils, Winapi.Windows, Optix.Exceptions;

//------------------------------------------------------------------------------
//
//  Drives
//
//------------------------------------------------------------------------------

(* TDriveInformation *)

{ TDriveInformation.DeSerialize }
procedure TDriveInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    Exit();
  ///

  FLetter    := ASerializedObject.S['Letter'];
  FName      := ASerializedObject.S['Name'];
  FFormat    := ASerializedObject.S['Format'];
  FType      := TDriveType(ASerializedObject.I['Type']);
  FTotalSize := ASerializedObject.I['TotalSize'];
  FFreeSize  := ASerializedObject.I['FreeSize'];
end;

{ TDriveInformation.Serialize }
function TDriveInformation.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

  result.S['Letter']    := FLetter;
  result.S['Name']      := FName;
  result.S['Format']    := FFormat;
  result.I['Type']      := Cardinal(FType);
  result.I['TotalSize'] := FTotalSize;
  result.I['FreeSize']  := FFreeSize;
end;

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
  inherited Create();
  ///

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
  if (FTotalSize = 0) or (FFreeSize = 0) then
    Exit(0);
  ///

  result := (GetUsedSize() * 100) div FTotalSize;
end;

{ TDriveInformation.UsedSize }
function TDriveInformation.GetUsedSize() : UInt64;
begin
  if FTotalSize = 0 then
    Exit(0);
  ///

  result := FTotalSize - FFreeSize;
end;

(* TDriveList *)

{ TDriveList.AfterCreate }
procedure TDriveList.BeforeCreate();
begin
  inherited;
  ///

  FList := TObjectList<TDriveInformation>.Create(True);
end;

{ TDriveList.Create }
constructor TDriveList.Create(const AWindowGUID : TGUID);
begin
  inherited;
  ///

  self.Refresh();
end;

{ TDriveList.Destroy }
destructor TDriveList.Destroy();
begin
  if Assigned(FList) then
    FreeAndNil(FList);

  ///
  inherited;
end;

{ TDriveList.Refresh }
procedure TDriveList.Refresh();
begin
  FList.Clear();
  ///

  {$I-}
  var ALogicalDrives := GetLogicalDrives();

  var AIndex := 0;
  for var ALetter : String in ['a'..'z'] do begin
    if (ALogicalDrives and (1 shl AIndex)) = 0 then begin
      Inc(AIndex);

      continue;
    end;
    ///

    ///
    Inc(AIndex);

    var ADrive := Format('%s:', [ALetter.ToUpper]);

    FList.Add(TDriveInformation.Create(ADrive, AIndex));
  end;
  {$I+}
end;

{ TDriveList.DeSerialize }
procedure TDriveList.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FList.Clear();

  for var I := 0 to ASerializedObject.A['List'].Length -1 do
    FList.Add(TDriveInformation.Create(ASerializedObject.A['List'].O[I]));
end;

{ TDriveList.Serialize }
function TDriveList.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FList do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.A['List'] := AJsonArray;
end;

//------------------------------------------------------------------------------
//
//  Files / Directories
//
//------------------------------------------------------------------------------

(* TFileInformation *)

{ TFileInformation.DeSerialize }
procedure TFileInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    Exit();
  ///

  FName        := ASerializedObject.S['Name'];
  FIsDirectory := ASerializedObject.B['IsDirectory'];
  FACL_SSDL    := ASerializedObject.S['ACL_SSDL'];

  FTypeDescription  := ASerializedObject.S['TypeDescription'];
  FSize             := ASerializedObject.I['Size'];
  FDateAreValid     := ASerializedObject.B['DateAreValid'];
  FCreatedDate      := ASerializedObject.D['CreatedDate'];
  FLastModifiedDate := ASerializedObject.D['LastModifiedDate'];
  FLastAccessDate   := ASerializedObject.D['LastAccessDate'];

  // Access (TODO: SetToString?)
  FAccess := [];
  if ASerializedObject.B['AccessRead'] then
    Include(FAccess, faRead);

  if ASerializedObject.B['AccessWrite'] then
    Include(FAccess, faWrite);

  if ASerializedObject.B['AccessExecute'] then
    Include(FAccess, faExecute);
end;

{ TFileInformation.Serialize }
function TFileInformation.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

  result.S['Name']        := FName;
  result.B['IsDirectory'] := FIsDirectory;
  result.S['ACL_SSDL']    := FACL_SSDL;

  result.S['TypeDescription']  := FTypeDescription;
  result.I['Size']             := FSize;
  result.B['DateAreValid']     := FDateAreValid;
  result.D['CreatedDate']      := FCreatedDate;
  result.D['LastModifiedDate'] := FLastModifiedDate;
  result.D['LastAccessDate']   := FLastAccessDate;

  // Access (TODO: StringToSet?)
  result.B['AccessRead']    := faRead in FAccess;
  result.B['AccessWrite']   := faWrite in FAccess;
  result.B['AccessExecute'] := faExecute in FAccess;
end;

{ TFileInformation.Assign }
procedure TFileInformation.Assign(ASource : TPersistent);
begin
  if ASource is TFileInformation then begin
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
constructor TFileInformation.Create(const AFilePath : String; const AIsDirectory : Boolean);
begin
  inherited Create();
  ///

  FName            := ExtractFileName(AFilePath);
  FIsDirectory     := AIsDirectory;
  FACL_SSDL        := TFileSystemHelper.TryGetFileACLString(AFilePath);
  FAccess          := TFileSystemHelper.TryGetCurrentUserFileAccess(AFilePath);
  FDateAreValid    := TFileSystemHelper.TryGetFileTime(AFilePath, FCreatedDate, FLastModifiedDate, FLastAccessDate);

  if not FIsDirectory then begin
    FTypeDescription := TFileSystemHelper.GetFileTypeDescription(AFilePath);
    FSize            := TFileSystemHelper.TryGetFileSize(AFilePath);
  end else begin
    FTypeDescription := '';
    FSize            := 0;
  end;
end;

(* TFileList *)

{ TFileList.AfterCreate }
procedure TFileList.BeforeCreate();
begin
  inherited;
  ///

  FIsRoot := False;
  FPath   := '';

  FList := TObjectList<TFileInformation>.Create(True);
end;

{ TFileList.Create }
constructor TFileList.Create(const AWindowGUID : TGUID; const APath : String);
begin
  inherited Create(AWindowGUID);
  ///

  FPath := IncludeTrailingPathDelimiter(APath);

  ///
  self.Refresh();
end;

{ TFileList.Destroy }
destructor TFileList.Destroy();
begin
  if Assigned(FList) then
    FreeAndNil(FList);

  ///
  inherited;
end;

{ TFileList.Refresh }
procedure TFileList.Refresh();
begin
  FList.Clear();
  ///

  if String.IsNullOrEmpty(FPath) then
    Exit();

  var ASearchParameter := Format('%s*.*', [FPath]);

  var AWin32FindData : TWin32FindDataW;

  var hSearch := FindFirstFileW(PWideChar(ASearchParameter), AWin32FindData);
  if hSearch = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('FindFirstFileW')
  else if hSearch = ERROR_FILE_NOT_FOUND then
    raise Exception.Create(Format('No files found so far in the directory: `%s`', [FPath]));
  try
    FIsRoot := True;
    repeat
      var AFileName := String(AWin32FindData.cFileName);
      if AFileName = '.' then
        continue;
      ///

      if AFileName = '..' then
        FIsRoot := False;

      if (AWin32FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY) then begin
        FList.Add(TFileInformation.Create(FPath + AFileName, True))
      end else
        FList.Add(TFileInformation.Create(FPath + AFileName, False));
    until FindNextFileW(hSearch, AWin32FindData) = False;
  finally
    FindClose(hSearch);
  end;
end;

{ TFileList.DeSerialize }
procedure TFileList.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FList.Clear();

  for var I := 0 to ASerializedObject.A['List'].Length -1 do
    FList.Add(TFileInformation.Create(ASerializedObject.A['List'].O[I]));

  FPath   := ASerializedObject.S['Path'];
  FIsRoot := ASerializedObject.B['IsRoot'];
end;

{ TFileList.Serialize }
function TFileList.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FList do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.A['List']   := AJsonArray;
  result.S['Path']   := FPath;
  result.B['IsRoot'] := FIsRoot;
end;

end.
