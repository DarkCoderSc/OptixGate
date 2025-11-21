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


unit Optix.Func.Commands.FileSystem;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Winapi.Windows,

  XSuperObject,

  Optix.Func.Commands.Base, Optix.FileSystem.Enum, Optix.FileSystem.Helper, Optix.Shared.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixRequestFileInformation = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FFileName : String;

    [OptixSerializableAttribute]
    FIsDirectory : Boolean;

    [OptixSerializableAttribute]
    FFileInformation : TFileInformation;
  public
    {@M}
    procedure DoAction(); override;

    {@C}
    constructor Create(); override;
    constructor Create(const AFileName : String; const AIsDirectory : Boolean); overload;
    destructor Destroy(); override;

    {@G}
    property FileName        : String           read FFileName;
    property IsDirectory     : Boolean          read FIsDirectory;
    property FileInformation : TFileInformation read FFileInformation;
  end;

  {@ALIASES: TOptixRequestFileInformation}
  TOptixRequestUploadedFileInformation = class(TOptixRequestFileInformation);

  TOptixCommandRefreshDrives = class(TOptixCommandActionResponse)
  private
    FList : TObjectList<TDriveInformation>;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;
    procedure DoAction(); override;

    {@C}
    constructor Create(); override;
    destructor Destroy(); override;

    {@G}
    property List : TObjectList<TDriveInformation> read FList;
  end;

  TOptixCommandRefreshFiles = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FPath : String;

    [OptixSerializableAttribute]
    FAccess : TFileAccessAttributes;

    [OptixSerializableAttribute]
    FIsRoot : Boolean;

    FParentFolders : TObjectList<TSimpleFolderInformation>;
    FFiles         : TObjectList<TFileInformation>;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;
    procedure DoAction(); override;

    {@C}
    constructor Create(); override;
    constructor Create(const APath : String); overload;
    destructor Destroy(); override;

    {@G}
    property Path          : String                                read FPath;
    property ParentFolders : TObjectList<TSimpleFolderInformation> read FParentFolders;
    property Files         : TObjectList<TFileInformation>         read FFiles;
    property Access        : TFileAccessAttributes                 read FAccess;
    property IsRoot        : Boolean                               read FIsRoot;
  end;

  TOptixCommandTransfer = class(TOptixCommand)
  private
    [OptixSerializableAttribute]
    FTransferId : TGUID;

    // FFilePath in TOptixDownloadFile -> File to download (Client)
    // FFilePath in TOptixUploadFile   -> Uploaded destination file path (Client)
    [OptixSerializableAttribute]
    FFilePath : String;
  public
    {@C}
    constructor Create(const AFilePath : String; const ATransferId : TGUID); overload;

    {@G}
    property TransferId : TGUID  read FTransferId;
    property FilePath   : String read FFilePath;
  end;

  TOptixCommandDownloadFile = class(TOptixCommandTransfer);

  TOptixCommandUploadFile = class(TOptixCommandTransfer)
  private
    [OptixSerializableAttribute]
    FFileSize : UInt64;
  public
    {@G}
    property FileSize : UInt64 read FFileSize;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.IOUtils;
// ---------------------------------------------------------------------------------------------------------------------

(***********************************************************************************************************************

  TOptixRequestFileInformation

(**********************************************************************************************************************)

{ TOptixRequestFileInformation.Create }
constructor TOptixRequestFileInformation.Create();
begin
  inherited Create();
  ///

  FFileInformation := TFileInformation.Create();
end;

{ TOptixRequestFileInformation.Create }
constructor TOptixRequestFileInformation.Create(const AFileName : String; const AIsDirectory : Boolean);
begin
  Create();
  ///

  FFileName    := AFileName;
  FIsDirectory := AIsDirectory;
end;

{ TOptixRequestFileInformation.Destroy }
destructor TOptixRequestFileInformation.Destroy();
begin
  if Assigned(FFileInformation) then
    FreeAndNil(FFileInformation);

  ///
  inherited Destroy();
end;

{ TOptixRequestFileInformation.DoAction }
procedure TOptixRequestFileInformation.DoAction();
begin
  inherited;
  ///

  if Assigned(FFileInformation) then
    FreeAndNil(FFileInformation);

  FFileInformation := TFileInformation.Create(FFileName, FIsDirectory);
end;

(***********************************************************************************************************************

  TOptixCommandRefreshDrives

(**********************************************************************************************************************)

{ TOptixCommandRefreshDrives.DoAction }
procedure TOptixCommandRefreshDrives.DoAction();
begin
  TOptixEnumDrives.Enum(FList);
end;

{ TOptixCommandRefreshDrives.DeSerialize }
procedure TOptixCommandRefreshDrives.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FList.Clear();

  for var I := 0 to ASerializedObject.A['List'].Length -1 do
    FList.Add(TDriveInformation.Create(ASerializedObject.A['List'].O[I]));
end;

{ TOptixCommandRefreshDrives.Serialize }
function TOptixCommandRefreshDrives.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FList do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.A['List'] := AJsonArray;
end;

{ TOptixCommandRefreshDrives.Create }
constructor TOptixCommandRefreshDrives.Create();
begin
  inherited;
  ///

  FList := TObjectList<TDriveInformation>.Create(True);
end;

{ TOptixCommandRefreshDrives.Destroy }
destructor TOptixCommandRefreshDrives.Destroy();
begin
  if Assigned(FList) then
    FreeAndNil(FList);

  ///
  inherited;
end;

(***********************************************************************************************************************

  TOptixCommandRefreshFiles

(**********************************************************************************************************************)

{ TOptixCommandRefreshFiles.DoAction }
procedure TOptixCommandRefreshFiles.DoAction();
begin
  FParentFolders.Clear();
  ///

  FPath := TFileSystemHelper.GetFullPathName(TFileSystemHelper.ExpandPath(FPath));

  TFileSystemHelper.PathExists(FPath);

  TFileSystemHelper.TraverseDirectories(
    FPath,
    (
      procedure (const ADirectoryName : String; const AAbsolutePath : String)
      begin
        var APath := IncludeTrailingPathDelimiter(AAbsolutePath);
        var AIsRoot := SameText(APath, TPath.GetPathRoot(APath));
        ///

        var AFileAccess : TFileAccessAttributes := [];
        if not AIsRoot then
          AFileAccess := TFileSystemHelper.TryGetCurrentUserFileAccess(APath);

        ///
        FParentFolders.Add(TSimpleFolderInformation.Create(ADirectoryName, AAbsolutePath, AFileAccess, AIsRoot));
      end
    )
  );

  ///
  TOptixEnumFiles.Enum(FPath, FFiles, FIsRoot, FAccess);
end;

{ TOptixCommandRefreshFiles.DeSerialize }
procedure TOptixCommandRefreshFiles.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FParentFolders.Clear();

  for var I := 0 to ASerializedObject.A['ParentFolders'].Length -1 do
    FParentFolders.Add(TSimpleFolderInformation.Create(ASerializedObject.A['ParentFolders'].O[I]));

  ///

  FFiles.Clear();

  for var I := 0 to ASerializedObject.A['Files'].Length -1 do
    FFiles.Add(TFileInformation.Create(ASerializedObject.A['Files'].O[I]));
end;

{ TOptixCommandRefreshFiles.Serialize }
function TOptixCommandRefreshFiles.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FParentFolders do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.A['ParentFolders'] := AJsonArray;

  ///

  AJsonArray := TSuperArray.Create();

  for var AItem in FFiles do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.A['Files'] := AJsonArray;
end;

{ TOptixCommandRefreshFiles.Create }
constructor TOptixCommandRefreshFiles.Create();
begin
  inherited;
  ///

  FParentFolders := TObjectList<TSimpleFolderInformation>.Create(True);
  FFiles := TObjectList<TFileInformation>.Create(True);
end;

{ TOptixCommandRefreshFiles.Create }
constructor TOptixCommandRefreshFiles.Create(const APath : String);
begin
  Create();
  ///

  FPath   := APath;
  FAccess := [];
  FIsRoot := False;
end;

{ TOptixCommandRefreshFiles.Destroy }
destructor TOptixCommandRefreshFiles.Destroy();
begin
  if Assigned(FFiles) then
    FreeAndNil(FFiles);

  if Assigned(FParentFolders) then
    FreeAndNil(FParentFolders);

  ///
  inherited;
end;

(***********************************************************************************************************************

  TOptixCommandTransfer

***********************************************************************************************************************)

{ TOptixCommandTransfer.Create }
constructor TOptixCommandTransfer.Create(const AFilePath : String; const ATransferId : TGUID);
begin
  inherited Create();
  ///

  FFilePath   := AFilePath;
  FTransferId := ATransferId;
end;

end.
