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



unit OptixCore.Commands.FileSystem;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Winapi.Windows,

  OptixCore.Commands.Base, OptixCore.System.FileSystem, OptixCore.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixCommandReceiveFileInformation = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FFileName : String;

    [OptixSerializableAttribute]
    FIsDirectory : Boolean;

    [OptixSerializableAttribute]
    FFileInformation : TFileInformation;
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
    procedure AfterCreate(); override;

    {@C}
    constructor Create(const AFileName : String; const AIsDirectory : Boolean); overload;
    destructor Destroy(); override;

    {@G}
    property FileName        : String           read FFileName;
    property IsDirectory     : Boolean          read FIsDirectory;
    property FileInformation : TFileInformation read FFileInformation;
  end;

  {@ALIASES: TOptixCommandReceiveFileInformation}
  TOptixCommandGetUploadedFileInformation = class(TOptixCommandReceiveFileInformation);

  TOptixCommandEnumDrives = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FDrives : TObjectList<TDriveInformation>;
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
    procedure AfterCreate(); override;

    {@C}
    destructor Destroy(); override;

    {@G}
    property Drives : TObjectList<TDriveInformation> read FDrives;
  end;

  TOptixCommandEnumDirectoryFiles = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FPath : String;

    [OptixSerializableAttribute]
    FAccess : TFileAccessAttributes;

    [OptixSerializableAttribute]
    FIsRoot : Boolean;

    [OptixSerializableAttribute]
    FParentFolders : TObjectList<TSimpleFolderInformation>;

    [OptixSerializableAttribute]
    FFiles : TObjectList<TFileInformation>;
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
    procedure AfterCreate(); override;

    {@C}
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

(* TOptixCommandReceiveFileInformation *)

procedure TOptixCommandReceiveFileInformation.AfterCreate();
begin
  inherited;
  ///

  FFileInformation := TFileInformation.Create();
end;

constructor TOptixCommandReceiveFileInformation.Create(const AFileName : String; const AIsDirectory : Boolean);
begin
  Create();
  ///

  FFileName    := AFileName;
  FIsDirectory := AIsDirectory;
end;

destructor TOptixCommandReceiveFileInformation.Destroy();
begin
  if Assigned(FFileInformation) then
    FreeAndNil(FFileInformation);

  ///
  inherited Destroy();
end;

{$IFNDEF SERVER}
procedure TOptixCommandReceiveFileInformation.DoAction();
begin
  inherited;
  ///

  if Assigned(FFileInformation) then
    FreeAndNil(FFileInformation);

  FFileInformation := TFileInformation.Create(FFileName, FIsDirectory);
end;
{$ENDIF}

(* TOptixCommandEnumDrives *)

{$IFNDEF SERVER}
procedure TOptixCommandEnumDrives.DoAction();
begin
  TOptixEnumDrives.Enum(FDrives);
end;
{$ENDIF}

procedure TOptixCommandEnumDrives.AfterCreate();
begin
  inherited;
  ///

  FDrives := TObjectList<TDriveInformation>.Create(True);
end;

destructor TOptixCommandEnumDrives.Destroy();
begin
  if Assigned(FDrives) then
    FreeAndNil(FDrives);

  ///
  inherited;
end;

(* TOptixCommandEnumDirectoryFiles *)

{$IFNDEF SERVER}
procedure TOptixCommandEnumDirectoryFiles.DoAction();
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
{$ENDIF}

procedure TOptixCommandEnumDirectoryFiles.AfterCreate();
begin
  inherited;
  ///

  FParentFolders := TObjectList<TSimpleFolderInformation>.Create(True);
  FFiles := TObjectList<TFileInformation>.Create(True);
end;

constructor TOptixCommandEnumDirectoryFiles.Create(const APath : String);
begin
  Create();
  ///

  FPath   := APath;
  FAccess := [];
  FIsRoot := False;
end;

destructor TOptixCommandEnumDirectoryFiles.Destroy();
begin
  if Assigned(FFiles) then
    FreeAndNil(FFiles);

  if Assigned(FParentFolders) then
    FreeAndNil(FParentFolders);

  ///
  inherited;
end;

(* TOptixCommandTransfer *)

constructor TOptixCommandTransfer.Create(const AFilePath : String; const ATransferId : TGUID);
begin
  inherited Create();
  ///

  FFilePath   := AFilePath;
  FTransferId := ATransferId;
end;

end.
