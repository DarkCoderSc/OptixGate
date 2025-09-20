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

unit Optix.Func.Commands.FileSystem;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Winapi.Windows,

  XSuperObject,

  Optix.Func.Commands.Base, Optix.FileSystem.Enum, Optix.FileSystem.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixRequestFileInformation = class(TOptixCommandActionResponse)
  private
    FFileName        : String;
    FIsDirectory     : Boolean;

    FFileInformation : TFileInformation;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    procedure DoAction(); override;

    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(); reintroduce; override;
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
    FPath   : String;
    FAccess : TFileAccessAttributes;
    FIsRoot : Boolean;
    FList   : TObjectList<TFileInformation>;
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
    property Path   : String                        read FPath;
    property List   : TObjectList<TFileInformation> read FList;
    property Access : TFileAccessAttributes         read FAccess;
    property IsRoot : Boolean                       read FIsRoot;
  end;

  TOptixCommandTransfer = class(TOptixCommand)
  private
    FTransferId : TGUID;

    // FFilePath in TOptixDownloadFile -> File to download (Client)
    // FFilePath in TOptixUploadFile   -> Uploaded destination file path (Client)
    FFilePath : String;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(const AFilePath : String; const ATransferId : TGUID); overload;

    {@G}
    property TransferId : TGUID  read FTransferId;
    property FilePath   : String read FFilePath;
  end;

  TOptixCommandDownloadFile = class(TOptixCommandTransfer);

  TOptixCommandUploadFile = class(TOptixCommandTransfer)
  private
    FFileSize : UInt64;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property FileSize : UInt64 read FFileSize;
  end;

implementation

(***********************************************************************************************************************

  TOptixRequestFileInformation

(**********************************************************************************************************************)

{ TOptixRequestFileInformation.Create }
constructor TOptixRequestFileInformation.Create();
begin
  inherited Create();
  ///

  FFileInformation := nil;
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

  FFileInformation := TFileInformation.Create(FFileName, FIsDirectory);
end;

{ TOptixRequestFileInformation.Serialize }
function TOptixRequestFileInformation.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['FileName']    := FFileName;
  result.B['IsDirectory'] := FIsDirectory;

  if Assigned(FFileInformation) then
    result.O['FileInformation'] := FFileInformation.Serialize();
end;

{ TOptixRequestFileInformation.DeSerialize }
procedure TOptixRequestFileInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FFileName    := ASerializedObject.S['FileName'];
  FIsDirectory := ASerializedObject.B['IsDirectory'];

  if ASerializedObject.Contains('FileInformation') then
    FFileInformation := TFileInformation.Create(ASerializedObject.O['FileInformation']);
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
  TOptixEnumFiles.Enum(FPath, FList, FIsRoot, FAccess);
end;

{ TOptixCommandRefreshFiles.DeSerialize }
procedure TOptixCommandRefreshFiles.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FList.Clear();

  FPath   := ASerializedObject.S['Path'];
  FAccess := StringToAccessSet(ASerializedObject.S['Access']);
  FIsRoot := ASerializedObject.B['IsRoot'];

  for var I := 0 to ASerializedObject.A['List'].Length -1 do
    FList.Add(TFileInformation.Create(ASerializedObject.A['List'].O[I]));
end;

{ TOptixCommandRefreshFiles.Serialize }
function TOptixCommandRefreshFiles.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FList do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.S['Path']   := FPath;
  result.A['List']   := AJsonArray;
  result.S['Access'] := AccessSetToString(FAccess);
  result.B['IsRoot'] := FIsRoot;
end;

{ TOptixCommandRefreshFiles.Create }
constructor TOptixCommandRefreshFiles.Create();
begin
  inherited;
  ///

  FList   := TObjectList<TFileInformation>.Create(True);
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
  if Assigned(FList) then
    FreeAndNil(FList);

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

{ TOptixCommandTransfer.Serialize }
function TOptixCommandTransfer.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['TransferId'] := FTransferId.ToString();
  result.S['FilePath']   := FFilePath;
end;

{ TOptixCommandTransfer.DeSerialize }
procedure TOptixCommandTransfer.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FTransferId := TGUID.Create(ASerializedObject.S['TransferId']);
  FFilePath   := ASerializedObject.S['FilePath'];
end;

(***********************************************************************************************************************

  TOptixCommandUploadFile

***********************************************************************************************************************)

{ TOptixCommandUploadFile.Serialize }
function TOptixCommandUploadFile.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.I['FileSize'] := FFileSize;
end;

{ TOptixCommandUploadFile.DeSerialize }
procedure TOptixCommandUploadFile.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FFileSize := ASerializedObject.I['FileSize'];
end;


end.
