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

unit Optix.Func.Commands.Registry;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Winapi.Windows,

  XSuperObject,

  Optix.Shared.Classes, Optix.Func.Commands.Base, Optix.Registry.Enum, Optix.Registry.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixRefreshRegistryKeys = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FPath : String;

    [OptixSerializableAttribute]
    FPermissions : TRegistryKeyPermissions;

    FParentKeys : TObjectList<TRegistryKeyInformation>;
    FSubKeys    : TObjectList<TRegistryKeyInformation>;
    FValues     : TObjectList<TRegistryValueInformation>;

    {@M}
    function GetIsRoot() : Boolean;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(); override;
    constructor Create(const APath : String); overload;
    destructor Destroy(); override;

    {@G}
    property IsRoot      : Boolean                                read GetIsRoot;
    property Path        : String                                 read FPath;
    property ParentKeys  : TObjectList<TRegistryKeyInformation>   read FParentKeys;
    property SubKeys     : TObjectList<TRegistryKeyInformation>   read FSubKeys;
    property Values      : TObjectList<TRegistryValueInformation> read FValues;
    property Permissions : TRegistryKeyPermissions                read FPermissions;
  end;

  TOptixGetRegistryHives = class(TOptixRefreshRegistryKeys)
  public
    {@M}
    procedure DoAction(); override;
  end;

  TOptixRefreshRegistrySubKeys = class(TOptixRefreshRegistryKeys)
  public
    {@M}
    procedure DoAction(); override;
  end;

  TOptixCommandRegistryCreateKey = class(TOptixRefreshRegistrySubKeys)
  private
    [OptixSerializableAttribute]
    FNewKeyFullPath : String;
  public
    {@C}
    constructor Create(const ANewKeyFullPath : String); overload;

    {@M}
    procedure DoAction(); override;

    {@G}
    property NewKeyFullPath : String read FNewKeyFullPath;
  end;

  TOptixCommandDeleteKey = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FKeyFullPath : String;
  public
    {@C}
    constructor Create(const AKeyFullPath : String); overload;

    {@M}
    procedure DoAction(); override;

    {@G}
    property KeyFullPath : String read FKeyFullPath;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.System.Helper, Optix.FileSystem.Helper;
// ---------------------------------------------------------------------------------------------------------------------

(***********************************************************************************************************************

  TOptixRefreshRegistryKeys

***********************************************************************************************************************)

{ TOptixRefreshRegistryKeys.DeSerialize }
procedure TOptixRefreshRegistryKeys.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FParentKeys.Clear();
  for var I := 0 to ASerializedObject.A['ParentKeys'].Length -1 do
    FParentKeys.Add(TRegistryKeyInformation.Create(ASerializedObject.A['ParentKeys'].O[I]));

  FSubKeys.Clear();
  for var I := 0 to ASerializedObject.A['SubKeys'].Length -1 do
    FSubKeys.Add(TRegistryKeyInformation.Create(ASerializedObject.A['SubKeys'].O[I]));

  FValues.Clear();
  for var I := 0 to ASerializedObject.A['Values'].Length -1 do
    FValues.Add(TRegistryValueInformation.Create(ASerializedObject.A['Values'].O[I]));
end;

{ TOptixRefreshRegistryKeys.Serialize }
function TOptixRefreshRegistryKeys.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FParentKeys do
    AJsonArray.Add(AItem.Serialize);

  result.A['ParentKeys'] := AJsonArray;

  ///

  AJsonArray := TSuperArray.Create();

  for var AItem in FSubKeys do
    AJsonArray.Add(AItem.Serialize);

  result.A['SubKeys'] := AJsonArray;

  ///

  AJsonArray := TSuperArray.Create();

  for var AItem in FValues do
    AJsonArray.Add(AItem.Serialize);

  result.A['Values'] := AJsonArray;
end;

{ TOptixRefreshRegistryKeys.Create }
constructor TOptixRefreshRegistryKeys.Create();
begin
  inherited Create();
  ///

  FParentKeys := TObjectList<TRegistryKeyInformation>.Create(True);
  FSubKeys    := TObjectList<TRegistryKeyInformation>.Create(True);
  FValues     := TObjectList<TRegistryValueInformation>.Create(True);

  FPath := '';
  FPermissions := [];
end;

{ TOptixRefreshRegistryKeys.Create }
constructor TOptixRefreshRegistryKeys.Create(const APath : String);
begin
  Create();
  ///

  FPath := ExcludeTrailingPathDelimiter(APath);
  TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(FPath, FPermissions);
end;

{ TOptixRefreshRegistryKeys.Destroy }
destructor TOptixRefreshRegistryKeys.Destroy();
begin
  if Assigned(FParentKeys) then
    FreeAndNil(FParentKeys);

  if Assigned(FSubKeys) then
    FreeAndNil(FSubKeys);

  if Assigned(FValues) then
    FreeAndNil(FValues);

  ///
  inherited;
end;

{ TOptixRefreshRegistryKeys.GetIsRoot }
function TOptixRefreshRegistryKeys.GetIsRoot() : Boolean;
begin
  result := String.IsNullOrWhiteSpace(FPath);
end;

(***********************************************************************************************************************

  TOptixGetRegistryHives

***********************************************************************************************************************)

{ TOptixGetRegistryHives.DoAction }
procedure TOptixGetRegistryHives.DoAction();
begin
  for var AHive in TRegistryHelper.RegistryHives.Keys do
    FSubKeys.Add(TRegistryKeyInformation.Create(AHive, AHive));
end;

(***********************************************************************************************************************

  TOptixRefreshRegistrySubKeys

***********************************************************************************************************************)

{ TOptixRefreshRegistrySubKeys.DoAction }
procedure TOptixRefreshRegistrySubKeys.DoAction();
begin
  TRegistryHelper.CheckRegistryPath(FPath);
  FParentKeys.Clear();
  ///

  TFileSystemHelper.TraverseDirectories(
    FPath,
    (
      procedure (const ADirectoryName : String; const AAbsolutePath : String)
      begin
        FParentKeys.Add(TRegistryKeyInformation.Create(ADirectoryName, AAbsolutePath));
      end
    )
  );

  ///
  TOptixEnumRegistry.Enum(FPath, FSubKeys, FValues);
end;

(***********************************************************************************************************************

TOptixCommandRegistryCreateKey

***********************************************************************************************************************)

{ TOptixCommandRegistryCreateKey.Create }
constructor TOptixCommandRegistryCreateKey.Create(const ANewKeyFullPath : String);
begin
  inherited Create();
  ///

  FNewKeyFullPath := ANewKeyFullPath;
end;

{ TOptixCommandRegistryCreateKey.DoAction }
procedure TOptixCommandRegistryCreateKey.DoAction();
begin
  TRegistryHelper.CreateSubKey(FNewKeyFullPath);
  ///

  FPath := ExcludeTrailingPathDelimiter(FNewKeyFullPath);
  TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(FPath, FPermissions);

  ///
  inherited;
end;

(***********************************************************************************************************************

TOptixCommandDeleteKey

***********************************************************************************************************************)

{ TOptixCommandDeleteKey.Create }
constructor TOptixCommandDeleteKey.Create(const AKeyFullPath: String);
begin
  inherited Create();
  ///

  FKeyFullPath := AKeyFullPath;
end;

{ TOptixCommandDeleteKey.DoAction }
procedure TOptixCommandDeleteKey.DoAction();
begin
  TRegistryHelper.DeleteKey(FKeyFullPath);
end;

end.
