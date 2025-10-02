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

    FKeys   : TObjectList<TRegistryKeyInformation>;
    FValues : TObjectList<TRegistryValueInformation>;

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
    property Keys        : TObjectList<TRegistryKeyInformation>   read FKeys;
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

implementation

(***********************************************************************************************************************

  TOptixRefreshRegistryKeys

***********************************************************************************************************************)

{ TOptixRefreshRegistryKeys.DeSerialize }
procedure TOptixRefreshRegistryKeys.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FKeys.Clear();
  for var I := 0 to ASerializedObject.A['Keys'].Length -1 do
    FKeys.Add(TRegistryKeyInformation.Create(ASerializedObject.A['Keys'].O[I]));

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

  for var AItem in FKeys do
    AJsonArray.Add(AItem.Serialize);

  result.A['Keys'] := AJsonArray;

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

  FKeys   := TObjectList<TRegistryKeyInformation>.Create(True);
  FValues := TObjectList<TRegistryValueInformation>.Create(True);

  FPath := '';
  FPermissions := [];
end;

{ TOptixRefreshRegistryKeys.Create }
constructor TOptixRefreshRegistryKeys.Create(const APath : String);
begin
  Create();
  ///

  FPath := APath;
  TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(FPath, FPermissions);
end;

{ TOptixRefreshRegistryKeys.Destroy }
destructor TOptixRefreshRegistryKeys.Destroy();
begin
  if Assigned(FKeys) then
    FreeAndNil(FKeys);

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
    FKeys.Add(TRegistryKeyInformation.Create(AHive, AHive));
end;

(***********************************************************************************************************************

  TOptixRefreshRegistrySubKeys

***********************************************************************************************************************)

{ TOptixRefreshRegistrySubKeys.DoAction }
procedure TOptixRefreshRegistrySubKeys.DoAction();
begin
  TOptixEnumRegistry.Enum(FPath, FKeys, FValues);
end;

end.
