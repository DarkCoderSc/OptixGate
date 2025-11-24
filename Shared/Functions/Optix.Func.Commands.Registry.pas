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



unit Optix.Func.Commands.Registry;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Winapi.Windows,

  Optix.Shared.Classes, Optix.Func.Commands.Base, Optix.Registry.Enum, Optix.Registry.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixCommandRegistryActionResponse = class(TOptixCommandActionResponse)
  protected
    [OptixSerializableAttribute]
    FKeyPath : String;
  public
    {@C}
    constructor Create(const AKeyPath : String); overload; virtual;

    {@G}
    property KeyPath : String read FKeyPath;
  end;

  TOptixCommandEnumRegistry = class(TOptixCommandRegistryActionResponse)
  private
    [OptixSerializableAttribute]
    FPermissions : TRegistryKeyPermissions;

    [OptixSerializableAttribute]
    FParentKeys : TObjectList<TRegistryKeyInformation>;

    [OptixSerializableAttribute]
    FSubKeys : TObjectList<TRegistryKeyInformation>;

    [OptixSerializableAttribute]
    FValues : TObjectList<TRegistryValueInformation>;

    {@M}
    function GetIsRoot() : Boolean;
  protected
    {@M}
    procedure AfterCreate(); override;
  public
    {@C}
    destructor Destroy(); override;

    {@G}
    property IsRoot      : Boolean                                read GetIsRoot;
    property ParentKeys  : TObjectList<TRegistryKeyInformation>   read FParentKeys;
    property SubKeys     : TObjectList<TRegistryKeyInformation>   read FSubKeys;
    property Values      : TObjectList<TRegistryValueInformation> read FValues;
    property Permissions : TRegistryKeyPermissions                read FPermissions;
  end;

  TOptixCommandEnumRegistryHives = class(TOptixCommandEnumRegistry)
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
  end;

  TOptixCommandEnumRegistryKeys = class(TOptixCommandEnumRegistry)
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
  end;

  TOptixCommandCreateRegistryKey = class(TOptixCommandEnumRegistryKeys)
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
  end;

  TOptixCommandDeleteRegistryKey = class(TOptixCommandRegistryActionResponse)
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
  end;

  TOptixCommandSetRegistryValue = class(TOptixCommandRegistryActionResponse)
  private
    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FKind : DWORD;

    [OptixSerializableAttribute]
    FNewValue : TRegistryValueInformation;
  protected
    {@M}
    procedure AfterCreate(); override;
  public
    {@C}
    constructor Create(const AKeyPath : String; const AName : String; const AKind : DWORD; const pData : Pointer;
      const ADataSize : UInt64); overload;
    destructor Destroy(); override;

    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}

    {@G}
    property Name        : String                    read FName;
    property Kind        : DWORD                     read FKind;
    property NewValue    : TRegistryValueInformation read FNewValue;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.System.Helper, Optix.FileSystem.Helper, Optix.Exceptions;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixCommandRegistryActionResponse *)

constructor TOptixCommandRegistryActionResponse.Create(const AKeyPath : String);
begin
  inherited Create();
  ///

  FKeyPath := ExcludeTrailingPathDelimiter(AKeyPath);
end;

(* TOptixCommandEnumRegistry *)

procedure TOptixCommandEnumRegistry.AfterCreate();
begin
  FParentKeys := TObjectList<TRegistryKeyInformation>.Create(True);
  FSubKeys    := TObjectList<TRegistryKeyInformation>.Create(True);
  FValues     := TObjectList<TRegistryValueInformation>.Create(True);

  FPermissions := [];
end;

destructor TOptixCommandEnumRegistry.Destroy();
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

function TOptixCommandEnumRegistry.GetIsRoot() : Boolean;
begin
  result := String.IsNullOrWhiteSpace(FKeyPath);
end;

(* TOptixCommandEnumRegistryHives *)

{$IFNDEF SERVER}
procedure TOptixCommandEnumRegistryHives.DoAction();
begin
  for var AHive in TRegistryHelper.RegistryHives.Keys do
    FSubKeys.Add(TRegistryKeyInformation.Create(AHive, AHive));
end;
{$ENDIF}

(* TOptixCommandEnumRegistryKeys *)

{$IFNDEF SERVER}
procedure TOptixCommandEnumRegistryKeys.DoAction();
begin
  TRegistryHelper.CheckRegistryPath(FKeyPath);
  FParentKeys.Clear();
  ///

  TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(FKeyPath, FPermissions);

  TFileSystemHelper.TraverseDirectories(
    FKeyPath,
    (
      procedure (const ADirectoryName : String; const AAbsolutePath : String)
      begin
        FParentKeys.Add(TRegistryKeyInformation.Create(ADirectoryName, AAbsolutePath));
      end
    )
  );

  ///
  TOptixEnumRegistry.Enum(FKeyPath, FSubKeys, FValues);
end;
{$ENDIF}

(* TOptixCommandCreateRegistryKey *)

{$IFNDEF SERVER}
procedure TOptixCommandCreateRegistryKey.DoAction();
begin
  TRegistryHelper.CreateSubKey(FKeyPath);
  ///

  TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(FKeyPath, FPermissions);

  ///
  inherited;
end;
{$ENDIF}

(* TOptixCommandDeleteRegistryKey *)

{$IFNDEF SERVER}
procedure TOptixCommandDeleteRegistryKey.DoAction();
begin
  TRegistryHelper.DeleteKey(FKeyPath);
end;
{$ENDIF}

(* TOptixCommandSetRegistryValue *)

procedure TOptixCommandSetRegistryValue.AfterCreate();
begin
  FNewValue := TRegistryValueInformation.Create();
end;

constructor TOptixCommandSetRegistryValue.Create(const AKeyPath : String; const AName : String; const AKind : DWORD;
  const pData : Pointer; const ADataSize : UInt64);
begin
  inherited Create(AKeyPath);
  ///

  FName := AName;
  FKind := AKind;

  FNewValue := TRegistryValueInformation.Create(AName, AKind, pData, ADataSize);
end;

destructor TOptixCommandSetRegistryValue.Destroy();
begin
  if Assigned(FNewValue) then
    FreeAndNil(FNewValue);

  ///
  inherited;
end;

{$IFNDEF SERVER}
procedure TOptixCommandSetRegistryValue.DoAction();
begin
  if not Assigned(FNewValue) or not Assigned(FNewValue.Value) then
    raise EOptixSystemException.Create('{DBBAF446-0898-4242-B566-86AB8C620B21}');
  ///

  TRegistryHelper.SetValue(
    FKeyPath,
    FName,
    FKind,
    FNewValue.Value.Address,
    FNewValue.Value.Size
  );
end;
{$ENDIF}

end.
