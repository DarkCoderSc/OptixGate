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



unit Optix.Registry.Enum;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  Winapi.Windows,

  Optix.Shared.Classes, Optix.Registry.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TRegistryKeyInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FACL_SSDL : String;

    [OptixSerializableAttribute]
    FPermissions : TRegistryKeyPermissions;
  public
    {@C}
    constructor Create(const AName : String; const APath : String = ''); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Name        : String                  read FName;
    property ACL_SSDL    : String                  read FACL_SSDL;
    property Permissions : TRegistryKeyPermissions read FPermissions;
  end;

  TRegistryValueInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FValue : String;

    [OptixSerializableAttribute]
    FType : DWORD;

    {@M}
    function GetIsDefault() : Boolean;
    function GetValue() : String;
  public
    {@C}
    constructor Create(const AName, AValue : String; AType : DWORD); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Name      : String  read FName;
    property Value     : String  read GetValue;
    property _Type     : DWORD   read FType;
    property IsDefault : Boolean read GetIsDefault;
  end;

  TOptixEnumRegistry = class
  public
    class procedure Enum(const AKeyFullPath : String; var AKeys : TObjectList<TRegistryKeyInformation>;
      var AValues : TObjectList<TRegistryValueInformation>); static;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Optix.Exceptions, Optix.WinApiEx;
// ---------------------------------------------------------------------------------------------------------------------

(***********************************************************************************************************************

  TRegistryKeyInformation

***********************************************************************************************************************)

{ TRegistryKeyInformation.Create }
constructor TRegistryKeyInformation.Create(const AName : String; const APath : String = '');
begin
  inherited Create();
  ///

  FName     := AName;
  FACL_SSDL := TRegistryHelper.TryGetFileACLString(APath);

  TRegistryHelper.TryGetCurrentUserRegistryKeyAccess(APath, FPermissions);
end;

{ TRegistryKeyInformation.Assign }
procedure TRegistryKeyInformation.Assign(ASource : TPersistent);
begin
  if ASource is TRegistryKeyInformation then begin
    FName        := TRegistryKeyInformation(ASource).FName;
    FACL_SSDL    := TRegistryKeyInformation(ASource).FACL_SSDL;
    FPermissions := TRegistryKeyInformation(ASource).FPermissions;
  end else
    inherited;
end;

(***********************************************************************************************************************

  TRegistryValueInformation

***********************************************************************************************************************)

{ TRegistryValueInformation.Create }
constructor TRegistryValueInformation.Create(const AName, AValue : String; AType : DWORD);
begin
  inherited Create();
  ///

  FName  := AName;
  FValue := AValue;
  FType  := AType;
end;


{ TRegistryValueInformation.Assign }
procedure TRegistryValueInformation.Assign(ASource : TPersistent);
begin
  if ASource is TRegistryValueInformation then begin
    FName  := TRegistryValueInformation(ASource).FName;
    FValue := TRegistryValueInformation(ASource).FValue;
    FType  := TRegistryValueInformation(ASource).FType;
  end else
    inherited;
end;

{ TRegistryValueInformation.GetIsDefault }
function TRegistryValueInformation.GetIsDefault() : Boolean;
begin
  result := FName.IsEmpty();
end;

{ TRegistryValueInformation.GetValue }
function TRegistryValueInformation.GetValue() : String;
begin
  if FType = REG_MULTI_SZ then
    result := FValue.Replace(TRegistryHelper.REG_MSZ_LINE_SEP, #13#10, [rfReplaceAll])
  else
    result := FValue;
end;

(***********************************************************************************************************************

  TOptixEnumRegistry

***********************************************************************************************************************)

{ TOptixEnumRegistry.Enum }
class procedure TOptixEnumRegistry.Enum(const AKeyFullPath : String; var AKeys : TObjectList<TRegistryKeyInformation>;
  var AValues : TObjectList<TRegistryValueInformation>);
begin
  var hOpenedKey : HKEY;
  var ACloseKey := False;

  if not Assigned(AKeys) then
    AKeys := TObjectList<TRegistryKeyInformation>.Create(True)
  else
    AKeys.Clear();

  if not Assigned(AValues) then
    AValues := TObjectList<TRegistryValueInformation>.Create(True)
  else
    AValues.Clear();

  var AHive : HKEY;
  var AKeyPath := '';
  TRegistryHelper.ExtractKeyPathInformation(AKeyFullPath, AHive, AKeyPath);

  var ADefaultValueExists := False;

  if not String.IsNullOrWhiteSpace(AKeyPath) then begin
    var ARet := RegOpenKeyEx(
      AHive,
      PWideChar(AKeyPath),
      0,
      KEY_READ,
      hOpenedKey
    );

    if ARet <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegOpenKeyEx', ARet);

    ACloseKey := True;
  end else
    hOpenedKey := AHive;
  try
    var ASubKeysCount : DWORD;
    var AMaxKeyNameLength : DWORD;

    var AValuesCount : DWORD;
    var AMaxValueNameLength : DWORD;

    var ARet := RegQueryInfoKeyW(
        hOpenedKey,
        nil,
        nil,
        nil,
        @ASubKeysCount,
        @AMaxKeyNameLength,
        nil,
        @AValuesCount,
        @AMaxValueNameLength,
        nil,
        nil,
        nil
    );

    if ARet <> ERROR_SUCCESS then
      raise EWindowsException.Create('RegQueryInfoKeyW', ARet);

    if (ASubKeysCount = 0) and (AValuesCount = 0) then
      Exit();

    // Include terminating NULL characters
    Inc(AMaxKeyNameLength);
    Inc(AMaxValueNameLength);

    var ABufferSize : DWORD;
    if AMaxKeyNameLength > AMaxValueNameLength then
      ABufferSize := AMaxKeyNameLength * SizeOf(WideChar)
    else
      ABufferSize := AMaxValueNameLength * SizeOf(WideChar);

    var pNameBuffer : PWideChar;
    GetMem(pNameBuffer, ABufferSize);
    try
      // Enumerate SubKeys
      if ASubKeysCount > 0 then begin
        for var I := 0 to ASubKeysCount -1 do begin
          ZeroMemory(pNameBuffer, AMaxKeyNameLength * SizeOf(WideChar));

          var AKeyLength := AMaxKeyNameLength;

          ARet := RegEnumKeyExW(hOpenedKey, I, pNameBuffer, AKeyLength, nil, nil, nil, nil);
          if ARet <> ERROR_SUCCESS then
            continue;

          var AKeyName := String(pNameBuffer);

          ///
          AKeys.Add(TRegistryKeyInformation.Create(AKeyName, IncludeTrailingPathDelimiter(AKeyFullPath) + AKeyName));
        end;
      end;

      if AValuesCount > 0 then begin
        for var  I := 0 to AValuesCount -1 do begin
          ZeroMemory(pNameBuffer, AMaxKeyNameLength * SizeOf(WideChar));

          var AValueNameLength := AMaxValueNameLength;

          ARet := RegEnumValueW(hOpenedKey, I, pNameBuffer, AValueNameLength, nil, nil, nil, nil);
          if ARet <> ERROR_SUCCESS then
            continue;

          ///
          var AValueName := String(pNameBuffer);
          if AValueName.IsEmpty then
            ADefaultValueExists := True;
          try
            var AValueKind    : DWORD;
            var ADataAsString : String;

            TRegistryHelper.ReadValueAsGenericString(hOpenedKey, AValueName, AValueKind, ADataAsString);

            AValues.Add(TRegistryValueInformation.Create(AValueName, ADataAsString, AValueKind));
          except
          end;
        end;
      end;
    finally
      FreeMem(pNameBuffer, ABufferSize);
    end;
  finally
    if not ADefaultValueExists then
        AValues.Add(TRegistryValueInformation.Create('', '', REG_SZ));
    ///

    if ACloseKey then
      RegCloseKey(hOpenedKey);
  end;
end;

end.
