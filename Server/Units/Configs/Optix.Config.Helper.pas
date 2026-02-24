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

unit Optix.Config.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.Win.Registry, System.JSON,

  Winapi.Windows,

  OptixCore.Interfaces;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixConfigEnumBase = class
  protected
    FItems : TJsonArray;
  public
    {@M}
    procedure Clear();
    function ToString() : String; override;
    function GetCount() : Integer;

    {@C}
    constructor Create(const AJsonString : String = '');
    destructor Destroy(); override;

    {@G}
    property Count : Integer read GetCount;
  end;

  TOptixConfigHelper = class
  private
    FRegistry : TRegistry;
    FKeyName  : String;

    {@M}
    procedure Open();
  public
    {@C}
    constructor Create(const AKeyName : String; const AHive : HKEY);
    destructor Destroy(); override;

    {@}
    procedure Write(const AName : String; const AConfig : TOptixConfigEnumBase);
    function Read(const AName : String) : TOptixConfigEnumBase;
  end;

  {$IF defined(SERVER) or defined(CLIENT_GUI)}
  var CONFIG_HELPER : TOptixConfigHelper;
  {$ENDIF}

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixConfigEnumBase *)

constructor TOptixConfigEnumBase.Create(const AJsonString : String);
begin
  inherited Create();
  ///

  try
    var AJsonValue := TJsonArray.ParseJsonValue(AJsonString);
    if Assigned(AJsonValue) and (AJsonValue is TJsonArray) then
      FItems := AJsonValue as TJsonArray
    else begin
      AJsonValue.Free;

      ///
      FItems := TJsonArray.Create();
    end;
  except
    Clear();
  end;
end;

destructor TOptixConfigEnumBase.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

procedure TOptixConfigEnumBase.Clear();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  FItems := TJsonArray.Create();
end;

function TOptixConfigEnumBase.ToString() : String;
begin
  if Assigned(FItems) then
    result := FItems.ToJson()
  else
    result := '';
end;

function TOptixConfigEnumBase.GetCount() : Integer;
begin
  if Assigned(FItems) then
    result := FItems.Count
  else
    result := 0;
end;

(* TOptixConfigHelper *)

constructor TOptixConfigHelper.Create(const AKeyName : String; const AHive : HKEY);
begin
  inherited Create();
  ///

  FRegistry := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
  FRegistry.RootKey := AHive;

  FKeyName := AKeyName;

  Open();
end;

destructor TOptixConfigHelper.Destroy();
begin
  if Assigned(FRegistry) then
    FreeAndNil(FRegistry);

  ///
  inherited Destroy();
end;

procedure TOptixConfigHelper.Open();
begin
  var AKeyPath := 'Software\' + FKeyName;
  ///

  if String.Compare(FRegistry.CurrentPath, AKeyPath, True) <> 0 then
    FRegistry.OpenKey(AKeyPath, True);
end;

procedure TOptixConfigHelper.Write(const AName : String; const AConfig : TOptixConfigEnumBase);
begin
  if not Assigned(AConfig) then
    Exit();
  ///

  Open();

  FRegistry.WriteString(AName, AConfig.ToString());
end;

function TOptixConfigHelper.Read(const AName : String) : TOptixConfigEnumBase;
begin
  result := nil;
  ///

  Open();
  try
    if FRegistry.ValueExists(AName) then
      result := TOptixConfigEnumBase.Create(FRegistry.ReadString(AName));
  except

  end;
end;

{$IF defined(SERVER) or defined(CLIENT_GUI)}
initialization
  var AKeyName : String;
  var AHive    : HKEY;

  {$IFDEF SERVER}
    AKeyName := 'OptixGate';
  {$ELSE}
    AKeyName := 'OptixGate_ClientGUI';
  {$ENDIF}
  AHive := HKEY_CURRENT_USER;

  CONFIG_HELPER := TOptixConfigHelper.Create(AKeyName, AHive);

finalization
  if Assigned(CONFIG_HELPER) then
    FreeAndNil(CONFIG_HELPER);
{$ENDIF}

end.
