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

unit Optix.Config.Servers;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.JSON,

  uFormServers,

  Optix.Config.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixConfigServers = class;

  TOptixServerConfigurationEnumerator = record
  private
    FTarget : TOptixConfigServers;
    FIndex  : Integer;

    {@M}
    function GetCurrent() : TServerConfiguration;
  public
    {@C}
    constructor Create(ATarget: TOptixConfigServers);

    {@M}
    function MoveNext() : Boolean;

    {@G}
    property Current: TServerConfiguration read GetCurrent;
  end;

  TOptixConfigServers = class(TOptixConfigEnumBase)
  private
    {@M}
    function GetItem(const AIndex : Integer) : TServerConfiguration;
  public
    {@M}
    procedure Add(const AServerConfiguration : TServerConfiguration);
    function GetEnumerator() : TOptixServerConfigurationEnumerator;

    {@G}
    property Count : Integer read GetCount;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Generics.Collections,

  Winapi.Windows,

  OptixCore.Sockets.Helper, Optix.Helper, OptixCore.Exceptions;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixServerConfigurationEnumerator *)

constructor TOptixServerConfigurationEnumerator.Create(ATarget: TOptixConfigServers);
begin
  FTarget := ATarget;
  FIndex  := -1;
end;

function TOptixServerConfigurationEnumerator.MoveNext() : Boolean;
begin
  Inc(FIndex);

  result := FIndex < FTarget.Count;
end;

function TOptixServerConfigurationEnumerator.GetCurrent() : TServerConfiguration;
begin
  result := FTarget.GetItem(FIndex);
end;

(* TOptixConfigServers *)

function TOptixConfigServers.GetItem(const AIndex : Integer) : TServerConfiguration;
begin
  ZeroMemory(@result, SizeOf(TServerConfiguration));
  if not Assigned(FItems) then
    Exit;
  ///

  if (AIndex < 0) or (AIndex > FItems.Count-1) then
    Exit;

  try
    var ARow := FItems.Items[AIndex];

    var AVersion : Integer;
    if not ARow.TryGetValue('Address', result.Address) or
       not ARow.TryGetValue<Word>('Port', result.Port) or
       not ARow.TryGetValue('Version', AVersion) or
       not ARow.TryGetValue('AutoStart', result.AutoStart)
       {$IFDEF USETLS}
          or not ARow.TryGetValue('CertificateFingerprint', result.CertificateFingerprint)
       {$ENDIF}
    then
      raise EOptixConfigException.Create(oceMissingField);

    {$IFDEF USETLS}
    if not TOptixHelper.IsCertificateFingerprintValid(result.CertificateFingerprint) then
      raise EOptixConfigException.Create(oceInvalidDataFormat);
    {$ENDIF}

    result.VersionAsInt := AVersion;
  except
    ZeroMemory(@result, SizeOf(TServerConfiguration));
  end;
end;

procedure TOptixConfigServers.Add(const AServerConfiguration : TServerConfiguration);
begin
  if not Assigned(FItems) then
    Exit;
  ///

  var AItem := TJsonObject.Create();
  try
    AItem.AddPair('Address', AServerConfiguration.Address);
    AItem.AddPair('Port', AServerConfiguration.Port);
    AItem.AddPair('Version', Cardinal(AServerConfiguration.Version));
    AItem.AddPair('AutoStart', AServerConfiguration.AutoStart);

    {$IFDEF USETLS}
    AItem.AddPair('CertificateFingerprint', AServerConfiguration.CertificateFingerprint);
    {$ENDIF}
  except
    FreeAndNil(AItem);
  end;

  FItems.AddElement(AItem);
end;

function TOptixConfigServers.GetEnumerator() : TOptixServerConfigurationEnumerator;
begin
  result := TOptixServerConfigurationEnumerator.Create(Self);
end;

end.
