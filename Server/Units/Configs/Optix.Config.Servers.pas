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
  XSuperObject,

  uFormServers,

  Optix.Config.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  // TODO: Create a custom iterator for..in
  TOptixConfigServers = class(TOptixConfigBase)
  private
    {@M}
    function GetCount() : Integer;

    function GetItem(AIndex: Integer): TServerConfiguration;
    procedure SetItem(AIndex: Integer; const AServerConfiguration : TServerConfiguration);
  public
    {@M}
    procedure Add(const AServerConfiguration : TServerConfiguration);

    {@G}
    property Count : Integer read GetCount;
    property Items[AIndex : Integer]: TServerConfiguration read GetItem write SetItem; default;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Winapi.Windows,

  Optix.Helper, Optix.Sockets.Helper;
// ---------------------------------------------------------------------------------------------------------------------

function TOptixConfigServers.GetCount() : Integer;
begin
  result := 0;
  ///

 if not FJsonObject.Contains('Items') then
  Exit();

  result := FJsonObject.A['Items'].Length;
end;

function TOptixConfigServers.GetItem(AIndex: Integer): TServerConfiguration;
begin
  ZeroMemory(@result, SizeOf(TServerConfiguration));
  ///

  if not FJsonObject.Contains('Items') then
    Exit();
  ///

  var AJsonArray := FJsonObject.A['Items'];

  if (AIndex < 0) or (AIndex > AJsonArray.Length-1) then
    Exit();

  var ANode := AJsonArray.O[AIndex];
  if not ANode.Contains('Address') or not ANode.Contains('Port') or not ANode.Contains('Version') or
     not ANode.Contains('AutoStart')
     {$IFDEF USETLS} or not ANode.Contains('CertificateFingerprint'){$ENDIF}
  then
    Exit();
  try
    var APort := ANode.I['Port'];
    if (APort < Low(Word)) or (APort > High(Word)) then
      Exit();

    var AVersion := ANode.I['Version'];
    if (AVersion < 0) or (AVersion > 1) then
      Exit();

    result.Address   := ANode.S['Address'];
    result.Port      := ANode.I['Port'];
    result.Version   := TIPVersion(ANode.I['Version']);
    result.AutoStart := ANode.B['AutoStart'];

    {$IFDEF USETLS}
    result.CertificateFingerprint  := ANode.S['CertificateFingerprint'];
    {$ENDIF}
  except
  end;
end;

procedure TOptixConfigServers.Add(const AServerConfiguration : TServerConfiguration);
begin
  SetItem(-1, AServerConfiguration);
end;

procedure TOptixConfigServers.SetItem(AIndex: Integer; const AServerConfiguration : TServerConfiguration);
begin
  var AJsonArray  : ISuperArray;

  if FJsonObject.Contains('Items') then
    AJsonArray := FJsonObject.A['Items']
  else
    AJsonArray := SA();

  var ANode := SO();

  ANode.S['Address']   := AServerConfiguration.Address;
  ANode.I['Port']      := AServerConfiguration.Port;
  ANode.I['Version']   := Cardinal(AServerConfiguration.Version);
  ANode.B['AutoStart'] := AServerConfiguration.AutoStart;

  {$IFDEF USETLS}
  ANode.S['CertificateFingerprint'] := AServerConfiguration.CertificateFingerprint;
  {$ENDIF}

  if AIndex < 0 then
    AJsonArray.Add(ANode)
  else if (AIndex >= 0) and (AIndex <= AJsonArray.Length-1) then
    AJsonArray.O[AIndex] := ANode;

  ///
  FJsonObject.A['Items'] := AJsonArray;
end;

end.
