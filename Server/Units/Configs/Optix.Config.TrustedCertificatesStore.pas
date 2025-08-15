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

unit Optix.Config.TrustedCertificatesStore;

interface

uses XSuperObject, Optix.Config.Helper;

type
  // TODO: Create a custom iterator for..in
  TOptixTrustedConfigCertificatesStore = class(TOptixConfigBase)
  private
    {@M}
    function GetCount() : Integer;

    function GetItem(AIndex: Integer): String;
    procedure SetItem(AIndex: Integer; const AValue: String);
  public
    {@M}
    procedure Add(const AFingerprint : String);

    {@G}
    property Count : Integer read GetCount;
    property Items[AIndex : Integer]: String read GetItem write SetItem; default;
  end;

implementation

uses Winapi.Windows, Optix.Helper;

{ TOptixTrustedConfigCertificatesStore.GetCount }
function TOptixTrustedConfigCertificatesStore.GetCount() : Integer;
begin
  result := 0;
  ///

 if not FJsonObject.Contains('Items') then
  Exit();

  result := FJsonObject.A['Items'].Length;
end;

{ TOptixTrustedConfigCertificatesStore.GetItem }
function TOptixTrustedConfigCertificatesStore.GetItem(AIndex: Integer): String;
begin
  result := '';

  if not FJsonObject.Contains('Items') then
    Exit();
  ///

  var AJsonArray := FJsonObject.A['Items'];

  if (AIndex < 0) or (AIndex > AJsonArray.Length-1) then
    Exit();

  var ANode := AJsonArray.O[AIndex];
  if not ANode.Contains('Fingerprint') then
    Exit();
  try
    var AFingerprint := ANode.S['Fingerprint'];
    CheckCertificateFingerprint(AFingerprint);

    result := AFingerprint;
  except
  end;
end;

{ TOptixTrustedConfigCertificatesStore.Add }
procedure TOptixTrustedConfigCertificatesStore.Add(const AFingerprint : String);
begin
  SetItem(-1, AFingerprint);
end;

{ TOptixTrustedConfigCertificatesStore.SetItem }
procedure TOptixTrustedConfigCertificatesStore.SetItem(AIndex: Integer; const AValue: String);
begin
  var AJsonArray  : ISuperArray;

  if FJsonObject.Contains('Items') then
    AJsonArray := FJsonObject.A['Items']
  else
    AJsonArray := SA();

  var ANode := SO();

  ANode.S['Fingerprint']  := AValue;

  if AIndex < 0 then
    AJsonArray.Add(ANode)
  else if (AIndex >= 0) and (AIndex <= AJsonArray.Length-1) then
    AJsonArray.O[AIndex] := ANode;

  ///
  FJsonObject.A['Items'] := AJsonArray;
end;

end.
