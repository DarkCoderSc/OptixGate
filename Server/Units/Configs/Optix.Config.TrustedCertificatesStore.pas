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

unit Optix.Config.TrustedCertificatesStore;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.JSON,

  Optix.Config.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixConfigTrustedCertificateStore = class;

  TOptixTrustedFingerprintEnumerator = record
  private
    FTarget : TOptixConfigTrustedCertificateStore;
    FIndex  : Integer;

    {@M}
    function GetCurrent() : String;
  public
    {@C}
    constructor Create(ATarget: TOptixConfigTrustedCertificateStore);

    {@M}
    function MoveNext() : Boolean;

    {@G}
    property Current: String read GetCurrent;
  end;

  TOptixConfigTrustedCertificateStore = class(TOptixConfigEnumBase)
  private
    {@M}
    function GetItem(const AIndex: Integer) : String;
  public
    {@M}
    procedure Add(const ATrustedFingerprint : String);
    function GetEnumerator() : TOptixTrustedFingerprintEnumerator;

    {@G}
    property Count : Integer read GetCount;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Generics.Collections,

  Winapi.Windows,

  Optix.Helper;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixTrustedFingerprintEnumerator *)

constructor TOptixTrustedFingerprintEnumerator.Create(ATarget : TOptixConfigTrustedCertificateStore);
begin
  FTarget := ATarget;
  FIndex  := -1;
end;

function TOptixTrustedFingerprintEnumerator.MoveNext() : Boolean;
begin
  Inc(FIndex);

  result := FIndex < FTarget.Count;
end;

function TOptixTrustedFingerprintEnumerator.GetCurrent() : String;
begin
  result := FTarget.GetItem(FIndex);
end;

(* TOptixConfigTrustedCertificateStore *)

function TOptixConfigTrustedCertificateStore.GetItem(const AIndex : Integer) : String;
begin
  result := '';
  if not Assigned(FItems) then
    Exit;
  ///

  if (AIndex < 0) or (AIndex > FItems.Count-1) then
    Exit;

  var ARow := FItems.Items[AIndex];

  var AFingerprint : String;
  if not ARow.TryGetValue('Fingerprint', AFingerprint) then
    Exit;
  ///

  result := AFingerprint;
end;

procedure TOptixConfigTrustedCertificateStore.Add(const ATrustedFingerprint : String);
begin
  if not Assigned(FItems) then
    Exit;
  ///

  var AItem := TJsonObject.Create();
  try
    AItem.AddPair('Fingerprint', ATrustedFingerprint);
  except
    FreeAndNil(AItem);
  end;

  FItems.AddElement(AItem);
end;

function TOptixConfigTrustedCertificateStore.GetEnumerator() : TOptixTrustedFingerprintEnumerator;
begin
  result := TOptixTrustedFingerprintEnumerator.Create(Self);
end;

end.
