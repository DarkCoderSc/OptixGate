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

unit Optix.Config.CertificatesStore;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.JSON,

  Optix.Config.Helper, OptixCore.OpenSSL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixConfigCertificatesStore = class;

  TX509CertificateEnumerator = record
  private
    FTarget : TOptixConfigCertificatesStore;
    FIndex  : Integer;

    {@M}
    function GetCurrent() : TX509Certificate;
  public
    {@C}
    constructor Create(ATarget : TOptixConfigCertificatesStore);

    {@M}
    function MoveNext() : Boolean;

    {@G}
    property Current: TX509Certificate read GetCurrent;
  end;

  TOptixConfigCertificatesStore = class(TOptixConfigEnumBase)
  private
    {@M}
    function GetItem(const AIndex : Integer) : TX509Certificate;
  public
    {@M}
    procedure Add(const ACertificate : TX509Certificate);
    function GetEnumerator() : TX509CertificateEnumerator;

    {@G}
    property Count : Integer read GetCount;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Generics.Collections,

  Winapi.Windows;
// ---------------------------------------------------------------------------------------------------------------------

(* TX509CertificateEnumerator *)

constructor TX509CertificateEnumerator.Create(ATarget: TOptixConfigCertificatesStore);
begin
  FTarget := ATarget;
  FIndex  := -1;
end;

function TX509CertificateEnumerator.MoveNext(): Boolean;
begin
  Inc(FIndex);

  result := FIndex < FTarget.Count;
end;

function TX509CertificateEnumerator.GetCurrent() : TX509Certificate;
begin
  result := FTarget.GetItem(FIndex);
end;

(* TOptixConfigCertificatesStore *)

function TOptixConfigCertificatesStore.GetItem(const AIndex : Integer) : TX509Certificate;
begin
  ZeroMemory(@result, SizeOf(TX509Certificate));
  if not Assigned(FItems) then
    Exit;
  ///

  if (AIndex < 0) or (AIndex > FItems.Count-1) then
    Exit;

  var ARow := FItems.Items[AIndex];

  var APublicKey, APrivateKey : String;
  if not ARow.TryGetValue('PublicKey', APublicKey) or not ARow.TryGetValue('PrivateKey', APrivateKey) then
    Exit;
  try
    TOptixOpenSSLHelper.LoadCertificate(APublicKey, APrivateKey, result);
  except
  end;
end;

procedure TOptixConfigCertificatesStore.Add(const ACertificate : TX509Certificate);
begin
  if not Assigned(FItems) then
    Exit;
  ///

  var AItem := TJsonObject.Create();
  try
    AItem.AddPair('PublicKey', TOptixOpenSSLHelper.SerializeCertificateKey(ACertificate, cktPublic));
    AItem.AddPair('PrivateKey', TOptixOpenSSLHelper.SerializeCertificateKey(ACertificate, cktPrivate));
  except
    FreeAndNil(AItem);
  end;

  FItems.AddElement(AItem);
end;

function TOptixConfigCertificatesStore.GetEnumerator() : TX509CertificateEnumerator;
begin
  result := TX509CertificateEnumerator.Create(Self);
end;

end.
