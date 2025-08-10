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

unit Optix.OpenSSL.Helper;

interface

uses Optix.OpenSSL.Headers;

type
  TX509Certificate = record
    pX509        : Pointer;
    pPrivKey     : Pointer;
    Fingerprint  : String;
  end;

  TOpenSSLCertificateKeyType = (
    cktPublic,
    cktPrivate
  );
  TOpenSSLCertificateKeyTypes = set of TOpenSSLCertificateKeyType;

  TOptixOpenSSLHelper = class
  public
    class function NewPrivateKey() : Pointer; static;
    class function NewX509(const pKey : Pointer; const C, O, CN : String) : Pointer; static;
    class procedure NewSelfSignedCertificate(const AOutputFile : String; const C, O, CN : String); static;
    class procedure ExportKeyToFile(const ACertificate : TX509Certificate; const AOutputFile : String; ACertificateTypes : TOpenSSLCertificateKeyTypes); static;
    class procedure LoadCertificate(const ACertificateFile : String; var ACertificate : TX509Certificate); static;
    class procedure LoadCertificateInformation(var ACertificate : TX509Certificate); static;
    class procedure CheckCertificateFile(const ACertificateFile : String); static;
    class function GetPeerSha512Fingerprint(const pSSL : Pointer) : String;
    class function GetX509Sha512Fingerprint(const pX509 : Pointer) : String;
  end;

implementation

uses Winapi.Windows, System.SysUtils, Optix.OpenSSL.Exceptions, Optix.OpenSSL.Context;

{ TOptixOpenSSLHelper.NewPrivateKey }
class function TOptixOpenSSLHelper.NewPrivateKey() : Pointer;
begin
  var pTempKey := EVP_PKEY_new();

  if not Assigned(pTempKey) then
    raise EOpenSSLLibraryException.Create('Could not generate private key structure.');
  try
    var pRSA := RSA_generate_key(4096, RSA_F4, nil, nil);
    if not Assigned(pRSA) then
      raise EOpenSSLLibraryException.Create('Could not generate private key.');

    if EVP_PKEY_assign(pTempKey, 6, pRSA) <> 1 then
      raise EOpenSSLLibraryException.Create('Could not assign private key.');

    ///
    result := pTempKey;
  except
    on E : Exception do begin
      EVP_PKEY_free(pTempKey);

      raise;
    end;
  end;
end;

{ TOptixOpenSSLHelper.NewX509 }
class function TOptixOpenSSLHelper.NewX509(const pKey : Pointer; const C, O, CN : String) : Pointer;
begin
  if not Assigned(pKey) then
    raise Exception.Create('Missing private key structure to generate X509 Certificate.');

  var pTempX509 := PX509(X509_new());
  if not Assigned(pTempX509) then
    raise EOpenSSLLibraryException.Create('Could not generate new certificate.');
  try
    if ASN1_INTEGER_set(X509_get_serialNumber(pTempX509), 1) <> 1 then
      raise EOpenSSLLibraryException.Create('Could not set certificate serial number.');

    X509_gmtime_adj(pTempX509.cert_info.validity.not_before, 0);
    X509_gmtime_adj(pTempX509.cert_info.validity.not_after, 31536000);

    X509_set_pubkey(pTempX509, pKey);

    var pX509Name := X509_get_subject_name(pTempX509);

    X509_NAME_add_entry_by_txt(pX509Name, 'C',  MBSTRING_ASC, PAnsiChar(C), -1, -1, 0);
    X509_NAME_add_entry_by_txt(pX509Name, 'O',  MBSTRING_ASC, PAnsiChar(O), -1, -1, 0);
    X509_NAME_add_entry_by_txt(pX509Name, 'CN', MBSTRING_ASC, PAnsiChar(CN), -1, -1, 0);

    X509_set_issuer_name(pTempX509, pX509Name);

    if X509_sign(pTempX509, pKey, EVP_sha512) <= 0 then
      raise EOpenSSLLibraryException.Create('Could not sign certificate.');

    ///
    result := pTempX509;
  except
    on E : Exception do begin
      X509_free(pTempX509);

      raise;
    end;
  end;
end;

{ TOptixOpenSSLHelper.ExportKeyToFile }
class procedure TOptixOpenSSLHelper.ExportKeyToFile(const ACertificate : TX509Certificate; const AOutputFile : String; ACertificateTypes : TOpenSSLCertificateKeyTypes);
begin
  var pBIO := BIO_new_file(PAnsiChar(AOutputFile), 'wb');
  if not Assigned(pBIO) then
    raise EOpenSSLLibraryException.Create('Could not create output file.');
  try
    if ACertificateTypes = [] then
      ACertificateTypes := [cktPublic, cktPrivate];

    // Private
    if Assigned(ACertificate.pPrivKey) and (cktPrivate in ACertificateTypes) then
      PEM_write_bio_PrivateKey(pBIO, ACertificate.pPrivKey, nil, nil, 0, 0, nil);

    // Public
    if Assigned(ACertificate.pX509) and Assigned(ACertificate.pPrivKey) and (cktPublic in ACertificateTypes) then
      PEM_write_bio_X509(pBIO, ACertificate.pX509);
  finally
    BIO_free(pBIO);
  end;
end;

{ TOptixOpenSSLHelper.NewSelfSignedCertificate }
class procedure TOptixOpenSSLHelper.NewSelfSignedCertificate(const AOutputFile : String; const C, O, CN : String);
begin
  var ACertificate : TX509Certificate;
  ///

  ACertificate.pPrivKey := NewPrivateKey();
  try
    ACertificate.pX509 := NewX509(ACertificate.pPrivKey, C, O, CN);
    try
      ExportKeyToFile(ACertificate, AOutputFile, [cktPublic, cktPrivate]);
    finally
      X509_free(ACertificate.pX509);
    end;
  finally
    EVP_PKEY_free(ACertificate.pPrivKey);
  end;
end;

{ TOptixOpenSSLHelper.LoadCertificate }
class procedure TOptixOpenSSLHelper.LoadCertificate(const ACertificateFile : String; var ACertificate : TX509Certificate);
begin
  ZeroMemory(@ACertificate, SizeOf(TX509Certificate));
  ///

  if not FileExists(ACertificateFile) then
    raise Exception.Create(Format('Certificate file "%s" does not exists.', [ACertificateFile]));

  var pBIO := BIO_new_file(PAnsiChar(ACertificateFile), 'rb');
  try
    // First we read the private key
    ACertificate.pPrivKey := PEM_read_bio_PrivateKey(pBIO, nil, nil, nil);
    if not Assigned(ACertificate.pPrivKey) then
      raise EOpenSSLLibraryException.Create('Could not read Private Key from file.');

    // Then we read the X509 structure
    ACertificate.pX509 := PEM_read_bio_X509(pBIO, nil, nil, nil);
    if not Assigned(ACertificate.pX509) then
      raise EOpenSSLLibraryException.Create('Could not read X509 structure from file.');

    // Retrieve Information about the certificate
    LoadCertificateInformation(ACertificate);
  finally
    BIO_free(pBIO);
  end;
end;

{ TOptixOpenSSLHelper.LoadCertificateInformation }
class procedure TOptixOpenSSLHelper.LoadCertificateInformation(var ACertificate : TX509Certificate);
begin
  if not Assigned(ACertificate.pX509) or
     not Assigned(ACertificate.pPrivKey) then
    raise Exception.Create('X509 structure and Private Key must be present in certificate record.');
  ///

  ACertificate.Fingerprint := GetX509Sha512Fingerprint(ACertificate.pX509);
end;

{ TOptixOpenSSLHelper.CheckCertificateFile }
class procedure TOptixOpenSSLHelper.CheckCertificateFile(const ACertificateFile : String);
var AContext : TOptixOpenSSLContext;
begin
  AContext := TOptixOpenSSLContext.Create(sslClient, ACertificateFile);
  try
    ///
  finally
    if Assigned(AContext) then
      FreeAndNil(AContext);
  end;
end;

{ TOptixOpenSSLHelper.GetPeerSha512Fingerprint }
class function TOptixOpenSSLHelper.GetPeerSha512Fingerprint(const pSSL : Pointer) : String;
begin
  var pX509 := SSL_get_peer_certificate(pSSL);
  if not Assigned(pX509) then
    raise EOpenSSLLibraryException.Create('Could not retrieve peer X509 certificate.');

  ///
  result := GetX509Sha512Fingerprint(pX509);
end;

{ TOptixOpenSSLHelper.GetX509Sha512Fingerprint }
class function TOptixOpenSSLHelper.GetX509Sha512Fingerprint(const pX509 : Pointer) : String;
begin
  var ACertificateFingerpring : TSSL_SHA512;
  if X509_digest(pX509, EVP_sha512(), PByte(@ACertificateFingerpring.Sha512), ACertificateFingerpring.Length) <> 1 then
    raise EOpenSSLLibraryException.Create('Could not retrieve X509 certificate digest.');

  ///

  result := '';
  for var i := 0 to ACertificateFingerpring.Length -1 do begin
    if i <> 0 then
      result := result + ':';

    ///
    result := result + Format('%.2x', [Byte(ACertificateFingerpring.Sha512[i])]);
  end;
end;

end.
