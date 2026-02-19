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



unit OptixCore.OpenSSL.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  OptixCore.OpenSSL.Headers;
// ---------------------------------------------------------------------------------------------------------------------

type
  TX509Certificate = record
    pX509        : Pointer;
    pPrivKey     : Pointer;
    Fingerprint  : String;
    C            : String;
    O            : String;
    CN           : String;
  end;
  PX509Certificate = ^TX509Certificate;

  TOpenSSLCertificateKeyType = (
    cktPublic,
    cktPrivate
  );
  TOpenSSLCertificateKeyTypes = set of TOpenSSLCertificateKeyType;

  TOptixOpenSSLHelper = class
  public
    class function NewPrivateKey() : Pointer; static;
    class function NewX509(const pKey : Pointer; const C, O, CN : String) : Pointer; static;
    class procedure LoadCertificate(const APublicKey : String; const APrivateKey : String; var ACertificate : TX509Certificate); static;
    class procedure ImportCertificate(const ACertificateFile : String; var ACertificate : TX509Certificate); static;
    class procedure ExportCertificate(const ADestinationFile : String; var ACertificate : TX509Certificate; AExportWhich : TOpenSSLCertificateKeyTypes = []); static;
    class procedure RetrieveCertificateInformation(var ACertificate : TX509Certificate); static;
    class procedure CheckCertificateFile(const ACertificateFile : String); static;
    class function GetPeerSha512Fingerprint(const pSSLConnection : Pointer) : String; static;
    class function GetX509Sha512Fingerprint(const pX509 : Pointer) : String; static;
    class procedure CopyCertificate(const ASource : TX509Certificate; var ADest : TX509Certificate); static;
    class procedure FreeCertificate(var ACertificate : TX509Certificate); static;
    class function SerializeCertificateKey(const ACertificate : TX509Certificate; ACertificateType : TOpenSSLCertificateKeyType) : String; static;
    class function SerializePublicKey(const ACertificate : TX509Certificate) : String; static;
    class function SerializePrivateKey(const ACertificate : TX509Certificate) : String; static;
    class procedure SerializeKeys(const ACertificate : TX509Certificate; var APublicKey : String; var APrivateKey : String); static;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows,

  OptixCore.OpenSSL.Exceptions, OptixCore.OpenSSL.Context;
// ---------------------------------------------------------------------------------------------------------------------

//class function TOptixOpenSSLHelper.NewPrivateKey() : Pointer;
//begin
//  var pTempKey := EVP_PKEY_new();
//  if not Assigned(pTempKey) then
//    raise EOpenSSLBaseException.Create();
//  try
//    var pRSA := RSA_generate_key(4096, RSA_F4, nil, nil);
//    if not Assigned(pRSA) then
//      raise EOpenSSLBaseException.Create();
//
//    if EVP_PKEY_assign(pTempKey, 6, pRSA) <> 1 then
//      raise EOpenSSLBaseException.Create();
//
//    ///
//    result := pTempKey;
//  except
//    on E : Exception do begin
//      EVP_PKEY_free(pTempKey);
//
//      raise;
//    end;
//  end;
//end;

class function TOptixOpenSSLHelper.NewPrivateKey() : Pointer;
begin
  var pTempKey := nil;

  var AContext := EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nil);
  if not Assigned(AContext) then
    raise EOpenSSLBaseException.Create();
  try
    if EVP_PKEY_keygen_init(AContext) <= 0 then
      raise EOpenSSLBaseException.Create();

    if EVP_PKEY_CTX_ctrl(AContext, -1, EVP_PKEY_CTRL_RSA_KEYGEN_BITS, 4096, nil) <= 0 then
      raise EOpenSSLBaseException.Create();

    if EVP_PKEY_keygen(AContext, pTempKey) <= 0 then
      raise EOpenSSLBaseException.Create();

    Result := pTempKey;
  finally
    EVP_PKEY_CTX_free(AContext);
  end;
end;

class function TOptixOpenSSLHelper.NewX509(const pKey : Pointer; const C, O, CN : String) : Pointer;
begin
  var pTempX509 := PX509(X509_new());
  if not Assigned(pTempX509) then
    raise EOpenSSLBaseException.Create();
  try
    if ASN1_INTEGER_set(X509_get_serialNumber(pTempX509), 1) <> 1 then
      raise EOpenSSLBaseException.Create();

    X509_gmtime_adj(pTempX509.cert_info.validity.not_before, 0);
    X509_gmtime_adj(pTempX509.cert_info.validity.not_after, 31536000);

    X509_set_pubkey(pTempX509, pKey);

    var pSubject := X509_get_subject_name(pTempX509);

    X509_NAME_add_entry_by_txt(pSubject, 'C',  MBSTRING_ASC, PAnsiChar(AnsiString(C)), -1, -1, 0);
    X509_NAME_add_entry_by_txt(pSubject, 'O',  MBSTRING_ASC, PAnsiChar(AnsiString(O)), -1, -1, 0);
    X509_NAME_add_entry_by_txt(pSubject, 'CN', MBSTRING_ASC, PAnsiChar(AnsiString(CN)), -1, -1, 0);

    X509_set_issuer_name(pTempX509, pSubject);

    if X509_sign(pTempX509, pKey, EVP_sha512) <= 0 then
      raise EOpenSSLBaseException.Create();

    ///
    result := pTempX509;
  except
    on E : Exception do begin
      X509_free(pTempX509);

      raise;
    end;
  end;
end;

class procedure TOptixOpenSSLHelper.LoadCertificate(const APublicKey : String; const APrivateKey : String; var ACertificate : TX509Certificate);

  function LoadKey(const AKey : String; const AKeyType : TOpenSSLCertificateKeyType) : Pointer;
  begin
    result := nil;
    ///

    var pBio := BIO_new_mem_buf(PAnsiChar(AnsiString(AKey)), Length(AKey));
    if not Assigned(pBio) then
      raise EOpenSSLBaseException.Create();
    try
      case AKeyType of
        cktPublic  : result := PEM_read_bio_X509(pBIO, nil, nil, nil);
        cktPrivate : result := PEM_read_bio_PrivateKey(pBIO, nil, nil, nil);
      end;

      if not Assigned(result) then
        raise EOpenSSLBaseException.Create();
    finally
      BIO_free(pBIO);
    end;
  end;

begin
  ZeroMemory(@ACertificate, SizeOf(TX509Certificate));
  ///

  ACertificate.pX509    := LoadKey(APublicKey, cktPublic);
  ACertificate.pPrivKey := LoadKey(APrivateKey, cktPrivate);

  // Retrieve Information about the certificate
  RetrieveCertificateInformation(ACertificate);
end;

class procedure TOptixOpenSSLHelper.ImportCertificate(const ACertificateFile : String; var ACertificate : TX509Certificate);
begin
  ZeroMemory(@ACertificate, SizeOf(TX509Certificate));
  ///

  var pBIO := BIO_new_file(PAnsiChar(AnsiString(ACertificateFile)), 'rb');
  if not Assigned(pBIO) then
    raise EOpenSSLBaseException.Create();
  try
    // First we read the private key
    ACertificate.pPrivKey := PEM_read_bio_PrivateKey(pBIO, nil, nil, nil);
    if not Assigned(ACertificate.pPrivKey) then
      raise EOpenSSLPrivateKeyException.Create();

    // Then we read the X509 structure
    ACertificate.pX509 := PEM_read_bio_X509(pBIO, nil, nil, nil);
    if not Assigned(ACertificate.pX509) then
      raise EOpenSSLPublicKeyException.Create();

    // Retrieve Information about the certificate
    RetrieveCertificateInformation(ACertificate);
  finally
    BIO_free(pBIO);
  end;
end;

class procedure TOptixOpenSSLHelper.ExportCertificate(const ADestinationFile : String; var ACertificate : TX509Certificate; AExportWhich : TOpenSSLCertificateKeyTypes = []);
begin
  if AExportWhich = [] then
    AExportWhich := [cktPublic, cktPrivate];
  ///

  var pBIO := BIO_new_file(PAnsiChar(AnsiString(ADestinationFile)), 'wb');
  if not Assigned(pBIO) then
    raise EOpenSSLBaseException.Create();
  try
    if cktPrivate in AExportWhich then
      if PEM_write_bio_PrivateKey(pBIO, ACertificate.pPrivKey, nil, nil, 0, nil, nil) <> 1 then
        raise EOpenSSLBaseException.Create();

    if cktPublic in AExportWhich then
      if PEM_write_bio_X509(pBIO, ACertificate.pX509) <> 1 then
        raise EOpenSSLBaseException.Create();
  finally
    BIO_free(pBIO);
  end;
end;

class procedure TOptixOpenSSLHelper.RetrieveCertificateInformation(var ACertificate : TX509Certificate);

  function GetTextFieldByNID(const pSubject : Pointer; const ANID : Integer) : String;
  begin
    result := '';
    ///

    if not Assigned(pSubject) then
      Exit();

    var pBuffer : PAnsiChar;
    var ABufferSize := 1024;

    GetMem(pBuffer, ABufferSize);
    try
      var AIndex := X509_NAME_get_index_by_NID(pSubject, ANID, -1);
      case AIndex of
        -1 : Exit();
        -2 : raise EOpenSSLBaseException.Create();
      end;

      var AReturnLength := X509_NAME_get_text_by_NID(pSubject, ANID, pBuffer, ABufferSize);
      if AReturnLength < 0 then
        raise EOpenSSLBaseException.Create();

      ///
      SetString(result, pBuffer, AReturnLength);
    finally
      FreeMem(pBuffer, ABufferSize);
    end;
  end;

begin
  if not Assigned(ACertificate.pX509) or
     not Assigned(ACertificate.pPrivKey) then
    Exit();
  ///

  ACertificate.Fingerprint := GetX509Sha512Fingerprint(ACertificate.pX509);

  var pSubject := X509_get_subject_name(ACertificate.pX509);
  if not Assigned(pSubject) then
    raise EOpenSSLBaseException.Create();

  ACertificate.C := GetTextFieldByNID(pSubject, NID_countryName);
  ACertificate.O  := GetTextFieldByNID(pSubject, NID_organizationName);
  ACertificate.CN := GetTextFieldByNID(pSubject, NID_commonName);
end;

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

class function TOptixOpenSSLHelper.GetPeerSha512Fingerprint(const pSSLConnection : Pointer) : String;
begin
  var pX509 := SSL_get_peer_certificate(pSSLConnection);
  if not Assigned(pX509) then
    raise EOpenSSLBaseException.Create();
  try
    result := GetX509Sha512Fingerprint(pX509);
  finally
    X509_free(pX509);
  end;
end;

class function TOptixOpenSSLHelper.GetX509Sha512Fingerprint(const pX509 : Pointer) : String;
begin
  var ACertificateFingerpring : TSSL_SHA512;
  if X509_digest(pX509, EVP_sha512(), PByte(@ACertificateFingerpring.Sha512), ACertificateFingerpring.Length) <> 1 then
    raise EOpenSSLBaseException.Create();
  ///

  result := '';
  for var i := 0 to ACertificateFingerpring.Length -1 do begin
    if i <> 0 then
      result := result + ':';

    ///
    result := result + Format('%.2x', [Byte(ACertificateFingerpring.Sha512[i])]);
  end;
end;

class procedure TOptixOpenSSLHelper.CopyCertificate(const ASource : TX509Certificate; var ADest : TX509Certificate);
begin
  FreeCertificate(ADest);
  ///

  ZeroMemory(@ADest, SizeOf(TX509Certificate));
  ///

  ADest.Fingerprint := Copy(ASource.Fingerprint, 1, Length(ASource.Fingerprint));
  ADest.C           := Copy(ASource.C, 1, Length(ASource.C));
  ADest.O           := Copy(ASource.O, 1, Length(ASource.O));
  ADest.CN          := Copy(ASource.CN, 1, Length(ASource.CN));

  if ASource.pX509 <> nil then begin
    X509_up_ref(ASource.pX509);

    ADest.pX509 := ASource.pX509;
  end;

  if ASource.pPrivKey <> nil then begin
    EVP_PKEY_up_ref(ASource.pPrivKey);

    ADest.pPrivKey := ASource.pPrivKey;
  end;
end;

class procedure TOptixOpenSSLHelper.FreeCertificate(var ACertificate : TX509Certificate);
begin
  if ACertificate.pX509 <> nil then
    X509_free(ACertificate.pX509);

  if ACertificate.pPrivKey <> nil then
    EVP_PKEY_free(ACertificate.pPrivKey);
end;

class function TOptixOpenSSLHelper.SerializeCertificateKey(const ACertificate : TX509Certificate; ACertificateType : TOpenSSLCertificateKeyType) : String;
begin
  result := '';
  ///

  var pBIO := BIO_new(BIO_s_mem());
  if not Assigned(pBIO) then
    raise EOpenSSLBaseException.Create();
  try
    var AResult : Integer;

    case ACertificateType of
      cktPublic  : begin
        if not Assigned(ACertificate.pX509) then
          Exit();
        ///

        AResult := PEM_write_bio_X509(pBIO, ACertificate.pX509);
      end;

      cktPrivate : begin
        if not Assigned(ACertificate.pPrivKey) then
          Exit();
        ///

        AResult := PEM_write_bio_PrivateKey(pBIO, ACertificate.pPrivKey, nil, nil, 0, nil, nil);
      end

      else
        Exit();
    end;
    if AResult <> 1 then
      raise EOpenSSLBaseException.Create();

    var pBuffer := PAnsiChar(nil);
    var ABufferLength := BIO_get_mem_data(pBIO, pBuffer);

    ///
    SetString(result, pBuffer, ABufferLength);
  finally
    BIO_free(pBIO);
  end;
end;

class function TOptixOpenSSLHelper.SerializePublicKey(const ACertificate : TX509Certificate) : String;
begin
  result := SerializeCertificateKey(ACertificate, cktPublic);
end;

class function TOptixOpenSSLHelper.SerializePrivateKey(const ACertificate : TX509Certificate) : String;
begin
  result := SerializeCertificateKey(ACertificate, cktPrivate);
end;

class procedure TOptixOpenSSLHelper.SerializeKeys(const ACertificate : TX509Certificate; var APublicKey : String; var APrivateKey : String);
begin
  APublicKey  := '';
  APrivateKey := '';
  ///

  if not Assigned(ACertificate.pX509) or not ASsigned(ACertificate.pPrivKey) then
    Exit();

  APublicKey  := SerializePublicKey(ACertificate);
  APrivateKey := SerializePrivateKey(ACertificate);
end;

end.
