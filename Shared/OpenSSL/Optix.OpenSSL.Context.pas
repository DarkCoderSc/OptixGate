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



unit Optix.OpenSSL.Context;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SyncObjs,

  Optix.OpenSSL.Headers, Optix.OpenSSL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOpenSSLMethod = (
    sslClient,
    sslServer
  );

  TOptixOpenSSLContext = class
  private
    FContext : Pointer;
    FMethod  : TOpenSSLMethod;

    {@M}
    procedure CreateContext();
    procedure LoadCertificate(const ACertificateFile : String); overload;
    procedure LoadCertificate(const ACertificate : TX509Certificate); overload;

    {@C}
    constructor Create(const AOpenSSLMethod : TOpenSSLMethod); overload;
  public
    {@C}
    constructor Create(const AOpenSSLMethod : TOpenSSLMethod; const ACertificateFile : String); overload;
    constructor Create(const AOpenSSLMethod : TOpenSSLMethod; const ACertificate : TX509Certificate); overload;
    destructor Destroy(); override;

    {@G}
    property Context : Pointer        read FContext;
    property Method  : TOpenSSLMethod read FMethod;
  end;

  var OPENSSL_VERIFY_CALLBACK_LOCK : TCriticalSection;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, Winapi.Windows,

  Optix.OpenSSL.Exceptions;
// ---------------------------------------------------------------------------------------------------------------------

(* Local *)

function OpenSSLVerifyCallback(AOk: Integer; AContext: Pointer): Integer; cdecl;
begin
  OPENSSL_VERIFY_CALLBACK_LOCK.Acquire();
  try
    result := 1; // I will handle validation myself
  finally
    OPENSSL_VERIFY_CALLBACK_LOCK.Leave();
  end;
end;

(* TOptixOpenSSLContext *)

procedure TOptixOpenSSLContext.CreateContext();
begin
  var pOpenSSLMethod := nil;
  case FMethod of
    sslClient : pOpenSSLMethod := TLS_client_method;
    sslServer : pOpenSSLMethod := TLS_server_method;
  end;

  FContext := SSL_CTX_new(pOpenSSLMethod);
  if not Assigned(FContext) then
    raise EOpenSSLBaseException.Create();

  // You have no choice man!
  var ACipherSuite := 'TLS_AES_256_GCM_SHA384';
  if SSL_CTX_set_ciphersuites(FContext, PAnsiChar(AnsiString(ACipherSuite))) <> 1 then
    raise EOpenSSLLibraryException.Create(Format('Missing cipher suite: "%s"', [ACipherSuite]));

  SSL_CTX_set_verify(FContext, SSL_VERIFY_PEER or SSL_VERIFY_FAIL_IF_NO_PEER_CERT, @OpenSSLVerifyCallback);
end;

procedure TOptixOpenSSLContext.LoadCertificate(const ACertificateFile : String);
begin
  if not Assigned(FContext) then
    Exit();
  ///

  // Load Public Key
  if SSL_CTX_use_certificate_file(FContext, PAnsiChar(AnsiString(ACertificateFile)), SSL_FILETYPE_PEM) <> 1 then
    raise EOpenSSLBaseException.Create();

  // Load Private Key
  if SSL_CTX_use_PrivateKey_file(FContext, PAnsiChar(AnsiString(ACertificateFile)), SSL_FILETYPE_PEM) <> 1 then
    raise EOpenSSLBaseException.Create();

  // Check if key pair match
  if SSL_CTX_check_private_key(FContext) <> 1 then
    raise EOpenSSLBaseException.Create();
end;

procedure TOptixOpenSSLContext.LoadCertificate(const ACertificate : TX509Certificate);
begin
  if SSL_CTX_use_certificate(FContext, ACertificate.pX509) <> 1 then
    raise EOpenSSLBaseException.Create();

  if SSL_CTX_use_PrivateKey(FContext, ACertificate.pPrivKey) <> 1 then
    raise EOpenSSLBaseException.Create();

  if SSL_CTX_check_private_key(FContext) <> 1 then
    raise EOpenSSLBaseException.Create();
end;

constructor TOptixOpenSSLContext.Create(const AOpenSSLMethod : TOpenSSLMethod);
begin
  inherited Create();
  ///

  FContext := nil;
  FMethod  := AOpenSSLMethod;

  CreateContext();
end;

constructor TOptixOpenSSLContext.Create(const AOpenSSLMethod : TOpenSSLMethod; const ACertificateFile : String);
begin
  Create(AOpenSSLMethod);
  ///

  LoadCertificate(ACertificateFile);
end;

constructor TOptixOpenSSLContext.Create(const AOpenSSLMethod : TOpenSSLMethod; const ACertificate : TX509Certificate);
begin
  Create(AOpenSSLMethod);
  ///

  LoadCertificate(ACertificate);
end;

destructor TOptixOpenSSLContext.Destroy();
begin
  if Assigned(FContext) then begin
    SSL_CTX_free(FContext);

    FContext := nil;
  end;

  ///
  inherited Destroy();
end;

initialization
  OPENSSL_VERIFY_CALLBACK_LOCK := TCriticalSection.Create();

finalization
  if Assigned(OPENSSL_VERIFY_CALLBACK_LOCK) then
    FreeAndNil(OPENSSL_VERIFY_CALLBACK_LOCK);

end.
