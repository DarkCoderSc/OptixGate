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



unit OptixCore.OpenSSL.Headers;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  Winapi.Windows;
// ---------------------------------------------------------------------------------------------------------------------

const LIB_CRYPTO_DLL = {$IFDEF WIN64}'libcrypto-1_1-x64.dll'{$ELSE}'libcrypto-1_1.dll'{$IFEND};
      LIB_SSL_DLL    = {$IFDEF WIN64}'libssl-1_1-x64.dll'{$ELSE}'libssl-1_1.dll'{$IFEND};

type
  //--------------------------------------------------------------------------------------------------------------------

  cuint = longword;

  {$IFDEF WIN64}
    culong = Int64;
    clong  = Int64;
  {$ELSE}
    culong = Cardinal;
    clong  = Longint;
  {$IFEND}

  TSSL_SHA512 = record
    Length : cuint;
    Sha512 : array [0..64-1] of AnsiChar;
  end;

  TX509Validity = record
    not_before : Pointer;
    not_after  : Pointer;
  end;
  PX509Validity = ^TX509Validity;

  ASN1_STRING = record
    length : Integer;
    _type  : Integer;
    data   : PAnsiChar;
    flags  : ulong;
  end;

   X509_ALGOR = record
    algorithm : Pointer;
    parameter : Pointer;
  end;

  TX509CertInfo = record
    version       : Pointer;
    serialNumber  : ASN1_STRING;
    signature     : X509_ALGOR;
    issuer        : Pointer;
    validity      : TX509Validity;

    // ... //
  end;
  PX509CertInfo = ^TX509CertInfo;

  TX509 = record
    cert_info : TX509CertInfo;
  end;
  PX509 = ^TX509;

//----------------------------------------------------------------------------------------------------------------------

const SSL_FILETYPE_PEM                = 1;
      SSL_ERROR_WANT_READ             = 2;
      SSL_ERROR_WANT_WRITE            = 3;
      SSL_ERROR_ZERO_RETURN           = 6;
      SSL_ERROR_SSL                   = 1;
      SSL_ERROR_NONE                  = 0;
      SSL_VERIFY_PEER                 = $01;
      SSL_VERIFY_FAIL_IF_NO_PEER_CERT = $02;
      RSA_F4                          = $10001;
      MBSTRING_ASC                    = $1000 or 1;
      NID_commonName                  = 13;
      NID_countryName                 = 14;
      NID_organizationName            = 17;
      EVP_PKEY_RSA                    = 6;
      EVP_PKEY_CTRL_RSA_KEYGEN_BITS   = 4097;

//----------------------------------------------------------------------------------------------------------------------

(* libssl.dll *)

function SSL_CTX_new(pMethod: Pointer): Pointer cdecl; external LIB_SSL_DLL;
function SSL_CTX_use_certificate_file(ctx: Pointer; const file_: PAnsiChar; type_: Integer): Integer cdecl; external LIB_SSL_DLL;
function SSL_CTX_use_PrivateKey_file(ctx: Pointer; const file_: PAnsiChar; type_: Integer): Integer cdecl; external LIB_SSL_DLL;
function SSL_CTX_use_certificate(ctx: Pointer; x: Pointer): Integer; cdecl; external LIB_SSL_DLL;
function SSL_CTX_use_PrivateKey(ctx: Pointer; pkey: Pointer): Integer; cdecl; external LIB_SSL_DLL;
function SSL_CTX_check_private_key(ctx: Pointer): Integer cdecl; external LIB_SSL_DLL;
function SSL_new(ctx: Pointer): Pointer cdecl; external LIB_SSL_DLL;
procedure SSL_CTX_free(ctx: Pointer) cdecl; external LIB_SSL_DLL;
function SSL_set_fd(ssl: Pointer; fd: Integer): Integer cdecl; external LIB_SSL_DLL;
function SSL_accept(ssl: Pointer): Integer cdecl; external LIB_SSL_DLL;
function SSL_read(ssl: Pointer; var buf; num: Integer): Integer cdecl; external LIB_SSL_DLL;
function SSL_peek(ssl: Pointer; buf: Pointer; num: Integer): Integer cdecl; external LIB_SSL_DLL;
function SSL_write(ssl: Pointer; const buf; num: Integer): Integer cdecl; external LIB_SSL_DLL;
procedure SSL_free(ssl: Pointer); cdecl; external LIB_SSL_DLL;
function SSL_CTX_set_ciphersuites(_para1: Pointer; const str: PAnsiChar): Integer cdecl; external LIB_SSL_DLL;
function SSL_connect(ssl: Pointer): Integer cdecl; external LIB_SSL_DLL;
function SSL_get_error(ssl: Pointer; ret: Integer): Integer cdecl; external LIB_SSL_DLL;
function TLS_server_method(): Pointer cdecl; external LIB_SSL_DLL;
function TLS_client_method(): Pointer cdecl; external LIB_SSL_DLL;
function SSL_has_pending(ssl : Pointer): Integer cdecl; external LIB_SSL_DLL;
function SSL_get_peer_certificate(ssl : Pointer): Pointer cdecl; external LIB_SSL_DLL;
procedure SSL_CTX_set_verify(ctx: Pointer; mode: Integer; verify_callback: Pointer) cdecl; external LIB_SSL_DLL;
function SSL_pending(ssl: Pointer): Integer; cdecl; external  LIB_SSL_DLL;

(* libcrypto.dll *)

function ERR_error_string_n (e: ULONG; buf: PAnsiChar; len : size_t): PAnsiChar cdecl; external LIB_CRYPTO_DLL;
function ERR_get_error(): ULONG cdecl; external LIB_CRYPTO_DLL;
function X509_NAME_oneline(a: Pointer; buf: PAnsiChar; size: Integer): PAnsiChar cdecl; external LIB_CRYPTO_DLL;
function EVP_add_cipher(const cipher: Pointer): Integer; cdecl; external LIB_CRYPTO_DLL;
function EVP_aes_256_gcm(): Pointer; cdecl; external LIB_CRYPTO_DLL;
function EVP_add_digest(const digest: Pointer): Integer; cdecl; external LIB_CRYPTO_DLL;
function EVP_sha512(): Pointer cdecl; external LIB_CRYPTO_DLL;
function EVP_sha384(): Pointer; cdecl; external LIB_CRYPTO_DLL;
function EVP_PKEY_new(): Pointer cdecl; external LIB_CRYPTO_DLL;
//function RSA_generate_key(bits: Integer; e: culong; callback: Pointer; cb_arg: Pointer): Pointer cdecl; external LIB_CRYPTO_DLL;
procedure EVP_PKEY_free(pkey: Pointer) cdecl; external LIB_CRYPTO_DLL;
function EVP_PKEY_assign(pkey: Pointer; _type: Integer; key: PAnsiChar): Integer cdecl; external LIB_CRYPTO_DLL;
function X509_new(): Pointer cdecl; external LIB_CRYPTO_DLL;
function ASN1_INTEGER_set(a: Pointer; v: clong): Integer cdecl; external LIB_CRYPTO_DLL;
function X509_get_serialNumber(x: Pointer): Pointer cdecl; external LIB_CRYPTO_DLL;
function X509_gmtime_adj(s: Pointer; adj: clong): Pointer cdecl; external LIB_CRYPTO_DLL;
function X509_set_pubkey(x: Pointer; pkey: Pointer): Integer cdecl; external LIB_CRYPTO_DLL;
function X509_get_subject_name(a: Pointer): Pointer cdecl; external LIB_CRYPTO_DLL;
function X509_NAME_add_entry_by_txt(name: Pointer; const field: PAnsiChar; _type: Integer; const bytes: PAnsiChar; len, loc, _set: Integer): Integer cdecl; external LIB_CRYPTO_DLL;
function X509_NAME_get_text_by_NID(name: Pointer; nid: Integer; buf: PAnsiChar; len: Integer): Integer; cdecl; external LIB_CRYPTO_DLL;
function X509_NAME_get_index_by_NID(name: Pointer; const nid : Integer; lastpos : Integer) : Integer; cdecl; external LIB_CRYPTO_DLL;
function X509_set_issuer_name(x: Pointer; name: Pointer): Integer cdecl; external LIB_CRYPTO_DLL;
function X509_sign(x: Pointer; pkey: Pointer; const md: Pointer): Integer cdecl; external LIB_CRYPTO_DLL;
procedure X509_free(x: Pointer) cdecl; external LIB_CRYPTO_DLL;
function BIO_new_mem_buf(buf: Pointer; len: Integer): Pointer; cdecl; external LIB_CRYPTO_DLL;
function BIO_new_file(const filename: PAnsiChar; const mode: PAnsiChar): Pointer cdecl; external LIB_CRYPTO_DLL;
function BIO_new(bio_type: Pointer): Pointer; cdecl; external LIB_CRYPTO_DLL;
function BIO_s_mem(): Pointer; cdecl; external LIB_CRYPTO_DLL;
function BIO_ctrl(bp: Pointer; cmd: LongInt; larg: LongInt; parg: Pointer): LongInt; cdecl; external LIB_CRYPTO_DLL;
function BIO_free(pBIO: Pointer): Integer cdecl; external LIB_CRYPTO_DLL;
function PEM_write_bio_X509 (pBIO: Pointer; pX509: Pointer): Integer cdecl; external LIB_CRYPTO_DLL;
function PEM_write_bio_PrivateKey(pBIO : Pointer; pKey : Pointer; const enc : Pointer; kstr :PAnsiChar; klen : Integer; cb : Pointer; u : Pointer) : Integer cdecl; external LIB_CRYPTO_DLL;
function PEM_read_bio_X509(pBIO: Pointer; pX509: Pointer; cb: Pointer; u: Pointer): Pointer cdecl; external LIB_CRYPTO_DLL;
function PEM_read_bio_PrivateKey(pBIO : Pointer; pKey : Pointer; cb : Pointer; u : Pointer) : Pointer cdecl; external LIB_CRYPTO_DLL;
function X509_digest(const data: PX509; const _type: Pointer; md: PByte; var len: cuint): Integer cdecl; external LIB_CRYPTO_DLL;
function X509_up_ref(x: Pointer): Integer; cdecl; external LIB_CRYPTO_DLL;
function EVP_PKEY_up_ref(pkey: Pointer): Integer; cdecl; external LIB_CRYPTO_DLL;
function EVP_PKEY_CTX_new_id(id: Integer; e: Pointer): Pointer; cdecl; external LIB_CRYPTO_DLL;
procedure EVP_PKEY_CTX_free(ctx: Pointer); cdecl; external LIB_CRYPTO_DLL;
function EVP_PKEY_keygen_init(ctx: Pointer): Integer; cdecl; external LIB_CRYPTO_DLL;
function EVP_PKEY_CTX_ctrl(ctx: Pointer; typ, cmd, p1: Integer; p2: Pointer): Integer; cdecl; external LIB_CRYPTO_DLL;
function EVP_PKEY_keygen(ctx: Pointer; var ppkey: Pointer): Integer; cdecl; external LIB_CRYPTO_DLL;

//----------------------------------------------------------------------------------------------------------------------

function BIO_get_mem_data(b: Pointer; var pp: PAnsiChar): LongInt;

implementation

(* MACROS *)

{ MACRO.BIO_get_mem_data }
function BIO_get_mem_data(b: Pointer; var pp: PAnsiChar): LongInt;
begin
  const BIO_CTRL_INFO = 3;

  result := BIO_ctrl(b, BIO_CTRL_INFO, 0, @pp);
end;

end.
