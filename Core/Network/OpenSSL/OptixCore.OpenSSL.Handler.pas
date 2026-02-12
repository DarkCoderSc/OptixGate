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



unit OptixCore.OpenSSL.Handler;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  Winapi.Winsock2,

  OptixCore.OpenSSL.Context;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixOpenSSLHandler = class
  private
    FContext       : TOptixOpenSSLContext;
    FSSLConnection : Pointer;
    FSocketFd      : TSocket;

    {@M}
    procedure SSLFree();
    function GetPeerCertificateFingerprint() : String;
  public
    {@C}
    constructor Create(AContext : TOptixOpenSSLContext; const ASocketFd : TSocket);
    destructor Destroy(); override;

    {@M}
    procedure Connect();
    function IsDataPending() : Boolean;
    function IsConnectionAlive() : Boolean;
    procedure Send(const buf; const len : Integer);
    procedure Recv(var buf; const len : Integer);

    {@G}
    property PeerCertificateFingerprint : String read GetPeerCertificateFingerprint;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows,

  OptixCore.OpenSSL.Exceptions, OptixCore.OpenSSL.Headers, OptixCore.OpenSSL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

constructor TOptixOpenSSLHandler.Create(AContext : TOptixOpenSSLContext; const ASocketFd : TSocket);
begin
  inherited Create();
  ///

  FContext  := AContext;
  FSocketFd := ASocketFd;

  FSSLConnection := nil;
end;

destructor TOptixOpenSSLHandler.Destroy();
begin
  SSLFree();

  ///
  inherited Destroy();
end;

procedure TOptixOpenSSLHandler.SSLFree();
begin
  if Assigned(FSSLConnection) then begin
    SSL_free(FSSLConnection);

    FSSLConnection := nil;
  end;
end;

procedure TOptixOpenSSLHandler.Connect();
begin
  if not Assigned(FContext) or (FSocketFd = INVALID_SOCKET) then
    Exit();
  ///

  if Assigned(FSSLConnection) then
    SSLFree();

  FSSLConnection := SSL_new(FContext.Context);
  if not Assigned(FSSLConnection) then
    raise EOpenSSLBaseException.Create();

  if SSL_set_fd(FSSLConnection, FSocketFd) <> 1 then
    raise EOpenSSLBaseException.Create();

  var AReturn : Integer;

  case FContext.Method of
    sslClient : AReturn := SSL_connect(FSSLConnection);
    sslServer : AReturn := SSL_accept(FSSLConnection);
    else
      Exit();
  end;

  if AReturn <> 1 then
    raise EOpenSSLBaseException.Create.Create(FSSLConnection);
end;

procedure TOptixOpenSSLHandler.Send(const buf; const len : Integer);
begin
  while True do begin
    var AReturn := SSL_Write(FSSLConnection, buf, len);
    if AReturn > 0 then
      break;
    ///

    case SSL_get_error(FSSLConnection, AReturn) of
      SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE :
        continue;

      else
        raise EOpenSSLBaseException.Create(FSSLConnection);
    end;
  end;
end;

procedure TOptixOpenSSLHandler.Recv(var buf; const len : Integer);
begin
  while True do begin
    var AReturn := SSL_Read(FSSLConnection, buf, len);
    if AReturn > 0 then
      break;
    ///

    case SSL_get_error(FSSLConnection, AReturn) of
      SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE :
        continue;

      else
        raise EOpenSSLBaseException.Create(FSSLConnection);
    end;
  end;
end;

function TOptixOpenSSLHandler.IsDataPending() : Boolean;
begin
  result := SSL_pending(FSSLConnection) > 0;
end;

function TOptixOpenSSLHandler.IsConnectionAlive() : Boolean;
begin
  var ADummyBuffer : array[0..0] of Byte;

  var AReturn := SSL_peek(FSSLConnection, @ADummyBuffer, 1);
  if AReturn > 0 then
    result := True
  else begin
    var AError := SSL_get_error(FSSLConnection, AReturn);
    case AError of
      SSL_ERROR_ZERO_RETURN : result := False;

      SSL_ERROR_WANT_READ,
      SSL_ERROR_WANT_WRITE : result := True;
    else
      raise EOpenSSLBaseException.Create(FSSLConnection);
    end;
  end;
end;

function TOptixOpenSSLHandler.GetPeerCertificateFingerprint() : String;
begin
  result := '';
  ///

  if Assigned(FSSLConnection) then
    result := TOptixOpenSSLHelper.GetPeerSha512Fingerprint(FSSLConnection);
end;

end.
