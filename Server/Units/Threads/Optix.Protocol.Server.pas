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

unit Optix.Protocol.Server;

interface

uses System.Classes, System.SyncObjs, Winapi.Winsock2, XSuperObject, Generics.Collections, Optix.Sockets.Helper,
     Optix.Thread, Optix.Protocol.SessionHandler, Optix.Protocol.Preflight
     {$IFDEF USETLS}, Optix.OpenSSL.Context, Optix.OpenSSL.Helper{$ENDIF};

type
  TOptixServerThread = class;

  TOnServerStart    = procedure(Sender : TOptixServerThread; const ASocketFd : TSocket) of object;
  TOnServerStop     = procedure(Sender : TOptixServerThread) of object;
  TOnServerError    = procedure(Sender : TOptixServerThread; const AErrorMessage : String) of object;

  {$IFDEF USETLS}
  TOnVerifyPeerCertificate = procedure(Sender : TObject; const APeerFingerprint : String; var ASuccess : Boolean) of object;
  {$ENDIF}

  TOnRegisterWorker = procedure(
    Sender            : TOptixServerThread;
    const AClient     : TClientSocket;
    const ASessionId  : TGUID;
    const AWorkerKind : TClientKind
  ) of object;

  (* TOptixServerThread *)
  TOptixServerThread = class(TOptixThread)
  private
    FBindAddress             : String;
    FBindPort                : Word;
    FIPVersion               : TIPVersion;
    FServer                  : TServerSocket;

    {$IFDEF USETLS}
    FSSLContext              : TOptixOpenSSLContext;
    FCertificate             : TX509Certificate;
    {$ENDIF}

    FClientSockets           : TList<TSocket>;

    FOnServerStart           : TOnServerStart;
    FOnServerError           : TOnServerError;
    FOnServerStop            : TOnServerStop;

    FOnSessionDisconnect     : TOnSessionDisconnect;
    FOnReceivePacket         : TOnReceivePacket;
    FOnRegisterWorker        : TOnRegisterWorker;

    {$IFDEF USETLS}
    FOnVerifyPeerCertificate : TOnVerifyPeerCertificate;
    {$ENDIF}

    {@M}
    procedure Close();
  protected
    {@M}
    procedure ThreadExecute(); override;
    procedure TerminatedSet(); override;
  public
    {@C}
    constructor Create({$IFDEF USETLS}const ACertificate : TX509Certificate; {$ENDIF}const ABindAddress : String; const ABindPort : Word; const AIPVersion : TIPVersion); overload;
    destructor Destroy(); override;

    {@G/S}
    property OnServerStart           : TOnServerStart           read FOnServerStart           write FOnServerStart;
    property OnServerError           : TOnServerError           read FOnServerError           write FOnServerError;
    property OnServerStop            : TOnServerStop            read FOnServerStop            write FOnServerStop;
    property OnSessionDisconnect     : TOnSessionDisconnect     read FOnSessionDisconnect     write FOnSessionDisconnect;
    property OnReceivePacket         : TOnReceivePacket         read FOnReceivePacket         write FOnReceivePacket;
    property OnRegisterWorker        : TOnRegisterWorker        read FOnRegisterWorker        write FOnRegisterWorker;
    {$IFDEF USETLS}
    property OnVerifyPeerCertificate : TOnVerifyPeerCertificate read FOnVerifyPeerCertificate write FOnVerifyPeerCertificate;
    {$ENDIF}

    {@G}
    property Port : Word read FBindPort;
  end;

implementation

uses Winapi.Windows, System.SysUtils, Optix.Protocol.Exceptions, Optix.Protocol.Worker.FileTransfer,
     Optix.Protocol.Client{$IFDEF USETLS}, Optix.OpenSSL.Exceptions{$ENDIF};

(* TOptixServerThread *)

{ TOptixServerThread.ThreadExecute }
procedure TOptixServerThread.ThreadExecute();
var AClient : TClientSocket;
begin
  try
    try
      FServer := TServerSocket.Create(FBindAddress, FBindPort, FIPVersion);
      FServer.Listen();

      if Assigned(FOnServerStart) then
        Synchronize(procedure begin
          FOnServerStart(self, FServer.Socket);
        end);

      while not Terminated do begin
        AClient := nil;
        try
          AClient := FServer.AcceptClient({$IFDEF USETLS}FSSLContext{$ENDIF});
          ///

          {$IFDEF USETLS}
          if Assigned(FOnVerifyPeerCertificate) then begin
            var ASuccess := False;
            var AFingerprint := AClient.PeerCertificateFingerprint;

            Synchronize(procedure begin
              FOnVerifyPeerCertificate(self, AFingerprint, ASuccess);
            end);

            if not ASuccess then
              raise EOptixPreflightException.Create(
                'The client certificate fingerprint does not match any certificate in the trusted certificate store.',
                pecUntrustedPeer
              );
          end;
          {$ENDIF}

          // Preflight packet
          var APreflight : TOptixPreflightRequest;
          AClient.Recv(APreflight, SizeOf(TOptixPreflightRequest));

          if APreflight.ProtocolVersion <> OPTIX_PROTOCOL_VERSION then
            raise EOptixPreflightException.Create(Format('Client:[%s] / Server:[%s] version mismatch.', [
              APreflight.ProtocolVersion,
              OPTIX_PROTOCOL_VERSION
            ]), pecVersionMismatch);

          // Connection established with remote peer (Success)
          var APreflightAck := pecSuccess;
          AClient.Send(APreflightAck, SizeOf(TPreflightErrorCode));

          // Ensure clients dies when server dies.
          if not FClientSockets.Contains(AClient.Socket) then
            FClientSockets.Add(AClient.Socket);

          if APreflight.ClientKind = ckHandler then begin
            // Main Handler --------------------------------------------------------------------------------------------
            var ASessionHandler := TOptixSessionHandlerThread.Create(AClient, APreflight.HandlerId);

            ASessionHandler.OnSessionDisconnect := OnSessionDisconnect;
            ASessionHandler.OnReceivePacket := OnReceivePacket;

            ASessionHandler.Start();
            // ---------------------------------------------------------------------------------------------------------
          end else if Assigned(FOnRegisterWorker) then
            Synchronize(procedure begin
              FOnRegisterWorker(self, AClient, APreflight.HandlerId, APreflight.ClientKind);
            end)
          else
            FreeAndNil(AClient);
        except
          on E : Exception do begin
            if E is EOptixPreflightException then
              try
                AClient.Send(EOptixPreflightException(E).ErrorCode, SizeOf(TPreflightErrorCode));
              except
              end;

            if Assigned(AClient) then
              FreeAndNil(AClient);
            ///

            if not (E is EOptixPreflightException)
            {$IFDEF USETLS}
            and not (E is EOpenSSLBaseException)
            {$ENDIF}
            then
              break;
          end;
        end;
      end;
    except
      on E : Exception do begin
        if Assigned(FOnServerError) then
          Synchronize(procedure begin
            FOnServerError(self, E.Message);
          end);
      end;
    end;
  finally
    if Assigned(FOnServerStop) then
      Synchronize(procedure begin
        FOnServerStop(self);
      end);

    ///
    self.Close();
  end;
end;

{ TOptixServerThread.Close }
procedure TOptixServerThread.Close();
begin
  if Assigned(FServer) then
    FServer.Close();
end;

{ TOptixServerThread.TerminatedSet }
procedure TOptixServerThread.TerminatedSet();
begin
  self.Close();

  ///
  inherited TerminatedSet();
end;

{ TOptixServerThread.Create }
constructor TOptixServerThread.Create({$IFDEF USETLS}const ACertificate : TX509Certificate; {$ENDIF}const ABindAddress : String; const ABindPort : Word; const AIPVersion : TIPVersion);
begin
  inherited Create();
  ///

  FOnServerStart        := nil;
  FOnServerError        := nil;
  FOnServerStop         := nil;
  FOnSessionDisconnect  := nil;
  FOnReceivePacket      := nil;
  FOnRegisterWorker     := nil;

  FBindAddress := ABindAddress;
  FBindPort    := ABindPort;
  FIPVersion   := AIPVersion;
  FServer      := nil;

  {$IFDEF USETLS}
  FCertificate := ACertificate;
  FSSLContext := TOptixOpenSSLContext.Create(sslServer, FCertificate);

  FOnVerifyPeerCertificate := nil;
  {$ENDIF}

  FClientSockets := TList<TSocket>.Create();
end;

{ TOptixServerThread.Destroy }
destructor TOptixServerThread.Destroy();
begin
  if Assigned(FClientSockets) then begin
    for var ASocket in FClientSockets do begin
      Shutdown(ASocket, SD_BOTH);
      CloseSocket(ASocket);
    end;

    ///
    FreeAndNil(FClientSockets);
  end;

  if Assigned(FServer) then
    FreeAndNil(FServer);

  {$IFDEF USETLS}
  TOptixOpenSSLHelper.FreeCertificate(FCertificate);

  if Assigned(FSSLContext) then
    FreeAndNil(FSSLContext);
  {$ENDIF}

  ///
  inherited Destroy();
end;

end.
