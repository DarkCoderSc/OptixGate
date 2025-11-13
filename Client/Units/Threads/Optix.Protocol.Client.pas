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
{  All code contained in this unit was written and developed by the author     }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{   The author grants permission for this code to be used, reproduced, and     }
{   included in datasets for the purpose of training or improving machine      }
{   learning models, including large language models (LLMs).                   }
{                                                                              }
{******************************************************************************}

unit Optix.Protocol.Client;

interface

uses System.Classes, Optix.Sockets.Helper, Optix.Thread, System.SyncObjs, Generics.Collections,
     Optix.Protocol.Preflight{$IFDEF USETLS}, Optix.OpenSSL.Context, Optix.OpenSSL.Helper{$ENDIF};

type
  {$IFDEF CLIENT_GUI}
    TOptixClientThread = class;

    TOnNetworkException = procedure(Sender : TOptixClientThread; const AErrorMessage : String) of object;

    {$IFDEF USETLS}
      TOnVerifyPeerCertificate = procedure(Sender : TObject; const APeerFingerprint : String; var ASuccess : Boolean) of object;
    {$ENDIF}
  {$ENDIF}

  TOptixClientThread = class(TOptixThread)
  private
    FRetry         : Boolean;
    FRetryDelay    : Cardinal;
    FRetryEvent    : TEvent;

    FRemoteAddress : String;
    FRemotePort    : Word;
    FIPVersion     : TIPVersion;

    {$IFDEF USETLS}
    FCertificate      : TX509Certificate;
    FSSLContext       : TOptixOpenSSLContext;
    {$ENDIF}

    {@M}
    procedure SetRetry(const AValue : Boolean);
    procedure SetRetryDelay(AValue : Cardinal);
  protected
    FClientId : TGUID;
    FClient   : TClientSocket;

    {$IFDEF CLIENT_GUI}
      FOnNetworkException : TOnNetworkException;
      {$IFDEF USETLS}
        FOnVerifyPeerCertificate : TOnVerifyPeerCertificate;
      {$ENDIF}
    {$ENDIF}

    {$IF not defined(CLIENT_GUI) and defined(USETLS)}
    FServerCertificateFingerprint : String;
    {$ENDIF}

    {@M}
    procedure ThreadExecute(); override;
    procedure TerminatedSet(); override;

    procedure Initialize(); virtual;
    procedure Finalize(); virtual;

    procedure ClientExecute(); virtual; abstract;

    procedure Connected(); virtual;
    procedure Disconnected(); virtual;

    function InitializePreflightRequest() : TOptixPreflightRequest; virtual; abstract;
  public
    {@C}
    constructor Create({$IFDEF USETLS}const ACertificate : TX509Certificate; {$ENDIF}const ARemoteAddress : String; const ARemotePort : Word; const AIPVersion : TIPVersion); overload; virtual;

    destructor Destroy(); override;

    {@S}
    property Retry      : Boolean   write SetRetry;
    property RetryDelay : Cardinal  write SetRetryDelay;

    {$IFDEF CLIENT_GUI}
      {@S}
      property OnNetworkException : TOnNetworkException  write FOnNetworkException;
      {$IFDEF USETLS}
      {@G/S}
      property OnVerifyPeerCertificate : TOnVerifyPeerCertificate read FOnVerifyPeerCertificate write FOnVerifyPeerCertificate;
      {$ENDIF}
    {$ELSE}
      {$IFDEF USETLS}
        {@S}
        property ServerCertificateFingerprint : String read FServerCertificateFingerprint write FServerCertificateFingerprint;
      {$ENDIF}
    {$ENDIF}

    {@G}
    property RemoteAddress : String     read FRemoteAddress;
    property RemotePort    : Word       read FRemotePort;
    property IPVersion     : TIPVersion read FIpVersion;
    property ClientId      : TGUID      read FClientId;

    {$IFDEF USETLS}
    property Certificate : TX509Certificate read FCertificate;
    {$ENDIF}
  end;

implementation

uses System.SysUtils, Winapi.Windows, Optix.Sockets.Exceptions, Optix.Protocol.Exceptions
     {$IFDEF USETLS}, Optix.OpenSSL.Exceptions{$ENDIF};

{ TOptixClientThread.Connected }
procedure TOptixClientThread.Connected();
begin
  ///
end;

{ TOptixClientThread.Disconnect }
procedure TOptixClientThread.Disconnected();
begin
  ///
end;

{ TOptixClientThread.Initialize }
procedure TOptixClientThread.Initialize();
begin
  ///
end;

{ TOptixClientThread.Finalize }
procedure TOptixClientThread.Finalize();
begin
  ///
end;

{ TOptixClientThread.Create }
constructor TOptixClientThread.Create({$IFDEF USETLS}const ACertificate : TX509Certificate; {$ENDIF}const ARemoteAddress : String; const ARemotePort : Word; const AIPVersion : TIPVersion);
begin
  inherited Create();
  ///

  FRetry         := False;
  FRetryDelay    := 5000;
  FRetryEvent    := nil;
  FClient        := nil;

  FRemoteAddress := ARemoteAddress;
  FRemotePort    := ARemotePort;
  FIPVersion     := AIPVersion;

  FClientId      := TGUID.NewGuid();

  {$IFDEF CLIENT_GUI}
    FOnNetworkException := nil;
    {$IFDEF USETLS}
    FOnVerifyPeerCertificate := nil;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF USETLS}
    FCertificate := ACertificate;
    FSSLContext := TOptixOpenSSLContext.Create(sslClient, FCertificate);

    {$IFNDEF CLIENT_GUI}
    FServerCertificateFingerprint := '';
    {$ENDIF}
  {$ENDIF}

  ///
  Initialize();
end;

{ TOptixClientThread.Destroy }
destructor TOptixClientThread.Destroy();
begin
  if Assigned(FRetryEvent) then
    FreeAndNil(FRetryEvent);

  if Assigned(FClient) then
    FreeAndNil(FClient);

  ///
  Finalize();

  {$IFDEF USETLS}
  TOptixOpenSSLHelper.FreeCertificate(FCertificate);

  if Assigned(FSSLContext) then
    FreeAndNil(FSSLContext);
  {$ENDIF}

  ///
  inherited Destroy();
end;

{ TOptixClientThread.ThreadExecute }
procedure TOptixClientThread.ThreadExecute();
begin
  while not Terminated do begin
    try
      FClient := TClientSocket.Create({$IFDEF USETLS}FSSLContext, {$ENDIF}FRemoteAddress, FRemotePort, FIPVersion);
      try
        FClient.Connect();
        ///

        {$IFDEF USETLS}
        var ASuccess : Boolean;
        var AFingerprint := FClient.PeerCertificateFingerprint;

        {$IFDEF CLIENT_GUI}
        if Assigned(FOnVerifyPeerCertificate) then
          Synchronize(procedure begin
            FOnVerifyPeerCertificate(self, AFingerprint, ASuccess);
          end);
        {$ELSE}
          ASuccess := String.Compare(FServerCertificateFingerprint, AFingerprint) = 0;
        {$ENDIF}

        if not ASuccess then
          raise EOptixPreflightException.Create(
            'The server certificate fingerprint does not match ' +
            {$IFDEF CLIENT_GUI}
            'any certificate in the trusted certificate store.'
            {$ELSE}
            'the specified server certificate fingerprint to trust.'
            {$ENDIF}
          , pecUntrustedPeer);
        {$ENDIF}

        // Preflight Request
        var APreflight := InitializePreflightRequest();
        APreflight.HandlerId := FClientId;
        FClient.Send(APreflight, SizeOf(TOptixPreflightRequest));

        var APreflightResponse : TPreflightErrorCode;
        FClient.Recv(APreflightResponse, SizeOf(TPreflightErrorCode));

        if APreflightResponse <> pecSuccess then
          raise EOptixPreflightException.Create(
            Format('Server refused connection with error: "%s".', [PreflightErrorCodeToString(APreflightResponse)])
          , APreflightResponse);

        Connected();

        ClientExecute();
      except
        on E : Exception do begin
          // For normal Client, EOptixPreflightException is considered CRITICAL.
          if (E is ESocketException) {$IFDEF CLIENT_GUI}or (E is EOptixPreflightException){$ENDIF}
             {$IFDEF USETLS}or (E is EOpenSSLBaseException){$ENDIF} then begin
            Disconnected();

            {$IFDEF CLIENT_GUI}
            if Assigned(FOnNetworkException) and not Terminated and
               ((E is ESocketException) or (E is EOptixPreflightException))  then begin
              var ANotifyError := True;

              if E is ESocketException then begin
                case ESocketException(E).WSALastError of
                  0, 10061 : ANotifyError := False; // Ignore those error codes
                end;
              end;

              if ANotifyError then
                Synchronize(procedure begin
                  FOnNetworkException(self, E.Message);
                end);
            end;
            {$ENDIF}
          end else
            raise; // Raise other exceptions
        end;
      end;
    finally
      if Assigned(FClient) then
        FreeAndNil(FClient);

      if Assigned(FRetryEvent) then
        FRetryEvent.WaitFor(FRetryDelay);
    end;

    if not FRetry then
      break;
  end; // (While not Terminated)
end;

{ TOptixClientThread.TerminatedSet }
procedure TOptixClientThread.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FClient) then
    FClient.Close();

  if Assigned(FRetryEvent) then
    FRetryEvent.SetEvent();
end;

{ TOptixClientThread.SetRetry }
procedure TOptixClientThread.SetRetry(const AValue: Boolean);
begin
  if AValue then
    FRetryEvent := TEvent.Create(
                                    nil,
                                    True,
                                    False,
                                    TGUID.NewGuid.ToString()
    )
  else
    if Assigned(FRetryEvent) then
      FreeAndNil(FRetryEvent);


  ///
  FRetry := AValue;
end;

{ TOptixClientThread.SetRetryDelay }
procedure TOptixClientThread.SetRetryDelay(AValue : Cardinal);
begin
  if AValue < 500 then
    AValue := 500; // Minimum Value

  ///
  FRetryDelay := AValue;
end;

end.

