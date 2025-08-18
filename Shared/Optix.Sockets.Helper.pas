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

unit Optix.Sockets.Helper;

interface

uses Winapi.Windows, Winapi.Winsock2, System.Classes, System.SysUtils, XSuperObject, Optix.Protocol.Packet
     {$IFDEF USETLS},Optix.OpenSSL.Handler, Optix.OpenSSL.Context{$ENDIF};

const PACKET_SIZE = 8192;

type
  TClientSocket = class;

  TSocketBase = class
  private
    FSocket     : TSocket;
    FIdentifier : TGUID;
    FData       : Pointer;
  protected
    {@M}
    procedure CreateSocket();
  public
    {@M}
    procedure Close(); overload;

    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@G}
    property Socket     : TSocket read FSocket;
    property Identifier : TGUID   read FIdentifier;

    {@G/S}
    property Data : Pointer read FData write FData;
  end;

  TServerSocket = class(TSocketBase)
  private
    FBindAddress : String;
    FBindPort    : Word;
  public
    {@C}
    constructor Create(const ABindAddress : String; const ABindPort : Word); overload;

    {@M}
    procedure Listen();
    function AcceptClient({$IFDEF USETLS}const ASSLContext : TOptixOpenSSLContext{$ENDIF}) : TClientSocket;

    {@G}
    property BindAddress : String read FBindAddress;
    property BindPort    : Word   read FBindPort;
  end;

  TClientSocket = class(TSocketBase)
  private
    FRemoteAddress : String;
    FRemotePort    : word;

    {$IFDEF USETLS}
    FSSLContext : TOptixOpenSSLContext;
    FSSLHandler : TOptixOpenSSLHandler;
    {$ENDIF}

    {@M}
    procedure GetPeerInformations();

    {$IFDEF USETLS}
    {@C}
    constructor Create(const ASSLContext : TOptixOpenSSLContext); overload;

    {@M}
    function GetPeerCertificateFingerprint() : String;
    {$ENDIF}
  public
    {@C}
    constructor Create({$IFDEF USETLS}const ASSLContext : TOptixOpenSSLContext;{$ENDIF}const ARemoteAddress : String; const ARemotePort : word); overload;
    constructor Create({$IFDEF USETLS}const ASSLContext : TOptixOpenSSLContext;{$ENDIF}const ASocket : TSocket); overload;
    {$IFDEF USETLS}
    destructor Destroy(); override;
    {$ENDIF}

    {@M}
    procedure Send(const buf; len : Integer);
    procedure Recv(var buf; len : Integer);

    function IsDataAvailable(): Boolean;
    function IsSocketAlive() : Boolean;

    procedure SendBuffer(const pValue : Pointer; const ABufferSize : UInt64);
    procedure ReceiveBuffer(var pBuffer : Pointer; var ABufferSize : UInt64);

    procedure SendStream(const AValue : TMemoryStream);
    procedure ReceiveStream(var AValue : TMemoryStream);

    procedure SendString(const AString : String);
    function ReceiveString() : String;

    procedure SendJson(const AJson : ISuperObject); overload;
    procedure SendJson(const AJsonArray : ISuperArray); overload;

    function ReceiveJson() : ISuperObject;
    function ReceiveJsonArray() : ISuperArray;

    procedure SendPacket(const APacket : TOptixPacket);
    procedure ReceivePacket(var APacketBody : ISuperObject; const ABlockUntilDataAvailable : Boolean = False);

    procedure Connect(); overload;

    {@G}
    property RemoteAddress : String read FRemoteAddress;
    property RemotePort    : Word   read FRemotePort;

    {$IFDEF USETLS}
    property PeerCertificateFingerprint : String read GetPeerCertificateFingerprint;
    {$ENDIF}
  end;

implementation

uses Optix.Sockets.Exceptions
     {$IFDEF USETLS}, Optix.OpenSSL.Headers, Optix.OpenSSL.Exceptions, Optix.OpenSSL.Helper{$ENDIF};


(* TSocketBase *)

{ TSocketBase.Create }
constructor TSocketBase.Create();
begin
  self.CreateSocket();
  ///

  FIdentifier := TGUID.NewGuid();

  FData := nil;
end;

{ TSocketBase.Destroy }
destructor TSocketBase.Destroy();
begin
  self.Close();

  ///
  inherited Destroy();
end;

{ TSocketBase.Close }
procedure TSocketBase.Close();
begin
  if FSocket <> INVALID_SOCKET then begin
    Winapi.Winsock2.shutdown(FSocket, SD_BOTH);
    Winapi.Winsock2.closesocket(FSocket);
  end;

  ///
  FSocket := INVALID_SOCKET;
end;

{ TSocketBase.CreateSocket }
procedure TSocketBase.CreateSocket();
begin
  FSocket := INVALID_SOCKET;
  ///

  var ASocket := Winapi.Winsock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (ASocket = INVALID_SOCKET) then
    raise ESocketException.Create('socket');

  var b := True;
  if setsockopt(ASocket, IPPROTO_TCP, TCP_NODELAY, @b, SizeOf(LongBool)) = SOCKET_ERROR then
    raise ESocketException.Create('setsockopt(TCP_NODELAY)');

  var dw := PACKET_SIZE * 2;

  if (setsockopt(ASocket, SOL_SOCKET, SO_RCVBUF, @dw, SizeOf(DWORD)) = SOCKET_ERROR) then
    raise ESocketException.Create('setsockopt(SO_RCVBUF)');

  if (setsockopt(ASocket, SOL_SOCKET, SO_SNDBUF, @dw, SizeOf(DWORD)) = SOCKET_ERROR) then
    raise ESocketException.Create('setsockopt(SO_SNDBUF)');

  ///
  FSocket := ASocket;
end;

(* TClientSocket *)

{ TClientSocket.Create }
constructor TClientSocket.Create({$IFDEF USETLS}const ASSLContext : TOptixOpenSSLContext;{$ENDIF}const ARemoteAddress : String; const ARemotePort : word);
begin
  {$IFNDEF USETLS}inherited{$ENDIF} Create({$IFDEF USETLS}ASSLContext{$ENDIF});
  ///

  FRemoteAddress := ARemoteAddress;
  FRemotePort    := ARemotePort;
end;

{ TClientSocket.Create }
constructor TClientSocket.Create({$IFDEF USETLS}const ASSLContext : TOptixOpenSSLContext;{$ENDIF}const ASocket : TSocket);
begin
  {$IFNDEF USETLS}inherited{$ENDIF} Create({$IFDEF USETLS}ASSLContext{$ENDIF});
  ///

  FSocket := ASocket;

  ///
  self.GetPeerInformations();

  {$IFDEF USETLS}
  FSSLHandler := TOptixOpenSSLHandler.Create(FSSLContext, FSocket);
  FSSLHandler.Connect();
  {$ENDIF}
end;

{ TClientSocket.GetPeerInformations }
procedure TClientSocket.GetPeerInformations();
var ASockAddr    : TSockAddrIn;
    ASockAddrLen : Integer;
begin
  ASockAddrLen := SizeOf(TSockAddrIn);
  if Winapi.Winsock2.getpeername(FSocket, TSockAddr(ASockAddr), ASockAddrLen) <> 0 then
    raise ESocketException.Create('getpeername');

  FRemotePort := ntohs(ASockAddr.sin_port);
  FRemoteAddress := string(inet_ntoa(ASockAddr.sin_addr));
end;

{ TClientSocket.Connect }
procedure TClientSocket.Connect();
var ASockAddrIn : TSockAddrIn;
begin
  ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

  ASockAddrIn.sin_port        := WinAPI.Winsock2.htons(FRemotePort);
  ASockAddrIn.sin_family      := AF_INET;
  ASockAddrIn.sin_addr.S_addr := WinAPI.Winsock2.inet_addr(PAnsiChar(AnsiString(FRemoteAddress)));

  // Resolve Host if any
  if ASockAddrIn.sin_addr.S_addr = INADDR_NONE then begin
    var AHostEnt := Winapi.Winsock2.GetHostByName(PAnsiChar(AnsiString(FRemoteAddress)));
    if AHostEnt <> nil then
      ASockAddrIn.sin_addr.S_addr := Integer(Pointer(AHostEnt^.h_addr^)^);
  end;

  // Attempt to connect to remote server
  if (WinAPI.Winsock2.connect(FSocket, TSockAddr(ASockAddrIn), SizeOf(TSockAddrIn)) = SOCKET_ERROR) then
    raise ESocketException.Create('connect');

  {$IFDEF USETLS}
  FSSLHandler := TOptixOpenSSLHandler.Create(FSSLContext, FSocket);
  FSSLHandler.Connect();
  {$ENDIF}
end;

{$IFDEF USETLS}

(* TClientSocket.OpenSSL *)

{ TClientSocket.Create }
constructor TClientSocket.Create(const ASSLContext : TOptixOpenSSLContext);
begin
  inherited Create();
  ///

  FSSLContext := ASSLContext;
  FSSLHandler := nil;
end;

{ TClientSocket.Destroy }
destructor TClientSocket.Destroy();
begin
  if Assigned(FSSLHandler) then
    FreeAndNil(FSSLHandler);

  ///
  inherited Destroy();
end;

{ TClientSocket.Send }
procedure TClientSocket.Send(const buf; len : Integer);
begin
  if Assigned(FSSLHandler) then
    FSSLHandler.Send(buf, len);
end;

{ TClientSocket.Recv }
procedure TClientSocket.Recv(var buf; len : Integer);
begin
  if Assigned(FSSLhandler) then
    FSSLHandler.Recv(buf, len);
end;

{ TClientSocket.GetPeerCertificateFingerprint }
function TClientSocket.GetPeerCertificateFingerprint() : String;
begin
  result := '';
  ///

  if Assigned(FSSLHandler) then
    result := FSSLHandler.PeerCertificateFingerprint;
end;

{$ELSE}

{ TClientSocket.Send }
procedure TClientSocket.Send(const buf; len : Integer);
begin
  if Winapi.Winsock2.Send(FSocket, buf, len, 0) <= 0 then
    raise ESocketException.Create('Send');
end;

{ TClientSocket.Recv }
procedure TClientSocket.Recv(var buf; len : Integer);
begin
  if Winapi.Winsock2.Recv(FSocket, buf, len, 0) <= 0 then
    raise ESocketException.Create('Recv');
end;

{$ENDIF}

{ TClientSocket.SendBuffer }
procedure TClientSocket.SendBuffer(const pValue : Pointer; const ABufferSize : UInt64);
var ABytesWritten : UInt64;
    AChunkSize    : UInt64;
    pOffset       : PByte;
    ACompleted    : Boolean;
begin
  if not Assigned(pValue) or (ABufferSize = 0) then
    Exit();
  ///

  Send(ABufferSize, SizeOf(UInt64));

  ABytesWritten := 0;
  repeat
    AChunkSize := (ABufferSize - ABytesWritten);

    if AChunkSize > PACKET_SIZE then
      AChunkSize := PACKET_SIZE;

    pOffset := PByte(NativeUInt(pValue) + ABytesWritten);

    Send(PByte(pOffset)^, AChunkSize);

    Inc(ABytesWritten, AChunkSize);

    ACompleted := (ABytesWritten >= ABufferSize);
  until ACompleted;
end;

{ TClientSocket.ReceiveBuffer }
procedure TClientSocket.ReceiveBuffer(var pBuffer : Pointer; var ABufferSize : UInt64);
var ABytesRead  : UInt64;
    ACompleted  : Boolean;
    AChunkSize  : UInt64;
begin
  Recv(ABufferSize, SizeOf(UInt64));
  if ABufferSize = 0 then
    raise ESocketException.Create(_MSG_SOCKET_NULLDATASIZE);
  ///

  GetMem(pBuffer, ABufferSize);

  ABytesRead := 0;
  repeat
    AChunkSize := (ABufferSize - ABytesRead);

    if AChunkSize >= PACKET_SIZE then
      AChunkSize := PACKET_SIZE;

    Recv(PByte(NativeUInt(pBuffer) + ABytesRead)^, AChunkSize);

    Inc(ABytesRead, AChunkSize);

    ACompleted := (ABytesRead >= ABufferSize);
  until ACompleted;
end;

{ TClientSocket.SendStream }
procedure TClientSocket.SendStream(const AValue : TMemoryStream);
begin
  if not Assigned(AValue) then
    Exit();

  if AValue.Size <= 0 then
    Exit();

  AValue.Position := 0;

  ///
  SendBuffer(AValue.Memory, AValue.Size);
end;

{ TClientSocket.ReceiveStream }
procedure TClientSocket.ReceiveStream(var AValue : TMemoryStream);
var pBuffer     : Pointer;
    ABufferSize : UInt64;
begin
  if not Assigned(AValue) then
    AValue := TMemoryStream.Create();
  ///
  ReceiveBuffer(pBuffer, ABufferSize);
  if Assigned(pBuffer) and (ABufferSize > 0) then begin
    try
      AValue.Write(PByte(pBuffer)^, ABufferSize);

      ///
      AValue.Position := 0;
    finally
      FreeMem(pBuffer, ABufferSize);
    end;
  end;
end;

{ TClientSocket.SendString }
procedure TClientSocket.SendString(const AString : String);
begin
  SendBuffer(PWideChar(AString), Length(AString) * SizeOf(WideChar));
end;

{ TClientSocket.RecvString }
function TClientSocket.ReceiveString() : String;
begin
  result := '';
  ///

  var pBuffer := nil;
  var ABufferSize : UInt64;
  try
    ReceiveBuffer(pBuffer, ABufferSize);

    SetString(result, PWideChar(pBuffer), ABufferSize div SizeOf(WideChar));
  finally
    if Assigned(pBuffer) then
      FreeMem(pBuffer, ABufferSize);
  end;
end;

{ TClientSocket.SendJson }
procedure TClientSocket.SendJson(const AJson : ISuperObject);
begin
  if not Assigned(AJson) then
    Exit();

  SendString(AJson.AsJSON());
end;

{ TClientSocket.SendJson }
procedure TClientSocket.SendJson(const AJsonArray : ISuperArray);
begin
  if not Assigned(AJsonArray) then
    Exit();

  ///
  SendString(AJsonArray.AsJSON());
end;

{ TClientSocket.ReceiveJson }
function TClientSocket.ReceiveJson() : ISuperObject;
begin
  var AJsonString := ReceiveString();
  ///

  try
    result := TSuperObject.Create(AJsonString);
  except
    result := nil;
  end;
end;

{ TClientSocket.ReceiveJsonArray }
function TClientSocket.ReceiveJsonArray() : ISuperArray;
begin
  var AJsonString := ReceiveString();
  ///

  try
    result := TSuperArray.Create(AJsonString);
  except
    result := nil;
  end;
end;

{ TClientSocket.SendPacket }
procedure TClientSocket.SendPacket(const APacket : TOptixPacket);
begin
  if not Assigned(APacket) then
    Exit();
  ///

  SendJson(APacket.Serialize);
end;

{ TClientSocket.ReceiveePacket }
procedure TClientSocket.ReceivePacket(var APacketBody : ISuperObject; const ABlockUntilDataAvailable : Boolean = False);
begin
  if not ABlockUntilDataAvailable then
    if not self.IsDataAvailable() then
      Exit();

  APacketBody := ReceiveJson();
end;

{ TClientSocket.IsDataAvailable }
function TClientSocket.IsDataAvailable(): Boolean;
begin
  {$IFDEF USETLS}
  if Assigned(FSSLhandler) then
    if FSSLHandler.IsDataPending() then
      Exit(True);
  {$ENDIF}

  var AReadFd : TFDSet;
  FD_ZERO(AReadFd);
  _FD_SET(FSocket, AReadFd);

  var ATimeVal : TTimeVal;
  ATimeVal.tv_sec  := 0;
  ATimeVal.tv_usec := 0;

  //
  var ARet := select(0, @AReadFd, nil, nil, @ATimeVal);
  if ARet = SOCKET_ERROR then
    raise ESocketException.Create('select');

  ///
  result := (ARet > 0);
end;

{ TClientSocket.IsSocketAlive }
function TClientSocket.IsSocketAlive() : Boolean;
begin
  var AReadFd : TFDSet;
  FD_ZERO(AReadFd);
  _FD_SET(FSocket, AReadFd);

  var ATimeVal : TTimeVal;
  ATimeVal.tv_sec  := 0;
  ATimeVal.tv_usec := 0;

  //
  var ARet := select(0, @AReadFd, nil, nil, @ATimeVal);
  if ARet = SOCKET_ERROR then
    raise ESocketException.Create('select');

  ///
  if ARet = 0 then
    result := True
  else begin
    {$IFDEF USETLS}
      if Assigned(FSSLHandler) then
        result := FSSLHandler.IsConnectionAlive();
    {$ELSE}
      var ADummyBuffer : array[0..0] of Byte;
      ARet := Winapi.Winsock2.recv(FSocket, ADummyBuffer, 1, MSG_PEEK);
      if ARet = 0 then
        Result := False
      else if ARet = SOCKET_ERROR then
        Result := WSAGetLastError() = WSAEWOULDBLOCK
      else
        Result := True;
    {$ENDIF}
  end;
end;

(* TServerSocket *)

{ TServerSocket.Create }
constructor TServerSocket.Create(const ABindAddress : String; const ABindPort : Word);
begin
  inherited Create();
  ///

  FBindAddress := ABindAddress;
  FBindPort    := ABindPort;
end;

{ TServerSocket.Listen }
procedure TServerSocket.Listen();
var ASockAddrIn : TSockAddrIn;
begin
  try
    ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

    ASockAddrIn.sin_port   := WinAPI.Winsock2.htons(FBindPort);
    ASockAddrIn.sin_family := AF_INET;

    if (FBindAddress = '0.0.0.0') or (FBindAddress = '') then
      ASockAddrIn.sin_addr.S_addr := INADDR_ANY
    else
      ASockAddrIn.sin_addr.S_addr := WinAPI.Winsock2.inet_addr(PAnsiChar(AnsiString(FBindAddress)));

    // Bind Socket
    if Winapi.Winsock2.bind(FSocket, TSockAddr(ASockAddrIn), SizeOf(TSockAddrIn)) = SOCKET_ERROR then
      raise ESocketException.Create('bind');

    // Listen on Socket
    if Winapi.Winsock2.listen(FSocket, SOMAXCONN) = SOCKET_ERROR then
      raise ESocketException.Create('listen');
  except
    on E: Exception do begin
      if (FSocket <> INVALID_SOCKET) then
        closesocket(FSocket);

      FSocket := INVALID_SOCKET;

      ///
      raise;
    end;
  end;
end;

{ TServerSocket.AcceptClient }
function TServerSocket.AcceptClient({$IFDEF USETLS}const ASSLContext : TOptixOpenSSLContext{$ENDIF}) : TClientSocket;
var ASockAddrIn : TSockAddrIn;
begin
  // Wait until a new client connects
  ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

  var ALen : Integer := SizeOf(TSockAddrIn);

  var AClient := Winapi.Winsock2.accept(FSocket, @ASockAddrIn, @ALen);
  if AClient = INVALID_SOCKET then
    raise ESocketException.Create('accept');

  ///
  result := TClientSocket.Create({$IFDEF USETLS}ASSLContext, {$ENDIF}AClient);
end;

(* Initialization / Finalization *)

var _WSAData : TWSAData;

initialization
  if WSAStartup(MakeWord(2, 2), _WSAData) <> 0 then
    raise ESocketException.Create('WSAStartup');

finalization
  WSACleanup();

end.
