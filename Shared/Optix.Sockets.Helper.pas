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

uses Winapi.Windows, Winapi.Winsock2, System.Classes, System.SysUtils,
     XSuperObject, Optix.Protocol.Packet;

type
  TClientSocket = class;

  TSocketBase = class
  private
    FSocket     : TSocket;
    FPacketSize : Cardinal;
    FIdentifier : TGUID;
    FData       : Pointer;

    {@M}
    procedure CheckSocket();
    function IsDataAvailable(): Boolean;
  protected
    {@M}
    procedure CreateSocket();
  public
    {@M}
    procedure Close(); overload;

    procedure Send(const buf; len : Integer);
    procedure Recv(var buf; len : Integer);

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

    {@C}
    constructor Create(APacketSize : Cardinal = 2048); virtual;
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
    function AcceptClient() : TClientSocket;

    {@G}
    property BindAddress : String read FBindAddress;
    property BindPort    : Word   read FBindPort;
  end;

  TClientSocket = class(TSocketBase)
  private
    FRemoteAddress : String;
    FRemotePort    : word;

    {@M}
    procedure GetPeerInformations();
  public
    {@C}
    constructor Create(const ARemoteAddress : String; const ARemotePort : word); overload;
    constructor Create(const ASocket : TSocket); overload;

    {@M}
    procedure Connect(); overload;

    {@G}
    property RemoteAddress : String read FRemoteAddress;
    property RemotePort    : Word   read FRemotePort;
  end;

implementation

uses Optix.Sockets.Exceptions;


(* TSocketBase *)

{ TSocketBase.Create }
constructor TSocketBase.Create(APacketSize : Cardinal = 2048);
begin
  self.CreateSocket();

  if APacketSize < 1024 then
    APacketSize := 1024;

  FIdentifier := TGUID.NewGuid();

  FData := nil;

  ///
  FPacketSize := APacketSize;
end;

{ TSocketBase.Destroy }
destructor TSocketBase.Destroy();
begin
  self.Close();

  ///
  inherited Destroy();
end;

{ TSocketBase.CheckSocket }
procedure TSocketBase.CheckSocket();
begin
  if FSocket = INVALID_SOCKET then
    raise Exception.Create('Invalid Socket');
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

{ TSocketBase.Send }
procedure TSocketBase.Send(const buf; len : Integer);
begin
  if Winapi.Winsock2.Send(FSocket, buf, len, 0) <= 0 then
    raise ESocketException.Create('Send');
end;

{ TSocketBase.Recv }
procedure TSocketBase.Recv(var buf; len : Integer);
begin
  if Winapi.Winsock2.Recv(FSocket, buf, len, 0) <= 0 then
    raise ESocketException.Create('Recv');
end;

{ TSocketBase.SendBuffer }
procedure TSocketBase.SendBuffer(const pValue : Pointer; const ABufferSize : UInt64);
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

    if AChunkSize > FPacketSize then
      AChunkSize := FPacketSize;

    pOffset := PByte(NativeUInt(pValue) + ABytesWritten);

    Send(PByte(pOffset)^, AChunkSize);

    Inc(ABytesWritten, AChunkSize);

    ACompleted := (ABytesWritten >= ABufferSize);
  until ACompleted;
end;

{ TSocketBase.ReceiveBuffer }
procedure TSocketBase.ReceiveBuffer(var pBuffer : Pointer; var ABufferSize : UInt64);
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

    if AChunkSize >= FPacketSize then
      AChunkSize := FPacketSize;

    Recv(PByte(NativeUInt(pBuffer) + ABytesRead)^, AChunkSize);

    Inc(ABytesRead, AChunkSize);

    ACompleted := (ABytesRead >= ABufferSize);
  until ACompleted;
end;

{ TSocketBase.SendStream }
procedure TSocketBase.SendStream(const AValue : TMemoryStream);
begin
  if not Assigned(AValue) then
    Exit();

  if AValue.Size <= 0 then
    Exit();

  AValue.Position := 0;

  ///
  self.SendBuffer(AValue.Memory, AValue.Size);
end;

{ TSocketBase.ReceiveStream }
procedure TSocketBase.ReceiveStream(var AValue : TMemoryStream);
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

{ TSocketBase.SendString }
procedure TSocketBase.SendString(const AString : String);
var AStream : TMemoryStream;
begin
  AStream := TStringStream.Create();
  try
    AStream.Write(AString[1], Length(AString) * SizeOf(WideChar));

    AStream.Position := 0;

    SendStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

{ TSocketBase.RecvString }
function TSocketBase.ReceiveString() : String;
var AStream : TMemoryStream;
begin
  result := '';
  ///

  AStream := TStringStream.Create();
  try
    ReceiveStream(AStream);
    ///

    AStream.Position := 0;

    SetString(result, PWideChar(AStream.Memory), AStream.Size div SizeOf(WideChar));
  finally
    FreeAndNil(AStream);
  end;
end;

{ TSocketBase.SendJson }
procedure TSocketBase.SendJson(const AJson : ISuperObject);
begin
  if not Assigned(AJson) then
    Exit();

  SendString(AJson.AsJSON());
end;

{ TSocketBase.SendJson }
procedure TSocketBase.SendJson(const AJsonArray : ISuperArray);
begin
  if not Assigned(AJsonArray) then
    Exit();

  ///
  SendString(AJsonArray.AsJSON());
end;

{ TSocketBase.ReceiveJson }
function TSocketBase.ReceiveJson() : ISuperObject;
var AJsonString : String;
begin
  AJsonString := ReceiveString();
  ///

  result := TSuperObject.Create(AJsonString);
end;

{ TSocketBase.ReceiveJsonArray }
function TSocketBase.ReceiveJsonArray() : ISuperArray;
var AJsonString : String;
begin
  AJsonString := ReceiveString();
  ///

  result := TSuperArray.Create(AJsonString);
end;

{ TSocketBase.SendPacket }
procedure TSocketBase.SendPacket(const APacket : TOptixPacket);
begin
  if not Assigned(APacket) then
    Exit();
  ///

  SendJson(APacket.Serialize);
end;

{ TSocketBase.ReceiveePacket }
procedure TSocketBase.ReceivePacket(var APacketBody : ISuperObject; const ABlockUntilDataAvailable : Boolean = False);
begin
  if not ABlockUntilDataAvailable then
    if not self.IsDataAvailable() then
      Exit();

  APacketBody := ReceiveJson();
end;

{ TSocketBase.IsDataAvailable }
function TSocketBase.IsDataAvailable(): Boolean;
var AReadFd  : TFDSet;
    ATimeVal : TTimeVal;
    ARet     : Integer;
begin
  FD_ZERO(AReadFd);
  _FD_SET(FSocket, AReadFd);

  ATimeVal.tv_sec  := 0;
  ATimeVal.tv_usec := 0;

  //
  ARet := select(0, @AReadFd, nil, nil, @ATimeVal);
  if ARet = SOCKET_ERROR then
    raise ESocketException.Create('select');

  ///
  result := (ARet > 0);
end;

{ TSocketBase.CreateSocket }
procedure TSocketBase.CreateSocket();
var ASocket : TSocket;
begin
  FSocket := INVALID_SOCKET;
  ///

  // Create Socket
  ASocket := Winapi.Winsock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (ASocket = INVALID_SOCKET) then
    raise ESocketException.Create('socket');
  ///

  // Assign Socket
  FSocket := ASocket;
end;

(* TClientSocket *)

{ TClientSocket.Create }
constructor TClientSocket.Create(const ARemoteAddress : String; const ARemotePort : word);
begin
  inherited Create();
  ///

  FRemoteAddress := ARemoteAddress;
  FRemotePort    := ARemotePort;
end;

{ TClientSocket.Create }
constructor TClientSocket.Create(const ASocket : TSocket);
begin
  inherited Create();
  ///

  FSocket := ASocket;

  ///
  self.GetPeerInformations();
end;

{ TClientSocket.GetPeerInformations }
procedure TClientSocket.GetPeerInformations();
var ASockAddr    : TSockAddrIn;
    ASockAddrLen : Integer;
begin
  self.CheckSocket();
  ///

  ASockAddrLen := SizeOf(TSockAddrIn);
  if Winapi.Winsock2.getpeername(FSocket, TSockAddr(ASockAddr), ASockAddrLen) <> 0 then
    raise ESocketException.Create('getpeername');

  FRemotePort := ntohs(ASockAddr.sin_port);
  FRemoteAddress := inet_ntoa(ASockAddr.sin_addr);
end;

{ TClientSocket.Connect }
procedure TClientSocket.Connect();
var AHostEnt    : PHostEnt;
    ASockAddrIn : TSockAddrIn;
begin
  CheckSocket();
  ///

  ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

  ASockAddrIn.sin_port        := WinAPI.Winsock2.htons(FRemotePort);
  ASockAddrIn.sin_family      := AF_INET;
  ASockAddrIn.sin_addr.S_addr := WinAPI.Winsock2.inet_addr(PAnsiChar(AnsiString(FRemoteAddress)));

  // Resolve Host if any
  if ASockAddrIn.sin_addr.S_addr = INADDR_NONE then begin
    AHostEnt := Winapi.Winsock2.GetHostByName(PAnsiChar(AnsiString(FRemoteAddress)));
    if AHostEnt <> nil then
      ASockAddrIn.sin_addr.S_addr := Integer(Pointer(AHostEnt^.h_addr^)^);
  end;

  // Attempt to connect to remote server
  if (WinAPI.Winsock2.connect(FSocket, TSockAddr(ASockAddrIn), SizeOf(TSockAddrIn)) = SOCKET_ERROR) then
    raise ESocketException.Create('connect');
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
  self.CheckSocket();
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
function TServerSocket.AcceptClient() : TClientSocket;
var AClient     : TSocket;
    ASockAddrIn : TSockAddrIn;
    ALen        : Integer;
begin
  // Wait until a new client connects
  ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

  ALen := SizeOf(TSockAddrIn);

  AClient := Winapi.Winsock2.accept(FSocket, @ASockAddrIn, @ALen);
  if AClient = INVALID_SOCKET then
    raise ESocketException.Create('accept');

  ///
  result := TClientSocket.Create(AClient);
end;

(* Initialization / Finalization *)

var _WSAData : TWSAData;

initialization
  if WSAStartup(MakeWord(2, 2), _WSAData) <> 0 then
    raise ESocketException.Create('WSAStartup');

finalization
  WSACleanup();

end.
