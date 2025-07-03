{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                              Project: Optix Neo                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://bsky.app/profile/darkcodersc.bsky.social           }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
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

unit Optix.Protocol.SessionHandler;

interface

uses System.Classes,
     Optix.Protocol.Client.Handler,
     Optix.Protocol.Packet,
     XSuperObject,
     Optix.Sockets.Helper,
     Winapi.Windows;

type
  TOnSessionConnect = procedure(Sender : TObject) of object;
  TOnSessionDisconnect = procedure(Sender : TObject) of object;
  TOnReceivePacket = procedure(Sender : TObject) of object;

  TOptixSessionHandlerThread = class(TOptixClientHandlerThread)
  private
    FOnSessionConnect    : TOnSessionConnect;
    FOnSessionDisconnect : TOnSessionDisconnect;
    FOnReceivePacket     : TOnReceivePacket;
  protected
    {@M}
    procedure ClientTerminate(); override;
    procedure PacketReceived(const APacketBody : ISuperObject); override;
  public
    {@C}
    constructor Create(const AClient : TClientSocket); overload;

    {@G/S}
    property OnSessionConnect    : TOnSessionConnect    read FOnSessionConnect    write FOnSessionConnect;
    property OnSessionDisconnect : TOnSessionDisconnect read FOnSessionDisconnect write FOnSessionDisconnect;
    property OnReceivePacket     : TOnReceivePacket     read FOnReceivePacket     write FOnReceivePacket;
  end;

implementation

{ TOptixSessionHandlerThread.Create }
constructor TOptixSessionHandlerThread.Create(const AClient : TClientSocket);
begin
  inherited Create(AClient);
  ///

  FOnSessionConnect    := nil;
  FOnSessionDisconnect := nil;
  FOnReceivePacket     := nil;
end;

{ TOptixSessionHandlerThread.ClientRelease }
procedure TOptixSessionHandlerThread.ClientTerminate();
begin
  inherited ClientTerminate();
  ///

  if Assigned(FOnSessionDisconnect) then
    Synchronize(procedure begin
      FOnSessionDisconnect(self);
    end);
end;

{ TOptixSessionHandlerThread.PacketReceived }
procedure TOptixSessionHandlerThread.PacketReceived(const APacketBody : ISuperObject);
begin
  // TODO
end;

end.
