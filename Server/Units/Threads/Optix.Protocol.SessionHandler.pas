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

unit Optix.Protocol.SessionHandler;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows,

  XSuperObject,

  Optix.Protocol.Client.Handler, Optix.Protocol.Packet, Optix.Sockets.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixSessionHandlerThread = class;

  TOnSessionDisconnect = procedure(Sender : TOptixSessionHandlerThread) of object;
  TOnReceivePacket = procedure(Sender : TOptixSessionHandlerThread; const ASerializedPacket : ISuperObject) of object;

  TOptixSessionHandlerThread = class(TOptixClientHandlerThread)
  private
    FOnSessionDisconnect : TOnSessionDisconnect;
    FOnReceivePacket     : TOnReceivePacket;
  protected
    {@M}
    procedure ClientTerminate(); override;
    procedure PacketReceived(const ASerializedPacket : ISuperObject); override;
    procedure Initialize(); override;
  public
    {@G/S}
    property OnSessionDisconnect : TOnSessionDisconnect read FOnSessionDisconnect write FOnSessionDisconnect;
    property OnReceivePacket     : TOnReceivePacket     read FOnReceivePacket     write FOnReceivePacket;
  end;

implementation

{ TOptixSessionHandlerThread.Initialize }
procedure TOptixSessionHandlerThread.Initialize();
begin
  inherited;
  ///

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
procedure TOptixSessionHandlerThread.PacketReceived(const ASerializedPacket : ISuperObject);
begin
  if not Assigned(ASerializedPacket) or
     not ASerializedPacket.Contains('PacketClass') or
     not Assigned(FOnReceivePacket) then
      Exit();
  ///

  Synchronize(procedure begin
    FOnReceivePacket(self, ASerializedPacket);
  end);
end;

end.
