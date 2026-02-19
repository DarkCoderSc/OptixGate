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



unit OptixCore.Protocol.Client.Handler;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows,

  Generics.Collections,

  XSuperObject,

  Optix.Protocol.Client, OptixCore.Protocol.Packet, OptixCore.Sockets.Helper, OptixCore.Protocol.Preflight;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixClientHandlerThread = class(TOptixClientThread)
  private
    {$IFDEF SERVER}
    FHandlerId : TGUID;
    {$ENDIF}

    FPacketQueue : TThreadedQueue<TOptixPacket>;
  protected
    {@M}
    procedure Initialize(); override;
    procedure Finalize(); override;
    procedure ClientExecute(); override;
    procedure PollActions(); virtual;
    procedure PacketReceived(const ASerializedPacket : ISuperObject); virtual; abstract;
  public
    {@M}
    procedure AddPacket(const APacket : TOptixPacket);

    {$IFDEF SERVER}
    {@C}
    constructor Create(const AClient : TClientSocket; const AHandlerId : TGUID); overload;

    {@G}
    property HandlerId : TGUID read FHandlerId;
    {$ENDIF}
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SyncObjs,

  OptixCore.Sockets.Exceptions{$IFDEF USETLS}, OptixCore.OpenSSL.Exceptions{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

procedure TOptixClientHandlerThread.PollActions();
begin
  ///
end;

procedure TOptixClientHandlerThread.ClientExecute();
var APacket           : TOptixPacket;
    ASerializedPacket : ISuperObject;
begin
  if not Assigned(FPacketQueue) then
    Exit();
  ///

  while not Terminated do begin
    APacket := nil;
    ///

    //------------------------------------------------------------------------------------------------------------------
    // Dispatch Outgoing Packets (Egress)
    //------------------------------------------------------------------------------------------------------------------
    while (FPacketQueue.PopItem(APacket) = TWaitResult.wrSignaled) do begin
      try
        if Terminated then
          break;
        ///

        // Dispatch Packet
        try
          if Assigned(APacket) then
            FClient.SendPacket(APacket);
        except
          on E : Exception do begin
            if (E is ESocketException) {$IFDEF USETLS}or (E is EOpenSSLBaseException){$ENDIF} then
              raise;

            // Other exception, for now, we ignore.
          end;
        end;
      finally
        if Assigned(APacket) then
          FreeAndNil(APacket);
      end;
    end;

    // -----------------------------------------------------------------------------------------------------------------
    // Dispatch Incomming Packets (Ingress)
    // -----------------------------------------------------------------------------------------------------------------
    try
      ASerializedPacket := nil;

      FClient.ReceivePacket(ASerializedPacket);

      if Assigned(ASerializedPacket) then
        PacketReceived(ASerializedPacket);
    except
      on E : Exception do begin
        if (E is ESocketException) {$IFDEF USETLS}or (E is EOpenSSLBaseException){$ENDIF} then
          raise;

        // Other exception, for now, we ignore.
      end;
    end;

    // -----------------------------------------------------------------------------------------------------------------
    // Perform additional actions after each iterations
    // -----------------------------------------------------------------------------------------------------------------
    PollActions();
  end; // while not Terminated do begin
end;

procedure TOptixClientHandlerThread.Initialize();
begin
  inherited;
  ///

  FPacketQueue := TThreadedQueue<TOptixPacket>.Create(1024, INFINITE, 500);
end;

procedure TOptixClientHandlerThread.Finalize();
begin
  inherited;
  ///

  if Assigned(FPacketQueue) then begin
    FPacketQueue.DoShutDown();

    var APacket : TOptixPacket;
    while True do begin
      APacket := FPacketQueue.PopItem;
      if not Assigned(APacket) then
        break;

      ///
      FreeAndNil(APacket);
    end;

    ///
    FreeAndNil(FPacketQueue);
  end;
end;

procedure TOptixClientHandlerThread.AddPacket(const APacket : TOptixPacket);
begin
  if not Assigned(APacket) then
    Exit();
  ///

  FPacketQueue.PushItem(APacket);
end;

{$IFDEF SERVER}
constructor TOptixClientHandlerThread.Create(const AClient : TClientSocket; const AHandlerId : TGUID);
begin
  inherited Create(AClient);
  ///

  FHandlerId := AHandlerId;
end;
{$ENDIF}

end.
