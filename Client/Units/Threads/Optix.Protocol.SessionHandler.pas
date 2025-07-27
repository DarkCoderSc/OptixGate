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

unit Optix.Protocol.SessionHandler;

interface

uses System.Classes, Optix.Protocol.Client.Handler, Optix.Protocol.Packet, XSuperObject, Optix.Protocol.Preflight,
     Optix.Protocol.Worker.FileTransfer, Optix.Func.Commands;

type
  TOptixSessionHandlerThread = class(TOptixClientHandlerThread)
  private
    FFileTransferOrchestrator : TOptixFileTransferOrchestratorThread;

    {@M}
    procedure InitializeFileTransferOrchestratorThread();
  protected
    {@M}
    function InitializePreflightRequest() : TOptixPreflightRequest; override;
    procedure Initialize(); override;
    procedure Finalize(); override;

    procedure RegisterNewFileTransfer(const ATransfer : TOptixTransfer);

    procedure Connected(); override;
    procedure Disconnected(); override;
    procedure PacketReceived(const ASerializedPacket : ISuperObject); override;
  end;

implementation

uses Winapi.Windows, Optix.Func.SessionInformation, System.SysUtils, Optix.Func.Enum.Process, Optix.Actions.Process,
     Optix.Func.LogNotifier, Optix.Func.Enum.FileSystem, Optix.Thread;


{ TOptixSessionHandlerThread.Initialize }
procedure TOptixSessionHandlerThread.Initialize();
begin
  inherited;
  ///

  FFileTransferOrchestrator := nil;
end;

{ TOptixSessionHandlerThread.Finalize }
procedure TOptixSessionHandlerThread.Finalize();
begin
  inherited;
  ///

end;

{ TOptixSessionHandlerThread.InitializePreflightRequest }
function TOptixSessionHandlerThread.InitializePreflightRequest() : TOptixPreflightRequest;
begin
  result.ClientKind := ckHandler;
end;

{ TOptixSessionHandlerThread.InitializeFileTransferOrchestratorThread }
procedure TOptixSessionHandlerThread.InitializeFileTransferOrchestratorThread();
begin
  var ADoCreate := True;
  ///

  if Assigned(FFileTransferOrchestrator) then
    ADoCreate := not TOptixThread.HasInstance(FFileTransferOrchestrator);

  if ADoCreate then begin
    FFileTransferOrchestrator := TOptixFileTransferOrchestratorThread.Create(
      FClient.RemoteAddress,
      FClient.RemotePort
    );

    ///
    FFileTransferOrchestrator.Start();
  end;
end;

{ TOptixSessionHandlerThread.RegisterNewFileTransfer }
procedure TOptixSessionHandlerThread.RegisterNewFileTransfer(const ATransfer : TOptixTransfer);
begin
  InitializeFileTransferOrchestratorThread();
  ///

  FFileTransferOrchestrator.AddTransfer(ATransfer);
end;

{ TOptixSessionHandlerThread.Connected }
procedure TOptixSessionHandlerThread.Connected();
begin
  inherited;
  ///

  self.AddPacket(TOptixSessionInformation.Create());
end;

{ TOptixSessionHandlerThread.Disconnected }
procedure TOptixSessionHandlerThread.Disconnected();
begin
  inherited;
  ///

  if Assigned(FFileTransferOrchestrator) then begin
    TOptixThread.TerminateInstance(FFileTransferOrchestrator);

    ///
    FFileTransferOrchestrator := nil;
  end;
end;

{ TOptixSessionHandlerThread.PacketReceived }
procedure TOptixSessionHandlerThread.PacketReceived(const ASerializedPacket : ISuperObject);
begin
  if not Assigned(ASerializedPacket) or
     not ASerializedPacket.Contains('PacketClass') then
      Exit();
  ///

  // TODO: make it more generic (Class Registry or RTTI)
  var AClassName := ASerializedPacket.S['PacketClass'];

  var AWindowGUID := TGUID.Empty;
  if ASerializedPacket.Contains('WindowGUID') then
    AWindowGUID := TGUID.Create(ASerializedPacket.S['WindowGUID']);

  var AOptixPacket : TOptixPacket := nil;
  var AHandleMemory : Boolean := False;
  try
    try
      // ---------------------------------------------------------------------------------------------------------------
      if AClassName = TOptixCommandTerminate.ClassName then
        Terminate
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixRefreshProcess.ClassName then
        AddPacket(TProcessList.Create(AWindowGUID))
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixKillProcess.ClassName then begin
        AHandleMemory := True;
        ///

        AOptixPacket := TOptixKillProcess.Create(ASerializedPacket);

        TProcessActions.TerminateProcess(TOptixKillProcess(AOptixPacket).ProcessId);

        AddPacket(AOptixPacket); // Success notification
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixRefreshDrives.ClassName then
        AddPacket(TDriveList.Create(AWindowGUID))
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixRefreshFiles.ClassName then begin
        AOptixPacket := TOptixRefreshFiles.Create(ASerializedPacket);

        AddPacket(TFileList.Create(AWindowGUID, TOptixRefreshFiles(AOptixPacket).Path));
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixDownloadFile.ClassName then
        RegisterNewFileTransfer(TOptixDownloadFile.Create(ASerializedPacket))
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixUploadFile.ClassName then
        RegisterNewFileTransfer(TOptixUploadFile.Create(ASerializedPacket))
      // ---------------------------------------------------------------------------------------------------------------
      // ... //
    except
      on E : Exception do
        AddPacket(TLogNotifier.Create(E.Message, AClassName, lkException));
    end;
  finally
    if not AHandleMemory and Assigned(AOptixPacket) then
      FreeAndNil(AOptixPacket);
  end;
end;

end.
