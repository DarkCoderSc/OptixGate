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

unit Optix.Protocol.Worker.FileTransfer;

interface

uses System.Classes, Generics.Collections, Optix.Protocol.Client, Optix.Protocol.Preflight, Optix.Func.Commands,
     Optix.Protocol.Client.Handler;

type
  TOptixFileTransferOrchestratorThread = class(TOptixClientThread)
  private
    FHandler       : TOptixClientHandlerThread;
    FTransferQueue : TThreadedQueue<TOptixCommandTransfer>;
  protected
    {@M}
    function InitializePreflightRequest() : TOptixPreflightRequest; override;
    procedure ClientExecute(); override;
    procedure Initialize(); override;
    procedure Finalize(); override;
  public
    {@M}
    procedure AddTransfer(const ATransfer : TOptixCommandTransfer);

    {@C}
    constructor Create(const AHandler : TOptixClientHandlerThread); reintroduce;
  end;

implementation

uses System.SysUtils, System.SyncObjs, Winapi.Windows, Optix.Exceptions, System.Diagnostics, Optix.Protocol.Packet,
     Optix.Shared.Protocol.FileTransfer, Optix.Func.LogNotifier, Optix.Sockets.Exceptions
     {$IFDEF USETLS}, Optix.OpenSSL.Exceptions{$ENDIF};

{ TOptixFileTransferOrchestratorThread.AddTransfer }
procedure TOptixFileTransferOrchestratorThread.AddTransfer(const ATransfer : TOptixCommandTransfer);
begin
  if not Assigned(ATransfer) then
    Exit();
  ///

  FTransferQueue.PushItem(ATransfer);
end;

{ TOptixFileTransferOrchestratorThread.ClientExecute }
procedure TOptixFileTransferOrchestratorThread.ClientExecute();
begin
  if not Assigned(FTransferQueue) then
    Exit();
  ///

  var ATasks := TObjectDictionary<TOptixCommandTransfer, TOptixTransferTask>.Create([doOwnsKeys, doOwnsValues]);
  var ATerminatedTransfers := TList<TOptixCommandTransfer>.Create();
  var AChunk : array[0..FILE_CHUNK_SIZE-1] of Byte;
  var AStopwatch : TStopwatch;
  try
    ZeroMemory(@AChunk, Length(AChunk));
    ///

    while not Terminated do begin
      // This call is used to detect a potential disconnection from the server and to exit the loop if necessary.
      // It does not consume any network data; it serves solely as a network exit control mechanism.
      if not FClient.IsSocketAlive() then
        break;

      var ATransfer : TOptixCommandTransfer := nil;
      ///

      if AStopwatch.IsRunning then
        AStopwatch.Stop();

      // Polling Transfer Request --------------------------------------------------------------------------------------
      while (FTransferQueue.PopItem(ATransfer) = TWaitResult.wrSignaled) do begin
        if Terminated then
          break;
        ///

        var ATask : TOptixTransferTask;
        try
          // Server Req File Download
          if ATransfer is TOptixCommandDownloadFile then
            ATask := TOptixUploadTask.Create(ATransfer.FilePath)
          // Server Req File upload
          else if ATransfer is TOptixCommandUploadFile then
            ATask := TOptixDownloadTask.Create(ATransfer.FilePath)
          else begin
            // Should never happend
            FreeAndNil(ATransfer);

            continue;
          end;

          ///
          ATasks.Add(ATransfer, ATask);
        except
          on E : Exception do begin
            if Assigned(FHandler) then
              FHandler.AddPacket(TLogTransferException.Create(ATransfer.TransferId, E.Message, 'Transfer Initialization'));

            FreeAndNil(ATransfer);

            continue;
          end;
        end;
      end;

      // Process Transfer ----------------------------------------------------------------------------------------------
      if ATasks.Count > 0 then begin
        AStopwatch.Reset();
        AStopwatch.Start();
        ///

        while not Terminated do begin
          // Process Tasks
          for ATransfer in ATasks.Keys do begin
            if Terminated then
              break;
            ///

            var ATask : TOptixTransferTask := nil;

            if not ATasks.TryGetValue(ATransfer, ATask) or (ATask.State = otsEnd) then
              continue;

            FClient.Send(ATransfer.TransferId, SizeOf(TGUID));

            var ASuccess : Boolean;
            FClient.Recv(ASuccess, SizeOf(Boolean));
            if not ASuccess then begin
              ATerminatedTransfers.Add(ATransfer);

              ///
              continue;
            end;

            try
              if ATask is TOptixUploadTask then begin
                if ATask.State = otsBegin then
                  FClient.Send(TOptixUploadTask(ATask).FileSize, SizeOf(Int64));
                ///

                TOptixUploadTask(ATask).UploadChunk(FClient);
              end else if ATask is TOptixDownloadTask then begin
                if ATask.State = otsBegin then begin
                  var AFileSize : Int64;

                  FClient.Recv(AFileSize, SizeOf(Int64));

                  TOptixDownloadTask(ATask).SetFileSize(AFileSize);
                end;
                ///

                TOptixDownloadTask(ATask).DownloadChunk(FClient);
              end;
            except
              on E : Exception do begin
                if (E is ESocketException) {$IFDEF USETLS}or (E is EOpenSSLBaseException){$ENDIF} then
                  raise
                else begin
                  if Assigned(FHandler) then
                    FHandler.AddPacket(TLogTransferException.Create(ATransfer.TransferId, E.Message, 'Transfer Progress'));

                  ///
                  ATerminatedTransfers.Add(ATransfer);
                end;
              end;
            end;

            ///
            if (ATask.State = otsEnd) or ATask.IsEmpty then
              ATerminatedTransfers.Add(ATransfer);
          end;

          // Cleanup ended transfers
          if ATerminatedTransfers.Count > 0 then begin
            for ATransfer in ATerminatedTransfers do
              ATasks.Remove(ATransfer);

            ///
            ATerminatedTransfers.Clear();
          end;

          // Exit work loop if one of those two condition are met. Then poll new
          // items.
          if (AStopwatch.ElapsedMilliseconds >= 5000) or (ATasks.Count = 0) then
            break;
        end; // end while not Terminated
      end;
      // ---------------------------------------------------------------------------------------------------------------
    end;
  finally
    FreeAndNil(ATasks);
  end;
end;

{ TOptixFileTransferOrchestratorThread.InitializePreflightRequest }
function TOptixFileTransferOrchestratorThread.InitializePreflightRequest() : TOptixPreflightRequest;
begin
  result.ClientKind := ckFileTransfer;
end;

{ TOptixFileTransferOrchestratorThread.Initialize }
procedure TOptixFileTransferOrchestratorThread.Initialize();
begin
  inherited;
  ///

  FHandler := nil;
  FTransferQueue := TThreadedQueue<TOptixCommandTransfer>.Create(1024, INFINITE, 100);
end;

{ TOptixFileTransferOrchestratorThread.Finalize }
constructor TOptixFileTransferOrchestratorThread.Create(const AHandler : TOptixClientHandlerThread);
begin
  // TODO: better, context cloning? context reuse from handler for workers?
  inherited Create(
    {$IFDEF USETLS}AHandler.PublicKey, AHandler.PrivateKey, {$ENDIF}
    AHandler.RemoteAddress,
    AHandler.RemotePort,
    AHandler.IPVersion
  );
  ///

  {$IF defined(CLIENT_GUI) and defined(USETLS)}
  FOnVerifyPeerCertificate := AHandler.OnVerifyPeerCertificate;
  {$ELSEIF defined(USETLS)}
  FServerCertificateFingerprint := AHandler.ServerCertificateFingerprint;
  {$ENDIF}

  FHandler  := AHandler;
  FClientId := AHandler.ClientId; // Same Group
end;

{ TOptixFileTransferOrchestratorThread.Finalize }
procedure TOptixFileTransferOrchestratorThread.Finalize();
begin
  inherited;
  ///

  if Assigned(FTransferQueue) then begin
    FTransferQueue.DoShutDown();

    var ATransfer : TOptixCommandTransfer;
    while True do begin
      ATransfer := FTransferQueue.PopItem;
      if not Assigned(ATransfer) then
        break;

      ///
      FreeAndNil(ATransfer);
    end;

    ///
    FreeAndNil(FTransferQueue);
  end;
end;

end.
