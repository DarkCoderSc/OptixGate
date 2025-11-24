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



unit Optix.Protocol.Worker.FileTransfer;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Optix.Protocol.Client, Optix.Shared.Protocol.FileTransfer;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOnRequestTransferTask = procedure(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask) of object;
  TOnTransferError       = procedure(Sender : TObject; const ATransferId : TGUID; const AReason : String) of object;
  TOnTransferBegins      = procedure(Sender : TObject; const ATransferId : TGUID; const AFileSize : Int64) of object;
  TOnTransferUpdate      = procedure(Sender : TObject; const ATransferId : TGUID; const AWorkCount : Int64; var ACanceled : Boolean) of object;
  TOnTransferEnds        = procedure(Sender : TObject; const ATransferId : TGUID) of object;

  TOptixFileTransferWorker = class(TOptixClientThread)
  private
    FOnRequestTransferTask : TOnRequestTransferTask;
    FOnTransferError       : TOnTransferError;
    FOnTransferBegins      : TOnTransferBegins;
    FOnTransferUpdate      : TOnTransferUpdate;
    FOnTransferEnds        : TOnTransferEnds;

    {@M}
    procedure UpdateAllTasks(const ATasks : TObjectDictionary<TGUID, TOptixTransferTask>);
  protected
    {@M}
    procedure ClientExecute(); override;
    procedure Initialize(); override;
  public
    {@G/S}
    property OnRequestTransferTask : TOnRequestTransferTask read FOnRequestTransferTask write FOnRequestTransferTask;
    property OnTransferError       : TOnTransferError       read FOnTransferError       write FOnTransferError;
    property OnTransferBegins      : TOnTransferBegins      read FOnTransferBegins      write FOnTransferBegins;
    property OnTransferUpdate      : TOnTransferUpdate      read FOnTransferUpdate      write FOnTransferUpdate;
    property OnTransferEnds        : TOnTransferEnds        read FOnTransferEnds        write FOnTransferEnds;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Diagnostics,

  Winapi.Windows,

  Optix.Exceptions, Optix.Sockets.Exceptions{$IFDEF USETLS}, Optix.OpenSSL.Exceptions{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

procedure TOptixFileTransferWorker.Initialize();
begin
  inherited;
  ///

  FOnRequestTransferTask := nil;
  FOnTransferError       := nil;
  FOnTransferBegins      := nil;
  FOnTransferUpdate      := nil;
  FOnTransferEnds        := nil;
end;

procedure TOptixFileTransferWorker.UpdateAllTasks(const ATasks : TObjectDictionary<TGUID, TOptixTransferTask>);
begin
  if not Assigned(ATasks) or not Assigned(FOnTransferUpdate) then
    Exit();
  ///

  var ATask : TOptixTransferTask;
  var ACanceled : Boolean;

  for var ATransferId in ATasks.Keys do begin
    if not ATasks.TryGetValue(ATransferId, ATask) then
      continue;
    ///

    Synchronize(procedure begin
      FOnTransferUpdate(self, ATransferId, ATask.WorkCount, ACanceled);
    end);

    ATask.Canceled := ACanceled;
  end;
end;

procedure TOptixFileTransferWorker.ClientExecute();

  procedure SendAcknowledgement(const AValue : Boolean);
  begin
    if Assigned(FClient) then
      FClient.Send(AValue, SizeOf(Boolean));
  end;

begin
  var ATasks := TObjectDictionary<TGUID, TOptixTransferTask>.Create([doOwnsValues]);
  var ATask : TOptixTransferTask;
  var AStopWatch := TStopwatch.StartNew;
  try
    if not Assigned(FOnRequestTransferTask) or
       not Assigned(FOnTransferError) or
       not Assigned(FOnTransferBegins) or
       not Assigned(FOnTransferUpdate) or
       not Assigned(FOnTransferEnds) then
      Exit();

    while not Terminated do begin
      ATask := nil;
      ///

      var ATransferId : TGUID;
      FClient.Recv(ATransferId, SizeOf(TGUID));
      ///

      // Create / Register a new task
      if not ATasks.TryGetValue(ATransferId, ATask) then begin
        Synchronize(procedure begin
          FOnRequestTransferTask(self, ATransferId, ATask);
        end);

        // Transfer does not exists, is not expected or is in error (locally)
        if ATask = nil then begin
          SendAcknowledgement(False);

          continue;
        end;

        ///
        ATasks.Add(ATransferId, ATask);
      end;

      SendAcknowledgement(True);

      // Step 1) Synchronize File Sizes
      if ATask.State = otsBegin then begin
        if ATask is TOptixDownloadTask then begin
          var AFileSize : Int64;

          FClient.Recv(AFileSize, SizeOf(Int64));

          ///
          TOptixDownloadTask(ATask).SetFileSize(AFileSize);
        end else if ATask is TOptixUploadTask then
          if TOptixUploadTask(ATask).State = otsBegin then
            FClient.Send(TOptixUploadTask(ATask).FileSize, SizeOf(Int64));

        ///
        Synchronize(procedure begin
          FOnTransferBegins(self, ATransferId, ATask.FileSize);
        end);
      end;

      try
        // Step 2) Upload / Download Chunks
        if ATask is TOptixDownloadTask then
          TOptixDownloadTask(ATask).DownloadChunk(FClient)
        else if ATask is TOptixUploadTask then
          TOptixUploadTask(ATask).UploadChunk(FClient);
      except
        on E : Exception do begin
          if (E is ESocketException) {$IFDEF USETLS}or (E is EOpenSSLBaseException){$ENDIF} then
            raise
          else begin
            Synchronize(procedure begin
              FOnTransferError(self, ATransferId, E.Message);
            end);

            ///
            ATasks.Remove(ATransferId);
          end;
        end;
      end;

      // Update GUI (Progress) every 1s
      if AStopwatch.ElapsedMilliseconds >= 1000 then begin
        UpdateAllTasks(ATasks);

        ///
        AStopwatch.Reset();
        AStopwatch.Start();
      end;

      ///
      if (ATask.State = otsEnd) or ATask.IsEmpty or ATask.Canceled then begin
        Synchronize(procedure begin
          FOnTransferEnds(self, ATransferId);
        end);

        ///
        ATasks.Remove(ATransferId);
      end;
    end;
  finally
    FreeAndNil(ATasks);
  end;
end;

end.
