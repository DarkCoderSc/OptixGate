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

uses System.Classes, System.SysUtils, Optix.Protocol.Client, Optix.Shared.Protocol.FileTransfer;

type
  TOnRequestTransferTask = procedure(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask) of object;

  TOptixFileTransferWorker = class(TOptixClientThread)
  private
    FSessionId             : TGUID;
    FOnRequestTransferTask : TOnRequestTransferTask;
  protected
    {@M}
    procedure ClientExecute(); override;
    procedure Initialize(); override;
  public
    {@G/S}
    property OnRequestTransferTask : TOnRequestTransferTask read FOnRequestTransferTask write FOnRequestTransferTask;
  end;

implementation

uses Winapi.Windows, Generics.Collections;

{ TOptixFileTransferWorker.Initialize }
procedure TOptixFileTransferWorker.Initialize();
begin
  inherited;
  ///

  FOnRequestTransferTask := nil;
end;

{ TOptixFileTransferWorker.ClientExecute }
procedure TOptixFileTransferWorker.ClientExecute();
begin
  var ATasks := TObjectDictionary<TGUID, TOptixTransferTask>.Create([doOwnsValues]);
  var ATask : TOptixTransferTask;
  var ABuffer : array[0..FILE_CHUNK_SIZE-1] of byte;
  try
    if not Assigned(FOnRequestTransferTask) then
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

        if ATask = nil then
          continue;

        // Step 1) Synchronize File Sizes
        if ATask is TOptixDownloadTask then begin
          var AFileSize : Int64;

          FClient.Recv(AFileSize, SizeOf(Int64));

          ///
          TOptixDownloadTask(ATask).FileSize := AFileSize;
        end else if ATask is TOptixUploadTask then begin
          if TOptixUploadTask(ATask).State = otsBegin then
            FClient.Send(TOptixUploadTask(ATask).FileSize, SizeOf(Int64));
        end;

        ///
        ATasks.Add(ATransferId, ATask);
      end;

      // Step 2) Upload / Download Chunks
      if ATask is TOptixDownloadTask then
        TOptixDownloadTask(ATask).DownloadChunk(FClient)
      else if ATask is TOptixUploadTask then
        TOptixUploadTask(ATask).UploadChunk(FClient);

      ///
      if ATask.State = otsEnd then
        ATasks.Remove(ATransferId);
    end;
  finally
    FreeAndNil(ATasks);
  end;
end;

end.
