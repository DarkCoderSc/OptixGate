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

unit Optix.Protocol.FileTransfer;

interface

uses Generics.Collections, System.Classes, Optix.Protocol.Sockets.Client,
     Optix.Protocol.Preflight, Optix.Func.Commands;

type
  TOptixTransferDirection = (
    otdClientIsUploading,
    otdClientIsDownloading
  );

  TOptixTransferState = (
    otsBegin,
    otsProgress,
    otsEnd
  );

  TOptixTransferHeader = record
    Id          : TGUID;
    PayloadSize : Cardinal;
    State       : TOptixTransferState;
    Direction   : TOptixTransferDirection;
  end;

  TOptixTransferTask = class
  private
    FFileHandle : THandle;
    FFileSize   : Int64;
    FWorkCount  : Int64;
    FDirection  : TOptixTransferDirection;

    {@M}
    function GetState() : TOptixTransferState;
  public
    {@M}
    procedure IncWorkCount(const AValue : UInt64);

    {@C}
    constructor Create(const AFileHandle : THandle; const ADirection : TOptixTransferDirection);
    destructor Destroy(); override;

    {@G}
    property FileHandle : THandle                 read FFileHandle;
    property FileSize   : Int64                   read FFileSize;
    property WorkCount  : Int64                   read FWorkCount;
    property State      : TOptixTransferState     read GetState;
    property Direction  : TOptixTransferDirection read FDirection;
  end;

  TOptixFileTransferOrchestratorThread = class(TOptixClientThread)
  private
    FTransferQueue : TThreadedQueue<TOptixTransfer>;
  protected
    {@M}
    procedure ClientExecute(); override;
  public
    {@C}
    constructor Create(const ARemoteAddress : String; const ARemotePort : Word; const AClientKind : TClientKind); override;
    destructor Destroy(); override;
  end;

implementation

uses System.SysUtils, Winapi.Windows, System.SyncObjs, Optix.Exceptions;

(* TOptixTransferTask.Create *)

{ TOptixTransferTask.Create }
constructor TOptixTransferTask.Create(const AFileHandle : THandle; const ADirection : TOptixTransferDirection);
begin
  inherited Create();
  ///

  FFileHandle := AFileHandle;
  FWorkCount  := 0;
  FDirection  := ADirection;

  ///
  if not GetFileSizeEx(FFileHandle, FFileSize) then
    raise EWindowsException.Create('GetFileSizeEx');
end;

{ TOptixTransferTask.Destroy }
destructor TOptixTransferTask.Destroy();
begin
  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);

  ///
  inherited Destroy();
end;

{ TOptixTransferTask.IncWorkCount }
procedure TOptixTransferTask.IncWorkCount(const AValue : UInt64);
begin
  if FWorkCount = FFileSize then
    Exit();

  Inc(FWorkCount, AValue);

  if FWorkCount > FFileSize then
    FWorkCount := FFileSize;
end;

{ TOptixTransferTask.GetState }
function TOptixTransferTask.GetState() : TOptixTransferState;
begin
  if FWorkCount < FFileSize then
    result := otsProgress
  else if FWorkCount = FFileSize then
    result := otsEnd
  else if FWorkCount = 0 then
    result := otsBegin;
end;

(* TOptixFileTransferOrchestratorThread *)

{ TOptixFileTransferOrchestratorThread.ClientExecute }
procedure TOptixFileTransferOrchestratorThread.ClientExecute();
begin
  if not Assigned(FTransferQueue) then
    Exit();
  ///

  var ATasks := TObjectDictionary<TOptixTransfer, TOptixTransferTask>.Create([doOwnsKeys, doOwnsValues]);
  try
    while not Terminated do begin
      var ATransfer : TOptixTransfer := nil;
      ///

      // New Transfer Request --------------------------------------------------
      while (FTransferQueue.PopItem(ATransfer) = TWaitResult.wrSignaled) do begin
        if Terminated then
          break;
        ///

        var ADesiredAccessFlag       : DWORD := 0;
        var AShareModeFlag           : DWORD := 0;
        var ACreationDispositionFlag : DWORD := 0;

        var ADirection : TOptixTransferDirection;

        // Server Req File Download
        if ATransfer is TOptixUploadFile then begin
          ADesiredAccessFlag       := GENERIC_READ;
          AShareModeFlag           := FILE_SHARE_READ;
          ACreationDispositionFlag := OPEN_EXISTING;

          ///
          ADirection := otdClientIsUploading;
        end
        // Server Req File upload
        else if ATransfer is TOptixDownloadFile then begin
          ADesiredAccessFlag       := GENERIC_WRITE;
          ACreationDispositionFlag := CREATE_ALWAYS;

          ///
          ADirection := otdClientIsDownloading;
        end else begin
          // Should never happend

          FreeAndNil(ATransfer);

          continue;
        end;


        // Create File Handle
        var hFile := CreateFileW(
          PWideChar(ATransfer.FilePath),
          ADesiredAccessFlag,
          AShareModeFlag,
          nil,
          ACreationDispositionFlag,
          FILE_ATTRIBUTE_NORMAL,
          0
        );
        if hFile = INVALID_HANDLE_VALUE then begin
          // TODO: notify main handler, exception packet

          FreeAndNil(ATransfer);

          ///
          continue;
        end;

        ///
        ATasks.Add(ATransfer, TOptixTransferTask.Create(hFile, ADirection));
      end;
      // -----------------------------------------------------------------------

      // ...

      var AHeader : TOptixTransferHeader;

      // Process Transfer ------------------------------------------------------
      for ATransfer in ATasks.Keys do begin
        var ATask : TOptixTransferTask;

        if not ATasks.TryGetValue(ATransfer, ATask) then
          continue;

        AHeader.Id          := ATransfer.TransferId;
        AHeader.PayloadSize := 0; // TODO
        AHeader.State       := ATask.State;
        AHeader.Direction   := ATask.Direction;

        ...continuer ici...
      end;
      // -----------------------------------------------------------------------
    end;
  finally
    FreeAndNil(ATasks);
  end;
end;


{ TOptixFileTransferOrchestratorThread.Create }
constructor TOptixFileTransferOrchestratorThread.Create(const ARemoteAddress : String; const ARemotePort : Word; const AClientKind : TClientKind);
begin
  inherited;
  ///

  FTransferQueue := TThreadedQueue<TOptixTransfer>.Create(1024, INFINITE, 0);
end;

{ TOptixFileTransferOrchestratorThread.Destroy }
destructor TOptixFileTransferOrchestratorThread.Destroy();
begin
  if Assigned(FTransferQueue) then begin
    FTransferQueue.DoShutDown();

    // TODO: Free un-poped resources

    ///
    FreeAndNil(FTransferQueue);
  end;

  ///
  inherited Destroy();
end;

end.
