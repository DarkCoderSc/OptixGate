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

unit Optix.Shared.Protocol.FileTransfer;

interface

uses System.Classes, Winapi.Windows, Optix.Sockets.Helper;

const
  FILE_CHUNK_SIZE = 8192;

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

  TOptixTransferTask = class
  private
    FFileHandle : THandle;
    FFileSize   : Int64;
    FWorkCount  : Int64;
    FBuffer     : array[0..FILE_CHUNK_SIZE-1] of byte;

    {@M}
    function GetState() : TOptixTransferState;
  protected
    {@M}
    procedure IncWorkCount(const AValue : UInt64);
  public
    {@C}
    constructor Create(const AFilePath : String); virtual;
    destructor Destroy(); override;

    {@G}
    property WorkCount : Int64               read FWorkCount;
    property State     : TOptixTransferState read GetState;
  end;

  TOptixDownloadTask = class(TOptixTransferTask)
  public
    {@M}
    procedure DownloadChunk(const AClient : TClientSocket);

    {@C}
    constructor Create(const AFilePath : String); override;

    {@G/S}
    property FileSize : Int64 read FFileSize write FFileSize;
  end;

  TOptixUploadTask = class(TOptixTransferTask)
  public
    {@M}
    procedure UploadChunk(const AClient : TClientSocket);

    {@C}
    constructor Create(const AFilePath : String); override;

    {@G}
    property FileSize : Int64 read FFileSize;
  end;

implementation

uses System.SysUtils, Optix.Exceptions, System.Math;

(* TOptixTransferTask *)

{ TOptixTransferTask.Create }
constructor TOptixTransferTask.Create(const AFilePath : String);
begin
  inherited Create();
  ///

  FFileHandle := INVALID_HANDLE_VALUE;
  FFileSize   := 0;
  FWorkCount  := 0;
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
  if FWorkCount = 0 then
    result := otsBegin
  else if FWorkCount = FFileSize then
    result := otsEnd
  else if FWorkCount < FFileSize then
    result := otsProgress;
end;

(* TOptixDownloadTask *)

{ TOptixDownloadTask.DownloadChunk }
procedure TOptixDownloadTask.DownloadChunk(const AClient : TClientSocket);
begin
  if (FFileHandle = INVALID_HANDLE_VALUE) or (FFileSize = 0) or not Assigned(AClient) then
    Exit(); // TODO: Log?

  var ABufferSize := min(FILE_CHUNK_SIZE, (FFileSize - FWorkCount));
  var ABytesWritten : DWORD;

  AClient.Recv(FBuffer, ABufferSize);

  if not WriteFile(FFileHandle, FBuffer[0], ABufferSize, ABytesWritten, 0) then
    Exit(); // TODO: raise exception and handle it

  ///
  IncWorkCount(ABufferSize);
end;

{ TOptixDownloadTask.WriteChunk }
constructor TOptixDownloadTask.Create(const AFilePath : String);
begin
  inherited Create(AFilePath);
  ///

  // Create File Handle
  FFileHandle := CreateFileW(PWideChar(AFilePath), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create(Format('CreateFileW->[%s]', [AFilePath]));
end;

(* TOptixUploadTask *)

{ TOptixUploadTask.UploadChunk }
procedure TOptixUploadTask.UploadChunk(const AClient : TClientSocket);
begin
  if (FFileHandle = INVALID_HANDLE_VALUE) or (FFileSize = 0) or not Assigned(AClient) then
    Exit(); // TODO: Log?

  var ABytesRead : DWORD;
  var ABufferSize := min(FILE_CHUNK_SIZE, (FFileSize - FWorkCount));

  if not ReadFile(FFileHandle, FBuffer[0], ABufferSize, ABytesRead, 0) then
    Exit(); // TODO: raise exception and handle it.

  AClient.Send(FBuffer[0], ABufferSize);

  IncWorkCount(ABufferSize);
end;

{ TOptixUploadTask.WriteChunk }
constructor TOptixUploadTask.Create(const AFilePath : String);
begin
  inherited Create(AFilePath);
  ///

  // Create File Handle
  FFileHandle := CreateFileW(PWideChar(AFilePath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create(Format('CreateFileW->[%s]', [AFilePath]));

  if not GetFileSizeEx(FFileHandle, FFileSize) then
    raise EWindowsException.Create(Format('GetFileSizeEx->[%s]', [AFilePath]));
end;

end.
