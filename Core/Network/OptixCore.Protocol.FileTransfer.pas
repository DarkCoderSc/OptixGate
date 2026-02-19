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



unit OptixCore.Protocol.FileTransfer;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Winapi.Windows,

  OptixCore.Sockets.Helper;
// ---------------------------------------------------------------------------------------------------------------------

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
    otsEnd,
    otsError
  );

  TOptixTransferTask = class
  private
    FFileHandle : THandle;
    FWorkCount  : Int64;
    FBuffer     : array[0..FILE_CHUNK_SIZE-1] of byte;
    FIsEmpty    : Boolean;
    FCanceled   : Boolean;

    {@M}
    function GetState() : TOptixTransferState;
  protected
    FFileSize : Int64;

    {@M}
    procedure IncWorkCount(const AValue : UInt64);
  public
    {@C}
    constructor Create(const AFilePath : String); virtual;
    destructor Destroy(); override;

    {@G}
    property WorkCount : Int64               read FWorkCount;
    property State     : TOptixTransferState read GetState;
    property FileSize  : Int64               read FFileSize;
    property IsEmpty   : Boolean             read FIsEmpty;

    {@G/S}
    property Canceled  : Boolean  read FCanceled write FCanceled;
  end;

  TOptixDownloadTask = class(TOptixTransferTask)
  public
    {@M}
    procedure DownloadChunk(const AClient : TClientSocket);
    procedure SetFileSize(const AValue : Int64);

    {@C}
    constructor Create(const AFilePath : String); override;
  end;

  TOptixUploadTask = class(TOptixTransferTask)
  public
    {@M}
    procedure UploadChunk(const AClient : TClientSocket);

    {@C}
    constructor Create(const AFilePath : String); override;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Math,

  OptixCore.Exceptions;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixTransferTask *)

constructor TOptixTransferTask.Create(const AFilePath : String);
begin
  inherited Create();
  ///

  FFileHandle := INVALID_HANDLE_VALUE;
  FFileSize   := 0;
  FWorkCount  := 0;
  FIsEmpty    := False;
  FCanceled   := False;
end;

destructor TOptixTransferTask.Destroy();
begin
  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);

  ///
  inherited Destroy();
end;

procedure TOptixTransferTask.IncWorkCount(const AValue : UInt64);
begin
  if FWorkCount = FFileSize then
    Exit();

  Inc(FWorkCount, AValue);

  if FWorkCount > FFileSize then
    FWorkCount := FFileSize;
end;

function TOptixTransferTask.GetState() : TOptixTransferState;
begin
  if FWorkCount = 0 then
    result := otsBegin
  else if FWorkCount = FFileSize then
    result := otsEnd
  else if FWorkCount < FFileSize then
    result := otsProgress
  else
    result := otsError;
end;

(* TOptixDownloadTask *)

procedure TOptixDownloadTask.DownloadChunk(const AClient : TClientSocket);
begin
  if (FFileHandle = INVALID_HANDLE_VALUE) or (FFileSize = 0) or not Assigned(AClient) then
    Exit(); // TODO: Log?

  var ABufferSize := min(FILE_CHUNK_SIZE, (FFileSize - FWorkCount));
  var ABytesWritten : DWORD;

  AClient.Recv(FBuffer, ABufferSize);

  if not WriteFile(FFileHandle, FBuffer[0], ABufferSize, ABytesWritten, nil) then
    Exit(); // TODO: raise exception and handle it

  ///
  IncWorkCount(ABufferSize);
end;

procedure TOptixDownloadTask.SetFileSize(const AValue : Int64);
begin
  FIsEmpty := AValue = 0;

  ///
  FFileSize := AValue;
end;

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

procedure TOptixUploadTask.UploadChunk(const AClient : TClientSocket);
begin
  if (FFileHandle = INVALID_HANDLE_VALUE) or (FFileSize = 0) or not Assigned(AClient) then
    Exit(); // TODO: Log?

  var ABytesRead : DWORD;
  var ABufferSize := min(FILE_CHUNK_SIZE, (FFileSize - FWorkCount));

  if not ReadFile(FFileHandle, FBuffer[0], ABufferSize, ABytesRead, nil) then
    Exit(); // TODO: raise exception and handle it.

  AClient.Send(FBuffer[0], ABufferSize);

  IncWorkCount(ABufferSize);
end;

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

  FIsEmpty := FFileSize = 0;
end;

end.
