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



unit OptixCore.Commands.ContentReader;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  OptixCore.Commands.Base, OptixCore.Classes, OptixCore.System.FileSystem;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixCommandContentReader = class(TOptixCommand);

  TOptixCommandCreateFileContentReader = class(TOptixCommandContentReader)
  private
    [OptixSerializableAttribute]
    FFilePath : String;

    [OptixSerializableAttribute]
    FPageSize : UInt64;
  public
    {@C}
    constructor Create(const AFilePath : String; const APageSize : UInt64); overload;

    {@G}
    property FilePath : String read FFilePath;
    property PageSize : UInt64 read FPageSize;
  end;

  TOptixCommandDeleteContentReader = class(TOptixCommandContentReader);

  TOptixCommandGetContentReaderPage = class(TOptixCommandContentReader)
  private
    [OptixSerializableAttribute]
    FPageNumber : UInt64;

    [OptixSerializableAttribute]
    FPageSize : UInt64;
  public
    {@G}
    property PageNumber : UInt64 read FPageNumber;
    property PageSize   : UInt64 read FPageSize;

    {@C}
    constructor Create(const APageNumber : UInt64; const ANewPageSize : UInt64 = 0); overload;
  end;

  TOptixCommandReadContentReaderPage = class(TOptixCommandContentReader)
  private
    [OptixSerializableAttribute]
    FData : TOptixMemoryObject;

    [OptixSerializableAttribute]
    FPageNumber : UInt64;

    [OptixSerializableAttribute]
    FPageCount : UInt64;

    [OptixSerializableAttribute]
    FPageSize : UInt64;

    [OptixSerializableAttribute]
    FTotalSize : UInt64;

    [OptixSerializableAttribute]
    FFilePath : String;

    {@M}
    function GetPageOffset() : UInt64;
    function GetData() : Pointer;
    function GetDataSize() : UInt64;
  public
    {@M}
    procedure AfterCreate(); override;

    {@C}
    constructor Create(const AWindowGUID : TGUID; const AReader : TContentReader; const APageNumber : UInt64); overload;
    destructor Destroy(); override;

    {@G}
    property Data       : Pointer read GetData;
    property DataSize   : UInt64  read GetDataSize;
    property PageNumber : UInt64  read FPageNumber;
    property PageCount  : UInt64  read FPageCount;
    property TotalSize  : UInt64  read FTotalSize;
    property FilePath   : String  read FFilePath;
    property PageSize   : UInt64  read FPageSize;
    property PageOffset : UInt64  read GetPageOffset;
  end;

  TOptixCommandReadContentReaderPageFirstPage = class(TOptixCommandReadContentReaderPage);

  TOptixCommandReadContentReaderPageClass = class of TOptixCommandReadContentReaderPage;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
   System.SysUtils,

   Winapi.Windows;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixCommandGetContentReaderPage *)

constructor TOptixCommandGetContentReaderPage.Create(const APageNumber : UInt64; const ANewPageSize : UInt64 = 0);
begin
  inherited Create();
  ///

  FPageNumber := APageNumber;
  FPageSize   := ANewPageSize;
end;

(* TOptixCommandCreateFileContentReader *)

constructor TOptixCommandCreateFileContentReader.Create(const AFilePath : String; const APageSize : UInt64);
begin
  inherited Create();
  ///

  FFilePath := AFilePath;
  FPageSize := APageSize;
end;

(* TOptixCommandReadContentReaderPage *)

constructor TOptixCommandReadContentReaderPage.Create(const AWindowGUID : TGUID; const AReader : TContentReader;
  const APageNumber : UInt64);
begin
  inherited Create();
  ///

  if not Assigned(AReader) then
    Exit();

  var pBuffer : Pointer;
  var ABufferSize : UInt64;

  FWindowGUID := AWindowGUID;
  FPageNumber := APageNumber;
  FPageCount  := AReader.PageCount;
  FPageSize   := AReader.PageSize;
  FTotalSize  := AReader.FileSize;
  FFilePath   := AReader.FilePath;

  AReader.ReadPage(FPageNumber, pBuffer, ABufferSize);
  try
    if Assigned(pBuffer) and (ABufferSize > 0) then begin
      FData := TOptixMemoryObject.Create();
      FData.CopyFrom(pBuffer, ABufferSize);
    end;
  finally
    FreeMem(pBuffer, ABufferSize);
  end;
end;

destructor TOptixCommandReadContentReaderPage.Destroy();
begin
  if Assigned(FData) then
    FreeAndNil(FData);

  ///
  inherited Destroy();
end;

procedure TOptixCommandReadContentReaderPage.AfterCreate();
begin
  inherited;
  ///

  FData := nil;
end;

function TOptixCommandReadContentReaderPage.GetPageOffset() : UInt64;
begin
  if Assigned(FData) then begin
    result := FPageNumber * FPageSize;
  end else
    result := 0;
end;

function TOptixCommandReadContentReaderPage.GetData() : Pointer;
begin
  if not Assigned(FData) then
    result := nil
  else
    result := FData.Address;
end;

function TOptixCommandReadContentReaderPage.GetDataSize() : UInt64;
begin
  if not Assigned(FData) or not Assigned(FData.Address) then
    result := 0
  else
    result := FData.Size;
end;

end.
