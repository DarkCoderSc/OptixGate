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

unit Optix.Func.Commands.ContentReader;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  XSuperObject,

  Optix.Func.Commands.Base, Optix.Shared.Classes, Optix.FileSystem.Helper;
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

  TOptixCommandCloseContentReader = class(TOptixCommandContentReader);

  TOptixCommandBrowseContentReader = class(TOptixCommandContentReader)
  private
    [OptixSerializableAttribute]
    FPageNumber : UInt64;
  public
    {@G}
    property PageNumber : UInt64 read FPageNumber;

    {@C}
    constructor Create(const APageNumber : UInt64); overload;
  end;

  TOptixCommandContentReaderPage = class(TOptixCommandContentReader)
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
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
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

  TOptixCommandContentReaderFirstPage = class(TOptixCommandContentReaderPage);

  TOptixCommandContentReaderPageClass = class of TOptixCommandContentReaderPage;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
   System.SysUtils,

   Winapi.Windows;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixCommandBrowseContentReader *)

{ TOptixCommandBrowseContentReader.Create }
constructor TOptixCommandBrowseContentReader.Create(const APageNumber: UInt64);
begin
  inherited Create();
  ///

  FPageNumber := APageNumber;
end;

(* TOptixCommandCreateFileContentReader *)

{ TOptixCommandCreateFileContentReader.Create }
constructor TOptixCommandCreateFileContentReader.Create(const AFilePath : String; const APageSize : UInt64);
begin
  inherited Create();
  ///

  FFilePath := AFilePath;
  FPageSize := APageSize;
end;

(* TOptixCommandContentReaderPage *)

{ TOptixCommandContentReaderPage.Create }
constructor TOptixCommandContentReaderPage.Create(const AWindowGUID : TGUID; const AReader : TContentReader;
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
    if Assigned(pBuffer) and (ABufferSize > 0) then
      FData := TOptixMemoryObject.Create(pBuffer, ABufferSize, True);
  finally
    FreeMem(pBuffer, ABufferSize);
  end;
end;

{ TOptixCommandContentReaderPage.Destroy }
destructor TOptixCommandContentReaderPage.Destroy();
begin
  if Assigned(FData) then
    FreeAndNil(FData);

  ///
  inherited Destroy();
end;

{ TOptixCommandContentReaderPage.DeSerialize }
procedure TOptixCommandContentReaderPage.DeSerialize(const ASerializedObject : ISuperObject);
begin
  FData := nil;

  ///
  inherited;
end;

{ TOptixCommandContentReaderPage.GetPageOffset }
function TOptixCommandContentReaderPage.GetPageOffset() : UInt64;
begin
  if Assigned(FData) then begin
    result := FPageNumber * FPageSize;
  end else
    result := 0;
end;

{ TOptixCommandContentReaderPage.GetData }
function TOptixCommandContentReaderPage.GetData() : Pointer;
begin
  if not Assigned(FData) then
    result := nil
  else
    result := FData.Address;
end;

{ TOptixCommandContentReaderPage.GetDataSize }
function TOptixCommandContentReaderPage.GetDataSize() : UInt64;
begin
  if not Assigned(FData) or not Assigned(FData.Address) then
    result := 0
  else
    result := FData.Size;
end;

end.
