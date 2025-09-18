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

unit Optix.Func.Commands;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Winapi.Windows,

  XSuperObject,

  Optix.Interfaces, Optix.Protocol.Packet;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixCommand = class(TOptixPacket);

  // Simple Commands
  TOptixCommandTerminate = class(TOptixCommand);
  TOptixCommandRefreshProcess = class(TOptixCommand);
  TOptixCommandRefreshDrives = class(TOptixCommand);

  // Parameterized Commands
  TOptixCommandProcess = class(TOptixCommand)
  private
    FProcessId : Cardinal;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal) overload;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property ProcessId : Cardinal read FProcessId;
  end;

  TOptixCommandKillProcess = class(TOptixCommandProcess);

  TOptixCommandProcessDump = class(TOptixCommandProcess)
  private
    FDestTempPath : Boolean;
    FDestFilePath : String;
    FTypesValue   : DWORD;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal; const ADestFilePath : String; const ATypesValue : DWORD); overload;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property DestTempPath : Boolean read FDestTempPath;
    property DestFilePath : String  read FDestFilePath;
    property TypesValue   : DWORD   read FTypesValue;
  end;

  TOptixCommandRefreshFiles = class(TOptixCommand)
  private
    FPath : String;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const APath : String) overload;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property Path : String read FPath;
  end;

  TOptixCommandTransfer = class(TOptixCommand)
  private
    FTransferId : TGUID;

    // FFilePath in TOptixDownloadFile -> File to download (Client)
    // FFilePath in TOptixUploadFile   -> Uploaded destination file path (Client)
    FFilePath   : String;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(const AFilePath : String; const ATransferId : TGUID); overload;

    {@G}
    property TransferId : TGUID  read FTransferId;
    property FilePath   : String read FFilePath;
  end;

  TOptixCommandDownloadFile = class(TOptixCommandTransfer);

  TOptixCommandUploadFile = class(TOptixCommandTransfer)
  private
    FFileSize : UInt64;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property FileSize : UInt64 read FFileSize;
  end;

  TOptixStartShellInstance = class(TOptixCommand);

  TOptixShellInstance = class(TOptixCommand)
  private
    FInstanceId : TGUID;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(const AInstanceId : TGUID); override;

    {@G}
    property InstanceId : TGUID read FInstanceId;
  end;

  TOptixTerminateShellInstance = class(TOptixShellInstance);
  TOptixBreakShellInstance = class(TOptixShellInstance);

  TOptixStdinShellInstance = class(TOptixShellInstance)
  private
    FCommandLine : String;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(const AInstanceId : TGUID; const ACommandLine : String); overload;

    {@G}
    property CommandLine : String read FCommandLine;
  end;

implementation

(* TOptixCommandProcess *)

{ TOptixCommandProcess.Create }
constructor TOptixCommandProcess.Create(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;
end;

{ TOptixCommandProcess.Serialize }
function TOptixCommandProcess.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.I['ProcessId'] := FProcessId;
end;

{ TOptixCommandProcess.DeSerialize }
procedure TOptixCommandProcess.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FProcessId := ASerializedObject.I['ProcessId'];
end;

(* TOptixCommandRefreshFiles *)

{ TOptixCommandRefreshFiles.Create }
constructor TOptixCommandRefreshFiles.Create(const APath : String);
begin
  inherited Create();
  ///

  FPath := APath;
end;

{ TOptixCommandRefreshFiles.Serialize }
function TOptixCommandRefreshFiles.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['Path'] := FPath;
end;

{ TOptixCommandRefreshFiles.DeSerialize }
procedure TOptixCommandRefreshFiles.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FPath := ASerializedObject.S['Path'];
end;

(* TOptixCommandTransfer *)

{ TOptixCommandTransfer.Create }
constructor TOptixCommandTransfer.Create(const AFilePath : String; const ATransferId : TGUID);
begin
  inherited Create();
  ///

  FFilePath   := AFilePath;
  FTransferId := ATransferId;
end;

{ TOptixCommandTransfer.Serialize }
function TOptixCommandTransfer.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['TransferId'] := FTransferId.ToString();
  result.S['FilePath']   := FFilePath;
end;

{ TOptixCommandTransfer.DeSerialize }
procedure TOptixCommandTransfer.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FTransferId := TGUID.Create(ASerializedObject.S['TransferId']);
  FFilePath   := ASerializedObject.S['FilePath'];
end;

(* TOptixCommandUploadFile *)

{ TOptixCommandUploadFile.Serialize }
function TOptixCommandUploadFile.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.I['FileSize'] := FFileSize;
end;

{ TOptixCommandUploadFile.DeSerialize }
procedure TOptixCommandUploadFile.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FFileSize := ASerializedObject.I['FileSize'];
end;

(* TOptixCommandProcessDump *)

{ TOptixCommandProcessDump.Create }
constructor TOptixCommandProcessDump.Create(const AProcessId : Cardinal; const ADestFilePath : String; const ATypesValue : DWORD);
begin
  inherited Create(AProcessId);
  ///

  FDestTempPath := String.IsNullOrWhiteSpace(ADestFilePath);
  if not FDestTempPath then
    FDestFilePath := ADestFilePath.Trim()
  else
    FDestFilePath := '';

  FTypesValue := ATypesValue;
end;

{ TOptixCommandProcessDump.Serialize }
function TOptixCommandProcessDump.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.B['DestTempPath'] := FDestTempPath;
  result.S['DestFilePath'] := FDestFilePath;
  result.I['TypesValue']   := FTypesValue;
end;

{ TOptixCommandProcessDump.DeSerialize }
procedure TOptixCommandProcessDump.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FDestTempPath := ASerializedObject.B['DestTempPath'];
  FDestFilePath := ASerializedObject.S['DestFilePath'];
  FTypesValue   := ASerializedObject.I['TypesValue'];
end;

(* TOptixShellInstance *)

{ TOptixShellInstance.Serialize }
function TOptixShellInstance.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['InstanceId'] := FInstanceId.ToString();
end;

{ TOptixShellInstance.DeSerialize }
procedure TOptixShellInstance.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FInstanceId := TGUID.Create(ASerializedObject.S['InstanceId']);
end;

{ TOptixShellInstance.Create }
constructor TOptixShellInstance.Create(const AInstanceId : TGUID);
begin
  inherited Create();
  ///

  FInstanceId := AInstanceId;
end;

(* TOptixStdinShellInstance *)

{ TOptixStdinShellInstance.Create }
constructor TOptixStdinShellInstance.Create(const AInstanceId : TGUID; const ACommandLine : String);
begin
  inherited Create(AInstanceId);
  ///

  FCommandLine := ACommandLine;
end;

{ TOptixStdinShellInstance.Serialize }
function TOptixStdinShellInstance.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['CommandLine'] := FCommandLine;
end;

{ TOptixStdinShellInstance.DeSerialize }
procedure TOptixStdinShellInstance.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FCommandLine := ASerializedObject.S['CommandLine'];
end;

end.
