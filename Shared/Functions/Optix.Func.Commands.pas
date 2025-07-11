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

uses Winapi.Windows, System.Classes, System.SysUtils, Optix.Interfaces,
     XSuperObject, Optix.Protocol.Packet;

type
  TOptixCommand = class(TOptixPacket);

  // Simple Commands
  TOptixCommandTerminate = class(TOptixCommand);
  TOptixRefreshProcess   = class(TOptixCommand);
  TOptixRefreshDrives    = class(TOptixCommand);

  // Parameterized Commands
  TOptixKillProcess = class(TOptixCommand)
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

  TOptixRefreshFiles = class(TOptixCommand)
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

  TOptixTransfer = class(TOptixCommand)
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

    {@G}
    property TransferId : TGUID  read FTransferId;
    property FilePath   : String read FFilePath;
  end;

  TOptixDownloadFile = class(TOptixTransfer);
  TOptixUploadFile = class(TOptixTransfer);

implementation

(* TOptixKillProcess **********************************************************)

{ TOptixKillProcess.Create }
constructor TOptixKillProcess.Create(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;
end;

{ TOptixKillProcess.Serialize }
function TOptixKillProcess.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.I['ProcessId'] := FProcessId;
end;

{ TOptixKillProcess.DeSerialize }
procedure TOptixKillProcess.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  if not Assigned(ASerializedObject) then
    Exit();

  FProcessId := ASerializedObject.I['ProcessId'];
end;

(* TOptixRefreshFiles **********************************************************)

{ TOptixRefreshFiles.Create }
constructor TOptixRefreshFiles.Create(const APath : String);
begin
  inherited Create();
  ///

  FPath := APath;
end;

{ TOptixRefreshFiles.Serialize }
function TOptixRefreshFiles.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['Path'] := FPath;
end;

{ TOptixRefreshFiles.DeSerialize }
procedure TOptixRefreshFiles.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  if not Assigned(ASerializedObject) then
    Exit();

  FPath := ASerializedObject.S['Path'];
end;

(* TOptixTransfer *************************************************************)

{ TOptixTransfer.Serialize }
function TOptixTransfer.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['TransferId'] := FTransferId.ToString();
  result.S['FilePath']   := FFilePath;
end;

{ TOptixTransfer.DeSerialize }
procedure TOptixTransfer.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  if not Assigned(ASerializedObject) then
    Exit();

  FTransferId := TGUID.Create(ASerializedObject.S['TransferId']);
  FFilePath   := ASerializedObject.S['FilePath'];
end;

end.
