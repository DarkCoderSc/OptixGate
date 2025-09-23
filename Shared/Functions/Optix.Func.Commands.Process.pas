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

unit Optix.Func.Commands.Process;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Winapi.Windows,

  XSuperObject,

  Optix.Func.Commands.Base, Optix.Process.Enum, Optix.Shared.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  {@TEMPLATE}
  TOptixCommandAndResponseProcess = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FProcessId : Cardinal;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal) overload;

    {@G}
    property ProcessId : Cardinal read FProcessId;
  end;

  TOptixCommandKillProcess = class(TOptixCommandAndResponseProcess)
  public
    {@M}
    procedure DoAction(); override;
  end;

  TOptixCommandProcessDump = class(TOptixCommandTask)
  private
    [OptixSerializableAttribute]
    FProcessId : Cardinal;

    [OptixSerializableAttribute]
    FDestTempPath : Boolean;

    [OptixSerializableAttribute]
    FDestFilePath : String;

    [OptixSerializableAttribute]
    FTypesValue : DWORD;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal; const ADestFilePath : String; const ATypesValue : DWORD); overload;

    {@M}
    function CreateTask(const ACommand : TOptixCommand) : TOptixTask; override;

    {@G}
    property ProcessId    : Cardinal read FProcessId;
    property DestTempPath : Boolean  read FDestTempPath;
    property DestFilePath : String   read FDestFilePath;
    property TypesValue   : DWORD    read FTypesValue;
  end;

  TOptixCommandRefreshProcess = class(TOptixCommandActionResponse)
  private
    FList : TObjectList<TProcessInformation>;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;
    procedure DoAction(); override;

    {@C}
    constructor Create(); override;
    destructor Destroy(); override;

    {@G}
    property List : TObjectList<TProcessInformation> read FList;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.Process.Helper, Optix.Task.ProcessDump;
// ---------------------------------------------------------------------------------------------------------------------

(***********************************************************************************************************************

  TOptixCommandAndResponseProcess

***********************************************************************************************************************)

{ TOptixCommandAndResponseProcess.Create }
constructor TOptixCommandAndResponseProcess.Create(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;
end;

(***********************************************************************************************************************

  TOptixCommandKillProcess

***********************************************************************************************************************)

{ TOptixCommandKillProcess.DoAction }
procedure TOptixCommandKillProcess.DoAction();
begin
  TProcessHelper.TerminateProcess(FProcessId);
end;

(***********************************************************************************************************************

  TOptixCommandProcessDump

***********************************************************************************************************************)

{ TOptixCommandProcessDump.Create }
constructor TOptixCommandProcessDump.Create(const AProcessId : Cardinal; const ADestFilePath : String; const ATypesValue : DWORD);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;

  FDestTempPath := String.IsNullOrWhiteSpace(ADestFilePath);
  if not FDestTempPath then
    FDestFilePath := ADestFilePath.Trim()
  else
    FDestFilePath := '';

  FTypesValue := ATypesValue;
end;

{ TOptixCommandProcessDump.CreateTask }
function TOptixCommandProcessDump.CreateTask(const ACommand : TOptixCommand) : TOptixTask;
begin
  if Assigned(ACommand) then
    result := TOptixProcessDumpTask.Create(ACommand)
  else
    result := nil;
end;

(***********************************************************************************************************************

  TOptixCommandRefreshProcess

***********************************************************************************************************************)

{ TOptixCommandRefreshProcess.Refresh }
procedure TOptixCommandRefreshProcess.DoAction();
begin
  TOptixEnumProcess.Enum(FList);
end;

{ TOptixCommandRefreshProcess.DeSerialize }
procedure TOptixCommandRefreshProcess.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FList.Clear();

  for var I := 0 to ASerializedObject.A['List'].Length -1 do
    FList.Add(TProcessInformation.Create(ASerializedObject.A['List'].O[I]));
end;

{ TOptixCommandRefreshProcess.Serialize }
function TOptixCommandRefreshProcess.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FList do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.A['List'] := AJsonArray;
end;

{ TOptixCommandRefreshProcess.Create }
constructor TOptixCommandRefreshProcess.Create();
begin
  inherited;
  ///

  FList := TObjectList<TProcessInformation>.Create(True);
end;

{ TOptixCommandRefreshProcess.Destroy }
destructor TOptixCommandRefreshProcess.Destroy();
begin
  if Assigned(FList) then
    FreeAndNil(FList);

  ///
  inherited;
end;


end.
