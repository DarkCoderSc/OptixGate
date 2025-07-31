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

unit Optix.Task;

interface

uses Generics.Collections, XSuperObject, System.Classes, System.Threading, Optix.Protocol.Packet, Optix.Func.Commands;

type
  TOptixTaskState = (
    otsPending,
    otsRunning,
    otsFailed,
    otsSuccess
  );

  TOptixTaskResult = class(TOptixPacket)
  private
    FId            : TGUID;
    FTaskClassName : String;
    FState         : TOptixTaskState;
    FResult        : ISuperObject;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const ATaskId : TGUID; const ATaskClassName : String); overload;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property Id            : TGUID           read FId;
    property TaskClassName : String          read FTaskClassName;
    property State         : TOptixTaskState read FState;
  end;

  TOptixTask = class
  private
    FId      : TGUID;
    FTask    : IFuture<ISuperObject>;
    FCommand : TOptixCommand;

    {@M}
    function Execute() : ISuperObject;
    function IsCompleted() : Boolean;
    function GetResult() : ISuperObject;
  protected
    {@M}
    function TaskCode() : ISuperObject; virtual; abstract;
  public
    {@C}
    constructor Create(const ACommand : TOptixCommand);
    destructor Destroy(); override;

    {@M}
    procedure Start(const AId : TGUID);

    {@G}
    property Completed : Boolean      read IsCompleted;
    property Result    : ISuperObject read GetResult;
  end;

implementation

uses Winapi.Windows, System.SysUtils;

(* TOptixTaskResult *)

{ TOptixTaskResult.Create }
constructor TOptixTaskResult.Create(const ATaskId : TGUID; const ATaskClassName : String);
begin
  inherited Create();
  ///

  FId            := ATaskId;
  FTaskClassName := ATaskClassName;
  FState         := otsPending;
  FResult        := SO();
end;

{ TOptixTaskResult.DeSerialize }
procedure TOptixTaskResult.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FId            := TGUID.Create(ASerializedObject.S['Id']);
  FTaskClassName := ASerializedObject.S['TaskClassName'];
  FState         := TOptixTaskState(ASerializedObject.I['State']);
  FResult        := ASerializedObject.O['Result'];
end;

{ TOptixTaskResult.Serialize }
function TOptixTaskResult.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['Id']            := FId.TOString();
  result.S['TaskClassName'] := FTaskClassName;
  result.I['State']         := Integer(FState);
  result.O['result']        := FResult;
end;

(* TOptixTask *)

{ TOptixTask.Execute }
function TOptixTask.Execute() : ISuperObject;
begin
  result := SO();
  ///

  var ASuccess := True;
  try
    var AStartTick := GetTickCount64();
    try
      result.O['result'] := TaskCode();
    finally
      result.I['elapsed_time'] := GetTickCount64() - AStartTick;
    end;
  except
    on E : Exception do begin
      ASuccess := False;
      ///

      result.S['exception'] := E.Message;
    end;
  end;

  ///
  result.B['success'] := ASuccess;
end;

{ TOptixTask.IsCompleted }
function TOptixTask.IsCompleted() : Boolean;
begin
  result := Assigned(FTask) and (FTask.Status in [TTaskStatus.Completed, TTaskStatus.Exception]);
end;

{ TOptixTask.Start }
procedure TOptixTask.Start(const AId : TGUID);
begin
  FId := AId;
  ///

  if FTask = nil then
    FTask := TTask.Future<ISuperObject>(
      function : ISuperObject begin
        result := Execute();
      end
    );
end;

{ TOptixTask.GetResult }
function TOptixTask.GetResult() : ISuperObject;
begin
  result := nil;
  ///

  if IsCompleted() then
    try
      result := FTask.Value
    except

    end;
end;

{ TOptixTask.Create }
constructor TOptixTask.Create(const ACommand : TOptixCommand);
begin
  inherited Create();
  ///

  FId      := TGUID.Empty;
  FTask    := nil;
  FCommand := FCommand;
end;

{ TOptixTask.Destroy }
destructor TOptixTask.Destroy();
begin
  FTask := nil;

  if Assigned(FCommand) then
    FreeAndNil(FCommand);

  ///
  inherited Destroy();
end;

end.
