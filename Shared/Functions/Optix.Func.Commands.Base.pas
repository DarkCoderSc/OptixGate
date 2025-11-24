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



unit Optix.Func.Commands.Base;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils, System.Threading,

  XSuperObject,

  Optix.FileSystem.Enum, Optix.Interfaces, Optix.Shared.Classes, Optix.Protocol.Packet;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixCommand = class(TOptixPacket);

  TOptixBasicCommand = class(TOptixCommand);

  TOptixCommandAction = class(TOptixCommand)
  public
    {$IFNDEF SERVER}
    procedure DoAction(); virtual; abstract;
    {$ENDIF}
  end;

  TOptixCommandActionResponse = class(TOptixCommandAction);

  TOptixTaskState = (
    otsPending,
    otsRunning,
    otsFailed,
    otsSuccess
  );
  TOptixTaskStates = set of TOptixTaskState;

  TOptixTask = class;

  TOptixTaskResult = class(TOptixPacket)
  private
    [OptixSerializableAttribute]
    FSuccess : Boolean;

    [OptixSerializableAttribute]
    FExceptionMessage : String;

    [OptixSerializableAttribute]
    FTaskDuration : UInt64;

    {@M}
    function GetDescription() : String;
  protected
    {@M}
    function GetExtendedDescription() : String; virtual;
  public
    {@M}
    procedure SetTaskDuration(const AValue : UInt64);
    procedure TaskFailed(const AExceptionMessage : String);
    procedure TaskSucceed();
    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(); override;
    constructor Create(const ASerializedObject : ISuperObject); override;
    destructor Destroy(); override;

    {@G}
    property Success          : Boolean read FSuccess;
    property ExceptionMessage : String  read FExceptionMessage;
    property TaskDuration     : UInt64  read FTaskDuration;
    property Description      : String  read GetDescription;
  end;

  TOptixTaskCallback = class(TOptixPacket)
  private
    [OptixSerializableAttribute]
    FId : TGUID;

    [OptixSerializableAttribute]
    FTaskClassName : String;

    [OptixSerializableAttribute]
    FState : TOptixTaskState;

    FResult  : TOptixTaskResult;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@C}
    constructor Create(const ATask : TOptixTask); overload;
    destructor Destroy(); override;

    {@M}
    function Serialize() : ISuperObject; override;

    {@G}
    property Id            : TGUID            read FId;
    property TaskClassName : String           read FTaskClassName;
    property State         : TOptixTaskState  read FState;
    property Result        : TOptixTaskResult read FResult;
  end;

  TOptixTask = class
  private
    FId             : TGUID;
    FTask           : IFuture<TOptixTaskResult>;
    FSuccess        : Boolean;
    FRunningAware   : Boolean;
    FResultIsPulled : Boolean;

    {@M}
    function Execute() : TOptixTaskResult;
    function IsCompleted() : Boolean;
    function IsRunning() : Boolean;
    function GetResult(const APullResult : Boolean = false) : TOptixTaskResult;
  protected
    FCommand : TOptixCommand;

    {@M}
    function TaskCode() : TOptixTaskResult; virtual; abstract;
  public
    {@C}
    constructor Create(const ACommand : TOptixCommand);
    destructor Destroy(); override;

    {@M}
    procedure Start();
    procedure SetTaskId(const ATaskId : TGUID);
    function PullResult() : TOptixTaskResult;

    {@G}
    property Id        : TGUID   read FId;
    property Completed : Boolean read IsCompleted;
    property Running   : Boolean read IsRunning;
    property Success   : Boolean read FSuccess;

    {@G/S}
    property RunningAware : Boolean read FRunningAware write FRunningAware;
  end;

  TOptixCommandTask = class(TOptixCommand)
  public
    {$IFNDEF SERVER}
    function CreateTask(const ACommand : TOptixCommand) : TOptixTask; virtual; abstract;
    {$ENDIF}
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Rtti,

  Winapi.Windows,

  Optix.ClassesRegistry, Optix.Task.ProcessDump;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixTaskCallback *)

constructor TOptixTaskCallback.Create(const ATask : TOptixTask);
begin
  inherited Create();
  ///

  FId            := ATask.Id;
  FTaskClassName := ATask.ClassName;

  if ATask.Completed then begin
    FResult := ATask.PullResult();
    ///

    if ATask.Success then
      FState := otsSuccess
    else
      FState := otsFailed;
  end else begin
    if ATask.Running then
      FState := otsRunning
    else
      FState := otsPending;

    ///
    FResult := nil;
  end;
end;

destructor TOptixTaskCallback.Destroy();
begin
  if Assigned(FResult) then
    FreeAndNil(FResult);

  ///
  inherited Destroy();
end;

procedure TOptixTaskCallback.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  var ASerializedResult := ASerializedObject.O['Result'];
  if Assigned(ASerializedResult) then begin
    if ASerializedResult.Contains('ResultClass') then begin
      var AResultClass := ASerializedResult.S['ResultClass'];
      ///

      FResult := TOptixTaskResult(TClassesRegistry.CreateInstance(AResultClass, [
        TValue.From<ISuperObject>(ASerializedResult)
      ]));
    end;
  end;
end;

function TOptixTaskCallback.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  if Assigned(FResult) then
    result.O['result'] := FResult.Serialize()
  else
    result.O['result'] := SO();
end;

(* TOptixTask *)

function TOptixTask.Execute() : TOptixTaskResult;
begin
  result := nil;
  try
    var AStartTick := GetTickCount64();
    ///

    result := TaskCode();
    if Assigned(result) then begin
      result.SetTaskDuration(GetTickCount64() - AStartTick);

      result.TaskSucceed();
    end;
  except
    on E : Exception do begin
      if not Assigned(result) then
        result := TOptixTaskResult.Create();

      ///
      result.TaskFailed(E.Message);
    end;
  end;
end;

function TOptixTask.IsCompleted() : Boolean;
begin
  result := Assigned(FTask) and (FTask.Status in [TTaskStatus.Completed, TTaskStatus.Exception]);
end;

function TOptixTask.IsRunning() : Boolean;
begin
  result := Assigned(FTask) and (FTask.Status in [TTaskStatus.Running]);
end;

procedure TOptixTask.Start();
begin
  if FTask = nil then
    FTask := TTask.Future<TOptixTaskResult>(
      function : TOptixTaskResult begin
        result := Execute();
      end
    );
end;

procedure TOptixTask.SetTaskId(const ATaskId : TGUID);
begin
  FId := ATaskId;
end;

function TOptixTask.GetResult(const APullResult : Boolean = false) : TOptixTaskResult;
begin
  result := nil;
  ///

  if IsCompleted() and not FResultIsPulled then begin
    try
      result := FTask.Value;
    except

    end;

    if Assigned(result) then begin
      FSuccess := result.Success;

      ///
      if APullResult then
        FResultIsPulled := APullResult;
    end;
  end;
end;

function TOptixTask.PullResult() : TOptixTaskResult;
begin
  result := GetResult(True);
end;

constructor TOptixTask.Create(const ACommand : TOptixCommand);
begin
  inherited Create();
  ///

  FId             := TGUID.Empty;
  FTask           := nil;
  FCommand        := ACommand;
  FRunningAware   := False;
  FSuccess        := False;
  FResultIsPulled := False;
end;
destructor TOptixTask.Destroy();
begin
  if Assigned(FCommand) then
    FreeAndNil(FCommand);

  var AResult := GetResult();
  if Assigned(AResult) then
    FreeAndNil(AResult);

  FTask := nil;

  ///
  inherited Destroy();
end;

(* TOptixTaskResult *)

constructor TOptixTaskResult.Create();
begin
  inherited Create();
  ///

  FSuccess          := False;
  FExceptionMessage := '';
  FTaskDuration     := 0;
end;

constructor TOptixTaskResult.Create(const ASerializedObject : ISuperObject);
begin
  Create();
  ///

  if Assigned(ASerializedObject) then
    DeSerialize(ASerializedObject);
end;

destructor TOptixTaskResult.Destroy();
begin

  ///
  inherited Destroy();
end;

procedure TOptixTaskResult.SetTaskDuration(const AValue : UInt64);
begin
  FTaskDuration := AValue;
end;

function TOptixTaskResult.GetExtendedDescription() : String;
begin
  result := '';
end;

function TOptixTaskResult.GetDescription() : String;
begin
  result := '';
  ///

  if FSuccess then begin
    var AExtendedDescription := GetExtendedDescription();
    var ATimingInformation := '';
    if FTaskDuration > 1000 then
      ATimingInformation := Format('Task completed in %d seconds.', [FTaskDuration div 1000]);
    ///

    if not String.IsNullOrWhiteSpace(AExtendedDescription) then
      result := Format('%s (%s)', [
        AExtendedDescription,
        ATimingInformation
      ])
    else
      result := ATimingInformation;
  end else
    result := Format('Task Failed: %s', [FExceptionMessage]);
end;

procedure TOptixTaskResult.TaskFailed(const AExceptionMessage : String);
begin
  FSuccess          := False;
  FExceptionMessage := AExceptionMessage;
end;

procedure TOptixTaskResult.TaskSucceed();
begin
  FSuccess          := True;
  FExceptionMessage := '';
end;

function TOptixTaskResult.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['ResultClass'] := ClassName;
end;

end.
