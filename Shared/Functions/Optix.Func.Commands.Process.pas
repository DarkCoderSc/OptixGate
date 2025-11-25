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



unit Optix.Func.Commands.Process;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils,

  Generics.Collections,

  Winapi.Windows,

  Optix.Func.Commands.Base, Optix.Process.Enum, Optix.Shared.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  {@TEMPLATE}
  TOptixCommandProcessActionResponse = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FProcessId : Cardinal;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal) overload;

    {@G}
    property ProcessId : Cardinal read FProcessId;
  end;

  TOptixCommandTerminateProcess = class(TOptixCommandProcessActionResponse)
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
  end;

  TOptixCommandDumpProcess = class(TOptixCommandTask)
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
    {$IFNDEF SERVER}
    function CreateTask(const ACommand : TOptixCommand) : TOptixTask; override;
    {$ENDIF}

    {@G}
    property ProcessId    : Cardinal read FProcessId;
    property DestTempPath : Boolean  read FDestTempPath;
    property DestFilePath : String   read FDestFilePath;
    property TypesValue   : DWORD    read FTypesValue;
  end;

  TOptixCommandEnumRunningProcesses = class(TOptixCommandActionResponse)
  private
    [OptixSerializableAttribute]
    FProcesses : TObjectList<TProcessInformation>;
  public
    {@M}
    {$IFNDEF SERVER}
    procedure DoAction(); override;
    {$ENDIF}
    procedure AfterCreate(); override;

    {@C}
    destructor Destroy(); override;

    {@G}
    property Processes : TObjectList<TProcessInformation> read FProcesses;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.Process.Helper{$IFNDEF SERVER}, Optix.Task.ProcessDump{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixCommandProcessActionResponse *)

constructor TOptixCommandProcessActionResponse.Create(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;
end;

(* TOptixCommandTerminateProcess *)

{$IFNDEF SERVER}
procedure TOptixCommandTerminateProcess.DoAction();
begin
  TProcessHelper.TerminateProcess(FProcessId);
end;
{$ENDIF}

(* TOptixCommandDumpProcess *)

constructor TOptixCommandDumpProcess.Create(const AProcessId : Cardinal; const ADestFilePath : String; const ATypesValue : DWORD);
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

{$IFNDEF SERVER}
function TOptixCommandDumpProcess.CreateTask(const ACommand : TOptixCommand) : TOptixTask;
begin
  if Assigned(ACommand) then
    result := TOptixProcessDumpTask.Create(ACommand)
  else
    result := nil;
end;
{$ENDIF}

(* TOptixCommandEnumRunningProcesses *)

{$IFNDEF SERVER}
procedure TOptixCommandEnumRunningProcesses.DoAction();
begin
  TOptixEnumProcess.Enum(FProcesses);
end;
{$ENDIF}

procedure TOptixCommandEnumRunningProcesses.AfterCreate();
begin
  inherited;
  ///

  FProcesses := TObjectList<TProcessInformation>.Create(True);
end;

destructor TOptixCommandEnumRunningProcesses.Destroy();
begin
  if Assigned(FProcesses) then
    FreeAndNil(FProcesses);

  ///
  inherited;
end;

end.
