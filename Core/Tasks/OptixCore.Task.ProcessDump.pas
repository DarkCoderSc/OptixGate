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



unit OptixCore.Task.ProcessDump;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  XSuperObject, OptixCore.Commands.Base;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixProcessDumpTask = class(TOptixTask)
  protected
    {@M}
    function TaskCode() : TOptixTaskResult; override;
  end;

  TOptixTaskGetProcessDumpResult = class(TOptixTaskResult)
  private
    FOutputFilePath    : String;
    FDumpedProcessId   : Cardinal;
    FDumpedProcessName : String;

    {@M}
    function GetProcessDisplayName() : String;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
    function GetExtendedDescription() : String; override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@C}
    constructor Create(const AOutputFileName : String; const ADumpedProcessId : Cardinal); overload;

    {@G}
    property OutputFilePath    : String   read FOutputFilePath;
    property DumpedProcessId   : Cardinal read FDumpedProcessId;
    property DumpedProcessName : String   read FDumpedProcessName;
    property Displayname       : String   read GetProcessDisplayName;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.IOUtils,

  Winapi.Windows,

  OptixCore.Commands.Process, OptixCore.System.Process;
// ---------------------------------------------------------------------------------------------------------------------

(* TOptixProcessDumpTask *)

function TOptixProcessDumpTask.TaskCode() : TOptixTaskResult;
begin
  result := nil;
  ///

  if not Assigned(FCommand) or (not (FCommand is TOptixCommandDumpProcess)) then
    Exit();

  var AOutputFilePath := TProcessHelper.MiniDumpWriteDump(
    TOptixCommandDumpProcess(FCommand).ProcessId,
    TOptixCommandDumpProcess(FCommand).TypesValue,
    TOptixCommandDumpProcess(FCommand).DestFilePath,
  );

  ///
  result := TOptixTaskGetProcessDumpResult.Create(AOutputFilePath, TOptixCommandDumpProcess(FCommand).ProcessId);
end;

(* TOptixTaskGetProcessDumpResult *)

constructor TOptixTaskGetProcessDumpResult.Create(const AOutputFileName : String; const ADumpedProcessId : Cardinal);
begin
  inherited Create();
  ///

  FOutputFilePath    := AOutputFileName;
  FDumpedProcessId   := ADumpedProcessId;
  FDumpedProcessName := TPath.GetFileName(TProcessHelper.TryGetProcessImagePath(FDumpedProcessId));
end;

function TOptixTaskGetProcessDumpResult.GetProcessDisplayName() : String;
begin
  if String.IsNullOrWhiteSpace(FDumpedProcessName) then
    result := IntToStr(FDumpedProcessId)
  else
    result := Format('%d (%s)', [
      FDumpedProcessId,
      FDumpedProcessName
    ]);
end;

function TOptixTaskGetProcessDumpResult.GetExtendedDescription() : String;
begin
  result := Format('%s successfully dumped to "%s"', [
    GetProcessDisplayName(),
    FOutputFilePath
  ]);
end;

procedure TOptixTaskGetProcessDumpResult.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FOutputFilePath    := ASerializedObject.S['OutputFilePath'];
  FDumpedProcessId   := ASerializedObject.I['DumpedProcessId'];
  FDumpedProcessName := ASerializedObject.S['DumpedProcessName'];
end;

function TOptixTaskGetProcessDumpResult.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['OutputFilePath']    := FOutputFilePath;
  result.I['DumpedProcessId']   := FDumpedProcessId;
  result.S['DumpedProcessName'] := FDumpedProcessName;
end;

end.
