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



unit Optix.Process.Enum;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  XSuperObject,

  Optix.Protocol.Packet, Optix.WinApiEx, Optix.Process.Helper, Optix.Shared.Types, Optix.Shared.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  TProcessInformation = class(TOptixSerializableObject)
  private
    [OptixSerializableAttribute]
    FName : String;

    [OptixSerializableAttribute]
    FImagePath : String;

    [OptixSerializableAttribute]
    FId : Cardinal;

    [OptixSerializableAttribute]
    FParentId : Cardinal;

    [OptixSerializableAttribute]
    FUsername : String;

    [OptixSerializableAttribute]
    FDomain : String;

    [OptixSerializableAttribute]
    FUserSid : String;

    [OptixSerializableAttribute]
    FElevated : TElevatedStatus;

    [OptixSerializableAttribute]
    FSessionId : Cardinal;

    [OptixSerializableAttribute]
    FThreadCount : Cardinal;

    [OptixSerializableAttribute]
    FCreatedTime : TDateTime;

    [OptixSerializableAttribute]
    FCurrentProcessId : Cardinal;

    [OptixSerializableAttribute]
    FIsWow64Process : TBoolResult;

    [OptixSerializableAttribute]
    FCommandLine : String;

    {@}
    function EvaluateIfCurrentProcess() : Boolean;
    function CheckIfSystemUser() : Boolean;
  public
    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@C}
    constructor Create(const pProcessInformation : PSystemProcessInformation); overload;

    {@G}
    property Name             : String          read FName;
    property ImagePath        : String          read FImagePath;
    property Id               : Cardinal        read FId;
    property ParentId         : Cardinal        read FParentId;
    property Username         : String          read FUsername;
    property Domain           : String          read FDomain;
    property UserSid          : String          read FUserSid;
    property Elevated         : TElevatedStatus read FElevated;
    property SessionId        : Cardinal        read FSessionId;
    property ThreadCount      : Cardinal        read FThreadCount;
    property CreatedTime      : TDateTime       read FCreatedTime;
    property IsCurrentProcess : Boolean         read EvaluateIfCurrentProcess;
    property IsWow64Process   : TBoolResult     read FIsWow64Process;
    property IsSystem         : Boolean         read CheckIfSystemUser;
    property CommandLine      : String          read FCommandLine;
  end;

  TOptixEnumProcess = class
  public
    class procedure Enum(var AList : TObjectList<TProcessInformation>); static;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows,

  Optix.Exceptions, Optix.InformationGathering.Helper, Optix.System.Helper;
// ---------------------------------------------------------------------------------------------------------------------

(* TProcessInformation *)

function TProcessInformation.CheckIfSystemUser() : Boolean;
begin
  result := String.Compare(FUserSid, 'S-1-5-18', True) =  0;
end;

procedure TProcessInformation.Assign(ASource : TPersistent);
begin
  if ASource is TProcessInformation then begin
    FName             := TProcessInformation(ASource).FName;
    FImagePath        := TProcessInformation(ASource).FImagePath;
    FId               := TProcessInformation(ASource).FId;
    FParentId         := TProcessInformation(ASource).FParentId;
    FUsername         := TProcessInformation(ASource).FUsername;
    FDomain           := TProcessInformation(ASource).FDomain;
    FUserSid          := TProcessInformation(ASource).FUserSid;
    FElevated         := TProcessInformation(ASource).FElevated;
    FSessionId        := TProcessInformation(ASource).FSessionId;
    FThreadCount      := TProcessInformation(ASource).FThreadCount;
    FCreatedTime      := TProcessInformation(ASource).FCreatedTime;
    FCurrentProcessId := TProcessInformation(ASource).FCurrentProcessId;
    FIsWow64Process   := TProcessInformation(ASource).FIsWow64Process;
    FCommandLine      := TProcessInformation(ASource).FCommandLine;
  end else
    inherited;
end;

function TProcessInformation.EvaluateIfCurrentProcess() : Boolean;
begin
  result := FCurrentProcessId = FId;
end;

constructor TProcessInformation.Create(const pProcessInformation : PSystemProcessInformation);
begin
  inherited Create();
  ///

  if not Assigned(pProcessInformation) then
    raise Exception.Create(''); // TODO

  FId := pProcessInformation^.ProcessID;
  ///

  FName := String(pProcessInformation^.ModuleName.Buffer);

  FImagePath := TProcessHelper.TryGetProcessImagePath(FId);
  FParentId   := pProcessInformation^.InheritedFromProcessId;
  FElevated   := TProcessHelper.TryIsElevatedByProcessId(FId);

  FUsername := '';
  FDomain   := '';
  FUserSid  := '';

  if TProcessHelper.TryGetProcessUserInformation(FId, FUsername, FDomain) then begin
    FUserSid := TOptixInformationGathering.GetUserSidByType(FUsername);
  end;

  FSessionId   := pProcessInformation^.SessionId;
  FThreadCount := pProcessInformation^.NumberOfThreads;
  FCreatedTime := TSystemHelper.TryFileTimeToDateTime(pProcessInformation^.CreateTime);

  FCurrentProcessId := GetCurrentProcessId();

  FIsWow64Process := TProcessHelper.TryIsWow64Process(FId);

  FCommandLine := TProcessHelper.TryGetProcessCommandLine(FId);
end;

(* TOptixEnumProcess *)

class procedure TOptixEnumProcess.Enum(var AList : TObjectList<TProcessInformation>);
begin
  if not Assigned(AList) then
    AList := TObjectList<TProcessInformation>.Create(True)
  else
    AList.Clear();
  ///

  var AReturnLength : DWORD;

  var ARet := NtQuerySystemInformation(SYSTEM_PROCESS_INFORMATION_CLASS, nil, 0, AReturnLength);
  if (ARet <> 0) and (ARet <> $C0000004) (* STATUS_INFO_LENGTH_MISMATCH *) then
    raise EWindowsException.Create('NtQuerySystemInformation(1)');

  var pFirstRow : PSystemProcessInformation;

  GetMem(pFirstRow, AReturnLength);
  try
    ARet := NtQuerySystemInformation(SYSTEM_PROCESS_INFORMATION_CLASS, pFirstRow, AReturnLength, AReturnLength);
    if ARet <> 0 then
      raise EWindowsException.Create('NtQuerySystemInformation(2)');

    var pNextRow := pFirstRow;
    while True do begin
      try
        AList.Add(TProcessInformation.Create(pNextRow));
      except
        continue;
      end;

      if pNextRow^.NextEntryOffset = 0 then
        break;

      ///
      pNextRow := Pointer(NativeUInt(pNextRow) + pNextRow.NextEntryOffset);
    end;
  finally
    FreeMem(pFirstRow, AReturnLength);
  end;
end;

end.

