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

unit Optix.Func.Enum.Process;

interface

uses Optix.Func.Response, XSuperObject, System.Classes, Generics.Collections,
     Optix.WinApiEx, Optix.Process.Helper, Optix.Types,
     Optix.Protocol.Packet, Optix.Classes;

type
  TProcessInformation = class(TEnumerableItem)
  private
    FName             : String;
    FImagePath        : String;
    FId               : Cardinal;
    FParentId         : Cardinal;
    FUsername         : String;
    FDomain           : String;
    FUserSid          : String;
    FElevated         : TElevatedStatus;
    FSessionId        : Cardinal;
    FThreadCount      : Cardinal;
    FCreatedTime      : TDateTime;
    FCurrentProcessId : Cardinal;
    FIsWow64Process   : TBoolResult;
    FCommandLine      : String;

    {@}
    function EvaluateIfCurrentProcess() : Boolean;
    function CheckIfSystemUser() : Boolean;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;
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

  TProcessList = class(TOptixResponse)
  private
    FList : TObjectList<TProcessInformation>;
  protected
    {@M}
    procedure Refresh(); override;
    procedure DeSerialize(const ASerializedObject : ISuperObject); override;
    procedure BeforeCreate(); override;
  public
    {@M}
    function Serialize() : ISuperObject; override;

    {@C}
    destructor Destroy(); override;

    {@G}
    property List : TObjectList<TProcessInformation> read FList;
  end;

implementation

uses Winapi.Windows, Optix.Exceptions, System.SysUtils,
     Optix.InformationGathering.Helper, Optix.System.Helper;

(* TProcessInformation *)

{ TProcessInformation.CheckIfSystemUser() }
function TProcessInformation.CheckIfSystemUser() : Boolean;
begin
  result := String.Compare(FUserSid, 'S-1-5-18', True) =  0;
end;

{ TProcessInformation.DeSerialize }
procedure TProcessInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    Exit();
  ///

  inherited;
  ///

  FName             := ASerializedObject.S['Name'];
  FImagePath        := ASerializedObject.S['ImagePath'];
  FId               := ASerializedObject.I['Id'];
  FParentId         := ASerializedObject.I['ParentId'];
  FUsername         := ASerializedObject.S['Username'];
  FDomain           := ASerializedObject.S['Domain'];
  FUserSid          := ASerializedObject.S['UserSid'];
  FElevated         := TElevatedStatus(ASerializedObject.I['Elevated']);
  FSessionId        := ASerializedObject.I['SessionId'];
  FThreadCount      := ASerializedObject.I['ThreadCount'];
  FCreatedTime      := ASerializedObject.D['CreatedTime'];
  FCurrentProcessId := ASerializedObject.I['CurrentProcessId'];
  FIsWow64Process   := TBoolResult(ASerializedObject.I['IsWow64Process']);
  FCommandLine      := ASerializedObject.S['CommandLine'];
end;

{ TProcessInformation.Serialize }
function TProcessInformation.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  result.S['Name']             := FName;
  result.S['ImagePath']        := FImagePath;
  result.I['Id']               := FId;
  result.I['ParentId']         := FParentId;
  result.S['Username']         := FUsername;
  result.S['Domain']           := FDomain;
  result.S['UserSid']          := FUserSid;
  result.I['Elevated']         := Cardinal(FElevated);
  result.I['SessionId']        := FSessionId;
  result.I['ThreadCount']      := FThreadCount;
  result.D['CreatedTime']      := FCreatedTime;
  result.I['CurrentProcessId'] := FCurrentProcessId;
  result.I['IsWow64Process']   := Cardinal(FIsWow64Process);
  result.S['CommandLine']      := FCommandLine;
end;

{ TProcessInformation.Assign }
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

{ TProcessInformation.EvaluateIfCurrentProcess }
function TProcessInformation.EvaluateIfCurrentProcess() : Boolean;
begin
  result := FCurrentProcessId = FId;
end;

{ TProcessInformation.Create }
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

(* TProcessList *)

{ TProcessList.Refresh }
procedure TProcessList.Refresh();
begin
  FList.Clear();
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
        FList.Add(TProcessInformation.Create(pNextRow));
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

{ TProcessList.DeSerialize }
procedure TProcessList.DeSerialize(const ASerializedObject : ISuperObject);
begin
  inherited;
  ///

  FList.Clear();

  for var I := 0 to ASerializedObject.A['List'].Length -1 do
    FList.Add(TProcessInformation.Create(ASerializedObject.A['List'].O[I]));
end;

{ TProcessList.Serialize }
function TProcessList.Serialize() : ISuperObject;
begin
  result := inherited;
  ///

  var AJsonArray := TSuperArray.Create();

  for var AItem in FList do
    AJsonArray.Add(AItem.Serialize);

  ///
  result.A['List'] := AJsonArray;
end;

{ TProcessList.AfterCreate }
procedure TProcessList.BeforeCreate();
begin
  inherited;
  ///

  FList := TObjectList<TProcessInformation>.Create(True);
end;

{ TProcessList.Destroy }
destructor TProcessList.Destroy();
begin
  if Assigned(FList) then
    FreeAndNil(FList);

  ///
  inherited;
end;

end.
