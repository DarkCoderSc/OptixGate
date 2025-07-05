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
{                   License: Apache License 2.0                                }
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

uses Optix.Func.Response, Optix.Interfaces, XSuperObject, System.Classes,
     Generics.Collections, Optix.WinApiEx, Optix.InformationGathering.Process;

type
  TProcessInformation = class(TInterfacedPersistent, IOptixSerializable)
  private
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

    {@}
    function EvaluateIfCurrentProcess() : Boolean;
  protected
    {@M}
    procedure DeSerialize(const ASerializedObject : ISuperObject); virtual;
  public
    {@M}
    function Serialize() : ISuperObject; virtual;
    procedure Assign(ASource : TPersistent); override;

    {@C}
    constructor Create(const ASerializedObject : ISuperObject = nil); overload;
    constructor Create(const pProcessInformation : PSystemProcessInformation); overload;
    constructor Create(const ASource : TProcessInformation); overload;

    {@G}
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
  end;

  TProcessList = class(TOptixWindowedResponse)
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

{ TProcessInformation.DeSerialize }
procedure TProcessInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    Exit();
  ///

  // TODO: One day, optimized Deserialization / Serializaion using RTTI. But must
  // be carefully tested to ensure it works as expected for common data formats.
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
end;

{ TProcessInformation.Serialize }
function TProcessInformation.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

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
end;

{ TProcessInformation.Assign }
procedure TProcessInformation.Assign(ASource : TPersistent);
begin
  if ASource is TProcessInformation then begin
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
  end else
    inherited;
end;

{ TProcessInformation.EvaluateIfCurrentProcess }
function TProcessInformation.EvaluateIfCurrentProcess() : Boolean;
begin
  result := FCurrentProcessId = FId;
end;

{ TProcessInformation.Create }
constructor TProcessInformation.Create(const ASerializedObject : ISuperObject = nil);
begin
  inherited Create();
  ///

  if Assigned(ASerializedObject) then
    DeSerialize(ASerializedObject);
end;

{ TProcessInformation.Create }
constructor TProcessInformation.Create(const ASource : TProcessInformation);
begin
  if Assigned(ASource) then
    Assign(ASource);
end;

{ TProcessInformation.Create }
constructor TProcessInformation.Create(const pProcessInformation : PSystemProcessInformation);
begin
  inherited Create();
  ///

  if not Assigned(pProcessInformation) then
    raise Exception.Create(''); // TODO

  FImagePath := TProcessInformationHelper.TryGetProcessImagePath(
    pProcessInformation^.ProcessID,
    String(pProcessInformation^.ModuleName.Buffer)
  );

  FId         := pProcessInformation^.ProcessID;
  FParentId   := pProcessInformation^.InheritedFromProcessId;
  FElevated   := TProcessInformationHelper.TryIsElevatedByProcessId(FId);

  FUsername := '';
  FDomain   := '';
  FUserSid  := '';

  if TProcessInformationHelper.TryGetProcessUserInformation(FId, FUsername, FDomain) then begin
    FUserSid := TOptixInformationGathering.GetUserSidByType(FUsername);
  end;

  FSessionId   := pProcessInformation^.SessionId;
  FThreadCount := pProcessInformation^.NumberOfThreads;
  FCreatedTime := TSystemHelper.TryFileTimeToDateTime(pProcessInformation^.CreateTime);

  FCurrentProcessId := GetCurrentProcessId();
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
