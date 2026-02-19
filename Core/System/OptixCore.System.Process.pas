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



unit OptixCore.System.Process;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  Generics.Collections,

  Winapi.Windows,

  OptixCore.WinApiEx, OptixCore.Types, OptixCore.Classes;
// ---------------------------------------------------------------------------------------------------------------------

type
  TElevatedStatus = (
    esUnknown,
    esLimited,
    esElevated
  );

  TProcessHelper = class
  public
    {@M}
    class function IsElevatedByProcessId(const AProcessId : Cardinal) : TElevatedStatus; static;
    class function TryIsElevatedByProcessId(const AProcessId : Cardinal) : TElevatedStatus; static;
    class function IsElevated(AProcessHandle : THandle = 0) : TElevatedStatus; static;
    class function TryGetIsElevated(AProcessHandle : THandle = 0) : TElevatedStatus; static;
    class procedure GetProcessUserInformation(const AProcessId : Cardinal; out AUserName, ADomain : String); static;
    class function TryGetProcessUserInformation(const AProcessId : Cardinal; out AUsername, ADomain : String) : Boolean; static;
    class function GetProcessImagePath(const AProcessID : Cardinal) : String; static;
    class function TryGetProcessImagePath(const AProcessID : Cardinal; const ADefault : String = '') : String; static;
    class function IsWow64Process(const AProcessId : Cardinal) : Boolean; static;
    class function TryIsWow64Process(const AProcessId : Cardinal) : TBoolResult; static;
    class function GetProcessCommandLine(const AProcessId : Cardinal) : String; static;
    class function TryGetProcessCommandLine(const AProcessId : Cardinal) : String; static;
    class function MiniDumpWriteDump(const ATargetProcessId : Cardinal; const ATypesValue : DWORD; AOutputFilePath : String = '') : String; static;
    class procedure TerminateProcess(const AProcessId : Cardinal); static;
  end;

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

  function ElevatedStatusToString(const AValue : TElevatedStatus) : String;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.IOUtils,

  OptixCore.Exceptions, OptixCore.System.InformationGathering, OptixCore.System.Helper;
// ---------------------------------------------------------------------------------------------------------------------

(* Local *)

function ElevatedStatusToString(const AValue : TElevatedStatus) : String;
begin
  result := 'Unknown';
  ///

  case AValue of
    esLimited  : result := 'Limited';
    esElevated : result := 'Elevated';
  end;
end;

(* TProcessHelper *)

class function TProcessHelper.IsElevated(AProcessHandle : THandle = 0) : TElevatedStatus;
var AToken        : THandle;
    ATokenInfo    : TTokenElevation;
    AReturnLength : DWORD;
begin
  if AProcessHandle = 0 then begin
    AProcessHandle := GetCurrentProcess();
    if AProcessHandle = 0 then
      raise EWindowsException.Create('GetCurrentProcess');
  end;

  if not OpenProcessToken(AProcessHandle, TOKEN_QUERY, AToken) then
    raise EWindowsException.Create('OpenProcessToken');

  if not GetTokenInformation(AToken, TokenElevation, @ATokenInfo, SizeOf(TTokenElevation), AReturnLength) then
    raise EWindowsException.Create('GetTokenInformation');

  ///
  if ATokenInfo.TokenIsElevated <> 0 then
    result := esElevated
  else
    result := esLimited;
end;

class function TProcessHelper.TryGetIsElevated(AProcessHandle : THandle = 0) : TElevatedStatus;
begin
  result := esUnknown;
  try
    result := IsElevated(AProcessHandle);
  except
    on E : EWindowsException do begin
      // Ignore, we just try but we can log
    end;
  end;
end;

class function TProcessHelper.IsElevatedByProcessId(const AProcessId : Cardinal) : TElevatedStatus;
begin
  var hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    result := IsElevated(hProcess);
  finally
    CloseHandle(hProcess);
  end;
end;

class function TProcessHelper.TryIsElevatedByProcessId(const AProcessId : Cardinal) : TElevatedStatus;
begin
  try
    result := IsElevatedByProcessId(AProcessId);
  except
    result := esUnknown;
  end;
end;

class procedure TProcessHelper.GetProcessUserInformation(const AProcessId : Cardinal; out AUsername, ADomain : String);
begin
  var AFlags : Cardinal;

  if TOSVersion.Major < 6 then
    AFlags := PROCESS_QUERY_INFORMATION
  else
    AFlags := PROCESS_QUERY_LIMITED_INFORMATION;

  var ptrTokenUser : PTokenUser := nil;
  var ATokenSize   : Cardinal := 0;
  var hToken       : THandle := 0;

  var hProcess := OpenProcess(AFlags, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    if not OpenProcessToken(hProcess, TOKEN_QUERY, hToken) then
      raise EWindowsException.Create('OpenProcessToken');
    ///

    var AReturnedLength : Cardinal;

    if not GetTokenInformation(hToken, TokenUser, nil, 0, AReturnedLength) then
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        raise EWindowsException.Create('GetTokenInformation(1)');

    ATokenSize := AReturnedLength;

    GetMem(ptrTokenUser, AReturnedLength);

    if not GetTokenInformation(hToken, TokenUser, ptrTokenUser, AReturnedLength, AReturnedLength) then
      raise EWindowsException.Create('GetTokenInformation(2)');

    var AUserLength   := DWORD(0);
    var ADomainLength := DWORD(0);
    ///

    var ASidNameUser : SID_NAME_USE;
    
    if not LookupAccountSid(
                            nil,
                            ptrTokenUser.User.Sid,
                            nil,
                            AUserLength,
                            nil,
                            ADomainLength,
                            ASidNameUser
    ) then
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        raise EWindowsException.Create('LookupAccountSid(1)');
    ///
    
    if (AUserLength > 0) and (ADomainLength > 0) then begin
      var pUserBuffer : PWideChar;
      var pDomainBuffer : PWideChar;

      AUserLength := AUserLength * SizeOf(WideChar);
      ADomainLength := ADomainLength * SizeOf(WideChar);

      GetMem(pUserBuffer, AUserLength);
      GetMem(pDomainBuffer, ADomainLength);
      try
        if LookupAccountSid(
                              nil,
                              ptrTokenUser.User.Sid,
                              pUserBuffer,
                              AUserLength,
                              pDomainBuffer,
                              ADomainLength,
                              ASidNameUser
        ) then begin
          AUsername := String(pUserBuffer);
          ADomain := String(pDomainBuffer);
        end else
          raise EWindowsException.Create('LookupAccountSid(2)');
      finally
        FreeMem(pUserBuffer, AUserLength);
        FreeMem(pDomainBuffer, ADomainLength);
      end;
    end;
  finally
    if hToken <> 0 then
      Closehandle(hToken);
      
    if Assigned(ptrTokenUser) then
      FreeMem(ptrTokenUser, ATokenSize);
    ///

    CloseHandle(hProcess);
  end;
end;

class function TProcessHelper.TryGetProcessUserInformation(const AProcessId : Cardinal; out AUsername, ADomain : String) : Boolean;
begin
  try
    GetProcessUserInformation(AProcessId, AUsername, ADomain);

    ///
    result := True;
  except
    result := False;
  end;
end;

class function TProcessHelper.GetProcessImagePath(const AProcessID : Cardinal) : String;
begin
  result := '';
  ///

  if (TOSVersion.Major < 6) then
    raise Exception.Create('Method not compatible with current OS Version. Requires >= 6');
  ///


  var hProc := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, AProcessID);
  if hProc = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    var ALength := DWORD(MAX_PATH * 2);

    // Alternative to GetMem
    SetLength(result, ALength);

    if NOT QueryFullProcessImageNameW(hProc, 0, @result[1], ALength) then
      raise EWindowsException.Create('QueryFullProcessImageNameW');

    SetLength(result, ALength);
  finally
    CloseHandle(hProc);
  end;
end;

class function TProcessHelper.TryGetProcessImagePath(const AProcessID : Cardinal; const ADefault : String = '') : String;
begin
  try
    result := GetProcessImagePath(AProcessId);
  except
    result := ADefault;
  end;
end;

class function TProcessHelper.IsWow64Process(const AProcessId : Cardinal) : Boolean;
begin
  var hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    // TODO: Support IsWow64Process2() for >= Windows 10
    var AWow64Process : BOOL;
    if not Winapi.Windows.IsWow64Process(hProcess, AWow64Process) then
      raise EWindowsException.Create('IsWow64Process');
    ///

    result := AWow64Process;
  finally
    CloseHandle(hProcess);
  end;
end;

class function TProcessHelper.TryIsWow64Process(const AProcessId : Cardinal) : TBoolResult;
begin
  try
    result := CastResult(IsWow64Process(AProcessId))
  except
    result := brError;
  end;
end;

class function TProcessHelper.GetProcessCommandLine(const AProcessId : Cardinal) : String;
begin
  var hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    var AProcessBasicInformation : TProcessBasicInformation;
    var AReturnLength : Cardinal;

    var ARet := NtQueryInformationProcess(
      hProcess,
      ProcessBasicInformation,
      @AProcessBasicInformation,
      SizeOf(TProcessBasicInformation),
      AReturnLength
    );

    if ARet < 0 then
      raise Exception.Create('Could not retrieve target process PEB address.');
    ///

    var pPEBOffset := Pointer(NativeUInt(AProcessBasicInformation.PebBaseAddress));

    var APEB : TPEB;
    var ABytesRead : SIZE_T;

    if not ReadProcessMemory(hProcess, pPEBOffset, @APEB, SizeOf(TPEB), ABytesRead) then
      raise EWindowsException.Create('ReadProcessMemory(1)');

    var ARTLUserProcessParameters : TRTLUserProcessParameters;
    if not ReadProcessMemory(hProcess, APEB.ProcessParameters, @ARTLUserProcessParameters, SizeOf(TRTLUserProcessParameters), ABytesRead) then
      raise EWindowsException.Create('ReadProcessMemory(2)');

    var pCommandLine : PWideChar;
    var pCommandLineSize := ARTLUserProcessParameters.CommandLine.Length * SizeOf(WideChar);

    GetMem(pCommandLine, pCommandLineSize);
    try
      if not ReadProcessMemory(hProcess, ARTLUserProcessParameters.CommandLine.Buffer, pCommandLine, pCommandLineSize, ABytesRead) then
        raise EWindowsException.Create('ReadProcessMemory(3)');

      ///
      result := string(pCommandLine);
    finally
      FreeMem(pCommandLine, pCommandLineSize);
    end;
  finally
    CloseHandle(hProcess);
  end;
end;

class function TProcessHelper.TryGetProcessCommandLine(const AProcessId : Cardinal) : String;
begin
  try
    result := GetProcessCommandLine(AProcessId);
  except
    result := '';
  end;
end;

class function TProcessHelper.MiniDumpWriteDump(const ATargetProcessId : Cardinal; const ATypesValue : DWORD; AOutputFilePath : String = '') : String;
begin
  if String.IsNullOrWhiteSpace(AOutputFilePath) then
    AOutputFilePath := TPath.GetTempFileName();
  ///

  var hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ATargetProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    var hFile := CreateFileW(PWideChar(AOutputFilePath), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if hFile = INVALID_HANDLE_VALUE then
      raise EWindowsException.Create('CreateFileW');
    try
      if not OptixCore.WinApiEx.MiniDumpWriteDump(hProcess, ATargetProcessId, hFile, ATypesValue, nil, nil, nil) then
        raise EWindowsException.Create('MiniDumpWriteDump', GetLastError() and $FFFF (* HRESULT *));

      ///
      result := AOutputFilePath;
    finally
      CloseHandle(hFile);
    end;
  finally
    CloseHandle(hProcess);
  end;
end;

class procedure TProcessHelper.TerminateProcess(const AProcessId : Cardinal);
begin
  var hProcess := OpenProcess(PROCESS_TERMINATE, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    if not Winapi.Windows.TerminateProcess(hProcess, 0) then
      raise EWindowsException.Create('TerminateProcess');
  finally
    CloseHandle(hProcess);
  end;
end;

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
