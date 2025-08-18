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

unit Optix.Process.Helper;

interface

uses Winapi.Windows, System.Classes, Optix.WinApiEx, Optix.Shared.Types;

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
    class procedure GetProcessUserInformation(const AProcessId : Cardinal; var AUserName, ADomain : String); static;
    class function TryGetProcessUserInformation(const AProcessId : Cardinal; var AUsername, ADomain : String) : Boolean; static;
    class function GetProcessImagePath(const AProcessID : Cardinal) : String; static;
    class function TryGetProcessImagePath(const AProcessID : Cardinal; const ADefault : String = '') : String; static;
    class function IsWow64Process(const AProcessId : Cardinal) : Boolean; static;
    class function TryIsWow64Process(const AProcessId : Cardinal) : TBoolResult; static;
    class function GetProcessCommandLine(const AProcessId : Cardinal) : String; static;
    class function TryGetProcessCommandLine(const AProcessId : Cardinal) : String; static;
    class function MiniDumpWriteDump(const ATargetProcessId : Cardinal; const ATypesValue : DWORD; AOutputFilePath : String = '') : String; static;
  end;

  function ElevatedStatusToString(const AValue : TElevatedStatus) : String;

implementation

uses Optix.Exceptions, System.SysUtils, System.IOUtils;

(* Local *)

{ _.ElevatedStatusToString }
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

{ TProcessHelper.IsElevated }
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

{ TProcessHelper.TryGetIsElevated }
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

{ TProcessHelper.IsElevatedByProcessId }
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

{ TProcessHelper.TryIsElevatedByProcessId }
class function TProcessHelper.TryIsElevatedByProcessId(const AProcessId : Cardinal) : TElevatedStatus;
begin
  try
    result := IsElevatedByProcessId(AProcessId);
  except
    result := esUnknown;
  end;
end;

{ TProcessHelper.GetProcessUserInformation }
class procedure TProcessHelper.GetProcessUserInformation(const AProcessId : Cardinal; var AUsername, ADomain : String);
begin
  AUserName    := '';
  ADomain      := '';

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

{ TProcessHelper.TryGetProcessUserInformation }
class function TProcessHelper.TryGetProcessUserInformation(const AProcessId : Cardinal; var AUsername, ADomain : String) : Boolean;
begin
  try
    GetProcessUserInformation(AProcessId, AUsername, ADomain);

    ///
    result := True;
  except
    result := False;
  end;
end;

{ TProcessHelper.GetProcessImagePath }
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

{ TProcessHelper.TryGetProcessImagePath }
class function TProcessHelper.TryGetProcessImagePath(const AProcessID : Cardinal; const ADefault : String = '') : String;
begin
  try
    result := GetProcessImagePath(AProcessId);
  except
    result := ADefault;
  end;
end;

{ TProcessHelper.IsWow64Process }
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

{ TProcessHelper.TryIsWow64Process }
class function TProcessHelper.TryIsWow64Process(const AProcessId : Cardinal) : TBoolResult;
begin
  try
    result := CastResult(IsWow64Process(AProcessId))
  except
    result := brError;
  end;
end;

{ TProcessHelper.GetProcessCommandLine }
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

{ TProcessHelper.TryGetProcessCommandLine }
class function TProcessHelper.TryGetProcessCommandLine(const AProcessId : Cardinal) : String;
begin
  try
    result := GetProcessCommandLine(AProcessId);
  except
    result := '';
  end;
end;

{ TProcessHelper.MiniDumpWriteDump }
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
      if not Optix.WinApiEx.MiniDumpWriteDump(hProcess, ATargetProcessId, hFile, ATypesValue, nil, nil, nil) then
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

end.
