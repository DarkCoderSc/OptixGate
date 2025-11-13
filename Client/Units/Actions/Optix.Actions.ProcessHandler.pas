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
{  All code contained in this unit was written and developed by the author     }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{   The author grants permission for this code to be used, reproduced, and     }
{   included in datasets for the purpose of training or improving machine      }
{   learning models, including large language models (LLMs).                   }
{                                                                              }
{******************************************************************************}

unit Optix.Actions.ProcessHandler;

interface

uses System.Classes, Winapi.Windows;

type
  TProcessHandler = class
  private
    FInstanceId         : TGUID;
    FGroupId            : TGUID;

    FJobObject          : THandle;
    
    FPipeOutRead        : THandle;
    FPipeOutWrite       : THandle;
    FPipeInRead         : THandle;
    FPipeInWrite        : THandle;
    
    FStartupInformation : TStartupInfo;
    FProcessInformation : TProcessInformation;

    FCommandLine  : String;
    FShowWindow   : Boolean;

    {@M}
    procedure Cleanup();
    function IsActive() : Boolean;
    function GetAvailableOutputBytes() : DWORD;
  public
    {@C}
    constructor Create(const ACommandLine : String); overload;
    constructor Create(const ACommandLine : String; const AGroupId : TGUID); overload;
    destructor Destroy(); override;

    {@M}
    procedure Start(const AControlIO : Boolean = False);
    procedure TryClose();
    procedure Close();

    procedure CtrlC();
    procedure TryCtrlC();

    function ReadAvailableOutput(var pBuffer : PVOID; var ABytesRead : Cardinal; const AOemToChar : Boolean) : Boolean; overload;
    function ReadAvailableOutput() : String; overload;

    procedure Write(const pData : PVOID; const ADataSize : Cardinal);
    procedure WriteLn(AStr : AnsiString = '');

    {@G/S - Options}
    property ShowWindow : Boolean read FShowWindow write FShowWindow;

    {@G}
    property InstanceId  : TGUID   read FInstanceId;
    property GroupId     : TGUID   read FGroupId;
    property CommandLine : String  read FCommandLine;
    property Active      : Boolean read IsActive;
  end;

implementation

uses System.SysUtils, Optix.WinApiEx, Optix.Exceptions;

{ TProcessHandler.Create }
constructor TProcessHandler.Create(const ACommandLine : String);
begin
  inherited Create();
  ///

  FShowWindow := True;

  FInstanceId := TGUID.NewGuid();
  FGroupId := TGUID.Empty;
  
  Cleanup();

  FCommandLine := ACommandLine;
end;

{ TProcessHandler.Create }
constructor TProcessHandler.Create(const ACommandLine : String; const AGroupId : TGUID);
begin
  Create(ACommandLine);
  ///

  FGroupId := AGroupId;
end;

{ TProcessHandler.Destroy }
destructor TProcessHandler.Destroy();
begin
  Close();

  ///
  inherited Destroy();
end;

{ TProcessHandler.Start }
procedure TProcessHandler.Start(const AControlIO : Boolean = False);
begin
  Close(); // Terminate + clean previous process instance informations, if any.
  try
    var AJobObjectExtendedLimitInformation : TJobObjectExtendedLimitInformation;
    ZeroMemory(@AJobObjectExtendedLimitInformation, SizeOf(TJobObjectExtendedLimitInformation));
  
    var ACreateProcessFlags := CREATE_NEW_CONSOLE;
    
    if AControlIO (* or ??? *) then begin
      FJobObject := CreateJobObjectW(nil, PWideChar(TGUID.NewGuid.ToString()));
      if (FJobObject = 0) then
        raise EWindowsException.Create('CreateJobObjectW');
      ///

      AJobObjectExtendedLimitInformation.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
      if not SetInformationJobObject(
        FJobObject,
        JobObjectExtendedLimitInformation,
        @AJobObjectExtendedLimitInformation,
        SizeOf(TJobObjectExtendedLimitInformation)
      ) then
        raise EWindowsException.Create('SetInformationJobObject');

      ACreateProcessFlags := ACreateProcessFlags or CREATE_BREAKAWAY_FROM_JOB;
    end;

    var ASecurityAttributes : TSecurityAttributes;

    ASecurityAttributes.nLength              := SizeOf(TSecurityAttributes);
    ASecurityAttributes.lpSecurityDescriptor := nil;
    ASecurityAttributes.bInheritHandle       := AControlIO (* or ??? *);

    FStartupInformation.dwFlags := STARTF_USESHOWWINDOW;
    
    if AControlIO then begin
      if not CreatePipe(FPipeOutRead, FPipeOutWrite, @ASecurityAttributes, 0) then
        raise EWindowsException.Create('CreatePipe(1)');

      if not CreatePipe(FPipeInRead, FPipeInWrite, @ASecurityAttributes, 0) then
      raise EWindowsException.Create('CreatePipe(2)');

      FStartupInformation.dwFlags := FStartupInformation.dwFlags or STARTF_USESTDHANDLES;
      
      FStartupInformation.hStdOutput := FPipeInWrite;
      FStartupInformation.hStdInput  := FPipeOutRead;
      FStartupInformation.hStdError  := FPipeInWrite;
    end;
    
    if FShowWindow then
      FStartupInformation.wShowWindow := SW_SHOWNORMAL
    else
      FStartupInformation.wShowWindow := SW_HIDE;
    
    if not CreateProcessW(
      nil,
      PWideChar(FCommandLine),
      nil,
      nil,
      AControlIO, 
      ACreateProcessFlags,
      nil,
      nil,
      FStartupInformation,
      FProcessInformation
    ) then
      raise EWindowsException.Create('CreateProcessW');  

    ///
    
    if AControlIO then
      if not AssignProcessToJobObject(FJobObject, FProcessInformation.hProcess) then
        raise EWindowsException.Create('AssignProcessToJobObject');
  except
    TryClose();

    raise;
  end;
end;

{ TProcessHandler.GetAvailableOutputBytes }
function TProcessHandler.GetAvailableOutputBytes() : DWORD;
begin
  if not PeekNamedPipe(FPipeInRead, nil, 0, nil, @result, nil) then
    raise EWindowsException.Create('PeekNamedPipe');
end;

{ TProcessHandler.ReadAvailableOutput }
function TProcessHandler.ReadAvailableOutput(var pBuffer : PVOID; var ABytesRead : Cardinal; const AOemToChar : Boolean) : Boolean;
begin  
  result := False;
  ///

  ABytesRead := 0;
  pBuffer := nil;
  
  if FPipeInRead = 0 then
    Exit();

  var ABytesAvailable := GetAvailableOutputBytes();
  if ABytesAvailable = 0 then
    Exit();
  ///

  GetMem(pBuffer, ABytesAvailable);
  try
    if not ReadFile(FPipeInRead, PByte(pBuffer)^, ABytesAvailable, ABytesRead, nil) then
      raise EWindowsException.Create('ReadFile');
    ///

    if AOemToChar then
      if not OemToCharBuffA(PAnsiChar(pBuffer), PAnsiChar(pBuffer), ABytesRead) then
        raise EWindowsException.Create('OemToCharBuffA');

    ///
    result := (ABytesRead > 0);
  except
    on E : Exception do begin
      FreeMem(pBuffer, ABytesAvailable);

      raise;
    end;
  end;
end;

{ TProcessHandler.ReadAvailableOutput }
function TProcessHandler.ReadAvailableOutput() : String;
begin
  result := '';
  ///

  var pBuffer := PVOID(nil);
  var ABufferSize := DWORD(0);
    
  try
    ReadAvailableOutput(pBuffer, ABufferSize, True);
    ///

    SetString(result, PAnsiChar(pBuffer), ABufferSize);
  finally
    if (ABufferSize > 0) and Assigned(pBuffer) then    
      FreeMem(pBuffer, ABufferSize);
  end;
end;

{ TProcessHandler.Write }
procedure TProcessHandler.Write(const pData : PVOID; const ADataSize : Cardinal);
begin
  if not IsActive() or (FPipeOutWrite = 0) or (ADataSize = 0) or not Assigned(pData) then
    Exit();
  ///

  var ABytesWritten : DWORD;
  
  if NOT WriteFile(FPipeOutWrite, PByte(pData)^, ADataSize, ABytesWritten, nil) then
    raise EWindowsException.Create('WriteFile');
end;

{ TProcessHandler.WriteLn }
procedure TProcessHandler.WriteLn(AStr : AnsiString = '');
begin
  Write(@AStr[1], Length(AStr));
end;

{ TProcessHandler.CtrlC }
procedure TProcessHandler.CtrlC();
begin
  if not IsActive() then
    Exit();
  ///

  if not AttachConsole(FProcessInformation.dwProcessId) then
    raise EWindowsException.Create('AttachConsole');
  try
    if not SetConsoleCtrlHandler(nil, True) then
      raise EWindowsException.Create('SetConsoleCtrlHandler');
    try
      if not GenerateConsoleCtrlEvent(CTRL_C_EVENT, FProcessInformation.dwProcessId) then
        raise EWindowsException.Create('GenerateConsoleCtrlEvent');
    finally
      if not SetConsoleCtrlHandler(nil, False) then
        raise EWindowsException.Create('SetConsoleCtrlHandler');
    end;
  finally
    if not FreeConsole() then
      raise EWindowsException.Create('FreeConsole');
  end;
end;

{ TProcessHandler.TryCtrlC }
procedure TProcessHandler.TryCtrlC();
begin
  try
    CtrlC();
  except

  end;
end;

{ TProcessHandler.IsActive }
function TProcessHandler.IsActive() : Boolean;
begin
  result := False;
  
  if FProcessInformation.hProcess = 0 then
    Exit();
  ///

  case WaitForSingleObject(FProcessInformation.hProcess, 0) of
    WAIT_OBJECT_0 : ;

    else
      result := True;
  end;
end;

{ TProcessHandler.Close }
procedure TProcessHandler.Close();
begin
  var AExitCode : LongWord := 0;

  if FJobObject > 0 then begin
    if not TerminateJobObject(FJobObject, AExitCode) then
      raise EWindowsException.Create('TerminateJobObject');

    if FJobObject > 0 then
      CloseHandle(FJobObject);
  end;

  if FProcessInformation.hProcess > 0 then begin
    CloseHandle(FProcessInformation.hProcess);
    CloseHandle(FProcessInformation.hThread);
  end;

  ///
  Cleanup();
end;

{ TProcessHandler.TryClose }
procedure TProcessHandler.TryClose();
begin
  try
    Close();
  except

  end;
end;

{ TProcessHandler.Cleanup }
procedure TProcessHandler.Cleanup();
begin
  FJobObject := 0;

  if FPipeOutRead > 0 then
    CloseHandle(FPipeOutRead);

  if FPipeOutWrite > 0 then
    CloseHandle(FPipeOutWrite);

  if FPipeInRead > 0 then
    CloseHandle(FPipeInRead);

  if FPipeInWrite > 0 then
    CloseHandle(FPipeInWrite);
  
  FPipeOutRead  := 0;
  FPipeOutWrite := 0;
  FPipeInRead   := 0;
  FPipeInWrite  := 0;

  ZeroMemory(@FStartupInformation, SizeOf(TStartupInfo));
  ZeroMemory(@FProcessInformation, Sizeof(TProcessInformation));
end;

end.
