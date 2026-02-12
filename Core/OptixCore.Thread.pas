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



unit OptixCore.Thread;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SyncObjs,

  Generics.Collections;
// ---------------------------------------------------------------------------------------------------------------------

type
  TOptixThreadWatchDogThread = class(TThread)
  protected
    FIntervalEvent : TEvent;

    {@M}
    procedure Execute(); override;

  public
    {@C}
    constructor Create(); overload;
    destructor Destroy(); override;

    {@M}
    procedure TerminatedSet(); override;
  end;

  TOptixThread = class(TThread)
  private
    FGuid            : TGUID;
    FCreatedDate     : TDateTime;
    FOnThreadExecute : TNotifyEvent;
    FOnThreadEnd     : TNotifyEvent;
    FStarted         : Boolean;
  protected
    {@M}
    procedure Execute(); override;
    procedure ThreadExecute(); virtual; abstract;
    function IsRunning() : Boolean;
  public
    {@C}
    constructor Create(); overload;
    destructor Destroy(); override;

    {@M}
    class function IsThreadRunning(const AThread : TThread): Boolean; static;
    class function HasRunningInstance(const AThread : TOptixThread; const ATryTerminate : Boolean = False) : Boolean; static;
    class procedure TerminateInstance(const AThread : TOptixThread); static;
    class procedure Terminate(const AThread : TThread; const AWaitFor : Boolean = False); overload;
    class procedure TerminateWait(const AThread : TThread); static;
    class procedure SignalHiveAndFlush(); static;
    class procedure FlushDeadThreads(); static;

    {@S}
    property OnThreadExecute : TNotifyEvent write FOnThreadExecute;
    property OnThreadEnd     : TNotifyEvent write FOnThreadEnd;

    {@G}
    property Guid        : TGUID     read FGUID;
    property Running     : Boolean   read IsRunning;
    property CreatedDate : TDateTime read FCreatedDate;
  end;

  var OPTIX_THREAD_HIVE : TThreadList<TOptixThread>  = nil;
      OPTIX_WATCHDOG    : TOptixThreadWatchDogThread = nil;

  {$WARN SYMBOL_PLATFORM OFF}
  function ThreadPriorityToString(const AThreadPriority : TThreadPriority) : String;
  {$WARN SYMBOL_PLATFORM ON}

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils,

  Winapi.Windows;
// ---------------------------------------------------------------------------------------------------------------------

(* Local *)

{$WARN SYMBOL_PLATFORM OFF}
function ThreadPriorityToString(const AThreadPriority : TThreadPriority) : String;
begin
  result := 'Unknown';
  ///

  case AThreadPriority of
    tpIdle         : result := 'Idle';
    tpLowest       : result := 'Lowest';
    tpLower        : result := 'Lower';
    tpNormal       : result := 'Normal';
    tpHigher       : result := 'Higher';
    tpHighest      : result := 'Highest';
    tpTimeCritical : result := 'Time Critical';
  end;
end;
{$WARN SYMBOL_PLATFORM ON}

(* TOptixThreadWatchDogThread *)

constructor TOptixThreadWatchDogThread.Create();
begin
  inherited Create(False);

  self.Priority := tpLowest; // maybe to be set in idle?
  self.FreeOnTerminate := False;

  FIntervalEvent := TEvent.Create(nil, True, False, TGUID.NewGuid.ToString());
end;

destructor TOptixThreadWatchDogThread.Destroy();
begin
  if Assigned(FIntervalEvent) then
    FreeAndNil(FIntervalEvent);

  ///
  inherited Destroy();
end;

procedure TOptixThreadWatchDogThread.Execute();
begin
  try
    if NOT Assigned(FIntervalEvent) then
      Exit();
    ///

    while not Terminated do begin
      TOptixThread.FlushDeadThreads();

      ///
      FIntervalEvent.WaitFor(1000);
    end;
  finally
    ExitThread(0); // !important
  end;
end;

procedure TOptixThreadWatchDogThread.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FIntervalEvent) then
    FIntervalEvent.SetEvent();
end;

(* TOptixThread *)

class function TOptixThread.IsThreadRunning(const AThread : TThread): Boolean;
var AExitCode : DWORD;
begin
  result := False;
  ///

  if not Assigned(AThread) then
    Exit();

  if not GetExitCodeThread(AThread.Handle, AExitCode) then
    Exit();

  ///
  result := (AExitCode = STILL_ACTIVE);
end;

class procedure TOptixThread.Terminate(const AThread : TThread; const AWaitFor : Boolean = False);
begin
  if not Assigned(AThread) then
    Exit();

  if TOptixThread.IsThreadRunning(AThread) then begin
    AThread.Terminate;

    if AWaitFor then
      AThread.WaitFor();
  end;
end;

class procedure TOptixThread.TerminateWait(const AThread : TThread);
begin
  Terminate(AThread, True);
end;

class procedure TOptixThread.SignalHiveAndFlush();
begin
  if not Assigned(OPTIX_THREAD_HIVE) then
    Exit();
  ///

  var AThreadsToFlush := TObjectList<TOptixThread>.Create(True);
  var AList := OPTIX_THREAD_HIVE.LockList();
  try
    for var AThread in AList do begin
      if not Assigned(AThread) then
        continue;

      AThreadsToFlush.Add(AThread);
    end;
    AList.CLear();
  finally
    OPTIX_THREAD_HIVE.UnlockList();
  end;

  for var AThread in AThreadsToFlush do
    TerminateWait(AThread);

  // Free Threads (:memory)
  FreeAndNil(AThreadsToFlush);
end;

class procedure TOptixThread.FlushDeadThreads();
begin
  if not Assigned(OPTIX_THREAD_HIVE) then
    Exit();
  ///

  var AThreadsToFlush := TObjectList<TOptixThread>.Create(True);
  var AList := OPTIX_THREAD_HIVE.LockList();
  try
    for var AThread in AList do
      if Assigned(AThread) and ((not AThread.Running) and (AThread.Started)) then
        AThreadsToFlush.Add(AThread);
  finally
    // Free Threads (:memory)
    if Assigned(AThreadsToFlush) then
      FreeAndNil(AThreadsToFlush);

    OPTIX_THREAD_HIVE.UnlockList();
  end;
end;

class function TOptixThread.HasRunningInstance(const AThread : TOptixThread; const ATryTerminate : Boolean = False) : Boolean;
begin
  result := False;
  ///

  if not Assigned(OPTIX_THREAD_HIVE) then
    Exit();
  ///

  var AList := OPTIX_THREAD_HIVE.LockList();
  try
    for var ACandidate in AList do begin
      if (ACandidate = AThread) and (AThread.Running) then begin
        result := True;

        ///
        break;
      end;
    end;
  finally
    OPTIX_THREAD_HIVE.UnlockList();
  end;
end;

class procedure TOptixThread.TerminateInstance(const AThread : TOptixThread);
begin
  if not Assigned(OPTIX_THREAD_HIVE) then
    Exit();
  ///

  var AList := OPTIX_THREAD_HIVE.LockList();
  try
    for var ACandidate in AList do begin
      if ACandidate = AThread then begin
        AThread.Terminate();

        ///
        break;
      end;
    end;
  finally
    OPTIX_THREAD_HIVE.UnlockList();
  end;
end;

function TOptixThread.IsRunning() : Boolean;
begin
  result := TOptixThread.IsThreadRunning(self);
end;

procedure TOptixThread.Execute();
begin
  FStarted := True;
  try
    if Assigned(FOnThreadExecute) then
      Synchronize(procedure begin
        FOnThreadExecute(self);
      end);
    ///

    ThreadExecute();
  finally
    if Assigned(FOnThreadEnd) then
      Synchronize(procedure begin
        FOnThreadEnd(self);
      end);

    ///
    ExitThread(0); // !important
  end;
end;

constructor TOptixThread.Create();
begin
  inherited Create(True);
  ///

  FStarted := False;

  if Assigned(OPTIX_THREAD_HIVE) then
    OPTIX_THREAD_HIVE.Add(self);

  self.FreeOnTerminate := False;
  self.Priority := tpNormal;

  FCreatedDate := Now;
  FGuid := TGUID.NewGuid;

  FOnThreadExecute := nil;
  FOnThreadEnd     := nil;
end;

destructor TOptixThread.Destroy();
begin
  if Assigned(OPTIX_THREAD_HIVE) then
    OPTIX_THREAD_HIVE.Remove(self);

  ///
  inherited Destroy();
end;

initialization
  OPTIX_THREAD_HIVE := TThreadList<TOptixThread>.Create();
  OPTIX_WATCHDOG := TOptixThreadWatchDogThread.Create();

finalization
  if Assigned(OPTIX_WATCHDOG) then begin
    TOptixThread.TerminateWait(OPTIX_WATCHDOG);

    ///
    FreeAndNil(OPTIX_WATCHDOG);
  end;

  ///

  if Assigned(OPTIX_THREAD_HIVE) then
    FreeAndNil(OPTIX_THREAD_HIVE);

end.
