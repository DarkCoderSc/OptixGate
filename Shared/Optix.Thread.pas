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

unit Optix.Thread;

interface

{$I Optix.inc}

uses System.Classes, Generics.Collections, System.SyncObjs;

type
  (* TOptixThreadWatchDogThread *)
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

  (* TOptixThread *)
  TOptixThread = class(TThread)
  private
    FCreatedDate     : TDateTime;
    FOnThreadExecute : TNotifyEvent;
    FOnThreadEnd     : TNotifyEvent;

    {@M}
    function GetPriorityString() : String;
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
    class procedure Terminate(const AThread : TThread; const AWaitFor : Boolean = False); overload;
    class procedure TerminateWait(const AThread : TThread); static;
    class procedure SignalHiveAndWait(); static;
    class procedure PurgeDeadThreads(); static;
    class function HasInstance(const AThread : TOptixThread; const ATryTerminate : Boolean = False) : Boolean; static;
    class function TerminateInstance(const AThread : TOptixThread) : Boolean; static;

    {@S}
    property OnThreadExecute : TNotifyEvent write FOnThreadExecute;
    property OnThreadEnd     : TNotifyEvent write FOnThreadEnd;

    {@G}
    property Running     : Boolean   read IsRunning;
    property CreatedDate : TDateTime read FCreatedDate;
    property PriorityStr : String    read GetPriorityString;
  end;

  var OPTIX_THREAD_HIVE : TThreadList<TOptixThread>;
      OPTIX_WATCHDOG    : TOptixThreadWatchDogThread;

implementation

uses Winapi.Windows, System.SysUtils;

(******************************************************************************)
(* TOptixThreadWatchDogThread                                                 *)
(******************************************************************************)

{ TOptixThreadWatchDogThread.Create }
constructor TOptixThreadWatchDogThread.Create();
begin
  inherited Create(False);

  self.Priority := tpLowest;
  self.FreeOnTerminate := False;

  FIntervalEvent := TEvent.Create(nil, True, False, TGUID.NewGuid.ToString());
end;

{ TOptixThreadWatchDogThread.Destroy }
destructor TOptixThreadWatchDogThread.Destroy();
begin
  if Assigned(FIntervalEvent) then
    FreeAndNil(FIntervalEvent);

  ///
  inherited Destroy();
end;

{ TOptixThreadWatchDogThread.Execute }
procedure TOptixThreadWatchDogThread.Execute();
begin
  try
    if NOT Assigned(FIntervalEvent) then
      Exit();
    ///

    while NOT Terminated do begin
      TOptixThread.PurgeDeadThreads();

      ///
      FIntervalEvent.WaitFor(1000);
    end;

    ///
    TOptixThread.SignalHiveAndWait();
  finally
    ExitThread(0); // !important
  end;
end;

{ TOptixThreadWatchDogThread.TerminatedSet }
procedure TOptixThreadWatchDogThread.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FIntervalEvent) then
    FIntervalEvent.SetEvent();
end;


(******************************************************************************)
(* TOptixThread                                                               *)
(******************************************************************************)

{ TOptixThread.IsThreadRunning }
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

{ TOptixThread.Terminate }
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

{ TOptixThread.TerminateWait }
class procedure TOptixThread.TerminateWait(const AThread : TThread);
begin
  Terminate(AThread, True);
end;

{ TOptixThread.SignalHiveAndWait }
class procedure TOptixThread.SignalHiveAndWait();
var AList           : TList<TOptixThread>;
    AThreadsToPurge : TObjectList<TOptixThread>;
    AThread         : TOptixThread;
begin
  if not Assigned(OPTIX_THREAD_HIVE) then
    Exit();

  AThreadsToPurge := TObjectList<TOptixThread>.Create(True);
  AList := OPTIX_THREAD_HIVE.LockList();
  try
    for AThread in AList do begin
      if not Assigned(AThread) then
        continue;
      ///

      if AThread.Running then
        TOptixThread.TerminateWait(AThread);

      ///
      AThreadsToPurge.Add(AThread);
    end;
  finally
    OPTIX_THREAD_HIVE.UnlockList();

    // Free Threads
    if Assigned(AThreadsToPurge) then
      FreeAndNil(AThreadsToPurge);
  end;
end;

{ TOptixThread.PurgeDeadThreads }
class procedure TOptixThread.PurgeDeadThreads();
begin
  if not Assigned(OPTIX_THREAD_HIVE) then
    Exit();
  ///

  var AThreadsToPurge := TObjectList<TOptixThread>.Create(True);
  var AList := OPTIX_THREAD_HIVE.LockList();
  try
    for var AThread in AList do begin
      if Assigned(AThread) and not AThread.Running then
        AThreadsToPurge.Add(AThread);
    end;
  finally
    OPTIX_THREAD_HIVE.UnlockList();

    // Free Threads
    if Assigned(AThreadsToPurge) then
      FreeAndNil(AThreadsToPurge);
  end;
end;

{ TOptixThread.HasInstance }
class function TOptixThread.HasInstance(const AThread : TOptixThread; const ATryTerminate : Boolean = False) : Boolean;
begin
  result := False;
  ///

  if not Assigned(OPTIX_THREAD_HIVE) then
    Exit();
  ///

  var AList := OPTIX_THREAD_HIVE.LockList();
  try
    for var ACandidate in AList do begin
      if ACandidate = AThread then begin
        result := True;

        if ATryTerminate then
          TOptixThread.Terminate(AThread);

        break;
      end;
    end;
  finally
    OPTIX_THREAD_HIVE.UnlockList();
  end;
end;

{ TOptixThread.TerminateInstance }
class function TOptixThread.TerminateInstance(const AThread : TOptixThread) : Boolean;
begin
  result := HasInstance(AThread, True);
end;

{ TOptixThread.IsRunning }
function TOptixThread.IsRunning() : Boolean;
begin
  result := TOptixThread.IsThreadRunning(self);
end;

{ TOptixThread.Execute }
procedure TOptixThread.Execute();
begin
  try
    if Assigned(FOnThreadExecute) then
      Synchronize(procedure begin
        FOnThreadExecute(self);
      end);
    ///

    // Not Mandatory, must be overriden.
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

{ TOptixThread.Create }
constructor TOptixThread.Create();
begin
  inherited Create(True);
  ///

  if Assigned(OPTIX_THREAD_HIVE) then
    OPTIX_THREAD_HIVE.Add(self);

  self.FreeOnTerminate := False;
  self.Priority := tpNormal;

  FCreatedDate := Now;

  FOnThreadExecute := nil;
  FOnThreadEnd     := nil;
end;

{ TOptixThread.Destroy }
destructor TOptixThread.Destroy();
begin
  if Assigned(OPTIX_THREAD_HIVE) then
    OPTIX_THREAD_HIVE.Remove(self);

  ///
  inherited Destroy();
end;

{ TOptixThread.GetPriorityString }
function TOptixThread.GetPriorityString() : String;
begin
  result := 'Unknown';
  ///

  case self.Priority of
    tpIdle         : result := 'Idle';
    tpLowest       : result := 'Lowest';
    tpLower        : result := 'Lower';
    tpNormal       : result := 'Normal';
    tpHigher       : result := 'Higher';
    tpHighest      : result := 'Highest';
    tpTimeCritical : result := 'Time Critical';
  end;
end;



initialization
  OPTIX_THREAD_HIVE := TThreadList<TOptixThread>.Create();

  OPTIX_WATCHDOG := TOptixThreadWatchDogThread.Create();

finalization
  if Assigned(OPTIX_WATCHDOG) then begin
    // The client can do it during process finalization, but the server, due to
    // its GUI nature, cannot. It must be called manually from the GUI (OnClose)
    {$IFDEF CLIENT}
    TOptixThread.TerminateWait(OPTIX_WATCHDOG);
    {$ENDIF}

    ///
    FreeAndNil(OPTIX_WATCHDOG);
  end;

  if Assigned(OPTIX_THREAD_HIVE) then
    FreeAndNil(OPTIX_THREAD_HIVE);

end.
