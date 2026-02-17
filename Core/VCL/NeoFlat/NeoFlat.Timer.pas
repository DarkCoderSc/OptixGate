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
{                   License: (!) CHECK README.md (!)                           }
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
{******************************************************************************}

unit NeoFlat.Timer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, VCL.Forms;

type
  TFlatTimer = class(TComponent)
  private
    FDueTime      : Cardinal;
    FPeriod       : Cardinal;
    FTimerHandle  : THandle;
    FWindowHandle : HWND;
    FThreadCount  : Cardinal;
    FCount        : Cardinal;
    FOnTimer      : TNotifyEvent;
    FEnabled      : Boolean;

    {@M}
    procedure SetEnabled(Value: Boolean);
    procedure WndProc(var Msg: TMessage);
    procedure SetDueTime(const Value: Cardinal);
    procedure SetPeriod(const Value: Cardinal);
  protected
    {@M}
    procedure Timer(); virtual;
  public
    {@C}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    {@G}
    property Count: Cardinal read FCount;
  published
    {@G/S}
    property Enabled : Boolean      read FEnabled write SetEnabled default False;
    property DueTime : Cardinal     read FDueTime write SetDueTime default 1000;
    property Period  : Cardinal     read FPeriod  write SetPeriod  default 1000;
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
  end;

  TWaitOrTimerCallback = procedure(lpParameter: Pointer; TimerOrWaitFired: Boolean); stdcall;

  function CreateTimerQueueTimer(
                                  var Timer: THandle;
                                  TimerQueue: THandle;
                                  Callback: TWaitOrTimerCallback;
                                  Parameter: Pointer;
                                  DueTime: LongWord;
                                  Period: LongWord;
                                  Flags: LongWord
  ): BOOL; stdcall;

  function DeleteTimerQueueTimer(
                                  TimerQueue: THandle;
                                  Timer: THandle;
                                  CompletionEvent: THandle
  ): BOOL; stdcall;

implementation

function CreateTimerQueueTimer; external kernel32 name 'CreateTimerQueueTimer';
function DeleteTimerQueueTimer; external kernel32 name 'DeleteTimerQueueTimer';

{ _.TimerCallback }

procedure TimerCallback(Timer: TFlatTimer; TimerOrWaitFired: Boolean); stdcall;
begin
  Inc(Timer.FThreadCount);

  PostMessage(Timer.FWindowHandle, WM_APP + 1, 0, Timer.FThreadCount);
end;

{ TFlatTimer.Create }

constructor TFlatTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDueTime := 1000;
  FPeriod  := 1000;

  FWindowHandle := System.Classes.AllocateHWnd(WndProc);
end;

{ TFlatTimer.Destroy }

destructor TFlatTimer.Destroy();
begin
  SetEnabled(False);

  System.Classes.DeallocateHWnd(FWindowHandle);

  ///
  inherited Destroy();
end;

{ TFlatTimer.WndProc }

procedure TFlatTimer.WndProc(var Msg: TMessage);
begin
  with Msg do begin
    if Msg = WM_APP + 1 then begin
      FCount := lParam;

      try
        Timer;
      except
        Application.HandleException(Self);
      end
    end else
      Result:= DefWindowProc(FWindowHandle, Msg, wParam, lParam);
  end;
end;

{ TFlatTimer.SetEnabled }

procedure TFlatTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    if Value then begin
      FThreadCount := 0;
      FCount       := 0;
      FEnabled     := CreateTimerQueueTimer(FTimerHandle, 0, @TimerCallback, Self, FDueTime, FPeriod, 0);
    end else begin
      DeleteTimerQueueTimer(0, FTimerHandle, 0);

      FEnabled := False;
    end;
  end;
end;

{ TFlatTimer.SetDueTime }

procedure TFlatTimer.SetDueTime(const Value: Cardinal);
begin
  if not FEnabled then
    FDueTime:= Value;
end;

{ TFlatTimer.SetPeriod }

procedure TFlatTimer.SetPeriod(const Value: Cardinal);
begin
  if not FEnabled then
    FPeriod:= Value;
end;

{ TFlatTimer.Timer }

procedure TFlatTimer.Timer();
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

end.
