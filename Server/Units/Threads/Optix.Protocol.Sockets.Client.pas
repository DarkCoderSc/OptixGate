{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                              Project: Optix Neo                              }
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

unit Optix.Protocol.Sockets.Client;

interface

uses System.Classes, Optix.Sockets.Helper, Optix.Thread, System.SyncObjs,
     Generics.Collections;

type
  TOnClientDisconnect = procedure(Sender : TObject; const AClient : TClientSocket) of object;

  TOptixClientThread = class(TOptixThread)
  private
    FOnClientDisconnect : TOnClientDisconnect;
  protected
    FClient : TClientSocket;

    {@M}
    procedure ThreadExecute(); override;
    procedure TerminatedSet(); override;

    procedure ClientExecute(); virtual; abstract;
    procedure ClientTerminate(); virtual;
  public
    {@C}
    constructor Create(const AClient : TClientSocket);
    destructor Destroy(); override;

    {@G/S}
    property OnClientDisconnect : TOnClientDisconnect read FOnClientDisconnect write FOnClientDisconnect;
  end;

implementation

uses System.SysUtils, Winapi.Windows, Optix.Sockets.Exceptions;

{ TOptixClientThread.Create }
constructor TOptixClientThread.Create(const AClient : TClientSocket);
begin
  inherited Create();
  ///

  FClient := AClient;
  FOnClientDisconnect := nil;
end;

{ TOptixClientThread.Destroy }
destructor TOptixClientThread.Destroy();
begin
  if Assigned(FClient) then
    FreeAndNil(FClient);

  ///
  inherited Destroy();
end;

procedure TOptixClientThread.ClientTerminate();
begin
  if Assigned(FOnClientDisconnect) then
    Synchronize(procedure begin
      FOnClientDisconnect(self, FClient);
    end);
end;

{ TOptixClientThread.ThreadExecute }
procedure TOptixClientThread.ThreadExecute();
begin
  if not Assigned(FClient) then
    Exit();
  try
    self.ClientExecute();
  finally
    ClientTerminate();
    ///

    if Assigned(FClient) then
      FreeAndNil(FClient);
  end;
end;

{ TOptixClientThread.TerminatedSet }
procedure TOptixClientThread.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FClient) then
    FClient.Close();
end;

end.
