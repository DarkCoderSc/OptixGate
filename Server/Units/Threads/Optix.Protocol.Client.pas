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

unit Optix.Protocol.Client;

interface

uses System.Classes, Optix.Sockets.Helper, Optix.Thread, System.SyncObjs,
     Generics.Collections;

type
  TOnClientDisconnect = procedure(Sender : TObject; const AClient : TClientSocket) of object;

  TOptixClientThread = class(TOptixThread)
  private
    FOnClientDisconnect : TOnClientDisconnect;
  protected
    FClient      : TClientSocket;
    FPeerAddress : String;

    {@M}
    procedure ThreadExecute(); override;
    procedure TerminatedSet(); override;

    procedure Initialize(); virtual;
    procedure Finalize(); virtual;

    procedure ClientExecute(); virtual; abstract;
    procedure ClientTerminate(); virtual;
  public
    {@C}
    constructor Create(const AClient : TClientSocket);
    destructor Destroy(); override;

    {@G/S}
    property OnClientDisconnect : TOnClientDisconnect read FOnClientDisconnect write FOnClientDisconnect;

    {@G}
    property PeerAddress : String read FPeerAddress;
  end;

implementation

uses System.SysUtils, Winapi.Windows, Optix.Sockets.Exceptions;

{ TOptixClientThread.Initialize }
procedure TOptixClientThread.Initialize();
begin
  ///
end;

{ TOptixClientThread.Finalize }
procedure TOptixClientThread.Finalize();
begin
  ///
end;

{ TOptixClientThread.Create }
constructor TOptixClientThread.Create(const AClient : TClientSocket);
begin
  inherited Create();
  ///

  FClient := AClient;
  FPeerAddress := FClient.RemoteAddress;

  FOnClientDisconnect := nil;

  ///
  Initialize();
end;

{ TOptixClientThread.Destroy }
destructor TOptixClientThread.Destroy();
begin
  if Assigned(FClient) then
    FreeAndNil(FClient);

  ///
  Finalize();

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
