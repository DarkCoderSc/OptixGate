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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Optix.Protocol.Network.Server, Optix.Sockets.Helper,
  Winapi.Winsock2, Vcl.ComCtrls, XSuperObject, Optix.Func.SessionInformation,
  Optix.Protocol.SessionHandler, Vcl.ExtCtrls, Optix.Func.Commands;

type
  TTreeData = record
    Handler            : TOptixSessionHandlerThread;
    SessionInformation : TOptixSessionInformation;
    SpawnDate          : TDateTime;
  end;
  PTreeData = ^TTreeData;

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
    TimerRefresh: TTimer;
    procedure Close1Click(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure erminate1Click(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    FServer : TOptixServerThread;

    {@M}
    procedure OnServerStart(Sender : TOptixServerThread; const ASocketFd : TSocket);
    procedure OnServerStop(Sender : TOptixServerThread);
    procedure OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);

    procedure OnSessionConnected(const pData : PTreeData);
    procedure OnSessionDisconnect(Sender : TOptixSessionHandlerThread);
    procedure OnReceivePacket(Sender : TOptixSessionHandlerThread; const ASerializedPacket : ISuperObject);

    procedure UpdateStatus(const ACaption : String = '');

    procedure RegisterSession(const AHandler : TOptixSessionHandlerThread; const ASessionInformation : TOptixSessionInformation);
    function GetNodeByHandler(const AHandler : TOptixSessionHandlerThread) : PVirtualNode;
    function GetNodeBySessionId(const ASessionId : TGUID) : PVirtualNode;
  public
    {@M}
    procedure SendCommand(const pNode : PVirtualNode; const ACommand : TOptixCommand); overload;
    procedure SendCommand(const ASessionId : TGUID; const ACommand : TOptixCommand); overload;
  end;

var
  FormMain: TFormMain;

implementation

uses Optix.Protocol.Packet, Optix.Helper, Optix.VCL.Helper;

{$R *.dfm}

procedure TFormMain.SendCommand(const pNode : PVirtualNode; const ACommand : TOptixCommand);
begin
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);
  if Assigned(pData^.Handler) then
    pData^.Handler.AddPacket(ACommand);
end;

procedure TFormMain.SendCommand(const ASessionId : TGUID; const ACommand : TOptixCommand);
begin
  SendCommand(GetNodeBySessionId(ASessionId), ACommand);
end;

procedure TFormMain.OnSessionConnected(const pData : PTreeData);
begin
  ///
end;

function TFormMain.GetNodeBySessionId(const ASessionId : TGUID) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if Assigned(pData^.SessionInformation) and (pData^.SessionInformation.SessionId = ASessionId) then begin
      result := pNode;

      break;
    end;
  end;
end;

function TFormMain.GetNodeByHandler(const AHandler : TOptixSessionHandlerThread) : PVirtualNode;
begin
  result := nil;
  ///

  if not Assigned(AHandler) then
    Exit();
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if Assigned(pData^.Handler) and (pData^.Handler = AHandler) then begin
      result := pNode;

      ///
      break;
    end;
  end;
end;

procedure TFormMain.RegisterSession(const AHandler : TOptixSessionHandlerThread; const ASessionInformation : TOptixSessionInformation);
begin
  if not Assigned(ASessionInformation) or not Assigned(AHandler) then
    Exit();
  ///

  // Should never happend!
  if GetNodeBySessionId(ASessionInformation.SessionId) <> nil then
    Exit();

  var pNode := VST.AddChild(nil);
  var pData := PTreeData(pNode.GetData);

  pData^.Handler := AHandler;
  pData^.SessionInformation := ASessionInformation;
  pData^.SpawnDate := Now;

  ///
  VST.Refresh();
end;

procedure TFormMain.UpdateStatus(const ACaption : String = '');
begin
  if String.IsNullOrEmpty(ACaption) then
    StatusBar.Panels[0].Text := 'Idle.'
  else
    StatusBar.Panels[0].Text := ACaption;
end;

procedure TFormMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormMain.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormMain.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PTreeData(Node);
  if not Assigned(pData) then
    Exit();
  ///

  if Assigned(pData^.Handler) then
    pData^.Handler.Terminate;

  if Assigned(pData^.SessionInformation) then
    FreeAndNil(pData^.SessionInformation);
end;

procedure TFormMain.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormMain.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) or not Assigned(pData^.SessionInformation) or
     not Assigned(pData^.Handler) then
    Exit();
  ///

  CellText := '';

  case Column of
    0 : CellText := pData^.Handler.PeerAddress;
    1 : CellText := pData^.SessionInformation.Username;
    2 : CellText := pData^.SessionInformation.Computer;
    3 : CellText := pData^.SessionInformation.WindowsVersion;
    4 : CellText := ElapsedDateTime(pData^.SpawnDate, Now);
    5 : CellText := pData^.SessionInformation.ProcessDetail;
    6 : CellText := pData^.SessionInformation.ElevatedStatus_STR;
  end;
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

  ///
  VST.Clear();
end;

procedure TFormMain.OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);
begin
  Application.MessageBox(PWideChar(AErrorMessage), 'Server Error', MB_ICONHAND);
end;

procedure TFormMain.OnSessionDisconnect(Sender : TOptixSessionHandlerThread);
begin
  var pNode := GetNodeByHandler(Sender);
  if not Assigned(pNode) then
    Exit();

  VST.DeleteNode(pNode);

  ///
  VST.Refresh();
end;

procedure TFormMain.PopupMenuPopup(Sender: TObject);
begin
  TOptixVCLHelper.HideAllPopupMenuRootItems(TPopupMenu(Sender));

  self.erminate1.Visible := VST.FocusedNode <> nil;
end;

procedure TFormMain.OnReceivePacket(Sender : TOptixSessionHandlerThread; const ASerializedPacket : ISuperObject);
begin
  if not Assigned(ASerializedPacket) or
     not ASerializedPacket.Contains('PacketClass') then
      Exit();
  ///

  // TODO: make it more generic (Class Registry or RTTI)
  var AClassName := ASerializedPacket.S['PacketClass'];
  var AHandleMemory := False;

  var AOptixPacket : TOptixPacket := nil;
  try
    try
      if AClassName = 'TOptixSessionInformation' then begin
        AOptixPacket := TOptixSessionInformation.Create(ASerializedPacket);


        // Dispatch to data handler
        if AOptixPacket is TOptixSessionInformation then begin
          AHandleMemory := True;

          ///
          RegisterSession(Sender, TOptixSessionInformation(AOptixPacket));
        end;
      end;

      // else if ...
    finally
      if not AHandleMemory and Assigned(AOptixPacket) then
        FreeAndNil(AOptixPacket);
    end;
  except
    // TODO: log packet errors
  end;
end;

procedure TFormMain.Close1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.erminate1Click(Sender: TObject);
begin
  SendCommand(VST.FocusedNode, TOptixCommandTerminate.Create());
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
      FServer.OnSessionDisconnect := OnSessionDisconnect;
      FServer.OnReceivePacket     := OnReceivePacket;

      ///
      FServer.Start();
    end;
  end;
end;

procedure TFormMain.TimerRefreshTimer(Sender: TObject);
begin
  VST.Refresh();
end;

end.
