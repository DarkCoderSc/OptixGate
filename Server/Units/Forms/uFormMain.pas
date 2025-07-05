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
  Optix.Protocol.SessionHandler, Vcl.ExtCtrls, Optix.Func.Commands,
  Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.StdCtrls, __uBaseFormControl__, Generics.Collections;

type
  TTreeData = record
    Handler            : TOptixSessionHandlerThread;
    SessionInformation : TOptixSessionInformation;
    SpawnDate          : TDateTime;
    Forms              : TObjectList<TBaseFormControl>;
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
    StatusBar: TStatusBar;
    TimerRefresh: TTimer;
    ImageCollection: TImageCollection;
    VirtualImageList: TVirtualImageList;
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
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure About1Click(Sender: TObject);
    procedure ProcessManager1Click(Sender: TObject);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
    function GetControlForm(const pData : PTreeData; const AClass : TClass) : TBaseFormControl; overload;
    function GetControlForm(const pNode : PVirtualNode; const AWindowGUID : TGUID) : TBaseFormControl; overload;
    function ControlFormExists(const pData : PTreeData; const AClass : TClass) : Boolean;
    function GetNodeByControlForm(const AForm : TBaseFormControl) : PVirtualNode;
  public
    {@M}
    procedure SendCommand(const pNode : PVirtualNode; const ACommand : TOptixCommand); overload;
    procedure SendCommand(const ASessionId : TGUID; const ACommand : TOptixCommand); overload;
    procedure SendCommand(const ACaller : TBaseFormControl; const ACommand : TOptixCommand); overload;
  end;

var
  FormMain: TFormMain;

implementation

uses Optix.Protocol.Packet, Optix.Helper, Optix.VCL.Helper, Optix.Constants,
     Optix.InformationGathering.Process, uFormAbout, uFormProcessManager,
     Optix.Thread;

{$R *.dfm}

function TFormMain.GetControlForm(const pData : PTreeData; const AClass : TClass) : TBaseFormControl;
begin
  result := nil;
  ///

  if not Assigned(pData) or not Assigned(pData^.Forms) then
    Exit();

  for var AForm in pData^.Forms do begin
    if AForm is AClass then begin
      result := AForm;

      ///
      break;
    end;
  end;
end;

function TFormMain.GetControlForm(const pNode : PVirtualNode; const AWindowGUID : TGUID) : TBaseFormControl;
begin
  result := nil;
  ///

  if not Assigned(pNode) then
    Exit();

  var pData := PTreeData(pNode.GetData);

  if not Assigned(pData^.Forms) then
    Exit();

  for var AForm in pData^.Forms do begin
    if AForm.GUID = AWindowGUID then begin
      result := AForm;

      ///
      break;
    end;
  end;
end;

function TFormMain.ControlFormExists(const pData : PTreeData; const AClass : TClass) : Boolean;
begin
  result := GetControlForm(pData, AClass) <> nil;
end;

procedure TFormMain.SendCommand(const pNode : PVirtualNode; const ACommand : TOptixCommand);
begin
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);
  if Assigned(pData^.Handler) then
    pData^.Handler.AddPacket(ACommand);
end;

function TFormMain.GetNodeByControlForm(const AForm : TBaseFormControl) : PVirtualNode;
begin
  result := nil;
  ///

  if not Assigned(AForm) then
    Exit();

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if not Assigned(pData^.Forms) then
      continue;

    for var ACandidate in pData^.Forms do begin
      if ACandidate = AForm then begin
        result := pNode;

        ///
        break;
      end;
    end;
  end;
end;

procedure TFormMain.SendCommand(const ACaller : TBaseFormControl; const ACommand : TOptixCommand);
begin
  if not Assigned(ACaller) then
    Exit();
  ///

  var pNode := GetNodeByControlForm(ACaller);

  ///
  SendCommand(pNode, ACommand);
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

procedure TFormMain.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);

  if not Assigned(pData^.SessionInformation) then
    Exit();

  var AColor := clNone;

  if pData^.SessionInformation.IsSystem then
    AColor := COLOR_USER_SYSTEM
  else if pData^.SessionInformation.ElevatedStatus = esElevated then
    AColor := COLOR_USER_ELEVATED;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    ///
    TargetCanvas.FillRect(CellRect);
  end;
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
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  if Assigned(pData^.SessionInformation) then
    FreeAndNil(pData^.SessionInformation);

  if Assigned(pData^.Forms) then
    FreeAndNil(pData^.Forms);
end;

procedure TFormMain.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);

  if Column <> 0 then
    Exit();

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected: begin
      if pData^.SessionInformation.IsSystem then
        ImageIndex := IMAGE_USER_SYSTEM
      else begin
        if pData^.SessionInformation.ElevatedStatus = esElevated then
          ImageIndex := IMAGE_USER_ELEVATED
        else begin
          if pData^.SessionInformation.IsInAdminGroup then
            ImageIndex := IMAGE_USER_ADMIN
          else
            ImageIndex := IMAGE_USER;
        end;
      end;
    end;
    TVTImageKind.ikState: ;
    TVTImageKind.ikOverlay: ;
  end;
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
    1 : CellText := Format('%s@%s', [
      pData^.SessionInformation.Username,
      pData^.SessionInformation.Computer
    ]);
    2 : CellText := DefaultIfEmpty(pData^.SessionInformation.Langroup);
    3 : CellText := DefaultIfEmpty(pData^.SessionInformation.DomainName);
    4 : CellText := pData^.SessionInformation.WindowsVersion;
    5 : CellText := ElapsedDateTime(pData^.SpawnDate, Now);
    6 : CellText := pData^.SessionInformation.ProcessDetail;
    7 : CellText := pData^.SessionInformation.ElevatedStatus_STR;
    8 : CellText := BoolToStr(pData^.SessionInformation.IsInAdminGroup, True);
  end;
end;

procedure TFormMain.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  var pData := PTreeData(Node.GetData);
  pData^.Forms := TObjectList<TBaseFormControl>.Create(True);
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

  FServer := nil;

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

  self.erminate1.Visible       := VST.FocusedNode <> nil;
  self.ProcessManager1.Visible := self.erminate1.Visible;
end;

procedure TFormMain.ProcessManager1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);

  var AForm := GetControlForm(pData, TFormProcessManager);

  if not Assigned(AForm) then begin
    AForm := TFormProcessManager.Create(self);

    ///
    pData^.Forms.Add(AForm);
  end;

  ///
  AForm.Show();
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

  var ASessionID := TGUID.Empty;
  if ASerializedPacket.Contains('SessionId') then
    ASessionId := TGUID.Create(ASerializedPacket.S['SessionId']);

  var AOptixPacket : TOptixPacket := nil;
  try
    try
      if not ASerializedPacket.Contains('WindowGUID') then begin
        { Responses }
        if AClassName = 'TOptixSessionInformation' then begin
          AOptixPacket := TOptixSessionInformation.Create(ASerializedPacket);


          // Dispatch to data handler
          if AOptixPacket is TOptixSessionInformation then begin
            AHandleMemory := True;

            ///
            RegisterSession(Sender, TOptixSessionInformation(AOptixPacket));
          end;
        end;

        // ... //
        // else if ...
        // ... //
      end else begin
        { Windowed Responses }
        var AWindowGUID := TGUID.Create(ASerializedPacket.S['WindowGUID']);
        var pNode := GetNodeBySessionId(ASessionId);
        var AControlForm := GetControlForm(pNode, AWindowGUID);
        if Assigned(AControlForm) then
          AControlForm.ReceivePacket(AClassName, ASerializedPacket);
      end;
    finally
      if not AHandleMemory and Assigned(AOptixPacket) then
        FreeAndNil(AOptixPacket);
    end;
  except
    // TODO: log packet errors
  end;
end;

procedure TFormMain.About1Click(Sender: TObject);
begin
  FormAbout.ShowModal();
end;

procedure TFormMain.Close1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.erminate1Click(Sender: TObject);
begin
  SendCommand(VST.FocusedNode, TOptixCommandTerminate.Create());
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TOptixThread.TerminateWait(OPTIX_WATCHDOG); // This is important to gracefully
                                              // terminate threads.
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
