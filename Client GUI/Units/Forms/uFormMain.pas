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

unit uFormMain;

interface

{$I Optix.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.Menus, Optix.Protocol.SessionHandler, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Optix.Protocol.Client, System.Notification, Generics.Collections,
  VirtualTrees.Types, System.UITypes;

type
  TClientStatus = (csDisconnected, csConnected, csOnError, csFree);

  TTreeData = record
    ServerAddress    : String; // Copy required (If Handler is Freed)
    ServerPort       : Word;   // Copy required

    {$IFDEF USETLS}
    PublicKey        : String; // Copy required
    PrivateKey       : String; // Copy required
    {$ENDIF}

    Handler          : TOptixSessionHandlerThread;
    Status           : TClientStatus;
    ExtraDescription : String;
  end;
  PTreeData = ^TTreeData;

  TFormMain = class(TForm)
    VST: TVirtualStringTree;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    ConnecttoServer1: TMenuItem;
    N1: TMenuItem;
    ImageCollectionDark: TImageCollection;
    VirtualImageList: TVirtualImageList;
    PopupMenu: TPopupMenu;
    RemoveClient1: TMenuItem;
    about1: TMenuItem;
    Debug1: TMenuItem;
    hreads1: TMenuItem;
    N2: TMenuItem;
    Reload1: TMenuItem;
    NotificationCenter: TNotificationCenter;
    Stores1: TMenuItem;
    Certificates1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure Close1Click(Sender: TObject);
    procedure ConnecttoServer1Click(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PopupMenuPopup(Sender: TObject);
    procedure RemoveClient1Click(Sender: TObject);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure about1Click(Sender: TObject);
    procedure hreads1Click(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Certificates1Click(Sender: TObject);
  private
    FNotifications : TList<TGUID>;

    {@M}
    procedure AddClient({$IFDEF USETLS}const APublicKey : String; const APrivateKey : String;{$ENDIF} const AServerAddress : String; const AServerPort : Word; const pExistingNode : PVirtualNode = nil);

    procedure OnConnectedToServer(Sender : TOptixSessionHandlerThread);
    procedure OnDisconnectedFromServer(Sender : TOptixSessionHandlerThread);
    procedure OnSessionHandlerDestroyed(Sender : TOptixSessionHandlerThread);
    procedure OnNetworkException(Sender : TOptixClientThread; const AErrorMessage : String);

    procedure UpdateNodeStatus(const pNode : PVirtualNode; const ANewStatus : TClientStatus; const AExtraDescription : String = ''); overload;
    procedure UpdateNodeStatus(const AHandler : TOptixSessionHandlerThread; const ANewStatus : TClientStatus; const AExtraDescription : String = ''); overload;
    function GetNodeFromHandler(const AHandler : TOptixSessionHandlerThread) : PVirtualNode;

    function DisplayNotification(const ATitle, ABody : String) : TGUID;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses Optix.Thread, Optix.VCL.Helper, Optix.Helper, uFormConnectToServer, Optix.Constants, uFormAbout, uFormDebugThreads,
     Optix.Protocol.Preflight{$IFDEF USETLS}, uFormCertificatesStore, Optix.DebugCertificate{$ENDIF};

{$R *.dfm}

function TFormMain.DisplayNotification(const ATitle, ABody : String) : TGUID;
begin
  var ANotification := NotificationCenter.CreateNotification();
  try
    var ANotificationId := TGUID.NewGuid();

    ANotification.Name      := ANotificationId.ToString();
    ANotification.Title     := ATitle;
    ANotification.AlertBody := ABody;

    NotificationCenter.PresentNotification(ANotification);

    FNotifications.Add(ANotificationId);

    ///
    result := ANotificationId;
  finally
    FreeAndNil(ANotification);
  end;
end;

function TFormMain.GetNodeFromHandler(const AHandler : TOptixSessionHandlerThread) : PVirtualNode;
begin
  result := nil;
  ///

  if not Assigned(AHandler) then
    Exit();

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);

    if pData^.Handler = AHandler then begin
      result := pNode;

      break
    end;
  end;
end;

procedure TFormMain.hreads1Click(Sender: TObject);
begin
  FormDebugThreads.Show();
end;

procedure TFormMain.UpdateNodeStatus(const pNode : PVirtualNode; const ANewStatus : TClientStatus; const AExtraDescription : String = '');
begin
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := pTreeData(pNode.GetData);
  if pData^.Status = ANewStatus then
    Exit();

  VST.BeginUpdate();
  try
    pData^.Status := ANewStatus;
    pData^.ExtraDescription := AExtraDescription;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormMain.UpdateNodeStatus(const AHandler : TOptixSessionHandlerThread; const ANewStatus : TClientStatus; const AExtraDescription : String = '');
begin
  UpdateNodeStatus(GetNodeFromHandler(AHandler), ANewStatus, AExtraDescription);
end;

procedure TFormMain.OnConnectedToServer(Sender : TOptixSessionHandlerThread);
begin
  UpdateNodeStatus(Sender, csConnected);
  ///

  FNotifications.Add(DisplayNotification(
    'Connected',
    Format(
      'A client is connected to a remote server (%s:%d). If you were not aware of this connection or did not initiate ' +
      'it, be aware that someone might have full control of your computer. You should immediately investigate for any ' +
      'unexpected running processes.',
       [
          Sender.RemoteAddress,
          Sender.RemotePort
       ]
    )));
end;

procedure TFormMain.OnDisconnectedFromServer(Sender : TOptixSessionHandlerThread);
begin
  UpdateNodeStatus(Sender, csDisconnected);
end;

procedure TFormMain.OnSessionHandlerDestroyed(Sender : TOptixSessionHandlerThread);
begin
  UpdateNodeStatus(Sender, csFree);
end;

procedure TFormMain.OnNetworkException(Sender : TOptixClientThread; const AErrorMessage : String);
begin
  UpdateNodeStatus(TOptixSessionHandlerThread(Sender), csOnError, AErrorMessage);
end;

procedure TFormMain.about1Click(Sender: TObject);
begin
  FormAbout.ShowModal();
end;

procedure TFormMain.Certificates1Click(Sender: TObject);
begin
  {$IFDEF USETLS}
  FormCertificatesStore.Show();
  {$ENDIF}
end;

procedure TFormMain.Close1Click(Sender: TObject);
begin
  Close();
end;

procedure TFormMain.AddClient({$IFDEF USETLS}const APublicKey : String; const APrivateKey : String;{$ENDIF} const AServerAddress : String; const AServerPort : Word; const pExistingNode : PVirtualNode = nil);
begin
  VST.beginUpdate();
  try
    var pNode : PVirtualNode;

    if pExistingNode = nil then
      pNode := VST.AddChild(nil)
    else
      pNode := pExistingNode;

    var pData := PTreeData(pNode.GetData);

    pData^.Status := csDisconnected;
    pData^.ExtraDescription := '';

    pData^.ServerAddress := AServerAddress;
    pData^.ServerPort    := AServerPort;

    {$IFDEF USETLS}
    pData^.PublicKey     := APublicKey;
    pData^.PrivateKey    := APrivateKey;
    {$ENDIF}

    pData^.Handler := TOptixSessionHandlerThread.Create({$IFDEF USETLS}APublicKey, APrivateKey, {$ENDIF}pData^.ServerAddress, pData^.ServerPort);
    pData^.Handler.Retry := True;
    pData^.Handler.RetryDelay := 1000;

    pData^.Handler.OnConnectedToServer         := OnConnectedToServer;
    pData^.Handler.OnDisconnectedFromServer    := OnDisconnectedFromServer;
    pData^.Handler.OnSessionHandlerDestroyed   := OnSessionHandlerDestroyed;
    pData^.Handler.OnNetworkException          := OnNetworkException;

    pData^.Handler.Start();
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormMain.ConnecttoServer1Click(Sender: TObject);
begin
  {$IFDEF DEBUG}
    {$IFDEF USETLS}
      var APublicKey  := '';
      var APrivateKey := '';

      {$IFDEF DEBUG}
      APublicKey  := DEBUG_CERTIFICATE_PUBLIC_KEY;
      APrivateKey := DEBUG_CERTIFICATE_PRIVATE_KEY;
      {$ENDIF}
    {$ENDIF}

    AddClient({$IFDEF USETLS}APublicKey, APrivateKey, {$ENDIF}'127.0.0.1', 2801);
  {$ELSE}
    var AForm := TFormConnectToServer.Create(self);
    try
      AForm.ShowModal();
      if AForm.Canceled then
        Exit();
      ///

      AddClient(AForm.EditServerAddress.Text, AForm.SpinPort.Value);
    finally
      FreeAndNil(AForm);
    end;
  {$ENDIF}
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  VST.Clear();

  NotificationCenter.CancelAll();

  ///
  TOptixThread.SignalHiveAndFlush();
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FNotifications := TList<TGUID>.Create(); // Maybe, I will need that l8r

  Caption := Format('%s - %s', [Caption, OPTIX_PROTOCOL_VERSION]);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FNotifications) then
    FreeAndNil(FNotifications);
end;

procedure TFormMain.PopupMenuPopup(Sender: TObject);
begin
  RemoveClient1.Visible :=  VST.FocusedNode <> nil;
  Reload1.Visible := False;

  var pNode := VST.FocusedNode;
  var pData : PTreeData;

  if Assigned(pNode) then begin
    pData := pNode.GetData;

    if pData^.Status = csFree then
      Reload1.Visible := True;
  end;
end;

procedure TFormMain.Reload1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);

  if pData^.Status = csFree then
    AddClient({$IFDEF USETLS}pData^.PublicKey, pData^.PrivateKey, {$ENDIF}pData^.ServerAddress, pData^.ServerPort, VST.FocusedNode);
end;

procedure TFormMain.RemoveClient1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  VST.BeginUpdate();
  try
    VST.DeleteNode(VST.FocusedNode);
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormMain.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);

  var AColor := clNone;

  case pData^.Status of
    csConnected : AColor := COLOR_LIST_LIMY;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormMain.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormMain.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  if Assigned(pData^.Handler) then
    pData^.Handler.Terminate;
end;

procedure TFormMain.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);

  if (Column <> 0) or ((Kind <> TVTImageKind.ikNormal) and (Kind <> TVTImageKind.ikSelected)) then
    Exit();

  case pData^.Status of
    csDisconnected : ImageIndex := IMAGE_CLIENT_DISCONNECTED;
    csConnected    : ImageIndex := IMAGE_CLIENT_CONNECTED;
    csOnError      : ImageIndex := IMAGE_CLIENT_ERROR;
    csFree         : ImageIndex := IMAGE_CLIENT_FREED;
  end;
end;

procedure TFormMain.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormMain.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  case Column of
    0 : CellText := pData^.ServerAddress;
    1 : CellText := IntToStr(pData^.ServerPort);
    2 : begin
      case pData^.Status of
        csDisconnected : CellText := 'Disconnected';
        csConnected    : CellText := 'Connected';
        csFree         : CellText := 'Free';
        csOnError      : CellText := 'Error';
      end;
    end;
    3 : CellText := pData^.ExtraDescription;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

end.
