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



unit uFormMain;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.UITypes, System.ImageList, System.Notification, System.Types,

  Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Vcl.ImgList, Vcl.VirtualImageList,

  VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees.Types,

  uFormConnectToServer,

  Optix.Protocol.SessionHandler, Optix.Protocol.Client

  {$IFDEF USETLS}, Optix.OpenSSL.Helper{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

type
  TClientStatus = (csDisconnected, csConnected, csOnError);

  TTreeData = record
    ClientConfiguration : TClientConfiguration;
    Handler             : TOptixSessionHandlerThread;

    Status              : TClientStatus;
    ExtraDescription    : String;

    {$IFDEF USETLS}
    Certificate : TX509Certificate;
    {$ENDIF}
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
    NotificationCenter: TNotificationCenter;
    Stores1: TMenuItem;
    Certificates1: TMenuItem;
    rustedCertificates1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Certificates1Click(Sender: TObject);
    procedure rustedCertificates1Click(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FNotifications : TList<TGUID>;

    {@M}
    procedure AddClient(const AClientConfiguration : TClientConfiguration; const pExistingNode : PVirtualNode = nil);

    procedure OnConnectedToServer(Sender : TOptixSessionHandlerThread);
    procedure OnDisconnectedFromServer(Sender : TOptixSessionHandlerThread);
    procedure OnSessionHandlerDestroyed(Sender : TOptixSessionHandlerThread);
    procedure OnNetworkException(Sender : TOptixClientThread; const AErrorMessage : String);

    procedure UpdateNodeStatus(const pNode : PVirtualNode; const ANewStatus : TClientStatus; const AExtraDescription : String = ''); overload;
    procedure UpdateNodeStatus(const AHandler : TOptixSessionHandlerThread; const ANewStatus : TClientStatus; const AExtraDescription : String = ''); overload;
    function GetNodeFromHandler(const AHandler : TOptixSessionHandlerThread) : PVirtualNode;

    {$IFNDEF DEBUG}
    function DisplayNotification(const ATitle, ABody : String) : TGUID;
    {$ENDIF}
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math,

  uFormAbout, uFormDebugThreads
  {$IFDEF USETLS}, uFormCertificatesStore, uFormTrustedCertificates{$ENDIF},

  Optix.Thread, Optix.VCL.Helper, Optix.Helper, Optix.Constants, Optix.Protocol.Preflight, Optix.Sockets.Helper
  {$IFDEF USETLS}, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

{$IFNDEF DEBUG}
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
{$ENDIF}

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

  {$IFNDEF DEBUG}
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
  {$ENDIF}
end;

procedure TFormMain.OnDisconnectedFromServer(Sender : TOptixSessionHandlerThread);
begin
  UpdateNodeStatus(Sender, csDisconnected);
end;

procedure TFormMain.OnSessionHandlerDestroyed(Sender : TOptixSessionHandlerThread);
begin
  var pNode := GetNodeFromHandler(Sender);
  if not Assigned(pNode) then
    Exit();

  ///
  VST.DeleteNode(pNode);
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

procedure TFormMain.AddClient(const AClientConfiguration : TClientConfiguration; const pExistingNode : PVirtualNode = nil);
begin
  {$IFDEF USETLS}
    var ACertificate : TX509Certificate;

    if not FormCertificatesStore.GetCertificateKeys(AClientConfiguration.CertificateFingerprint, ACertificate)
    then begin
      Application.MessageBox(
        'Client certificate fingerprint does not exist in the store. Please import an existing certificate first or ' +
        'generate a new one.',
        'Connect To Server',
        MB_ICONERROR
      );

      Exit();
    end;
  {$ENDIF}

  VST.BeginUpdate();
  try
    var pNode : PVirtualNode;

    if pExistingNode = nil then
      pNode := VST.AddChild(nil)
    else
      pNode := pExistingNode;

    var pData := PTreeData(pNode.GetData);

    pData^.ClientConfiguration := AClientConfiguration;

    pData^.Status := csDisconnected;
    pData^.ExtraDescription := '';

    pData^.Handler := TOptixSessionHandlerThread.Create(
      {$IFDEF USETLS}ACertificate,{$ENDIF}
      AClientConfiguration.Address,
      AClientConfiguration.Port,
      AClientConfiguration.Version
    );

    pData^.Handler.Retry := True;
    pData^.Handler.RetryDelay := 1000;

    pData^.Handler.OnConnectedToServer         := OnConnectedToServer;
    pData^.Handler.OnDisconnectedFromServer    := OnDisconnectedFromServer;
    pData^.Handler.OnSessionHandlerDestroyed   := OnSessionHandlerDestroyed;
    pData^.Handler.OnNetworkException          := OnNetworkException;
    {$IFDEF USETLS}
    pData^.Handler.OnVerifyPeerCertificate     := FormTrustedCertificates.OnVerifyPeerCertificate;
    {$ENDIF}

    pData^.Handler.Start();
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormMain.ConnecttoServer1Click(Sender: TObject);
var AForm : TFormConnectToServer;
begin
  {$IFDEF USETLS}
    var AFingerprints := FormCertificatesStore.GetCertificatesFingerprints();
    try
      if AFingerprints.Count = 0 then
        raise Exception.Create('No existing certificate was found in the certificate store. You cannot connect ' +
                               'to a remote server without registering at least one certificate.');

//        if FormTrustedCertificates.TrustedCertificateCount = 0 then
//              raise Exception.Create('No trusted certificate (fingerprint) was found in the trusted certificate ' +
//                                     'store. You cannot connect to a remote server without registering at least one ' +
//                                     'trusted certificate. A trusted certificate represents the fingerprint of a ' +
//                                     'server certificate and is required for mutual authentication, ensuring that ' +
//                                     'network communications are secure and not tampered with or eavesdropped on.');

      ///

      AForm := TFormConnectToServer.Create(self, AFingerprints);
    finally
      FreeAndNil(AFingerprints);
    end;
  {$ELSE}
    AForm := TFormConnectToServer.Create(self);
  {$ENDIF}
  try
    AForm.ShowModal();
    if AForm.Canceled then
      Exit();
    ///

    AddClient(AForm.GetClientConfiguration());
  finally
    FreeAndNil(AForm);
  end;
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
  ///

  {$IFNDEF USETLS}
  Stores1.Visible := False;
  Certificates1.Visible := False;
  rustedCertificates1.Visible := False;

  VST.Header.Columns[5].Options := VST.Header.Columns[5].Options - [coVisible];
  {$ENDIF}

  ///
  Caption := Format('%s - %s', [Caption, OPTIX_PROTOCOL_VERSION]);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  VST.Clear();
  ///

  if Assigned(FNotifications) then
    FreeAndNil(FNotifications);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  {$IFDEF DEBUG}
    // Ipv4
    var AClientConfiguration : TClientConfiguration;
    AClientConfiguration.Address := '127.0.0.1';
    AClientConfiguration.Port    := 2801;
    AClientConfiguration.Version := ipv4;

    {$IFDEF USETLS}
    AClientConfiguration.CertificateFingerprint := Optix.DebugCertificate.DEBUG_CERTIFICATE_FINGERPRINT;
    {$ENDIF}

    AddClient(AClientConfiguration);

    // Ipv6
//    AClientConfiguration.Version := ipv6;
//    AClientConfiguration.Address := '::1';
//    AddClient(AClientConfiguration);

    // Minimize main window to taskbar on app startup (during development / debug)
    WindowState := wsMinimized;
  {$ENDIF}
end;

procedure TFormMain.PopupMenuPopup(Sender: TObject);
begin
  RemoveClient1.Visible :=  VST.FocusedNode <> nil;
end;

procedure TFormMain.RemoveClient1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  ///
  VST.DeleteNode(VST.FocusedNode);
end;

procedure TFormMain.rustedCertificates1Click(Sender: TObject);
begin
  {$IFDEF USETLS}
  FormTrustedCertificates.Show();
  {$ENDIF}
end;

procedure TFormMain.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  var AColor := clNone;

  case pData^.Status of
    csConnected : AColor := COLOR_LIST_LIMY;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormMain.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if (not Assigned(pData1) or not Assigned(pData2)) then
    Result := 0
  else begin
    case Column of
      0 : Result := CompareText(pData1^.ClientConfiguration.Address, pData2^.ClientConfiguration.Address);
      1 : Result := CompareValue(pData1^.ClientConfiguration.Port, pData2^.ClientConfiguration.Port);
      2 : Result := CompareValue(Cardinal(pData1^.Status), Cardinal(pData2^.Status));
      3 : Result := CompareValue(Cardinal(pData1^.Status), Cardinal(pData2^.Status));
      4 : Result := CompareText(pData1^.ExtraDescription, pData2^.ExtraDescription);

      {$IFDEF USETLS}
      5 : Result := CompareText(
        pData1^.ClientConfiguration.CertificateFingerprint,
        pData2^.ClientConfiguration.CertificateFingerprint
      );
      {$ENDIF}
    end;
  end;
end;

procedure TFormMain.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  ///

  if Assigned(pData) and Assigned(pData^.Handler) then
    pData^.Handler.Terminate;
end;

procedure TFormMain.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  if (Column <> 0) or ((Kind <> TVTImageKind.ikNormal) and (Kind <> TVTImageKind.ikSelected)) then
    Exit();

  case pData^.Status of
    csDisconnected : ImageIndex := IMAGE_CLIENT_DISCONNECTED;
    csConnected    : ImageIndex := IMAGE_CLIENT_CONNECTED;
    csOnError      : ImageIndex := IMAGE_CLIENT_ERROR;
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
  ///

  CellText := '';

  if Assigned(pData) then begin
    case Column of
      0 : CellText := pData^.ClientConfiguration.Address;
      1 : CellText := pData^.ClientConfiguration.Port.ToString;
      2 : begin
        case pData^.Status of
          csDisconnected : CellText := 'Disconnected';
          csConnected    : CellText := 'Connected';
          csOnError      : CellText := 'Error';
        end;
      end;
      3 : begin
        case pData^.ClientConfiguration.Version of
          ipv4 : CellText := 'IPv4';
          ipv6 : CellText := 'IPv6';
        end;
      end;
      4 : CellText := pData^.ExtraDescription;
      {$IFDEF USETLS}
      5 : CellText := pData^.ClientConfiguration.CertificateFingerprint;
      {$ENDIF}
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TFormMain.VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TBaseVirtualTree(Sender).GetNodeAt(Point(X, Y)) = nil then begin
    TBaseVirtualTree(Sender).ClearSelection();

    TBaseVirtualTree(Sender).FocusedNode := nil;
  end;
end;

end.
