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

unit uFormServers;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.Types,

  Winapi.Windows, Winapi.Messages, Winapi.Winsock2,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,

  VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees.Types,

  Optix.Protocol.Server, Optix.Sockets.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TServerStatus = (
    ssStopped,
    ssListening,
    ssOnError
  );

  TServerConfiguration = record
    Address   : String;
    Port      : Word;
    Version   : TIpVersion;
    AutoStart : Boolean;

    {$IFDEF USETLS}
    CertificateFingerprint : String;
    {$ENDIF}
  end;

  TTreeData = record
    ServerConfiguration : TServerConfiguration;

    Status              : TServerStatus;
    StatusMessage       : String;
    StartDateTime       : TDateTime;
    Server              : TOptixServerThread;
  end;
  PTreeData = ^TTreeData;

  TFormServers = class(TForm)
    VST: TVirtualStringTree;
    MainMenu: TMainMenu;
    Server1: TMenuItem;
    New1: TMenuItem;
    PopupMenu: TPopupMenu;
    Remove1: TMenuItem;
    Start1: TMenuItem;
    N1: TMenuItem;
    AutoStart1: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure New1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure PopupMenuPopup(Sender: TObject);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure AutoStart1Click(Sender: TObject);
  private
    {@M}
    function GetNodeByPort(const APort : Word; const AVersion : TIPVersion) : PVirtualNode;
    function GetNodeByServer(const AServer : TOptixServerThread) : PVirtualNode;
    {$IFDEF USETLS}
    function GetNodeByServerFingerprint(const AFingerprint : String) : PVirtualNode;
    {$ENDIF}
    procedure UpdateStatus(const pNode : PVirtualNode; const AStatus : TServerStatus; const AStatusMessage : String = ''); overload;
    procedure UpdateStatus(const AServer : TOptixServerThread; const AStatus : TServerStatus; const AStatusMessage : String = ''); overload;
    procedure RegisterServer(const AServerConfiguration : TServerConfiguration);
    procedure StartServer(const pNode : PVirtualNode);

    procedure OnServerStart(Sender : TOptixServerThread; const ASocketFd : TSocket);
    procedure OnServerStop(Sender : TOptixServerThread);
    procedure OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);

    procedure Save();
    procedure Load();
  public
    {$IFDEF USETLS}
    {@M}
    function ServerCertificateIsInUse(const AFingerprint : String) : Boolean;
    {$ENDIF}
  end;

var
  FormServers: TFormServers;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math, System.DateUtils,

  uFormMain, uFormListen

  {$IFDEF USETLS}, uFormCertificatesStore, uFormTrustedCertificates{$ENDIF},

  Optix.Helper, Optix.Constants, Optix.Config.Servers, Optix.Config.Helper
  {$IFDEF USETLS}, Optix.OpenSSL.Helper{$ENDIF}

  {$IF Defined(DEBUG) and Defined(USETLS)}, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

(* TFormServers *)

procedure TFormServers.Save();
begin
  var AConfig := TOptixConfigServers.Create();
  try
    for var pNode in VST.Nodes do begin
      var pData := PTreeData(pNode.GetData);

      ///
      AConfig.Add(pData^.ServerConfiguration);
    end;
  finally
    CONFIG_HELPER.Write('Servers'{$IFDEF USETLS}+ '+OpenSSL'{$ENDIF}, AConfig);

    ///
    FreeAndNil(AConfig);
  end;
end;

procedure TFormServers.Load();
begin
  VST.Clear();
  ///

  var AConfig := TOptixConfigServers(CONFIG_HELPER.Read('Servers'{$IFDEF USETLS}+ '+OpenSSL'{$ENDIF}));
  if not Assigned(AConfig) then
    Exit();
  try
    VST.BeginUpdate();
    try
      for var I := 0 to AConfig.Count -1 do begin
        var AServerConfiguration := AConfig.Items[I];
        if String.IsNullOrWhitespace(AServerConfiguration.Address) then
          continue;


        ///
        RegisterServer(AServerConfiguration);
      end;
    finally
      VST.EndUpdate();
    end;
  finally
    FreeAndNil(AConfig);
  end;
end;

procedure TFormServers.UpdateStatus(const pNode : PVirtualNode; const AStatus : TServerStatus; const AStatusMessage : String = '');
begin
  if not Assigned(pNode) then
    Exit();

  var pData := PTreeData(pNode.GetData);

  VST.BeginUpdate();
  try
    pData^.Status        := AStatus;
    pData^.StatusMessage := AStatusMessage;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServers.UpdateStatus(const AServer : TOptixServerThread; const AStatus : TServerStatus; const AStatusMessage : String = '');
begin
  var pNode := GetNodeByServer(AServer);

  ///
  UpdateStatus(pNode, AStatus, AStatusMessage);
end;

procedure TFormServers.OnServerStart(Sender : TOptixServerThread; const ASocketFd : TSocket);
begin
  UpdateStatus(Sender, ssListening);
end;

procedure TFormServers.OnServerStop(Sender : TOptixServerThread);
begin
  var pNode := GetNodeByServer(Sender);
  if not Assigned(pNode) then
    Exit();

  var pData := PTreeData(pNode.GetData);

  if pData^.Status <> ssOnError then
    UpdateStatus(Sender, ssStopped);

  pData^.Server := nil;
end;

procedure TFormServers.PopupMenuPopup(Sender: TObject);
begin
  var pData := PTreeData(nil);

  var pNode := VST.FocusedNode;
  if Assigned(pNode) then
    pData := pNode.GetData;

  Start1.Visible := Assigned(pData);

  if Assigned(pData) then
    case pData^.Status of
      ssStopped, ssOnError:
        Start1.Caption := 'Start';

      ssListening:
        Start1.Caption := 'Stop';
    end;

  Remove1.Visible    := Assigned(pData);
  AutoStart1.Visible := Assigned(pData);

  if AutoStart1.Visible then
    AutoStart1.Checked := pData^.ServerConfiguration.AutoStart;
end;

procedure TFormServers.OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);
begin
  UpdateStatus(Sender, ssOnError, AErrorMessage);
end;

procedure TFormServers.AutoStart1Click(Sender: TObject);
begin
  var pNode := VST.FocusedNode;
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  VST.BeginUpdate();
  try
    pData^.ServerConfiguration.AutoStart := TMenuItem(Sender).Checked;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServers.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Save();
end;

procedure TFormServers.FormCreate(Sender: TObject);
begin
  {$IFNDEF USETLS}
  VST.Header.Columns[4].Options := VST.Header.Columns[4].Options - [coVisible];
  {$ENDIF}

  ///
  Load();
end;

procedure TFormServers.FormDestroy(Sender: TObject);
begin
  VST.Clear();
end;

function TFormServers.GetNodeByPort(const APort : Word; const AVersion : TIPVersion) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if (pData^.ServerConfiguration.Port = APort) and (pData^.ServerConfiguration.Version = AVersion) then begin
      result := pNode;

      break;
    end;
  end;
end;

function TFormServers.GetNodeByServer(const AServer : TOptixServerThread) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if Assigned(pData^.Server) and (pData^.Server = AServer) then begin
      result := pNode;

      break;
    end;
  end;
end;

{$IFDEF USETLS}
function TFormServers.GetNodeByServerFingerprint(const AFingerprint : String) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if String.Compare(pData^.ServerConfiguration.CertificateFingerprint, AFingerprint, True) = 0 then begin
      result := pNode;

      break;
    end;
  end;
end;

function TFormServers.ServerCertificateIsInUse(const AFingerprint : String) : Boolean;
begin
  result := GetNodeByServerFingerprint(AFingerprint) <> nil;
end;
{$ENDIF}

procedure TFormServers.RegisterServer(const AServerConfiguration : TServerConfiguration);
begin
  if GetNodeByPort(AServerConfiguration.Port, AServerConfiguration.Version) <> nil then
    raise Exception.Create('The specified port is already present in the server list. A port can only be bound and ' +
                           'listened to by one server instance at a time.');
  ///

  VST.BeginUpdate();
  try
    var pNode := VST.AddChild(nil);
    var pData := PTreeData(pNode.GetData);
    ///

    pData^.ServerConfiguration := AServerConfiguration;
    pData^.Status              := ssStopped;
    pData^.StatusMessage       := '';
    pData^.StartDateTime       := Now;

    if AServerConfiguration.AutoStart then
      StartServer(pNode)
    else
      pData^.Server := nil;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServers.Remove1Click(Sender: TObject);
begin
  var pNode := VST.FocusedNode;
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  if Assigned(pData^.Server) then
    pData^.Server.Terminate;

  ///
  VST.DeleteNode(pNode);
end;

procedure TFormServers.Start1Click(Sender: TObject);
begin
  var pNode := VST.FocusedNode;
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  case pData^.Status of
    ssStopped, ssOnError:
      StartServer(pNode);

    ssListening:
      if Assigned(pData^.Server) then
        pData^.Server.Terminate;
  end;
end;

procedure TFormServers.StartServer(const pNode : PVirtualNode);
begin
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);
  if not Assigned(pData) then
    Exit();

  {$IFDEF USETLS}
    var ACertificate : TX509Certificate;

    if not FormCertificatesStore.GetCertificateKeys(pData^.ServerConfiguration.CertificateFingerprint, ACertificate)
    then begin
      Application.MessageBox(
        'Server certificate fingerprint does not exist in the store. Please import an existing certificate first or ' +
        'generate a new one.',
        'Start Server',
        MB_ICONERROR
      );

      Exit();
    end;
  {$ENDIF}

  pData^.Server := TOptixServerThread.Create(
    {$IFDEF USETLS}
    ACertificate,
    {$ENDIF}
    pData^.ServerConfiguration.Address,
    pData^.ServerConfiguration.Port,
    pData^.ServerConfiguration.Version
  );

  pData^.Server.OnServerStart       := OnServerStart;
  pData^.Server.OnServerError       := OnServerError;
  pData^.Server.OnServerStop        := OnServerStop;

  pData^.Server.OnSessionDisconnect := FormMain.OnSessionDisconnect;
  pData^.Server.OnReceivePacket     := FormMain.OnReceivePacket;
  pData^.Server.OnRegisterWorker    := FormMain.OnRegisterWorker;

  {$IFDEF USETLS}
  pData^.Server.OnVerifyPeerCertificate := FormTrustedCertificates.OnVerifyPeerCertificate;
  {$ENDIF}

  ///
  pData^.Server.Start();
end;

procedure TFormServers.New1Click(Sender: TObject);
begin
  var AForm : TFormListen;

  {$IFDEF USETLS}
  var AFingerprints := FormCertificatesStore.GetCertificatesFingerprints();
  try
    if AFingerprints.Count = 0 then
      raise Exception.Create('No existing certificate was found in the certificate store. You cannot start ' +
                             'listening for clients without registering at least one certificate.');
    ///

//  if FormTrustedCertificates.TrustedCertificateCount = 0 then
//    raise Exception.Create('No trusted certificate (fingerprint) was found in the trusted certificate ' +
//                           'store. You cannot start listening for clients without registering at least one ' +
//                           'trusted certificate. A trusted certificate represents the fingerprint of a ' +
//                           'client certificate and is required for mutual authentication, ensuring that ' +
//                           'network communications are secure and not tampered with or eavesdropped on.');

    AForm := TFormListen.Create(self, AFingerprints);
  finally
    FreeAndNil(AFingerprints);
  end;
{$ELSE}
  AForm := TFormListen.Create(self);
{$ENDIF}
  try
    AForm.ShowModal();
    if AForm.Canceled then
      Exit();

    ///
    RegisterServer(AForm.GetServerConfiguration());
  finally
    FreeAndNil(AForm);
  end;
end;

procedure TFormServers.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  var AColor := clNone;

  case pData^.Status of
    ssListening : AColor := COLOR_LIST_LIMY;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormServers.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormServers.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := 0
  else begin
    case Column of
      0 : Result := CompareText(pData1^.ServerConfiguration.Address, pData2^.ServerConfiguration.Address);
      1 : Result := CompareValue(pData1^.ServerConfiguration.Port, pData2^.ServerConfiguration.Port);
      2 : Result := CompareValue(
        Cardinal(pData1^.ServerConfiguration.Version), Cardinal(pData2^.ServerConfiguration.Version)
      );
      3 : Result := CompareValue(Cardinal(pData1^.Status), Cardinal(pData2^.Status));
      4 : Result := Ord(pData1^.ServerConfiguration.AutoStart) - Ord(pData2^.ServerConfiguration.AutoStart);
      {$IFDEF USETLS}
      5 : Result := CompareText(
        pData1^.ServerConfiguration.CertificateFingerprint,
        pData2^.ServerConfiguration.CertificateFingerprint
      );
      {$ENDIF}
      6 : Result := CompareText(pData1^.StatusMessage, pData2^.StatusMessage);
    end;
  end;
end;

procedure TFormServers.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormServers.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) or (Column <> 0) then
    Exit();
  ///

  case Kind of
    ikNormal, ikSelected : begin
      case pData^.Status of
        ssStopped   : ImageIndex := IMAGE_SERVER_STOPPED;
        ssListening : ImageIndex := IMAGE_SERVER_LISTENING;
        ssOnError   : ImageIndex := IMAGE_SERVER_ERROR;
      end;
    end;

    ikState: ;
    ikOverlay: ;
  end;
end;

procedure TFormServers.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormServers.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);
  ///

  CellText := '';

  if Assigned(pData) then begin
    case Column of
      0 : CellText := pData^.ServerConfiguration.Address;
      1 : CellText := pData^.ServerConfiguration.Port.ToString;

      2 : begin
        case pData^.ServerConfiguration.Version of
          ipv4 : CellText := 'IPv4';
          ipv6 : CellText := 'IPv6';
        end;
      end;

      3 : begin
        case pData^.Status of
          ssStopped   : CellText := 'Stopped';
          ssListening : CellText := 'Listening';
          ssOnError   : CellText := 'Error';
        end;
      end;

      4 : CellText := BoolToStr(pData^.ServerConfiguration.AutoStart, True);

      {$IFDEF USETLS}
      5 : CellText := pData^.ServerConfiguration.CertificateFingerprint;
      {$ENDIF}

      6 : CellText := pData^.StatusMessage;
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TFormServers.VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TBaseVirtualTree(Sender).GetNodeAt(Point(X, Y)) = nil then begin
    TBaseVirtualTree(Sender).ClearSelection();

    TBaseVirtualTree(Sender).FocusedNode := nil;
  end;
end;

end.
