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
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages, Winapi.Winsock2,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,

  VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees.Types,

  Optix.Protocol.Server;
// ---------------------------------------------------------------------------------------------------------------------

type
  TIpVersion = (
    ipv4,
    ipv6
  );

  TServerStatus = (
    ssStopped,
    ssListening,
    ssOnError
  );

  TServerConfiguration = record
    Address : String;
    Port    : Word;
    Version : TIpVersion;

    {$IFDEF USETLS}
    CertificateFingerprint : String;
    {$ENDIF}
  end;

  TTreeData = record
    Address           : String;
    Port              : Word;
    IpVersion         : TIpVersion;
    Status            : TServerStatus;
    {$IFDEF USETLS}
    ServerCertificate : String;
    {$ENDIF}
    StatusMessage     : String;
    StartDateTime     : TDateTime;
    Server            : TOptixServerThread;
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
  private
    {@M}
    function GetNodeByPort(const APort : Word) : PVirtualNode;
    function GetNodeByServer(const AServer : TOptixServerThread) : PVirtualNode;
    {$IFDEF USETLS}
    function GetNodeByServerFingerprint(const AFingerprint : String) : PVirtualNode;
    {$ENDIF}
    procedure UpdateStatus(const pNode : PVirtualNode; const AStatus : TServerStatus; const AStatusMessage : String = ''); overload;
    procedure UpdateStatus(const AServer : TOptixServerThread; const AStatus : TServerStatus; const AStatusMessage : String = ''); overload;
    procedure RegisterServer(const AServerConfiguration : TServerConfiguration; const AStart : Boolean);
    procedure StartServer(const pNode : PVirtualNode);

    procedure OnServerStart(Sender : TOptixServerThread; const ASocketFd : TSocket);
    procedure OnServerStop(Sender : TOptixServerThread);
    procedure OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);
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
  uFormMain, uFormListen

  {$IFDEF USETLS}, uFormCertificatesStore, uFormTrustedCertificates{$ENDIF},

  Optix.Helper

  {$IFDEF USETLS}, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

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
  UpdateStatus(Sender, ssStopped);
end;

procedure TFormServers.OnServerError(Sender : TOptixServerThread; const AErrorMessage : String);
begin
  UpdateStatus(Sender, ssOnError, AErrorMessage);
end;

procedure TFormServers.FormCreate(Sender: TObject);
begin
  {$IFNDEF USETLS}
  VST.Header.Columns[4].Options := VST.Header.Columns[4].Options - [coVisible];
  {$ENDIF}
end;

procedure TFormServers.FormDestroy(Sender: TObject);
begin
  VST.Clear();
end;

function TFormServers.GetNodeByPort(const APort : Word) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if pData^.Port = APort then begin
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
    if String.Compare(pData^.ServerCertificate, AFingerprint, True) = 0 then begin
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

procedure TFormServers.RegisterServer(const AServerConfiguration : TServerConfiguration; const AStart : Boolean);
begin
  if GetNodeByPort(AServerConfiguration.Port) <> nil then
    raise Exception.Create('The specified port is already present in the server list. A port can only be bound and ' +
                           'listened to by one server instance at a time.');
  ///

  VST.BeginUpdate();
  try
    var pNode := VST.AddChild(nil);
    var pData := PTreeData(pNode.GetData);
    ///

    pData^.Address       := AServerConfiguration.Address;
    pData^.Port          := AServerConfiguration.Port;
    pData^.IpVersion     := ipv4;
    pData^.Status        := ssStopped;
    pData^.StatusMessage := '';
    pData^.StartDateTime := Now;

    {$IFDEF USETLS}
    pData^.ServerCertificate := AServerConfiguration.CertificateFingerprint;
    {$ENDIF}

    if AStart then
      StartServer(pNode)
    else
      pData^.Server := nil;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServers.Remove1Click(Sender: TObject);
begin
  ///
end;

procedure TFormServers.Start1Click(Sender: TObject);
begin
  ///
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
    var APublicKey  : String;
    var APrivateKey : String;

    {$IFDEF DEBUG}
      APublicKey  := DEBUG_CERTIFICATE_PUBLIC_KEY;
      APrivateKey := DEBUG_CERTIFICATE_PRIVATE_KEY;
    {$ELSE}
      if not FormCertificatesStore.GetCertificateKeys(pData^.ServerCertificate, APublicKey, APrivateKey) then begin
        Application.MessageBox(
          'Server certificate fingerprint does not exist in the store. Please import an existing certificate first or ' +
          'generate a new one.',
          'Start Server',
          MB_ICONERROR
        );

        Exit();
      end;
    {$ENDIF}
  {$ENDIF}

  pData^.Server := TOptixServerThread.Create(
    {$IFDEF USETLS}
    APublicKey,
    APrivateKey,
    {$ENDIF}
    pData^.Address,
    pData^.Port
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
var AForm : TFormListen;
begin
  {$IFDEF DEBUG}
    var ADebugServerConfiguration : TServerConfiguration;

    ADebugServerConfiguration.Address := '0.0.0.0';
    ADebugServerConfiguration.Port    := 2801;
    ADebugServerConfiguration.Version := ipv4;
    {$IFDEF USETLS}
    ADebugServerConfiguration.CertificateFingerprint := DEBUG_CERTIFICATE_FINGERPRINT;
    {$ENDIF}

    StartServer(ADebugServerConfiguration);
  {$ELSE}
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
      RegisterServer(AForm.GetServerConfiguration(), True);
    finally
      FreeAndNil(AForm);
    end;
  {$ENDIF}
end;

procedure TFormServers.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormServers.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
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
      0 : CellText := pData^.Address;
      1 : CellText := pData^.Port.ToString;

      2 : begin
        case pData^.IpVersion of
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

      {$IFDEF USETLS}
      4 : CellText := pData^.ServerCertificate;
      {$ENDIF}

      5 : CellText := pData^.StatusMessage;
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

end.
