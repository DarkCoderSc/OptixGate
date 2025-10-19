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

(*
  When Delphi 13 in Community Edition (Code Improvements):
    - Replace "not (x in y)" by "x not in y"
    - Ternary operator
*)

unit uFormMain;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.ImageList, Generics.Collections, System.Types,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.StdCtrls, Vcl.ImageCollection, Vcl.ExtCtrls, Vcl.BaseImageCollection,

  Winapi.ShellAPI, Winapi.Messages, Winapi.Windows, Winapi.Winsock2,

  VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL, VirtualTrees, VirtualTrees.Types, VirtualTrees.BaseTree,
  XSuperObject,

  __uBaseFormControl__,

  Optix.Thread, Optix.Protocol.Preflight, Optix.Protocol.Server, Optix.Sockets.Helper, Optix.Func.SessionInformation,
  Optix.Protocol.SessionHandler, Optix.Func.Commands.Base, Optix.Func.Commands;
// ---------------------------------------------------------------------------------------------------------------------

type
  TTreeData = record
    Handler            : TOptixSessionHandlerThread;
    SessionInformation : TOptixSessionInformation;
    SpawnDate          : TDateTime;
    Forms              : TObjectList<TBaseFormControl>;
    Workers            : TObjectList<TOptixThread>;

    ///
    function ToString: String;
    function GetUPN() : String;
  end;
  PTreeData = ^TTreeData;

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    Server1: TMenuItem;
    About1: TMenuItem;
    File1: TMenuItem;
    Close1: TMenuItem;
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    ProcessManager1: TMenuItem;
    N1: TMenuItem;
    FileManager1: TMenuItem;
    RegistryManager1: TMenuItem;
    N3: TMenuItem;
    RemoteShell1: TMenuItem;
    N4: TMenuItem;
    erminate1: TMenuItem;
    TimerRefresh: TTimer;
    VirtualImageList: TVirtualImageList;
    N5: TMenuItem;
    transfers1: TMenuItem;
    Logs1: TMenuItem;
    ControlForms1: TMenuItem;
    ImageSystem: TImageList;
    Debug1: TMenuItem;
    hreads1: TMenuItem;
    asks1: TMenuItem;
    ImageCollectionDark: TImageCollection;
    Stores1: TMenuItem;
    Certificates1: TMenuItem;
    rustedCertificates1: TMenuItem;
    FileSystem1: TMenuItem;
    System1: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N2: TMenuItem;
    ContentReader1: TMenuItem;
    procedure Close1Click(Sender: TObject);
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
    procedure Logs1Click(Sender: TObject);
    procedure FileManager1Click(Sender: TObject);
    procedure ControlForms1Click(Sender: TObject);
    procedure transfers1Click(Sender: TObject);
    procedure hreads1Click(Sender: TObject);
    procedure asks1Click(Sender: TObject);
    procedure RemoteShell1Click(Sender: TObject);
    procedure Certificates1Click(Sender: TObject);
    procedure rustedCertificates1Click(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure Server1Click(Sender: TObject);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure RegistryManager1Click(Sender: TObject);
    procedure ContentReader1Click(Sender: TObject);
  private
    FFileInfo : TSHFileInfo;

    {@M}
    procedure RegisterSession(const AHandler : TOptixSessionHandlerThread; const ASessionInformation : TOptixSessionInformation);
    function GetNodeByHandler(const AHandler : TOptixSessionHandlerThread) : PVirtualNode;
    function GetHandlerByHandlerId(const AHandlerId : TGUID) : TOptixSessionHandlerThread;
    function GetNodeByHandlerId(const AHandlerId : TGUID) : PVirtualNode;
    function GetNodeByControlForm(const AForm : TBaseFormControl) : PVirtualNode;
    procedure CreateOrOpenControlForm(const pNode : PVirtualNode; const AFormClass : TBaseFormControlClass);
    function CreateNewControlForm(const pNode : PVirtualNode;
      const AFormClass : TBaseFormControlClass) : TBaseFormControl;
  public
    {@M}
    function GetControlForm(const pNode : PVirtualNode; const AClass : TClass) : TBaseFormControl; overload;
    function GetControlForm(const pNode : PVirtualNode; const AWindowGUID : TGUID) : TBaseFormControl; overload;
    function GetControlForm(const AControlForm : TBaseFormControl; const AClass : TClass) : TBaseFormControl; overload;
    function GetControlForms(const ASourceControlForm : TBaseFormControl; const AClassFamily : TClass) : TList<TBaseFormControl>;
    procedure PurgeControlForm(const AControlForm : TBaseFormControl; const AWindowGUID : TGUID);
    function ControlFormExists(const pNode : PVirtualNode; const AClass : TClass) : Boolean;
    procedure SendCommand(const pNode : PVirtualNode; const ACommand : TOptixCommand); overload;
    procedure SendCommand(const ACaller : TBaseFormControl; const ACommand : TOptixCommand); overload;

    {@ME}
    procedure OnSessionDisconnect(Sender : TOptixSessionHandlerThread);
    procedure OnReceivePacket(Sender : TOptixSessionHandlerThread; const ASerializedPacket : ISuperObject);
    procedure OnRegisterWorker(Sender : TOptixServerThread; const AClient : TClientSocket; const AHandlerId  : TGUID; const AWorkerKind : TClientKind);
  end;

var
  FormMain: TFormMain;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.DateUtils, System.Rtti,

  uControlFormProcessManager, uControlFormLogs, uFormAbout, uControlFormTransfers, uControlFormControlForms,
  uControlFormFileManager, uFormDebugThreads, uControlFormTasks, uControlFormRemoteShell, uFormListen, uFormServers,
  uControlFormRegistryManager, uControlFormSetupContentReader, uControlFormContentReader
  {$IFDEF USETLS}, uFormCertificatesStore, uFormTrustedCertificates{$ENDIF},

  Optix.Protocol.Packet, Optix.Helper, Optix.VCL.Helper, Optix.Constants, Optix.Process.Helper,
  Optix.Func.LogNotifier, Optix.Protocol.Worker.FileTransfer, Optix.ClassesRegistry, Optix.Func.Commands.ContentReader
  {$IFDEF USETLS}, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

(* TTreeData *)

{ TTreeData.ToString }
function TTreeData.ToString: String;
begin
  if not Assigned(SessionInformation) or not Assigned(Handler) then
    result := ''
  else
    result := Format('%s@%s:%s', [
      SessionInformation.Username,
      SessionInformation.Computer,
      Handler.PeerAddress
    ]);
end;

{ TTreeData.GetUPN }
function TTreeData.GetUPN() : String;
begin
  if Assigned(SessionInformation) then
    result := Format('%s@%s', [SessionInformation.Username, SessionInformation.Computer])
  else
    result := '';
end;

(* TFormMain *)

procedure TFormMain.PurgeControlForm(const AControlForm : TBaseFormControl; const AWindowGUID : TGUID);
begin
  var pNode := GetNodeByControlForm(AControlForm);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  if not Assigned(pData^.Forms) then
    Exit();

  var AForm := GetControlForm(pNode, AWindowGUID);
  if Assigned(AForm) then begin
    AForm.PurgeRequest();

    pData.Forms.Remove(AForm);
  end;
end;

function TFormMain.GetControlForm(const pNode : PVirtualNode; const AClass : TClass) : TBaseFormControl;
begin
  result := nil;
  ///

  if not Assigned(pNode) then
    Exit();

  var pData := PTreeData(pNode.GetData);

  if not Assigned(pData^.Forms) then
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

function TFormMain.GetControlForm(const AControlForm : TBaseFormControl; const AClass : TClass) : TBaseFormControl;
begin
  result := nil;
  ///

  if not Assigned(AControlForm) then
    Exit();

  var pNode := GetNodeByControlForm(AControlForm);
  if not Assigned(pNode) then
    Exit();

  ///
  result := GetControlForm(pNode, AClass);
end;

function TFormMain.GetControlForms(const ASourceControlForm : TBaseFormControl; const AClassFamily : TClass) : TList<TBaseFormControl>;
begin
  result := nil;
  ///

  if not Assigned(ASourceControlForm) then
    Exit();

  var pNode := GetNodeByControlForm(ASourceControlForm);
  if not Assigned(pNode) then
    Exit();

  var pData := PTreeData(pNode.GetData);
  if not Assigned(pData) or not Assigned(pData^.Forms) then
    Exit();

  result := TList<TBaseFormControl>.Create();

  for var AForm in pData^.Forms do begin
    if AForm is AClassFamily then
      result.Add(AForm);
  end;
end;

procedure TFormMain.ContentReader1Click(Sender: TObject);
begin
  CreateNewControlForm(VST.FocusedNode, TControlFormSetupContentReader);
end;

function TFormMain.ControlFormExists(const pNode : PVirtualNode; const AClass : TClass) : Boolean;
begin
  result := GetControlForm(pNode, AClass) <> nil;
end;

procedure TFormMain.ControlForms1Click(Sender: TObject);
begin
  CreateOrOpenControlForm(VST.FocusedNode, TControlFormControlForms);
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

procedure TFormMain.Server1Click(Sender: TObject);
begin
  FormServers.Show();
end;

procedure TFormMain.hreads1Click(Sender: TObject);
begin
  FormDebugThreads.Show();
end;

procedure TFormMain.CreateOrOpenControlForm(const pNode : PVirtualNode; const AFormClass : TBaseFormControlClass);
begin
  if not Assigned(pNode) then
    Exit();

  var AForm := GetControlForm(pNode, AFormClass);

  // Should always exists
  if not Assigned(AForm) then begin
    var pData := PTreeData(pNode.GetData);

    if AFormClass = TControlFormProcessManager then
      AForm := TControlFormProcessManager.Create(
        self,
        pData^.ToString,
        pData^.SessionInformation.Architecture,
        pData^.SessionInformation.WindowsArchitecture
      );

    ///
    if Assigned(AForm) then
      pData^.Forms.Add(AForm);
  end;

  if Assigned(AForm) then
    TOptixVCLHelper.ShowForm(AForm);
end;

function TFormMain.CreateNewControlForm(const pNode : PVirtualNode;
  const AFormClass : TBaseFormControlClass) : TBaseFormControl;
begin
  result := nil;
  ///

  if pNode = nil then
    Exit();

  var pData := PTreeData(pNode.GetData);
  if not Assigned(pData^.Forms) then
    Exit();

  var AForm := AFormClass.Create(self, pData^.ToString);
  pData^.Forms.Add(AForm);

  result := AForm;

  ///
  TOptixVCLHelper.ShowForm(AForm);
end;

procedure TFormMain.Logs1Click(Sender: TObject);
begin
  CreateOrOpenControlForm(VST.FocusedNode, TControlFormLogs);
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

  var pNode := VST.AddChild(nil);
  var pData := PTreeData(pNode.GetData);
  ///

  VST.BeginUpdate(); // Trigger sort. I haven't found a better method yet...
  try
    pData^.Handler := AHandler;
    pData^.SessionInformation := ASessionInformation;
    pData^.SpawnDate := Now;

    // Create not mandatory windows

    // -----------------------------------------------------------------------------------------------------------------
    pData^.Forms.Add(TControlFormLogs.Create(self, pData^.ToString, True));
    // -----------------------------------------------------------------------------------------------------------------
    pData^.Forms.Add(TControlFormControlForms.Create(self, pData^.ToString, pData));
    // -----------------------------------------------------------------------------------------------------------------
    pData^.Forms.Add(TControlFormTransfers.Create(self, pData^.ToString, True));
    // -----------------------------------------------------------------------------------------------------------------
    pData^.Forms.Add(TControlFormTasks.Create(self, pData^.ToString, True));
    // -----------------------------------------------------------------------------------------------------------------
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormMain.RegistryManager1Click(Sender: TObject);
begin
  CreateNewControlForm(VST.FocusedNode, TControlFormRegistryManager);
end;

procedure TFormMain.RemoteShell1Click(Sender: TObject);
begin
  CreateNewControlForm(VST.FocusedNode, TControlFormRemoteShell);
end;

procedure TFormMain.rustedCertificates1Click(Sender: TObject);
begin
  {$IFDEF USETLS}
  FormTrustedCertificates.Show();
  {$ENDIF}
end;

procedure TFormMain.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) or not Assigned(pData^.SessionInformation) then
    Exit();
  ///

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

  if Assigned(pData^.Handler) then begin
    TOptixThread.TerminateInstance(pData^.Handler);

    ///
    pData^.Handler := nil;
  end;

  if Assigned(pData^.Workers) then begin
    for var AWorker in pData^.Workers do
      TOptixThread.TerminateInstance(AWorker);

    ///
    FreeAndNil(pData^.Workers);
  end;
end;

procedure TFormMain.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) or (Column <> 0) then
    Exit();
  ///

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

procedure TFormMain.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then begin
    Result := 0;

    Exit();
  end;

  if not Assigned(pData1^.SessionInformation) or not Assigned(pData2^.SessionInformation) then
    Result := CompareObjectAssignement(pData1^.SessionInformation, pData2^.SessionInformation)
  else if not Assigned(pData1^.Handler) or not Assigned(pData2^.Handler) then
    Result := CompareObjectAssignement(pData1^.Handler, pData2^.Handler)
  else begin
    case Column of
      0 : Result := CompareText(pData1^.Handler.PeerAddress, pData2^.Handler.PeerAddress);
      1 : Result := CompareText(pData1^.GetUPN(), pData2^.GetUPN());
      2 : Result := CompareText(pData1^.SessionInformation.Langroup, pData2^.SessionInformation.Langroup);
      3 : Result := CompareText(pData1^.SessionInformation.DomainName, pData2^.SessionInformation.DomainName);
      4 : Result := CompareText(pData1^.SessionInformation.WindowsVersion, pData2^.SessionInformation.WindowsVersion);
      5 : Result := CompareDateTime(pData1^.SpawnDate, pData2^.SpawnDate);
      6 : Result := CompareText(pData1^.SessionInformation.ProcessDetail, pData2^.SessionInformation.ProcessDetail);
      7 : Result := CompareText(
        pData1^.SessionInformation.ElevatedStatus_STR, pData2^.SessionInformation.ElevatedStatus_STR
      );
      8 : Result := Ord(pData1^.SessionInformation.IsInAdminGroup) - Ord(pData2^.SessionInformation.IsInAdminGroup);
    end;
  end;
end;

procedure TFormMain.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);
  ///

  CellText := '';

  if Assigned(pData) and Assigned(pData^.SessionInformation) and Assigned(pData^.Handler) then begin
    case Column of
      0 : CellText := pData^.Handler.PeerAddress;
      1 : CellText := pData^.GetUPN;
      2 : CellText := DefaultIfEmpty(pData^.SessionInformation.Langroup);
      3 : CellText := DefaultIfEmpty(pData^.SessionInformation.DomainName);
      4 : CellText := pData^.SessionInformation.WindowsVersion;
      5 : CellText := ElapsedDateTime(pData^.SpawnDate, Now);
      6 : CellText := pData^.SessionInformation.ProcessDetail;
      7 : CellText := pData^.SessionInformation.ElevatedStatus_STR;
      8 : CellText := BoolToStr(pData^.SessionInformation.IsInAdminGroup, True);
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TFormMain.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  var pData := PTreeData(Node.GetData);
  ///

  pData^.Forms := TObjectList<TBaseFormControl>.Create(True);
  pData^.Workers := TObjectList<TOptixThread>.Create(False);
end;

procedure TFormMain.VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TBaseVirtualTree(Sender).GetNodeAt(Point(X, Y)) = nil then begin
    TBaseVirtualTree(Sender).ClearSelection();

    TBaseVirtualTree(Sender).FocusedNode := nil;
  end;
end;

procedure TFormMain.OnSessionDisconnect(Sender : TOptixSessionHandlerThread);
begin
  var pNode := GetNodeByHandler(Sender);
  if not Assigned(pNode) then
    Exit();

  VST.DeleteNode(pNode);
end;

procedure TFormMain.PopupMenuPopup(Sender: TObject);
begin
  TOptixVCLHelper.HideAllPopupMenuRootItems(TPopupMenu(Sender));

  var AVisible := VST.FocusedNode <> nil;

  erminate1.Visible        := AVisible;
  System1.Visible          := AVisible;
  FileSystem1.Visible      := AVisible;
  Logs1.Visible            := AVisible;
  ControlForms1.Visible    := AVisible;
  transfers1.Visible       := AVisible;
  asks1.Visible            := AVisible;
  RemoteShell1.Visible     := AVisible;
end;

procedure TFormMain.ProcessManager1Click(Sender: TObject);
begin
  CreateOrOpenControlForm(VST.FocusedNode, TControlFormProcessManager);
end;

procedure TFormMain.transfers1Click(Sender: TObject);
begin
  CreateOrOpenControlForm(VST.FocusedNode, TControlFormTransfers);
end;

procedure TFormMain.OnReceivePacket(Sender : TOptixSessionHandlerThread; const ASerializedPacket : ISuperObject);
begin
  if not Assigned(ASerializedPacket) or
     not ASerializedPacket.Contains('PacketClass') or
     not ASerializedPacket.Contains('FWindowGUID') then
      Exit();
  ///

  var AClassName := ASerializedPacket.S['PacketClass'];
  var AWindowGUID := TGUID.Create(ASerializedPacket.S['FWindowGUID']);

  var AOptixPacket : TOptixPacket := nil;
  var AHandleMemory := False;
  try
    try
      AOptixPacket := TOptixPacket(TClassesRegistry.CreateInstance(AClassName, [
        TValue.From<ISuperObject>(ASerializedPacket)
      ]));
      if not Assigned(AOptixPacket) then
        raise Exception.Create(Format('Unknown or Unregistered Packet Class=[%s]', [AClassName]));
      ///

      if AOptixPacket is TOptixSessionInformation then begin
        AHandleMemory := True;

        ///
        RegisterSession(Sender, TOptixSessionInformation(AOptixPacket));
      end else begin
        var pNode := GetNodeByHandler(Sender);
        if not Assigned(pNode) then
          Exit();
        ///

        // -------------------------------------------------------------------------------------------------------------
        if AOptixPacket is TOptixCommandContentReaderFirstPage then begin
          var AForm := CreateNewControlForm(pNode, TControlFormContentReader);

          ///
          AForm.OverrideWindowGUID(AOptixPacket.WindowGUID);
        end;
        // -------------------------------------------------------------------------------------------------------------

        if AWindowGUID.IsEmpty then begin
          // -----------------------------------------------------------------------------------------------------------
          if (AOptixPacket is TLogNotifier) or (AOptixPacket is TLogTransferException) then begin
            var ALogsForm := GetControlForm(pNode, TControlFormLogs);
            if Assigned(ALogsForm) then
              ALogsForm.ReceivePacket(AOptixPacket, AHandleMemory);
          // -----------------------------------------------------------------------------------------------------------
          end else if AOptixPacket is TOptixTaskCallback then begin
            var ATaskForm := GetControlForm(pNode, TControlFormTasks);
            if Assigned(ATaskForm) then
              ATaskForm.ReceivePacket(AOptixPacket, AHandleMemory);
          end;
          // -----------------------------------------------------------------------------------------------------------
        end else begin
          var AControlForm := GetControlForm(pNode, AWindowGUID);
          if Assigned(AControlForm) then
            AControlForm.ReceivePacket(AOptixPacket, AHandleMemory);
        end;
      end;
    finally
      if not AHandleMemory and Assigned(AOptixPacket) then
        FreeAndNil(AOptixPacket);
    end;
  except
    on E : Exception do begin
      // TODO: log packet errors
    end;
  end;
end;

function TFormMain.GetHandlerByHandlerId(const AHandlerId : TGUID) : TOptixSessionHandlerThread;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if Assigned(pData^.Handler) and (pData^.Handler.HandlerId = AHandlerId) then begin
      result := pData^.Handler;

      break;
    end;
  end;
end;

function TFormMain.GetNodeByHandlerId(const AHandlerId : TGUID) : PVirtualNode;
begin
  result := nil;
  ///

  var AHandler := GetHandlerByHandlerId(AHandlerId);
  if Assigned(AHandler) then
    result := GetNodeByHandler(AHandler);
end;

procedure TFormMain.OnRegisterWorker(Sender : TOptixServerThread; const AClient : TClientSocket; const AHandlerId  : TGUID; const AWorkerKind : TClientKind);
begin
  var pNode := GetNodeByHandlerId(AHandlerId);
  if not Assigned(pNode) then
    FreeAndNil(AClient)
  else begin
    var pData := PTreeData(pNode.GetData);
    ///

    var AWorker : TOptixThread := nil;

    case AWorkerKind of
      // File Transfer Worker ------------------------------------------------------------------------------------------
      ckFileTransfer : begin
        AWorker := TOptixFileTransferWorker.Create(AClient);

        var AForm := TControlFormTransfers(GetControlForm(pNode, TControlFormTransfers));
        if Assigned(AWorker) then begin
          TOptixFileTransferWorker(AWorker).OnRequestTransferTask := AForm.OnRequestTransferTask;
          TOptixFileTransferWorker(AWorker).OnTransferError       := AForm.OnTransferError;
          TOptixFileTransferWorker(AWorker).OnTransferBegins      := AForm.OnTransferBegins;
          TOptixFileTransferWorker(AWorker).OnTransferUpdate      := AForm.OnTransferUpdate;
          TOptixFileTransferWorker(AWorker).OnTransferEnds        := AForm.OnTransferEnds;
        end;
      // Unknown -------------------------------------------------------------------------------------------------------
      end else
        FreeAndNil(AClient);
    end;

    if Assigned(AWorker) then begin
      pData^.Workers.Add(AWorker);

      ///
      AWorker.Start();
    end;
  end;
end;

procedure TFormMain.About1Click(Sender: TObject);
begin
  FormAbout.ShowModal();
end;

procedure TFormMain.asks1Click(Sender: TObject);
begin
  CreateOrOpenControlForm(VST.FocusedNode, TControlFormTasks);
end;

procedure TFormMain.Certificates1Click(Sender: TObject);
begin
  {$IFDEF USETLS}
  FormCertificatesStore.Show();
  {$ENDIF}
end;

procedure TFormMain.Close1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.erminate1Click(Sender: TObject);
begin
  SendCommand(VST.FocusedNode, TOptixCommandTerminate.Create());
end;

procedure TFormMain.FileManager1Click(Sender: TObject);
begin
  CreateNewControlForm(VST.FocusedNode, TControlFormFileManager);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TOptixThread.SignalHiveAndFlush();
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeSystemIcons(ImageSystem, FFileInfo);
  ///

  Caption := Format('%s - %s', [Caption, OPTIX_PROTOCOL_VERSION]);

  {$IFNDEF USETLS}
  Stores1.Visible := False;
  Certificates1.Visible := False;
  rustedCertificates1.Visible := False;
  {$ENDIF}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin

end;

//procedure TFormMain.StopServer();
//begin
//  if Assigned(FServer) then begin
//    TOptixThread.TerminateWait(FServer);
//
//    ///
//    FServer := nil;
//  end;
//end;

procedure TFormMain.TimerRefreshTimer(Sender: TObject);
begin
  VST.Refresh();
end;

end.
