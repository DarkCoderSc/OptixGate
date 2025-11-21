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


unit uFormCertificatesStore;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.Types,

  Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,

  VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees.Types,

  Optix.OpenSSL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TTreeData = record
    Certificate : TX509Certificate;
  end;
  PTreeData = ^TTreeData;

  TFormCertificatesStore = class(TForm)
    VST: TVirtualStringTree;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    GeneratenewCertificate1: TMenuItem;
    Import1: TMenuItem;
    OD: TOpenDialog;
    SD: TSaveDialog;
    PopupMenu: TPopupMenu;
    ExportCertificate1: TMenuItem;
    ExportPublicKey1: TMenuItem;
    ExportPrivateKey1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    RemoveCertificate1: TMenuItem;
    CopySelectedFingerprint1: TMenuItem;
    N3: TMenuItem;
    procedure GeneratenewCertificate1Click(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Import1Click(Sender: TObject);
    function GetCertificateFromNode(const pNode : PVirtualNode) : PX509Certificate;
    procedure PopupMenuPopup(Sender: TObject);
    procedure ExportPublicKey1Click(Sender: TObject);
    procedure ExportCertificate1Click(Sender: TObject);
    procedure ExportPrivateKey1Click(Sender: TObject);
    procedure RemoveCertificate1Click(Sender: TObject);
    procedure CopySelectedFingerprint1Click(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    {@M}
    function GetNodeByFingerprint(const AFingerPrint : String) : PVirtualNode;
    procedure ExportCertificate(const pNode : PVirtualNode; const AExportWhich : TOpenSSLCertificateKeyTypes = []);
    procedure RegisterCertificate(const ACertificate : TX509Certificate);
    function GetCertificateCount() : Integer;
    procedure Save();
    procedure Load();
  public
    {@M}
    function GetCertificateKeys(const AFingerPrint : String; var ACertificate : TX509Certificate) : Boolean;
    function GetCertificatesFingerprints() : TList<String>;

    {@G}
    property CertificateCount : Integer read GetCertificateCount;
  end;

var
  FormCertificatesStore: TFormCertificatesStore;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  VCL.Clipbrd,

  uFormGenerateNewCertificate

  {$IFDEF SERVER}, uFormServers{$ENDIF},

  Optix.OpenSSL.Headers, Optix.Helper, Optix.Constants, Optix.Config.CertificatesStore, Optix.Config.Helper,
  Optix.VCL.Helper, Optix.OpenSSL.Exceptions

  {$IFDEF DEBUG}, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

function TFormCertificatesStore.GetCertificateCount() : Integer;
begin
  result := VST.RootNodeCount;
end;

function TFormCertificatesStore.GetCertificatesFingerprints() : TList<String>;
begin
  result := TList<String>.Create();
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);

    ///
    result.Add(pData^.Certificate.Fingerprint);
  end;
end;

function TFormCertificatesStore.GetNodeByFingerprint(const AFingerPrint : String) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);

    if String.Compare(pData^.Certificate.Fingerprint, AFingerPrint, True) = 0 then begin
      result := pNode;

      break;
    end;
  end;
end;

function TFormCertificatesStore.GetCertificateKeys(const AFingerPrint : String; var ACertificate : TX509Certificate) : Boolean;
begin
  result := False;

  var pNode := GetNodeByFingerprint(AFingerPrint);
  if not Assigned(pNode) then
    Exit();

  var pData := PTreeData(pNode.GetData);
  TOptixOpenSSLHelper.CopyCertificate(pData^.Certificate, ACertificate);

  ///
  result := True;
end;

procedure TFormCertificatesStore.Save();
begin
  var AConfig := TOptixConfigCertificatesStore.Create();
  try
    for var pNode in VST.Nodes do begin
      var pData := PTreeData(pNode.GetData);

      {$IFDEF DEBUG}
      if String.Compare(pData^.Certificate.Fingerprint, DEBUG_CERTIFICATE_FINGERPRINT, True) = 0 then
        continue;
      {$ENDIF}

      AConfig.Add(pData^.Certificate);
    end;
  finally
    CONFIG_HELPER.Write('Certificates', AConfig);

    ///
    FreeAndNil(AConfig);
  end;
end;

procedure TFormCertificatesStore.Load();
var ACertificate : TX509Certificate;
begin
  VST.Clear();
  ///

  {$IFDEF DEBUG}
  TOptixOpenSSLHelper.LoadCertificate(
    DEBUG_CERTIFICATE_PUBLIC_KEY,
    DEBUG_CERTIFICATE_PRIVATE_KEY,
    ACertificate
  );

  RegisterCertificate(ACertificate);
  {$ENDIF}

  var AConfig := TOptixConfigCertificatesStore(CONFIG_HELPER.Read('Certificates'));
  if not Assigned(AConfig) then
    Exit();
  try
    VST.BeginUpdate();
    try
      for var I := 0 to AConfig.Count -1 do begin
        ACertificate := AConfig.Items[I];

        if not Assigned(ACertificate.pX509) or not Assigned(ACertificate.pPrivKey) then
          continue;

        ///
        RegisterCertificate(ACertificate);
      end;
    finally
      VST.EndUpdate();
    end;
  finally
    FreeAndNil(AConfig);
  end;
end;

procedure TFormCertificatesStore.PopupMenuPopup(Sender: TObject);
begin
  TOptixVCLHelper.HideAllPopupMenuRootItems(TPopupMenu(Sender));

  var pCertificate := GetCertificateFromNode(VST.FocusedNode);
  if not Assigned(pCertificate) then
    Exit();

  ExportCertificate1.Visible       := Assigned(pCertificate);
  ExportPublicKey1.Visible         := Assigned(pCertificate);
  ExportPrivateKey1.Visible        := Assigned(pCertificate);
  RemoveCertificate1.Visible       := Assigned(pCertificate);
  CopySelectedFingerprint1.Visible := Assigned(pCertificate);
end;

procedure TFormCertificatesStore.RegisterCertificate(const ACertificate : TX509Certificate);
begin
  if GetNodeByFingerprint(ACertificate.Fingerprint) <> nil then begin
    Application.MessageBox('The certificate already exists in the store and cannot be added again.', 'Register Certificate', MB_ICONERROR);
    Exit();
  end;
  ///

  VST.BeginUpdate();
  try
    var pNode := VST.AddChild(nil);
    var pData := PTreeData(pNode.GetData);

    pData^.Certificate := ACertificate;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormCertificatesStore.RemoveCertificate1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  {$IFDEF SERVER}
  var pData := PTreeData(VST.FocusedNode.GetData);
  if not Assigned(pData) then
    Exit();

  if FormServers.ServerCertificateIsInUse(pData^.Certificate.Fingerprint) then
    raise Exception.Create(
      'Cannot delete certificate. The certificate is currently in use by a server and must be unassigned before ' +
      'removal from the store.'
    );
  {$ENDIF}

  if Application.MessageBox(
    'This action will delete the certificate and its associated information. If you do not have any physical backups,' +
    ' the certificate will be permanently lost. Are you sure?',
    'Delete Certificate', MB_ICONQUESTION + MB_YESNO) = ID_NO then
      Exit();

  ///
  VST.DeleteNode(VST.FocusedNode);
end;

procedure TFormCertificatesStore.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  {$IFDEF DEBUG}
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  var AColor := clNone;

  if String.Compare(pData^.Certificate.Fingerprint, DEBUG_CERTIFICATE_FINGERPRINT, True) = 0 then
    AColor := COLOR_LIST_RED;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;
    TargetCanvas.FillRect(CellRect);
  end;
  {$ENDIF}
end;

procedure TFormCertificatesStore.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormCertificatesStore.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := 0
  else begin
    case Column of
      0 : Result := CompareText(pData1^.Certificate.C, pData2^.Certificate.C);
      1 : Result := CompareText(pData1^.Certificate.O, pData2^.Certificate.O);
      2 : Result := CompareText(pData1^.Certificate.CN, pData2^.Certificate.CN);
      3 : Result := CompareText(pData1^.Certificate.Fingerprint, pData2^.Certificate.Fingerprint);
    end;
  end;
end;

procedure TFormCertificatesStore.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormCertificatesStore.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  if Assigned(pData) then
    TOptixOpenSSLHelper.FreeCertificate(pData^.Certificate);
end;

procedure TFormCertificatesStore.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if Column <> 0 then
    Exit();

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected : begin
      {$IFDEF DEBUG}
      var pData := PTreeData(Node.GetData);

      if Assigned(pData) and (String.Compare(pData^.Certificate.Fingerprint, DEBUG_CERTIFICATE_FINGERPRINT) = 0) then
        ImageIndex := IMAGE_BUG
      else
      {$ENDIF}
      ImageIndex := IMAGE_CERTIFICATE;
    end;

    TVTImageKind.ikState: ;
    TVTImageKind.ikOverlay: ;
  end;
end;

procedure TFormCertificatesStore.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormCertificatesStore.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData) then begin
    case Column of
      0 : CellText := pData^.Certificate.C;
      1 : CellText := pData^.Certificate.O;
      2 : CellText := pData^.Certificate.CN;
      3 : CellText := pData^.Certificate.Fingerprint;
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

function TFormCertificatesStore.GetCertificateFromNode(const pNode : PVirtualNode) : PX509Certificate;
begin
  result := nil;
  if not Assigned(pNode) then
    Exit();

  var pData := PTreeData(pNode.GetData);
  if not Assigned(pData^.Certificate.pX509) or not Assigned(pData^.Certificate.pPrivKey) then
    Exit();

  ///
  result := PX509Certificate(@pData^.Certificate);
end;

procedure TFormCertificatesStore.CopySelectedFingerprint1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);

  Clipboard.AsText := pData^.Certificate.Fingerprint;
end;

procedure TFormCertificatesStore.ExportCertificate(const pNode : PVirtualNode; const AExportWhich : TOpenSSLCertificateKeyTypes = []);
begin
  SD.FileName := '';

  if ((AExportWhich = []) or ((cktPublic in AExportWhich) and (cktPrivate in AExportWhich))) then
    SD.DefaultExt := 'pem'
  else if cktPublic in AExportWhich then
    SD.DefaultExt := 'pub'
  else if cktPrivate in AExportWhich then
    SD.DefaultExt := 'key';

  if not SD.Execute() then
    Exit();

  var pCertificate := GetCertificateFromNode(pNode);
  if not Assigned(pCertificate) then
    Exit();

  TOptixOpenSSLHelper.ExportCertificate(SD.FileName, pCertificate^, AExportWhich);
end;

procedure TFormCertificatesStore.ExportCertificate1Click(Sender: TObject);
begin
  ExportCertificate(VST.FocusedNode);
end;

procedure TFormCertificatesStore.ExportPrivateKey1Click(Sender: TObject);
begin
  ExportCertificate(VST.FocusedNode, [cktPrivate]);
end;

procedure TFormCertificatesStore.ExportPublicKey1Click(Sender: TObject);
begin
  ExportCertificate(VST.FocusedNode, [cktPublic]);
end;

procedure TFormCertificatesStore.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Save();
end;

procedure TFormCertificatesStore.FormCreate(Sender: TObject);
begin
  Load();
end;

procedure TFormCertificatesStore.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if TBaseVirtualTree(Sender).GetNodeAt(Point(X, Y)) = nil then begin
    TBaseVirtualTree(Sender).ClearSelection();

    TBaseVirtualTree(Sender).FocusedNode := nil;
  end;
end;

procedure TFormCertificatesStore.GeneratenewCertificate1Click(Sender: TObject);
begin
  var AForm := TFormGenerateNewCertificate.Create(self);
  try
    AForm.ShowModal();
    ///

    if AForm.Canceled then
      Exit();

    var ACertificate : TX509Certificate;
    Zeromemory(@ACertificate, SizeOf(TX509Certificate));
    try
      ACertificate.pPrivKey := TOptixOpenSSLHelper.NewPrivateKey();

      ACertificate.pX509 := TOptixOpenSSLHelper.NewX509(
        ACertificate.pPrivKey,
        AForm.EditC.Text,
        AForm.EditO.Text,
        AForm.EditCN.Text
      );

      TOptixOpenSSLHelper.RetrieveCertificateInformation(ACertificate);

      ///
      RegisterCertificate(ACertificate);
    except
      on E : Exception do begin
        TOptixOpenSSLHelper.FreeCertificate(ACertificate);
      end;
    end;
  finally
    FreeAndNil(AForm);
  end;
end;

procedure TFormCertificatesStore.Import1Click(Sender: TObject);

  function GetErrorMessage(const AKeyName : String) : String;
  begin
    var ATemplate := 'Could not import the certificate. The %s key is either missing, corrupted, or in a format ' +
                     'that does not match the expected file format (must contain both the private and public keys).';

    result := Format(ATemplate, [AKeyName]);
  end;

begin
  if not OD.Execute() then
    Exit();
  ///

  var ACertificate : TX509Certificate;

  var AErrorMessage := '';

  try
    TOptixOpenSSLHelper.ImportCertificate(OD.FileName, ACertificate);
  except
    on E : EOpenSSLPrivateKeyException do
      AErrorMessage := GetErrorMessage('private');

    on E : EOpenSSLPublicKeyException do
      AErrorMessage := GetErrorMessage('public');
  end;

  if String.IsNullOrWhiteSpace(AErrorMessage) then
    RegisterCertificate(ACertificate)
  else
    Application.MessageBox(PWideChar(AErrorMessage), 'Import Certificate', MB_ICONERROR);
end;

end.
