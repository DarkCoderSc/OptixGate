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

unit uFormTrustedCertificates;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,

  VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TTreeData = record
    Fingerprint : String;
  end;
  PTreeData = ^TTreeData;

  TFormTrustedCertificates = class(TForm)
    VST: TVirtualStringTree;
    MainMenu: TMainMenu;
    Certificate1: TMenuItem;
    AddTrustedCertificate1: TMenuItem;
    PopupMenu: TPopupMenu;
    Remove1: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure AddTrustedCertificate1Click(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure Remove1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenuPopup(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
  private
    {@M}
    function GetNodeByFingerprint(const AFingerprint : String) : PVirtualNode;
    procedure AddTrustedCertificate(const AFingerprint : String);
    function GetTrustedCertificateCount() : Integer;
    procedure Save();
    procedure Load();
  public
    {@M}
    procedure OnVerifyPeerCertificate(Sender : TObject; const APeerFingerprint : String; var ASuccess : Boolean);

    {@G}
    property TrustedCertificateCount : Integer read GetTrustedCertificateCount;
  end;

var
  FormTrustedCertificates: TFormTrustedCertificates;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  Optix.Helper, Optix.Constants, Optix.Config.Helper, Optix.Config.TrustedCertificatesStore
  {$IFDEF DEBUG}, Optix.DebugCertificate{$ENDIF};
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TFormTrustedCertificates.Save();
begin
  var AConfig := TOptixTrustedConfigCertificatesStore.Create();
  try
    for var pNode in VST.Nodes do begin
      var pData := PTreeData(pNode.GetData);

      AConfig.Add(pData^.Fingerprint);
    end;
  finally
    CONFIG_HELPER.Write('TrustedCertificates', AConfig);

    ///
    FreeAndNil(AConfig);
  end;
end;

procedure TFormTrustedCertificates.Load();
begin
  VST.Clear();
  ///

  var AConfig := TOptixTrustedConfigCertificatesStore(CONFIG_HELPER.Read('TrustedCertificates'));
  if not Assigned(AConfig) then
    Exit();
  try
    VST.BeginUpdate();
    try
      for var I := 0 to AConfig.Count -1 do begin
        var AFingerprint := AConfig.Items[I];
        if String.IsNullOrWhitespace(AFingerprint) then
          continue;

        ///
        AddTrustedCertificate(AFingerprint);
      end;
    finally
      VST.EndUpdate();
    end;
  finally
    FreeAndNil(AConfig);
  end;
end;

procedure TFormTrustedCertificates.OnVerifyPeerCertificate(Sender : TObject; const APeerFingerprint : String; var ASuccess : Boolean);
begin
  {$IFDEF DEBUG}
    ASuccess := String.Compare(DEBUG_PEER_CERTIFICATE_FINGERPRINT, APeerFingerprint, True) = 0;
    if ASuccess then
      Exit();
  {$ENDIF}

  ASuccess := GetNodeByFingerprint(APeerFingerprint) <> nil;
end;

function TFormTrustedCertificates.GetTrustedCertificateCount() : Integer;
begin
  result := VST.RootNodeCount;
end;

function TFormTrustedCertificates.GetNodeByFingerprint(const AFingerprint : String) : PVirtualNode;
begin
  result := nil;
  ///

  if String.IsNullOrWhitespace(AFingerprint) then
    Exit();

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);

    if String.Compare(pData^.Fingerprint, AFingerprint, True) = 0 then begin
      result := pNode;

      break;
    end;
  end;
end;

procedure TFormTrustedCertificates.AddTrustedCertificate(const AFingerprint : String);
begin
  VST.BeginUpdate();
  try
    var pNode := VST.AddChild(nil);
    var pData := PTreeData(pNode.GetData);

    pData^.Fingerprint := AFingerprint;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormTrustedCertificates.AddTrustedCertificate1Click(Sender: TObject);
begin
  var AFingerprint := '';

  if not InputQuery('Add Trusted Certificate', 'Please enter a valid SHA-512 certificate fingerprint:', AFingerprint) then
    Exit();

  CheckCertificateFingerprint(AFingerprint);

  AddTrustedCertificate(AFingerprint);
end;

procedure TFormTrustedCertificates.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Save();
end;

procedure TFormTrustedCertificates.FormCreate(Sender: TObject);
begin
  Load();
end;

procedure TFormTrustedCertificates.PopupMenuPopup(Sender: TObject);
begin
  Remove1.Visible := VST.FocusedNode <> nil;
end;

procedure TFormTrustedCertificates.Remove1Click(Sender: TObject);
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

procedure TFormTrustedCertificates.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormTrustedCertificates.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := 0
  else begin
    case Column of
      0 : Result := CompareText(pData1^.Fingerprint, pData2^.Fingerprint);
    end;
  end;
end;

procedure TFormTrustedCertificates.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormTrustedCertificates.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if Column <> 0 then
    Exit();

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected :
      ImageIndex := IMAGE_TRUSTED_CERTIFICATE;
  end;
end;

procedure TFormTrustedCertificates.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormTrustedCertificates.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  CellText := '';

  var pData := PTreeData(Node.GetData);

  if Assigned(pData) then begin
    case Column of
      0 : CellText := pData^.Fingerprint;
    end;
  end;

  CellText := DefaultIfEmpty(CellText);
end;

end.
