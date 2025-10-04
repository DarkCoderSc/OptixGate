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

unit uControlFormRegistryManager;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Menus,

  VirtualTrees.AncestorVCL, VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.Types,
  OMultiPanel,

  __uBaseFormControl__,

  Optix.Protocol.Packet, Optix.Func.Commands.Registry, Optix.Registry.Enum, Optix.Registry.Helper;
// ---------------------------------------------------------------------------------------------------------------------

type
  TKeysTreeData = record
    KeyInformation : TRegistryKeyInformation;
    ImageIndex     : Integer;
    Path           : String;
  end;
  PKeysTreeData = ^TKeysTreeData;

  TValuesTreeData = record
    ValueInformation : TRegistryValueInformation;
    ImageIndex       : Integer;
  end;
  PValuesTreeData = ^TValuesTreeData;

  TControlFormRegistryManager = class(TBaseFormControl)
    OMultiPanel: TOMultiPanel;
    VSTKeys: TVirtualStringTree;
    VSTValues: TVirtualStringTree;
    EditPath: TEdit;
    MainMenu: TMainMenu;
    Options1: TMenuItem;
    HideUnenumerableKeys1: TMenuItem;
    PopupKeys: TPopupMenu;
    FullExpand1: TMenuItem;
    FullCollapse1: TMenuItem;
    FullCollapse2: TMenuItem;
    Registry1: TMenuItem;
    Refresh1: TMenuItem;
    N1: TMenuItem;
    GoTo1: TMenuItem;
    procedure VSTKeysGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTKeysGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTKeysFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTKeysChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTKeysGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTKeysFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTKeysCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure VSTKeysDblClick(Sender: TObject);
    procedure HideUnenumerableKeys1Click(Sender: TObject);
    procedure VSTValuesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTValuesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTValuesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTValuesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure VSTValuesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTValuesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTValuesCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure FullExpand1Click(Sender: TObject);
    procedure FullCollapse1Click(Sender: TObject);
    procedure PopupKeysPopup(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure GoTo1Click(Sender: TObject);
  private
    {@M}
    procedure BrowsePath(const AKeyFullPath : String);
    procedure DisplayKeys(const AList : TOptixRefreshRegistryKeys);
    procedure DisplayValues(const AList : TOptixRefreshRegistryKeys);
    procedure RefreshNodesVisibility();
  protected
    {@M}
    procedure OnFirstShow(); override;
  public
    {@M}
    procedure ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean); override;
  end;

var
  ControlFormRegistryManager: TControlFormRegistryManager;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math,

  Generics.Collections,

  uFormMain,

  Optix.Helper, Optix.Constants, Optix.System.Helper, Optix.VCL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormRegistryManager.Refresh1Click(Sender: TObject);
begin
  if not String.IsNullOrWhiteSpace(EditPath.Text) then
    BrowsePath(EditPath.Text);
end;

procedure TControlFormRegistryManager.RefreshNodesVisibility();
begin
  VSTKeys.BeginUpdate();
  try
    for var pNode in VSTKeys.Nodes do begin
      var pData := PKeysTreeData(pNode.GetData);

      // TODO: when Delphi CE is in version 13 replace not (x in y) by x not in y
      var AExclude := HideUnenumerableKeys1.Checked and (not Assigned(pData^.KeyInformation) or
        not (rkpEnumerateSubKeys in pData^.KeyInformation.Permissions));

      VSTKeys.IsVisible[pNode] := not AExclude;
    end;
  finally
    VSTKeys.EndUpdate();
  end;
end;

procedure TControlFormRegistryManager.DisplayKeys(const AList : TOptixRefreshRegistryKeys);
begin
  if not Assigned(AList) and (AList.SubKeys.Count > 0) then
    Exit();
  ///

  try
    TOptixVirtualTreesFolderTreeHelper.UpdateTree<TRegistryKeyInformation>(
      VSTKeys,
      AList.ParentKeys,
      AList.SubKeys,
      (
        function (const pData : Pointer) : String
        begin
          result := PKeysTreeData(pData)^.KeyInformation.Name;
        end
      ),
      (
        function (const AItem: TRegistryKeyInformation): String
        begin
          if Assigned(AItem) then
            result := AItem.Name
          else
            result := '';
        end
      ),
      (
        procedure (var pNode, pParentNode : PVirtualNode; const AItem : TRegistryKeyInformation)
        begin
          var pData : PKeysTreeData;
          if not Assigned(pNode) then begin
            pNode := VSTKeys.AddChild(pParentNode);

            pData := PKeysTreeData(pNode.GetData);

            pData^.KeyInformation := TRegistryKeyInformation.Create();

            pData^.ImageIndex := SystemFolderIcon();

            if Assigned(pParentNode) then begin
              var pParentData := PKeysTreeData(pParentNode.GetData);

              pData^.Path := TSystemHelper.IncludeTrailingPathDelimiterIfNotEmpty(pParentData^.Path) + AItem.Name;
            end else
              pData^.Path := AItem.Name;
          end else
            pData := PKeysTreeData(pNode.GetData);

          ///
          if Assigned(pData^.KeyInformation) then
            pData^.KeyInformation.Assign(AItem);
        end
      )
    );
  finally
    RefreshNodesVisibility();

    ///
    EditPath.Text := AList.Path;
  end;
end;

procedure TControlFormRegistryManager.DisplayValues(const AList : TOptixRefreshRegistryKeys);
begin
  VSTValues.Clear();
  ///

  if not Assigned(AList) and (AList.Values.Count > 0) then
    Exit();
  ///

  VSTValues.BeginUpdate();
  try
    for var AItem in AList.Values do begin
      var pNode := VSTValues.AddChild(nil);
      var pData := PValuesTreeData(pNode.GetData);
      ///

      pData^.ValueInformation := TRegistryValueInformation.Create();
      pData^.ValueInformation.Assign(AItem);

      case pData^.ValueInformation._Type of
        REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ:
          pData^.ImageIndex := IMAGE_REG_SZ;
        else
          pData^.ImageIndex := IMAGE_REG_DATA;
      end;
    end;
  finally
    VSTValues.EndUpdate();
  end;
end;

procedure TControlFormRegistryManager.FullCollapse1Click(Sender: TObject);
begin
  VSTKeys.FullCollapse(nil);
end;

procedure TControlFormRegistryManager.FullExpand1Click(Sender: TObject);
begin
  VSTKeys.FullExpand(nil);
end;

procedure TControlFormRegistryManager.GoTo1Click(Sender: TObject);
begin
  var APath := '';

  if not InputQuery('Go To', 'Key Path:', APath) then
    Exit();

  APath := TRegistryHelper.ExpandHiveShortName(APath);

  if not String.IsNullOrWhiteSpace(APath) then
    BrowsePath(APath);
end;

procedure TControlFormRegistryManager.HideUnenumerableKeys1Click(Sender: TObject);
begin
  RefreshNodesVisibility();
end;

procedure TControlFormRegistryManager.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  inherited;
  ///

  // -------------------------------------------------------------------------------------------------------------------
  if AOptixPacket is TOptixRefreshRegistryKeys then begin
    DisplayKeys(TOptixRefreshRegistryKeys(AOptixPacket));
    DisplayValues(TOptixRefreshRegistryKeys(AOptixPacket));
  end;
  // -------------------------------------------------------------------------------------------------------------------
end;

procedure TControlFormRegistryManager.BrowsePath(const AKeyFullPath : String);
begin
  SendCommand(TOptixRefreshRegistrySubKeys.Create(AKeyFullPath));
end;

procedure TControlFormRegistryManager.OnFirstShow();
begin
  VSTKeys.Clear();
  VSTValues.Clear();

  ///
  SendCommand(TOptixGetRegistryHives.Create());
end;

procedure TControlFormRegistryManager.PopupKeysPopup(Sender: TObject);
begin
//  var pData := PKeysTreeData(nil);
//  if VSTKeys.FocusedNode <> nil then
//    pData := VSTKeys.FocusedNode.GetData;
  ///

end;

procedure TControlFormRegistryManager.VSTKeysChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormRegistryManager.VSTKeysCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PKeysTreeData(Node1.GetData);
  var pData2 := PKeysTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := ComparePointerAssigmenet(pData1, pData2)
  else if not Assigned(pData1^.KeyInformation) or not Assigned(pData2^.KeyInformation) then
    Result := CompareObjectAssigmenet(pData1^.KeyInformation, pData2^.KeyInformation)
  else
    Result := CompareText(pData1^.KeyInformation.Name, pData2^.KeyInformation.Name);
end;

procedure TControlFormRegistryManager.VSTKeysDblClick(Sender: TObject);
begin
  var pNode := VSTKeys.FocusedNode;
  if not Assigned(pNode) then
    Exit();

  var pData := PKeysTreeData(pNode.GetData);
  if not Assigned(pData) or not Assigned(pData^.KeyInformation) then
    Exit();

  ///
  BrowsePath(pData^.Path);
end;

procedure TControlFormRegistryManager.VSTKeysFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormRegistryManager.VSTKeysFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PKeysTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  if Assigned(pData^.KeyInformation) then
    FreeAndNil(pData^.KeyInformation);
end;

procedure TControlFormRegistryManager.VSTKeysGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Column <> 0) or ((Kind <> ikNormal) and (Kind <> ikSelected)) then
    Exit();
  ///

  var pData := PKeysTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  ImageIndex := pData^.ImageIndex;
end;

procedure TControlFormRegistryManager.VSTKeysGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TKeysTreeData);
end;

procedure TControlFormRegistryManager.VSTKeysGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PKeysTreeData(Node.GetData);
  if not Assigned(pData) or (Column <> 0) then
    Exit();
  ///

  CellText := pData^.KeyInformation.Name;
end;

procedure TControlFormRegistryManager.VSTValuesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormRegistryManager.VSTValuesCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PValuesTreeData(Node1.GetData);
  var pData2 := PValuesTreeData(Node2.GetData);

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := ComparePointerAssigmenet(pData1, pData2)
  else if not Assigned(pData1^.ValueInformation) or not Assigned(pData2^.ValueInformation) then
    Result := CompareObjectAssigmenet(pData1^.ValueInformation, pData2^.ValueInformation)
  else begin
    case Column of
      0 : Result := CompareText(pData1^.ValueInformation.Name, pData2^.ValueInformation.Name);
      1 : Result := CompareValue(pData1^.ValueInformation._Type, pData2^.ValueInformation._Type);
      2 : Result := CompareText(pData1^.ValueInformation.Value, pData2^.ValueInformation.Value);
    end;
  end;
end;

procedure TControlFormRegistryManager.VSTValuesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormRegistryManager.VSTValuesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PValuesTreeData(Node.GetData);
  if Assigned(pData) and Assigned(pData^.ValueInformation) then
    FreeAndNil(pData^.ValueInformation);
end;

procedure TControlFormRegistryManager.VSTValuesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if Column <> 0 then
    Exit();

  var pData := PValuesTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();

  case Kind of
    ikNormal, ikSelected :
      ImageIndex := pData^.ImageIndex;

    ikState: ;
    ikOverlay: ;
  end;
end;

procedure TControlFormRegistryManager.VSTValuesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TValuesTreeData);
end;

procedure TControlFormRegistryManager.VSTValuesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  CellText := '';
  ///

  var pData := PValuesTreeData(Node.GetData);
  if Assigned(pData) and Assigned(pData^.ValueInformation) then begin
    case Column of
      0 : begin
        // TODO Ternary op when Delphi 13 CE released
        if pData^.ValueInformation.IsDefault then
          CellText := '(Default)'
        else
          CellText := pData^.ValueInformation.Name;
      end;

      1 : begin
        case pData^.ValueInformation._Type of
          REG_SZ        : CellText := 'REG_SZ';
          REG_EXPAND_SZ : CellText := 'REG_EXPAND_SZ';
          REG_MULTI_SZ  : CellText := 'REG_MULTI_SZ';
          REG_DWORD     : CellText := 'REG_DWORD';
          REG_QWORD     : CellText := 'REG_QWORD';
          REG_BINARY    : CellText := 'REG_BINARY';
          else
            CellText := '(unknown)';
        end;
      end;

      2 : begin
        // TODO Ternary op when Delphi 13 CE released
        if pData^.ValueInformation.IsDefault and pData^.ValueInformation.Value.IsEmpty then
          CellText := '(value not set)'
        else
          CellText := pData^.ValueInformation.Value.Replace(#13#10, '\0', [rfReplaceAll]);
      end;
    end;
  end;
end;

end.
