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

  TControlFormRegistryManager = class(TBaseFormControl)
    OMultiPanel: TOMultiPanel;
    VSTKeys: TVirtualStringTree;
    VSTValues: TVirtualStringTree;
    EditPath: TEdit;
    MainMenu: TMainMenu;
    Options1: TMenuItem;
    HideUnenumerableKeys1: TMenuItem;
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
  private
    {@M}
    procedure BrowsePath(const AKeyFullPath : String);
    procedure DisplayKeys(const AList : TOptixRefreshRegistryKeys);
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
  Generics.Collections,

  uFormMain,

  Optix.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

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

  function GetParentNode(const APath : String) : PVirtualNode;
  begin
    result := nil;
    ///

    for var pNode in VSTKeys.Nodes do begin
      var pData := PKeysTreeData(pNode.GetData);
      if String.Compare(pData^.Path, APath, True) = 0 then begin
        result := pNode;

        break;
      end;
    end;
  end;

  function GetLevelFolder(const AName : String; const pParent : PVirtualNode) : PVirtualNode;
  begin
    result := nil;
    ///

    var pChildNode := VSTKeys.GetFirstChild(pParent);
    while Assigned(pChildNode) do begin
      var pData := PKeysTreeData(pChildNode.GetData);
      if not Assigned(pData^.KeyInformation) then
        continue;
      ///

      if String.Compare(pData^.KeyInformation.Name, AName, True) = 0 then begin
        result := pChildNode;

        break;
      end;

      ///
      pChildNode := VSTKeys.GetNextSibling(pChildNode);
    end;
  end;

begin
  if not Assigned(AList) then
    Exit();
  ///

  var pParentNode := GetParentNode(AList.Path);

  VSTKeys.BeginUpdate();
  try
    for var AItem in AList.Keys do begin
      var pNode := GetLevelFolder(AItem.Name, pParentNode);
      if not Assigned(pNode) then
        pNode := VSTKeys.AddChild(pParentNode);

      var pData := PKeysTreeData(pNode.GetData);

      pData^.KeyInformation := TRegistryKeyInformation.Create();
      pData^.KeyInformation.Assign(AItem);

      pData^.Path := IncludeTrailingPathDelimiterIfNotEmpty(AList.Path) + AItem.Name;

      pData^.ImageIndex := SystemFolderIcon();
    end;
  finally
    VSTKeys.FullExpand(pParentNode);

    VSTKeys.SortTree(0, TSortDirection.sdAscending);

    RefreshNodesVisibility();

    VSTKeys.EndUpdate();

    ///
    EditPath.Text := AList.Path;
  end;
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
  if AOptixPacket is TOptixRefreshRegistryKeys then
    DisplayKeys(TOptixRefreshRegistryKeys(AOptixPacket));
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
    Result := 0
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

end.
