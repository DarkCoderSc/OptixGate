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

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  VirtualTrees.AncestorVCL, VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.Types,
  OMultiPanel,

  __uBaseFormControl__, Vcl.StdCtrls;
// ---------------------------------------------------------------------------------------------------------------------

type
  TKeysTreeData = record
    Name       : String;
    Path       : String;
    ImageIndex : Integer;
  end;
  PKeysTreeData = ^TKeysTreeData;

  TControlFormRegistryManager = class(TBaseFormControl)
    OMultiPanel: TOMultiPanel;
    VSTKeys: TVirtualStringTree;
    VSTValues: TVirtualStringTree;
    EditPath: TEdit;
    procedure VSTKeysGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTKeysGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTKeysFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTKeysChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure VSTKeysGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
  private
    {@M}
    procedure InsertKey(const AKeyName : String; const AKeyPath : String = '');
    procedure BrowsePath(const ARegistryHive : HKEY; const AKeyPath : String);
  public
    { Public declarations }
  end;

var
  ControlFormRegistryManager: TControlFormRegistryManager;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  uFormMain,

  Optix.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormRegistryManager.BrowsePath(const ARegistryHive : HKEY; const AKeyPath : String);
begin
  ///
end;

procedure TControlFormRegistryManager.FormCreate(Sender: TObject);
begin
  // TODO: Receive them from remote probing for permission
  InsertKey('HKEY_CLASSES_ROOT');
  InsertKey('HKEY_CURRENT_USER');
  InsertKey('HKEY_LOCAL_MACHINE');
  InsertKey('HKEY_USERS');
  InsertKey('HKEY_PERFORMANCE_DATA');
  InsertKey('HKEY_CURRENT_CONFIG');
  InsertKey('HKEY_DYN_DATA');
end;

procedure TControlFormRegistryManager.InsertKey(const AKeyName : String; const AKeyPath : String = '');
begin
  VSTKeys.BeginUpdate();
  try
    var pNode := VSTKeys.AddChild(nil);
    var pData := PKeysTreeData(pNode.GetData);

    pData^.Name       := AKeyName;
    pData^.Path       := AKeyPath;
    pData^.ImageIndex := SystemFolderIcon();
  finally
    VSTKeys.EndUpdate();
  end;
end;

procedure TControlFormRegistryManager.VSTKeysChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormRegistryManager.VSTKeysFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
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

  CellText := pData^.Name;
end;

end.
