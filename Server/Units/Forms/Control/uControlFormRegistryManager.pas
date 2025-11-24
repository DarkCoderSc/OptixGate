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

unit uControlFormRegistryManager;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Menus,

  VirtualTrees.AncestorVCL, VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.Types,
  OMultiPanel,

  Optix.Protocol.Packet, Optix.Func.Commands.Registry, Optix.Registry.Enum, Optix.Registry.Helper,

  __uBaseFormControl__;
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
    ValueAsString    : String;
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
    CreateSubKey1: TMenuItem;
    CreateKey1: TMenuItem;
    PopupValues: TPopupMenu;
    New1: TMenuItem;
    NewKey1: TMenuItem;
    DeleteSelectedKey1: TMenuItem;
    N2: TMenuItem;
    NewStringValue1: TMenuItem;
    NewMultiLineStringValue1: TMenuItem;
    NewDWORDValue1: TMenuItem;
    NewQWORDValue1: TMenuItem;
    NewBinaryValue1: TMenuItem;
    EditSelectedValue1: TMenuItem;
    N3: TMenuItem;
    Refresh2: TMenuItem;
    N4: TMenuItem;
    procedure VSTKeysGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTKeysGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTKeysGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTKeysFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTKeysCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure VSTKeysDblClick(Sender: TObject);
    procedure HideUnenumerableKeys1Click(Sender: TObject);
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
    procedure CreateSubKey1Click(Sender: TObject);
    procedure CreateKey1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewKey1Click(Sender: TObject);
    procedure PopupValuesPopup(Sender: TObject);
    procedure DeleteSelectedKey1Click(Sender: TObject);
    procedure NewStringValue1Click(Sender: TObject);
    procedure NewMultiLineStringValue1Click(Sender: TObject);
    procedure NewDWORDValue1Click(Sender: TObject);
    procedure NewQWORDValue1Click(Sender: TObject);
    procedure NewBinaryValue1Click(Sender: TObject);
    procedure EditSelectedValue1Click(Sender: TObject);
    procedure VSTValuesDblClick(Sender: TObject);
    procedure Refresh2Click(Sender: TObject);
  private
    FCurrentKeyPath        : String;
    FCurrentKeyPermissions : TRegistryKeyPermissions;

    {@M}
    procedure BrowsePath(const AKeyFullPath : String);
    procedure DisplayKeys(const AList : TOptixCommandEnumRegistry); overload;
    procedure DisplayKeys(const AParentKeys, ALevelKeys : TObjectList<TRegistryKeyInformation>); overload;
    procedure AddNewValueNode(const AValueInformation : TRegistryValueInformation);
    procedure DisplayValues(const AList : TOptixCommandEnumRegistry);
    function GetNodeByKeyPath(const AKeyPath : String) : PVirtualNode;
    procedure RefreshNodesVisibility();
    procedure CreateOrEditRegistryValue(const AValueKind : DWORD;
      const AExistingValue : TRegistryValueInformation = nil);
    function GetValueNode(const AFullKeyPath, AValueName : String) : PVirtualNode;
  protected
    {@M}
    procedure OnFirstShow(); override;
    procedure CreateNewRegistryKey(const ANewKeyFullPath : String);
    procedure CreateNewRegistryKeyEx(const ANewKeyBaseFullPath : String);
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

  uFormMain,

  Optix.Helper, Optix.Constants, Optix.System.Helper, Optix.VCL.Helper,

  uControlFormRegistryEditor;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

function TControlFormRegistryManager.GetValueNode(const AFullKeyPath, AValueName : String) : PVirtualNode;
begin
  result := nil;
  ///

  if String.Compare(FCurrentKeyPath, AFullKeyPath, True) <> 0 then
    Exit();

  for var pNode in VSTValues.Nodes do begin
    var pData := PValuesTreeData(pNode.GetData);
    if not Assigned(pData) or not Assigned(pData^.ValueInformation) then
      continue;
    ///

    if String.Compare(AValueName, pData^.ValueInformation.Name, True) = 0 then begin
      result := pNode;

      break;
    end;
  end;
end;

procedure TControlFormRegistryManager.CreateOrEditRegistryValue(const AValueKind : DWORD;
  const AExistingValue : TRegistryValueInformation = nil);
begin
  if not (rkpSetValue in FCurrentKeyPermissions) then
    Exit();
  ///

  var AForm := TControlFormRegistryEditor(FormMain.CreateNewControlForm(self, TControlFormRegistryEditor, False));
  if not Assigned(AForm) then
    Exit();

  AForm.ManagerGUID := GUID;
  AForm.ValueKind   := AValueKind;
  AForm.FullKeyPath := FCurrentKeyPath;

  if Assigned(AExistingValue) then begin
    AForm.EditMode := True;
    AForm.EditName.Text := AExistingValue.Name;

    AForm.SetData(AExistingValue.Value);
  end;

  ///
  TOptixVCLHelper.ShowForm(AForm);
end;

function TControlFormRegistryManager.GetNodeByKeyPath(const AKeyPath : String) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VSTKeys.Nodes do begin
    var pData := PKeysTreeData(pNode.GetData);
    if not Assigned(pData) then
      continue;
    ///

    if String.Compare(pData^.Path, AKeyPath, True) = 0 then begin
      result := pNode;

      ///
      break;
    end;
  end;
end;

procedure TControlFormRegistryManager.CreateKey1Click(Sender: TObject);
begin
  var APath := '';

  if not InputQuery('Create New Registry Key', 'Key Full Path:', APath) then
    Exit();

  APath := TRegistryHelper.ExpandHiveShortName(APath);

  if not String.IsNullOrWhiteSpace(APath) then
    CreateNewRegistryKey(IncludeTrailingPathDelimiter(APath));
end;

procedure TControlFormRegistryManager.CreateNewRegistryKey(const ANewKeyFullPath : String);
begin
  SendCommand(TOptixCommandCreateRegistryKey.Create(ANewKeyFullPath));
end;

procedure TControlFormRegistryManager.CreateNewRegistryKeyEx(const ANewKeyBaseFullPath : String);
begin
  var ANewKeyName := '';
  if not InputQuery('Create New Registry Key', 'New Key Name:', ANewKeyName) or
     String.IsNullOrWhiteSpace(ANewKeyName) then
      Exit();
  ///

  CreateNewRegistryKey(IncludeTrailingPathDelimiter(ANewKeyBaseFullPath) + ANewKeyName);
end;

procedure TControlFormRegistryManager.CreateSubKey1Click(Sender: TObject);
begin
  if VSTKeys.FocusedNode = nil then
    Exit();
  ///

  var pData := PKeysTreeData(VSTKeys.FocusedNode.GetData);
  if not Assigned(pData) or not Assigned(pData^.KeyInformation) or
     (not (rkpCreateSubKey in pData^.KeyInformation.Permissions)) then
    Exit();
  ///

  CreateNewRegistryKeyEx(pData^.Path);
end;

procedure TControlFormRegistryManager.Refresh1Click(Sender: TObject);
begin
  if not String.IsNullOrWhiteSpace(EditPath.Text) then
    BrowsePath(EditPath.Text);
end;

procedure TControlFormRegistryManager.Refresh2Click(Sender: TObject);
begin
  if String.IsNullOrWhiteSpace(FCurrentKeyPath) then
    Exit();

  BrowsePath(FCurrentKeyPath);
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

procedure TControlFormRegistryManager.DisplayKeys(const AParentKeys, ALevelKeys : TObjectList<TRegistryKeyInformation>);
begin
  if not Assigned(AParentKeys) and not Assigned(ALevelKeys) then
    Exit();
  ///
  try
    TOptixVirtualTreesFolderTreeHelper.UpdateTree<TRegistryKeyInformation>(
      VSTKeys,
      AParentKeys,
      ALevelKeys,
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
  end;
end;

procedure TControlFormRegistryManager.DeleteSelectedKey1Click(Sender: TObject);
begin
  if VSTKeys.FocusedNode = nil then
    Exit();
  ///

  var pData := PKeysTreeData(VSTKeys.FocusedNode.GetData);
  if not Assigned(pData) or not Assigned(pData^.KeyInformation) or
     (not (rkpDelete in pData^.KeyInformation.Permissions)) then
    Exit();
  ///

  if Application.MessageBox(
    'You are about to permanently delete the registry key "keyname". All of its subkeys and all associated values will'+
    ' be permanently removed.',
    'Delete Selected Key',
     MB_ICONQUESTION + MB_YESNO
  ) = ID_NO then
    Exit();

  ///
  SendCommand(TOptixCommandDeleteRegistryKey.Create(pData^.Path));
end;

procedure TControlFormRegistryManager.DisplayKeys(const AList : TOptixCommandEnumRegistry);
begin
  if not Assigned(AList) then
    Exit();
  ///

  DisplayKeys(AList.ParentKeys, AList.SubKeys);
end;

procedure TControlFormRegistryManager.AddNewValueNode(const AValueInformation : TRegistryValueInformation);
begin
  if not Assigned(AValueInformation) then
    Exit();
  ///

  var pNode := VSTValues.AddChild(nil);
  var pData := PValuesTreeData(pNode.GetData);
  ///

  pData^.ValueInformation := TRegistryValueInformation.Create();
  pData^.ValueInformation.Assign(AValueInformation);
  pData^.ValueAsString := AValueInformation.ToString;

  case pData^.ValueInformation._Type of
    REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ:
      pData^.ImageIndex := IMAGE_REG_SZ;
    else
      pData^.ImageIndex := IMAGE_REG_DATA;
  end;
end;

procedure TControlFormRegistryManager.DisplayValues(const AList : TOptixCommandEnumRegistry);
begin
  VSTValues.Clear();
  ///

  if not Assigned(AList) and (AList.Values.Count > 0) then
    Exit();
  ///

  VSTValues.BeginUpdate();
  try
    for var AItem in AList.Values do
      AddNewValueNode(AItem);
  finally
    VSTValues.EndUpdate();
  end;
end;

procedure TControlFormRegistryManager.EditSelectedValue1Click(Sender: TObject);
begin
  if VSTValues.FocusedNode = nil then
    Exit();
  ///

  var pData := PValuesTreeData(VSTValues.FocusedNode.GetData);
  if Assigned(pData) and Assigned(pData^.ValueInformation) then
    CreateOrEditRegistryValue(pData^.ValueInformation._Type, pData^.ValueInformation);
end;

procedure TControlFormRegistryManager.FormCreate(Sender: TObject);
begin
  FCurrentKeyPath        := '';
  FCurrentKeyPermissions := [];
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

procedure TControlFormRegistryManager.NewBinaryValue1Click(Sender: TObject);
begin
   CreateOrEditRegistryValue(REG_BINARY);
end;

procedure TControlFormRegistryManager.NewDWORDValue1Click(Sender: TObject);
begin
   CreateOrEditRegistryValue(REG_DWORD);
end;

procedure TControlFormRegistryManager.NewKey1Click(Sender: TObject);
begin
  if String.IsNullOrWhiteSpace(FCurrentKeyPath) or not (rkpCreateSubKey in FCurrentKeyPermissions) then
    Exit();
  ///

  CreateNewRegistryKeyEx(FCurrentKeyPath)
end;

procedure TControlFormRegistryManager.NewMultiLineStringValue1Click(Sender: TObject);
begin
   CreateOrEditRegistryValue(REG_MULTI_SZ);
end;

procedure TControlFormRegistryManager.NewQWORDValue1Click(Sender: TObject);
begin
 CreateOrEditRegistryValue(REG_QWORD);
end;

procedure TControlFormRegistryManager.NewStringValue1Click(Sender: TObject);
begin
  CreateOrEditRegistryValue(REG_SZ);
end;

procedure TControlFormRegistryManager.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  inherited;
  ///

  // -------------------------------------------------------------------------------------------------------------------
  if AOptixPacket is TOptixCommandEnumRegistry then begin
    var AResult := TOptixCommandEnumRegistry(AOptixPacket);
    ///

    FCurrentKeyPath := AResult.KeyPath;
    FCurrentKeyPermissions := AResult.Permissions;

    DisplayKeys(AResult);
    DisplayValues(AResult);
    EditPath.Text := AResult.KeyPath;
  end
  // -------------------------------------------------------------------------------------------------------------------
  else if AOptixPacket is TOptixCommandDeleteRegistryKey then begin
    var AResult := TOptixCommandDeleteRegistryKey(AOptixPacket);
    ///

    var pNode := GetNodeByKeyPath(AResult.KeyPath);
    VSTKeys.BeginUpdate();
    try
      VSTKeys.DeleteNode(pNode);
    finally
      VSTKeys.EndUpdate();
    end;

    if String.Compare(FCurrentKeyPath, AResult.KeyPath, True) = 0 then begin
      VSTValues.Clear();

      FCurrentKeyPath := '';
      FCurrentKeyPermissions := [];
      EditPath.Clear();
    end;
  // -------------------------------------------------------------------------------------------------------------------
  end else if AOptixPacket is TOptixCommandSetRegistryValue then begin
    var AResult := TOptixCommandSetRegistryValue(AOptixPacket);
    ///

    VSTValues.BeginUpdate();
    try
      var pNode := GetValueNode(AResult.KeyPath, AResult.Name);
      if not Assigned(pNode) then
        AddNewValueNode(AResult.NewValue)
      else begin
        var pData := PValuesTreeData(pNode.GetData);
        if Assigned(pData) and Assigned(pData^.ValueInformation) then begin
          pData^.ValueInformation.Assign(AResult.NewValue);

          pData^.ValueAsString := AResult.NewValue.ToString; // Save computing time for VST
        end;
      end;
    finally
      VSTValues.EndUpdate();
    end;
  end;
  // -------------------------------------------------------------------------------------------------------------------
end;

procedure TControlFormRegistryManager.BrowsePath(const AKeyFullPath : String);
begin
  SendCommand(TOptixCommandEnumRegistryKeys.Create(AKeyFullPath));
end;

procedure TControlFormRegistryManager.OnFirstShow();
begin
  VSTKeys.Clear();
  VSTValues.Clear();

  ///
  SendCommand(TOptixCommandEnumRegistryHives.Create());
end;

procedure TControlFormRegistryManager.PopupKeysPopup(Sender: TObject);
begin
  var pData := PKeysTreeData(nil);
  if VSTKeys.FocusedNode <> nil then
    pData := VSTKeys.FocusedNode.GetData;
  ///

  var AIsDataAssigned := Assigned(pData) and Assigned(pData^.KeyInformation);

  ///
  CreateSubKey1.Enabled      := AIsDataAssigned and (rkpCreateSubKey in pData^.KeyInformation.Permissions);
  DeleteSelectedKey1.Enabled := AIsDataAssigned and (rkpDelete in pData^.KeyInformation.Permissions);
end;

procedure TControlFormRegistryManager.PopupValuesPopup(Sender: TObject);
begin
  TOptixVCLHelper.UpdatePopupMenuRootItemsVisibility(TPopupMenu(Sender), not String.IsNullOrWhiteSpace(FCurrentKeyPath));

  ///
  NewKey1.Enabled := rkpCreateSubKey in FCurrentKeyPermissions;

  // New Values
  NewStringValue1.Enabled          := rkpSetValue in FCurrentKeyPermissions;
  NewMultiLineStringValue1.Enabled := NewStringValue1.Enabled;
  NewDWORDValue1.Enabled           := NewStringValue1.Enabled;
  NewQWORDValue1.Enabled           := NewStringValue1.Enabled;
  NewBinaryValue1.Enabled          := NewStringValue1.Enabled;

  // Edit Values
  EditSelectedValue1.Enabled       := NewStringValue1.Enabled;
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
    Result := CompareObjectAssignement(pData1^.KeyInformation, pData2^.KeyInformation)
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

procedure TControlFormRegistryManager.VSTValuesCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PValuesTreeData(Node1.GetData);
  var pData2 := PValuesTreeData(Node2.GetData);

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := ComparePointerAssigmenet(pData1, pData2)
  else if not Assigned(pData1^.ValueInformation) or not Assigned(pData2^.ValueInformation) then
    Result := CompareObjectAssignement(pData1^.ValueInformation, pData2^.ValueInformation)
  else begin
    case Column of
      0 : Result := CompareText(pData1^.ValueInformation.Name, pData2^.ValueInformation.Name);
      1 : Result := CompareValue(pData1^.ValueInformation._Type, pData2^.ValueInformation._Type);
      2 : Result := CompareText(pData1^.ValueAsString, pData2^.ValueAsString);
    end;
  end;
end;

procedure TControlFormRegistryManager.VSTValuesDblClick(Sender: TObject);
begin
  EditSelectedValue1Click(EditSelectedValue1);
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

      1 : CellText := TRegistryHelper.ValueKindToString(pData^.ValueInformation._Type);

      2 : begin
        // TODO Ternary op when Delphi 13 CE released
        if pData^.ValueInformation.IsDefault and (pData^.ValueAsString = '') then
          CellText := '(value not set)'
        else
          CellText := pData^.ValueAsString.Replace(#13#10, '\0', [rfReplaceAll]);
      end;
    end;
  end;
end;

end.
