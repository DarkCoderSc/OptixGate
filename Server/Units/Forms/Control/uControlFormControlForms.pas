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

unit uControlFormControlForms;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Forms, Vcl.Dialogs, Vcl.Graphics, Vcl.Controls, Vcl.Menus, Vcl.ExtCtrls,

  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees,  VirtualTrees.Types,

   __uBaseFormControl__;
// ---------------------------------------------------------------------------------------------------------------------

type
  TTreeData = record
    Title              : String;
    ClassName          : String;
    ContextDescription : String;
    FormInformation    : TFormControlInformation;
    Special            : Boolean;
    Tick               : UInt64;
  end;
  PTreeData = ^TTreeData;

  TControlFormControlForms = class(TBaseFormControl)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    Refresh1: TMenuItem;
    TimerRefresh: TTimer;
    N1: TMenuItem;
    Purge1: TMenuItem;
    Show1: TMenuItem;
    N2: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure FormShow(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure Refresh1Click(Sender: TObject);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenuChange(Sender: TObject; Source: TMenuItem;
      Rebuild: Boolean);
    procedure Purge1Click(Sender: TObject);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure Show1Click(Sender: TObject);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
  private
    FTick       : UInt64;
    FClientData : Pointer;

    {@M}
    procedure Refresh(const AStartRefreshTimer : Boolean = False);
    function GetNodeByGUID(const AGUID : TGUID) : PVirtualNode;
    function GetFormByGUID(const AGUID : TGUID) : TBaseFormControl;
    function GetSelectedNodeGUID() : TGUID;
  public
    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String; const pClientData : Pointer); reintroduce;
  end;

var
  ControlFormControlForms: TControlFormControlForms;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math, System.DateUtils,

  uFormMain,

  Optix.Helper, Generics.Collections, Optix.Constants, Optix.VCL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

function TControlFormControlForms.GetNodeByGUID(const AGUID : TGUID) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    ///

    if not Assigned(pData.FormInformation) or
      (pData^.FormInformation.GUID <> AGUID) then
      continue;

    result := pNode;

    break;
  end;
end;

procedure TControlFormControlForms.PopupMenuChange(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
  var pNode := VST.FocusedNode;
  var pData : PTreeData := nil;
  if Assigned(pNode) then
    pData := PTreeData(pNode.GetData);

  ///
  self.Purge1.Visible := Assigned(pData) and not pData^.Special;
end;

function TControlFormControlForms.GetSelectedNodeGUID() : TGUID;
begin
  result := TGUID.Empty;
  ///

  var pData := PTreeData(VST.FocusedNode.GetData);

  if not Assigned(pData^.FormInformation) then
    Exit();

  result := pData^.FormInformation.GUID;
end;

function TControlFormControlForms.GetFormByGUID(const AGUID : TGUID) : TBaseFormControl;
begin
  result := nil;
  ///

  var pNodeClientData := uFormMain.PTreeData(FClientData);
  ///

  if (VST.FocusedNode = nil) or (not Assigned(pNodeClientData^.Forms)) then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);

  if not Assigned(pData^.FormInformation) then
    Exit();

  for var AForm in pNodeClientData^.Forms do begin
    if not Assigned(pData^.FormInformation) or
       (AForm.GUID <> pData^.FormInformation.GUID ) then
      continue;
      ///

    result := AForm;

    ///
    break;
  end;
end;

procedure TControlFormControlForms.Purge1Click(Sender: TObject);
begin
  var ATargetGUID := GetSelectedNodeGUID();
  if ATargetGUID.IsEmpty then
    Exit();
  ///

  if Application.MessageBox(
    'Purging this control form will permanently remove the form instance and all' +
    ' associated data, including possible cached data. Do you want to continue?',
    'Purge Window',
    MB_ICONQUESTION + MB_YESNO
  ) = ID_NO then
    Exit();
  ///

  var pNodeClientData := uFormMain.PTreeData(FClientData);
  ///

  if not Assigned(pNodeClientData^.Forms) then
    Exit();

  var AForm := GetFormByGUID(ATargetGUID);
  if Assigned(AForm) then begin
    AForm.PurgeRequest();

    pNodeClientData.Forms.Remove(AForm);
  end;

  ///
  Refresh();
end;

constructor TControlFormControlForms.Create(AOwner : TComponent; const AUserIdentifier : String; const pClientData : Pointer);
begin
  inherited Create(AOwner, AUserIdentifier, True);
  ///

  FClientData  := pClientData;
  FTick        := 0;
end;

procedure TControlFormControlForms.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  TimerRefresh.Enabled := False;
end;

procedure TControlFormControlForms.FormShow(Sender: TObject);
begin
  Refresh(True);
end;

procedure TControlFormControlForms.Refresh(const AStartRefreshTimer : Boolean = False);
begin
  var pClientNodeData := uFormMain.PTreeData(FClientData);

  if not Assigned(pClientNodeData) or
     not Assigned(pClientNodeData^.Forms) then begin
    VST.Clear();
    Exit();
  end;

  Inc(FTick);

  VST.BeginUpdate();
  try
    // -- Create or update forms -- //
    for var AForm in pClientNodeData^.Forms do begin
      var pNode := GetNodeByGUID(AForm.FormInformation.GUID);
      if not Assigned(pNode) then
        pNode := VST.AddChild(nil);

      var pData := PTreeData(pNode.GetData);
      if not Assigned(pData^.FormInformation) then begin
        pData^.FormInformation := TFormControlInformation.Create();

        pData^.Title := AForm.Caption;
        pData^.ClassName := AForm.ClassName;
        pData^.Special := AForm.SpecialForm;
      end;

      pData^.FormInformation.Assign(AForm.FormInformation);
      pData^.ContextDescription := AForm.ContextInformation;

      ///
      pData^.Tick := FTick;
    end;

    var ANodeArray := TList<PVirtualNode>.Create();
    try
      // -- Delete disapeared forms -- //
      for var pNode in VST.Nodes do begin
        var pData := PTreeData(pNode.GetData);
        if pData^.Tick <> FTick then
          ANodeArray.Add(pNode);
      end;
    finally
      if ANodeArray.Count > 0 then
        VST.DeleteNodes(TNodeArray(ANodeArray.ToArray()));

      ///
      FreeAndNil(ANodeArray);
    end;
  finally
    VST.EndUpdate();
  end;

  ///
  if AStartRefreshTimer then
    TimerRefresh.Enabled := True;
end;

procedure TControlFormControlForms.Refresh1Click(Sender: TObject);
begin
  Refresh();
end;

procedure TControlFormControlForms.Show1Click(Sender: TObject);
begin
  var ATargetGUID := GetSelectedNodeGUID();
  if ATargetGUID.IsEmpty then
    Exit();
  ///

  var AForm := GetFormByGUID(ATargetGUID);
  if Assigned(AForm) then
    TOptixVCLHelper.ShowForm(AForm);
end;

procedure TControlFormControlForms.TimerRefreshTimer(Sender: TObject);
begin
  Refresh();
end;

procedure TControlFormControlForms.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) or not Assigned(pData^.FormInformation) then
    Exit();
  ///

  var AColor := clNone;

  // Priority:
  //  - Unseen Data
  //  - Focused Form
  //  - Closed Form

  if pData^.FormInformation.HasUnseenData then
    AColor := COLOR_LIST_BLUE
  else if pData^.FormInformation.HasFocus then
    AColor := COLOR_LIST_LIMY
  else if pData^.FormInformation.State = fcsClosed then
    AColor := COLOR_LIST_GRAY;


  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TControlFormControlForms.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormControlForms.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := 0
  else begin
    if (Column in [2..4, 6]) and (not Assigned(pData1^.FormInformation) or not Assigned(pData2^.FormInformation)) then
      Result := CompareObjectAssigmenet(pData1^.FormInformation, pData2^.FormInformation)
    else begin
      case Column of
        0 : Result := CompareText(pData1^.Title, pData2^.Title);
        1 : Result := CompareText(pData1^.ClassName, pData2^.ClassName);
        2 : Result := CompareValue(Cardinal(pData1^.FormInformation.State), Cardinal(pData2^.FormInformation.State));
        3 : Result := CompareDateTime(pData1^.FormInformation.CreatedTime, pData2^.FormInformation.CreatedTime);

        4 : Result := CompareDateTime(
                        pData1^.FormInformation.LastReceivedDataTime,
                        pData2^.FormInformation.LastReceivedDataTime
                      );

        5 : Result := CompareText(pData1^.ContextDescription, pData2^.ContextDescription);

        6 : Result := CompareText(
                        pData1^.FormInformation.GUID.ToString,
                        pData2^.FormInformation.GUID.ToString
                      );
      end;
    end;
  end;
end;

procedure TControlFormControlForms.VSTDblClick(Sender: TObject);
begin
  Show1Click(Show1);
end;

procedure TControlFormControlForms.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormControlForms.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  if Assigned(pData) and Assigned(pData^.FormInformation) then
    FreeAndNil(pData^.FormInformation);
end;

procedure TControlFormControlForms.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) or not Assigned(pData^.FormInformation) or (Column <> 0) then
    Exit();
  ///

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected : begin
      if pData^.FormInformation.HasUnseenData then
        ImageIndex := IMAGE_FORM_CONTROL_DATA
      else if pData^.FormInformation.HasFocus then
        ImageIndex := IMAGE_FORM_CONTROL_ACTIVE
      else if pData^.FormInformation.State = fcsClosed then
        ImageIndex := IMAGE_FORM_CONTROL_CLOSED
      else
        ImageIndex := IMAGE_FORM_CONTROL;
    end;

    TVTImageKind.ikState : begin
      if pData^.Special then
        ImageIndex := IMAGE_STAR_FILLED
      else
        ImageIndex := IMAGE_STAR_EMPTY;
    end;
  end;
end;

procedure TControlFormControlForms.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TControlFormControlForms.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData) and Assigned(pData^.FormInformation) then begin
    case Column of
      0 : CellText := pData^.Title;
      1 : CellText := pData^.ClassName;
      2 : CellText := FormControlStateToString(pData^.FormInformation.State);
      3 : CellText := DateTimeToStr(pData^.FormInformation.CreatedTime);
      4 :begin
        if pData^.FormInformation.HasReceivedData then
          CellText := ElapsedDateTime(
            pData^.FormInformation.LastReceivedDataTime,
            Now
          );
      end;
      5 : CellText := pData^.ContextDescription;
      6 : CellText := pData^.FormInformation.GUID.ToString;
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

end.
