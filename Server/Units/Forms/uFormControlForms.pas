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

{
  TODO:
    - Column Sorting (Default Sorting)
}

unit uFormControlForms;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, __uBaseFormControl__,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.Menus, Vcl.ExtCtrls;

type
  TTreeData = record
    Title           : String;
    ClassName       : String;
    FormInformation : TFormControlInformation;
    Cycle           : UInt64;
  end;
  PTreeData = ^TTreeData;

  TFormControlForms = class(TBaseFormControl)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    Refresh1: TMenuItem;
    TimerRefresh: TTimer;
    N1: TMenuItem;
    Purge1: TMenuItem;
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
  private
    FCycle      : UInt64;
    FClientData : Pointer;

    {@M}
    procedure Refresh(const AStartRefreshTimer : Boolean = False);
    function GetNodeByGUID(const AGUID : TGUID) : PVirtualNode;
  public
    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String; const pClientData : Pointer);
  end;

var
  FormControlForms: TFormControlForms;

implementation

uses uFormMain, Optix.Helper, Generics.Collections, Optix.Constants;

{$R *.dfm}

function TFormControlForms.GetNodeByGUID(const AGUID : TGUID) : PVirtualNode;
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

procedure TFormControlForms.PopupMenuChange(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
  self.Purge1.Visible := VST.FocusedNode <> nil;
end;

procedure TFormControlForms.Purge1Click(Sender: TObject);
begin
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

    pNodeClientData.Forms.Remove(AForm);

    ///
    break;
  end;

  ///
  Refresh();
end;

constructor TFormControlForms.Create(AOwner : TComponent; const AUserIdentifier : String; const pClientData : Pointer);
begin
  inherited Create(AOwner, AUserIdentifier);
  ///

  FClientData  := pClientData;
  FSpecialForm := True;
  FCycle       := 0;
end;

procedure TFormControlForms.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  TimerRefresh.Enabled := False;
end;

procedure TFormControlForms.FormShow(Sender: TObject);
begin
  Refresh(True);
end;

procedure TFormControlForms.Refresh(const AStartRefreshTimer : Boolean = False);
begin
  var pClientNodeData := uFormMain.PTreeData(FClientData);

  if not Assigned(pClientNodeData) or
     not Assigned(pClientNodeData^.Forms) then begin
    VST.Clear();
    Exit();
  end;

  Inc(FCycle);

  VST.BeginUpdate();
  try
    // -- Create or update forms -- //
    for var AForm in pClientNodeData^.Forms do begin
      if AForm.SpecialForm then
        continue;
      ///

      var pNode := GetNodeByGUID(AForm.FormInformation.GUID);
      if not Assigned(pNode) then
        pNode := VST.AddChild(nil);

      var pData := PTreeData(pNode.GetData);
      if not Assigned(pData^.FormInformation) then begin
        pData^.FormInformation := TFormControlInformation.Create();

        // Title & ClassName never changes
        pData^.Title := AForm.Caption;
        pData^.ClassName := AForm.ClassName;
      end;

      pData^.FormInformation.Assign(AForm.FormInformation);

      ///
      pData^.Cycle := FCycle;
    end;

    var ANodeArray := TList<PVirtualNode>.Create();
    try
      // -- Delete disapeared forms -- //
      for var pNode in VST.Nodes do begin
        var pData := PTreeData(pNode.GetData);
        if pData^.Cycle <> FCycle then
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

procedure TFormControlForms.Refresh1Click(Sender: TObject);
begin
  Refresh();
end;

procedure TFormControlForms.TimerRefreshTimer(Sender: TObject);
begin
  Refresh();
end;

procedure TFormControlForms.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData^.FormInformation) then
    Exit();

  var AColor := clNone;

  if pData^.FormInformation.HasUnseenData then
    AColor := COLOR_LIST_BLUE
  else if pData^.FormInformation.HasFocus then
    AColor := COLOR_LIST_LIMY;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormControlForms.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormControlForms.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormControlForms.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  if Assigned(pData^.FormInformation) then
    FreeAndNil(pData^.FormInformation);
end;

procedure TFormControlForms.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if Column <> 0 then
    Exit();
  ///

  var pData := PTreeData(Node.GetData);

  if not Assigned(pData^.FormInformation) then
    Exit();

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected : begin
      if pData^.FormInformation.HasUnseenData then
        ImageIndex := IMAGE_FORM_CONTROL_DATA
      else if pData^.FormInformation.HasFocus then
        ImageIndex := IMAGE_FORM_CONTROL_ACTIVE
      else
        ImageIndex := IMAGE_FORM_CONTROL;
    end;
  end;
end;

procedure TFormControlForms.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormControlForms.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if not Assigned(pData^.FormInformation) then
    Exit();

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
    5 : CellText := '';
    6 : CellText := pData^.FormInformation.GUID.ToString;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

end.
