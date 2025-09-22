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

unit uControlFormTasks;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.Types,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,

  VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees.Types,

  __uBaseFormControl__,

  Optix.Func.Commands.Base, Optix.Protocol.Packet;
// ---------------------------------------------------------------------------------------------------------------------

type
  TTreeData = record
    TaskCallBack : TOptixTaskCallback;
    Created      : TDateTime;
    Ended        : TDateTime;
    HasEnded     : Boolean;
  end;
  PTreeData = ^TTreeData;

  TControlFormTasks = class(TBaseFormControl)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    Action1: TMenuItem;

    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure PopupMenuPopup(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Action1Click(Sender: TObject);
    { Private declarations }
  private
    {@M}
    function GetNodeByTaskId(const ATaskId : TGUID) : PVirtualNode;
    function GetSelectedTaskCallBack(const AFilterState : TOptixTaskStates = []) : TOptixTaskCallBack;
    function GetSelectedSucceededTaskCallBack() : TOptixTaskCallBack;
  public
    {@M}
    procedure ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean); override;
  end;

var
  ControlFormTasks: TControlFormTasks;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math, System.DateUtils,

  VCL.FileCtrl,

  uFormMain,

  Optix.Helper, Optix.Constants, Optix.VCL.Helper, Optix.Task.ProcessDump;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormTasks.FormDestroy(Sender: TObject);
begin
  VST.Clear();
end;

function TControlFormTasks.GetNodeByTaskId(const ATaskId : TGUID) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);

    if Assigned(pData^.TaskCallBack) and (pData^.TaskCallBack.Id = ATaskId) then begin
      result := pNode;

      break;
    end;
  end;
end;

function TControlFormTasks.GetSelectedTaskCallBack(const AFilterState : TOptixTaskStates = []) : TOptixTaskCallBack;
begin
  result := nil;
  ///

  if VST.FocusedNode = nil then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);
  if not Assigned(pData^.TaskCallBack) then
    Exit();

  if (pData^.TaskCallBack.State in AFilterState) or (AFilterState = []) then
    result := pData^.TaskCallBack;
end;

function TControlFormTasks.GetSelectedSucceededTaskCallBack() : TOptixTaskCallBack;
begin
  result := GetSelectedTaskCallBack([otsSuccess]);
end;

procedure TControlFormTasks.Action1Click(Sender: TObject);
begin
  var ACallBack := GetSelectedSucceededTaskCallBack();
  if not Assigned(ACallBack) or not Assigned(ACallBack.Result) then
    Exit();
  ///

  // -------------------------------------------------------------------------------------------------------------------
  if ACallBack.Result is TOptixProcessDumpTaskResult then begin
    var ADirectory := '';

    if not SelectDirectory('Select destination', '', ADirectory) then
      Exit();

    RequestFileDownload(
      TOptixProcessDumpTaskResult(ACallBack.Result).OutputFilePath,
      IncludeTrailingPathDelimiter(ADirectory) + TOptixProcessDumpTaskResult(ACallBack.Result).DisplayName + '.dmp',
      'Process Dump'
    );
  end;
  // -------------------------------------------------------------------------------------------------------------------
end;

procedure TControlFormTasks.PopupMenuPopup(Sender: TObject);
begin
  Action1.Visible := False;
  ///

  var ACallBack := GetSelectedSucceededTaskCallBack();
  if not Assigned(ACallBack) or not Assigned(ACallBack.Result) then
    Exit();

  case ACallBack.State of
    otsPending :;
    otsRunning :;
    otsFailed  :;
    otsSuccess : begin
      Action1.Visible := True;
      // ---------------------------------------------------------------------------------------------------------------
      if ACallBack.Result is TOptixProcessDumpTaskResult then
        Action1.Caption := 'Download Process Dump File'
      // ---------------------------------------------------------------------------------------------------------------
      else
        Action1.Visible := False;
      // ---------------------------------------------------------------------------------------------------------------
    end;
  end;
end;

procedure TControlFormTasks.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  inherited;
  ///

  if not (AOptixPacket is TOptixTaskCallback) then
    Exit();
  ///

  var ATaskResult := TOptixTaskCallback(AOptixPacket);
  var pNode := GetNodeByTaskId(ATaskResult.Id);
  var pData : PTreeData;

  VST.BeginUpdate();
  try
    if not Assigned(pNode) then begin
      pNode := VST.AddChild(nil);
      pData := pNode.GetData;
      pData^.TaskCallBack := nil; // Ensure
      pData^.Created := Now;
      pData^.HasEnded := False;

      ///
      TOptixVCLHelper.ShowForm(self);
    end else
      pData := pNode.GetData;

    if Assigned(pData^.TaskCallBack) then
      FreeAndNil(pData^.TaskCallBack);

    pData^.TaskCallBack := ATaskResult;

    AHandleMemory := True;

    if (pData^.TaskCallBack.State = otsFailed) or (pData^.TaskCallBack.State = otsSuccess) then begin
      pData^.Ended := Now;
      pData^.HasEnded := True;
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TControlFormTasks.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormTasks.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);

  function GetStateOrder(const AState: TOptixTaskState): Integer;
  begin
    case AState of
      otsRunning : result := 0;
      otsSuccess : result := 1;
      otsPending : result := 2;
      otsFailed  : result := 3;

      else
        result := 4;
    end;
  end;

begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if (not Assigned(pData1) or not Assigned(pData2)) or
     (not Assigned(pData1^.TaskCallBack) or not Assigned(pData2^.TaskCallBack)) then
    Result := 0
  else begin
    case Column of
      0 : Result := CompareText(pData1^.TaskCallBack.Id.ToString, pData2^.TaskCallBack.Id.ToString);
      1 : Result := CompareText(pData1^.TaskCallBack.TaskClassName, pData2^.TaskCallBack.TaskClassName);

      2 : begin
        var AOrder1 := GetStateOrder(pData1^.TaskCallBack.State);
        var AOrder2 := GetStateOrder(pData2^.TaskCallBack.State);
        ///

        Result := AOrder1 - AOrder2;
      end;

      3 : Result := CompareDateTime(pData1^.Created, pData2^.Created);
      4 : Result := CompareDateTimeEx(pData1^.Ended, pData1^.HasEnded, pData2^.Ended, pData2^.HasEnded);

      5 : begin
        if not Assigned(pData1^.TaskCallBack.Result) or not Assigned(pData2^.TaskCallBack.Result) then
          Result := CompareObjectAssigmenet(pData1^.TaskCallBack.Result, pData2^.TaskCallBack.Result)
        else
          Result := CompareText(pData1^.TaskCallBack.Result.Description, pData2^.TaskCallBack.Result.Description);
      end;
    end;
  end;
end;

procedure TControlFormTasks.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormTasks.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  if Assigned(pData) and Assigned(pData^.TaskCallBack) then
    FreeAndNil(pData^.TaskCallBack);
end;

procedure TControlFormTasks.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) or not Assigned(pData^.TaskCallBack) or (Column <> 0) then
    Exit();
  ///

  case Kind of
    ikNormal, ikSelected : begin
      case pData^.TaskCallBack.State of
        otsRunning : ImageIndex := IMAGE_TASK_RUNNING;
        otsFailed  : ImageIndex := IMAGE_TASK_FAILED;
        otsSuccess : ImageIndex := IMAGE_TASK_SUCCESS;

        else
          ImageIndex := IMAGE_TASK_PENDING;
      end;
    end;

    ikState: ;
    ikOverlay: ;
  end;
end;

procedure TControlFormTasks.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TControlFormTasks.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData) and Assigned(pData^.TaskCallBack) then begin
    case Column of
      0 : CellText := pData^.TaskCallBack.Id.ToString();
      1 : CellText := pData^.TaskCallBack.TaskClassName;
      2 : begin
        case pData^.TaskCallBack.State of
          otsPending : CellText := 'Pending';
          otsRunning : CellText := 'Running';
          otsFailed  : CellText := 'Failed';
          otsSuccess : CellText := 'Success';
        end;
      end;
      3 : CellText := DateTimeToStr(pData^.Created);
      4 : begin
        if pData^.HasEnded then
          CellText := DateTimeToStr(pData^.Ended);
      end;
      5 : begin
        if Assigned(pData^.TaskCallBack.Result) then
          CellText := pData^.TaskCallBack.Result.Description;
      end;
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TControlFormTasks.VSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TBaseVirtualTree(Sender).GetNodeAt(Point(X, Y)) = nil then begin
    TBaseVirtualTree(Sender).ClearSelection();

    TBaseVirtualTree(Sender).FocusedNode := nil;
  end;
end;

end.
