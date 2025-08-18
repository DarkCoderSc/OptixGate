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

unit uFormTasks;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, __uBaseFormControl__, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree,
  VirtualTrees.AncestorVCL, VirtualTrees, XSuperObject, Optix.Task, Vcl.Menus, VirtualTrees.Types;

type
  TTreeData = record
    TaskCallBack : TOptixTaskCallback;
    Created      : TDateTime;
    Ended        : TDateTime;
    HasEnded     : Boolean;
  end;
  PTreeData = ^TTreeData;

  TFormTasks = class(TBaseFormControl)
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
    procedure PopupMenuPopup(Sender: TObject);  private
    { Private declarations }
  private
    {@M}
    function GetNodeByTaskId(const ATaskId : TGUID) : PVirtualNode;
    function GetSelectedTaskCallBack(const AFilterState : TOptixTaskStates = []) : TOptixTaskCallBack;
    function GetSelectedSucceededTaskCallBack() : TOptixTaskCallBack;
    procedure DownloadProcessDumpFile(Sender : TObject);
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;
  end;

var
  FormTasks: TFormTasks;

implementation

uses uFormMain, Optix.Helper, Optix.Constants, Optix.VCL.Helper, Optix.Task.ProcessDump, VCL.FileCtrl;

{$R *.dfm}

function TFormTasks.GetNodeByTaskId(const ATaskId : TGUID) : PVirtualNode;
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

function TFormTasks.GetSelectedTaskCallBack(const AFilterState : TOptixTaskStates = []) : TOptixTaskCallBack;
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

function TFormTasks.GetSelectedSucceededTaskCallBack() : TOptixTaskCallBack;
begin
  result := GetSelectedTaskCallBack([otsSuccess]);
end;

procedure TFormTasks.DownloadProcessDumpFile(Sender : TObject);
begin
  var ACallBack := GetSelectedSucceededTaskCallBack();
  if not Assigned(ACallBack) or not Assigned(ACallBack.Result) and (ACallBack.Result is TOptixProcessDumpTaskResult) then
    Exit();
  ///

  var ADirectory := '';

  if not SelectDirectory('Select destination', '', ADirectory) then
    Exit();

  RequestFileDownload(
    TOptixProcessDumpTaskResult(ACallBack.Result).OutputFilePath,
    IncludeTrailingPathDelimiter(ADirectory) + TOptixProcessDumpTaskResult(ACallBack.Result).DisplayName + '.dmp',
    'Process Dump'
  );
end;

procedure TFormTasks.PopupMenuPopup(Sender: TObject);
begin
  Action1.Visible := False;
  Action1.Caption := '';
  Action1.OnClick := nil;
  ///

  var ACallBack := GetSelectedTaskCallBack([]);
  if not Assigned(ACallBack) then
    Exit();

  case ACallBack.State of
    otsPending :;
    otsRunning :;
    otsFailed  :;
    otsSuccess : begin
      if ACallBack.TaskClassName = TOptixProcessDumpTask.ClassName then begin
        Action1.Visible := True;
        Action1.Caption := 'Download Process Dump File';
        Action1.OnClick := DownloadProcessDumpFile;
      end;
    end;
  end;
end;

procedure TFormTasks.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  if not Assigned(ASerializedPacket) or (AClassName <> TOptixTaskCallback.ClassName) then
    Exit();
  ///

  var ATaskResult := TOptixTaskCallback.Create(ASerializedPacket);
  var pNode := GetNodeByTaskId(ATaskResult.Id);
  var pData : PTreeData;

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

  if (pData^.TaskCallBack.State = otsFailed) or (pData^.TaskCallBack.State = otsSuccess) then begin
    pData^.Ended := Now;
    pData^.HasEnded := True;
  end;

  ///
  VST.Refresh();
end;

procedure TFormTasks.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormTasks.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormTasks.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);

  if Assigned(pData) and Assigned(pData^.TaskCallBack) then
    FreeAndNil(pData^.TaskCallBack);
end;

procedure TFormTasks.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if ((Kind <> TVTImageKind.ikNormal) and (Kind <> TVTImageKind.ikSelected)) or (Column <> 0) then
    Exit();
  ///

  var pData := PTreeData(Node.GetData);
  if not Assigned(pData^.TaskCallBack) then
    Exit();

  case pData^.TaskCallBack.State of
    otsRunning : ImageIndex := IMAGE_TASK_RUNNING;
    otsFailed  : ImageIndex := IMAGE_TASK_FAILED;
    otsSuccess : ImageIndex := IMAGE_TASK_SUCCESS;

    else
      ImageIndex := IMAGE_TASK_PENDING;
  end;

end;

procedure TFormTasks.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormTasks.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData^.TaskCallBack) then begin
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

end.
