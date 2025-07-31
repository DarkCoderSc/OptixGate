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
  VirtualTrees.AncestorVCL, VirtualTrees, XSuperObject, Optix.Task;

type
  TTreeData = record
    TaskResult : TOptixTaskResult;
  end;
  PTreeData = ^TTreeData;

  TFormTasks = class(TBaseFormControl)
    VST: TVirtualStringTree;

    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);  private
    { Private declarations }
  private
    {@M}
    function GetNodeByTaskId(const ATaskId : TGUID) : PVirtualNode;
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;
  end;

var
  FormTasks: TFormTasks;

implementation

uses Optix.Helper;

{$R *.dfm}

function TFormTasks.GetNodeByTaskId(const ATaskId : TGUID) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);

    if Assigned(pData^.TaskResult) and (pData^.TaskResult.Id = ATaskId) then begin
      result := pNode;

      break;
    end;
  end;
end;

procedure TFormTasks.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  if not Assigned(ASerializedPacket) or (AClassName <> TOptixTaskResult.ClassName) then
    Exit();
  ///

  var ATaskResult := TOptixTaskResult.Create(ASerializedPacket);
  var pNode := GetNodeByTaskId(ATaskResult.Id);
  var pData : PTreeData := nil;

  if not Assigned(pNode) then begin
    pNode := VST.AddChild(nil);
    pData := pNode.GetData;
    pData^.TaskResult := nil; // Ensure
  end else
    pData := pNode.GetData;

  if Assigned(pData^.TaskResult) then
    FreeAndNil(pData^.TaskResult);

  pData^.TaskResult := ATaskResult;

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

  if Assigned(pData) and Assigned(pData^.TaskResult) then
    FreeAndNil(pData^.TaskResult);
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

  if Assigned(pData^.TaskResult) then begin
    case Column of
      0 : CellText := pData^.TaskResult.Id.ToString();
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

end.
