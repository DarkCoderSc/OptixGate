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
{                   License: Apache License 2.0                                }
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
    - Display + Filter Process Architecture (x86-32 / x86-64)
    - Filter Out-Of-Privilege Processes (Check Thread Count, Elevation Status, Missing Username to guess)
    - Highlight and iconize NTA/SYSTEM process like for S.I.N
    - Same for Elevated Process
    - Kill Process
    - Dump Process to File
}

unit uFormProcessManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, __uBaseFormControl__, Vcl.ComCtrls,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.Menus, XSuperObject, Optix.Func.Enum.Process;

type
  TTreeData = record
    ProcessInformation : TProcessInformation;
  end;
  PTreeData = ^TTreeData;

  TFormProcessManager = class(TBaseFormControl)
    PopupMenu: TPopupMenu;
    VST: TVirtualStringTree;
    Refresh1: TMenuItem;
    procedure Refresh1Click(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
  private
    {@M}
    procedure Refresh(const AProcessList : TProcessList);
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;
  end;

var
  FormProcessManager: TFormProcessManager;

implementation

uses uFormMain, Optix.Func.Commands, Optix.Helper,
     Optix.InformationGathering.Process, Optix.Constants;

{$R *.dfm}

procedure TFormProcessManager.Refresh(const AProcessList : TProcessList);
begin
  if not Assigned(AProcessList) then
    Exit();
  ///

  VST.Clear();

  VST.BeginUpdate();
  try
    for var AProcessInformation in AProcessList.List do begin
      var pNode := VST.AddChild(nil);
      var pData := PTreeData(pNode.GetData);

      pData^.ProcessInformation := TProcessInformation.Create(AProcessInformation);
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormProcessManager.Refresh1Click(Sender: TObject);
begin
  SendCommand(TOptixRefreshProcess.Create());
end;

procedure TFormProcessManager.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);

  if not Assigned(pData^.ProcessInformation) then
    Exit();

  var AColor := clNone;

  if pData^.ProcessInformation.IsCurrentProcess then
    AColor := COLOR_LIST_LIMY
  else begin
    // ... Other checks (TODO) ... //
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormProcessManager.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessManager.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessManager.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  ///

  if Assigned(pData^.ProcessInformation) then
    FreeAndNil(pData^.ProcessInformation);
end;

procedure TFormProcessManager.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData^.ProcessInformation) or (Column <> 0) then
    Exit();
  ///

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected: begin
      if pData^.ProcessInformation.IsCurrentProcess then
        ImageIndex := IMAGE_ALIEN
      else begin
        ImageIndex := IMAGE_PROCESS;

        // ... Other checks (TODO) ... //
      end;
    end;
  end;
end;

procedure TFormProcessManager.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormProcessManager.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if not Assigned(pData^.ProcessInformation) then
    Exit();

  case Column of
    0  : CellText := ExtractFileName(pData^.ProcessInformation.ImagePath);
    1  : CellText := FormatInt(pData^.ProcessInformation.Id);
    2  : CellText := FormatInt(pData^.ProcessInformation.ParentId);
    3  : CellText := IntToStr(pData^.ProcessInformation.ThreadCount);
    4  : CellText := pData^.ProcessInformation.Username;
    5  : CellText := pData^.ProcessInformation.Domain;
    6  : CellText := IntToStr(pData^.ProcessInformation.SessionId);
    7  : CellText := ElevatedStatusToString(pData^.ProcessInformation.Elevated);
    8  : CellText := DateTimeToStr(pData^.ProcessInformation.CreatedTime);
    9  : CellText := pData^.ProcessInformation.ImagePath;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TFormProcessManager.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  if AClassName = 'TProcessList' then begin
    var AProcessList := TProcessList.Create(ASerializedPacket);
    try
      Refresh(AProcessList);
    finally
      FreeAndNil(AProcessList);
    end;
  end;
end;

end.
