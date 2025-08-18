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

unit uFormDebugThreads;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.ExtCtrls, VirtualTrees.Types, Vcl.Menus;

type
  TTreeData = record
    Guid         : TGUID;
    Id           : Cardinal;
    ClassName    : String;
    {$WARN SYMBOL_PLATFORM OFF}
    Priority     : TThreadPriority;
    {$WARN SYMBOL_PLATFORM ON}
    CreatedTime  : TDateTime;
    Running      : Boolean;
    Tick         : UInt64;
    TerminateReq : Boolean;
  end;
  PTreeData = ^TTreeData;

  TFormDebugThreads = class(TForm)
    VST: TVirtualStringTree;
    TimerRefresh: TTimer;
    PopupMenu: TPopupMenu;
    Terminate1: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure PopupMenuPopup(Sender: TObject);
    procedure Terminate1Click(Sender: TObject);
  private
    {@M}
    FRefreshTick : UInt64;

    procedure RefreshThreads();
    function GetNodeByGUID(const AGuid : TGUID) : PVirtualNode;
  public
    { Public declarations }
  end;

var
  FormDebugThreads: TFormDebugThreads;

implementation

uses Optix.Thread, Optix.Constants, Optix.Helper, System.StrUtils, uFormMain, Optix.Protocol.SessionHandler,
     Optix.Protocol.Worker.FileTransfer {$IFDEF SERVER}, Optix.Protocol.Server{$ENDIF};

{$R *.dfm}

function TFormDebugThreads.GetNodeByGUID(const AGuid : TGUID) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);

    if pData^.Guid = AGuid then begin
      result := pNode;

      break;
    end;
  end;
end;

procedure TFormDebugThreads.PopupMenuPopup(Sender: TObject);
begin
  var pNode := VST.FocusedNode;
  var pData : PTreeData := nil;
  if Assigned(pNode) then
    pData := pNode.GetData;

  self.Terminate1.Visible := Assigned(pData) and (pData^.Running and not pData^.TerminateReq);
end;

procedure TFormDebugThreads.RefreshThreads();
begin
  VST.BeginUpdate();
  try
    Inc(FRefreshTick);
    ///

    var AList := OPTIX_THREAD_HIVE.LockList();
    try
      for var AThread in AList do begin
        if not Assigned(AThread) then
          continue;
        ///

        var pNode := GetNodeByGUID(AThread.Guid);
        var ACreated := False;

        if not Assigned(pNode) then begin
          pNode := VST.AddChild(nil);

          ACreated := True;
        end;

        var pData := PTreeData(pNode.GetData);

        if ACreated then begin
          pData^.Guid         := AThread.Guid;
          pData^.Id           := AThread.ThreadID;
          pData^.ClassName    := AThread.ClassName;
          pData^.CreatedTime  := AThread.CreatedDate;
          pData^.TerminateReq := False;
        end;

        pData^.Priority := AThread.Priority;
        pData^.Running  := AThread.Running;
        pData^.Tick     := FRefreshTick;

        if pData^.TerminateReq then
          AThread.Terminate();
      end;
    finally
      OPTIX_THREAD_HIVE.UnlockList();
    end;

    // Clean destroyed threads
    for var pNode in VST.Nodes do begin
      var pData := PTreeData(pNode.GetData);
      if pData^.Tick <> FRefreshTick then
        VST.DeleteNode(pNode);
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormDebugThreads.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TimerRefresh.Enabled := False;

  ///
  VST.Clear();
end;

procedure TFormDebugThreads.FormShow(Sender: TObject);
begin
  FRefreshTick := 0;
  VST.Clear();
  ///

  RefreshThreads();

  ///
  TimerRefresh.Enabled := True;
end;

procedure TFormDebugThreads.Terminate1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  var pData := PTreeData(VST.FocusedNode.GetData);
  if pData^.Running then
    pData^.TerminateReq := True;
end;

procedure TFormDebugThreads.TimerRefreshTimer(Sender: TObject);
begin
  RefreshThreads();
end;

procedure TFormDebugThreads.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);

  var AColor := clNone;

  if not pData^.Running then
    AColor := COLOR_LIST_GRAY;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormDebugThreads.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormDebugThreads.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormDebugThreads.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Kind <> TVTImageKind.ikNormal) and (Kind <> TVTImageKind.ikSelected) then
    Exit();

  var pData := PTreeData(Node.GetData);

  case Column of
    0 : begin
      if pData^.TerminateReq then
        ImageIndex := IMAGE_THREAD_STOP_WAIT
      else if pData^.Running then
        ImageIndex := IMAGE_THREAD_RUNNING
      else
        ImageIndex := IMAGE_THREAD_STOPPED;
    end;

    1 : begin
      {$IFDEF SERVER}
      if pData^.ClassName = TOptixServerThread.ClassName then
        ImageIndex := IMAGE_THREAD_SERVER
      else
      {$ENDIF}
      if pData^.ClassName = TOptixSessionHandlerThread.ClassName then
        ImageIndex := IMAGE_THREAD_HANDLER
      else if
      {$IFDEF SERVER}
        pData^.ClassName = TOptixFileTransferWorker.ClassName
      {$ELSE}
        pData^.ClassName = TOptixFileTransferOrchestratorThread.ClassName
      {$ENDIF}
        then
        ImageIndex := IMAGE_THREAD_TRANSFER
      else
        ImageIndex := IMAGE_THREAD_GENERIC;
    end;
  end;

end;

procedure TFormDebugThreads.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormDebugThreads.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  case Column of
    0 : CellText := Format('%d (0x%x)' , [pData^.Id, pData^.id]);
    1 : CellText := pData^.ClassName;
    2 : CellText := IfThen(pData^.Running, 'Yes', 'No');
    3 : CellText := ElapsedDateTime(pData^.CreatedTime, Now);
    4 : CellText := ThreadPriorityToString(pData^.Priority);
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

end.
