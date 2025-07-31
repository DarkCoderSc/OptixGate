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

unit uFormTransfers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, __uBaseFormControl__, Vcl.StdCtrls, Optix.Shared.Protocol.FileTransfer,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees, Vcl.Menus,
  Optix.Func.LogNotifier, VirtualTrees.Types;

type
  TTransferState = (
    tsQueued,
    tsProgress,
    tsEnded,
    tsError,
    tsCancelRequest,
    tsCanceled
  );

  TTreeData = record
    Id                  : TGUID;
    SourceFilePath      : String;
    DestinationFilePath : String;
    Direction           : TOptixTransferDirection;
    FileSize            : Int64;
    Context             : String;
    Description         : String;
    State               : TTransferState;
    WorkCount           : Int64;
    ImageIndex          : Integer;
  end;
  PTreeData = ^TTreeData;

  TFormTransfers = class(TBaseFormControl)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    DownloadaFile1: TMenuItem;
    UploadaFile1: TMenuItem;
    OpenDialog: TOpenDialog;
    N1: TMenuItem;
    CancelTransfer1: TMenuItem;
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure DownloadaFile1Click(Sender: TObject);
    procedure UploadaFile1Click(Sender: TObject);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure PopupMenuPopup(Sender: TObject);
    procedure CancelTransfer1Click(Sender: TObject);
  private
    {@M}
    function GetNodeByTransferId(const ATransferId : TGUID) : PVirtualNode;
    procedure VSTRefresh();
  public
    {@M}
    function RequestFileDownload(const ARemoteFilePath, ALocalFilePath : String; const AContext : String = '') : TGUID;
    function RequestFileUpload(const ALocalFilePath, ARemoteFilePath : String; const AContext : String = '') : TGUID;

    procedure ApplyTransferException(const ATransferId : TGUID; const AReason : String); overload;
    procedure ApplyTransferException(const ALogTransferException : TLogTransferException); overload;

    procedure OnRequestTransferTask(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask);
    procedure OnTransferError(Sender : TObject; const ATransferId : TGUID; const AReason : String);
    procedure OnTransferBegins(Sender : TObject; const ATransferId : TGUID; const AFileSize : Int64);
    procedure OnTransferUpdate(Sender : TObject; const ATransferId : TGUID; const AWorkCount : Int64; var ACanceled : Boolean);
    procedure OnTransferEnds(Sender : TObject; const ATransferId : TGUID);
  end;

var
  FormTransfers: TFormTransfers;

implementation

uses Optix.Func.Commands, Optix.Helper, VCL.FileCtrl, Optix.FileSystem.Helper, System.IOUtils, Optix.Exceptions,
     Optix.Constants, uFormMain, Optix.VCL.Helper;

{$R *.dfm}

procedure TFormTransfers.VSTRefresh();
begin
  VST.SortTree(VST.Header.SortColumn, VST.Header.SortDirection, False);
  VST.Refresh();
end;

procedure TFormTransfers.OnTransferBegins(Sender : TObject; const ATransferId : TGUID; const AFileSize : Int64);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  pData^.State    := tsProgress;
  pData^.FileSize := AFileSize;

  ///
  VSTRefresh();
end;

procedure TFormTransfers.OnTransferUpdate(Sender : TObject; const ATransferId : TGUID; const AWorkCount : Int64; var ACanceled : Boolean);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  ACanceled := pData^.State = tsCancelRequest;

  if pData^.State = tsProgress then begin
    pData^.WorkCount := AWorkCount;

    ///
    VSTRefresh();
  end;
end;

procedure TFormTransfers.PopupMenuPopup(Sender: TObject);
begin
  TOptixVCLHelper.HideAllPopupMenuRootItems(TPopupMenu(Sender));
  ///

  UploadaFile1.Visible   := True;
  DownloadaFile1.Visible := True;

  var pNode := VST.FocusedNode;
  if Assigned(pNode) then begin
    var pData := PTreeData(pNode.GetData);
    CancelTransfer1.Visible := (pData^.State = tsQueued) or (pData^.State = tsProgress);
  end;
end;

procedure TFormTransfers.OnTransferEnds(Sender : TObject; const ATransferId : TGUID);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  if pData^.State = tsCancelRequest then
    pData^.State := tsCanceled
  else
    pData^.State := tsEnded;

  ///
  VSTRefresh();
end;

function TFormTransfers.GetNodeByTransferId(const ATransferId : TGUID) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if pData^.Id <> ATransferId then
      continue;

    result := pNode;

    break;
  end;
end;

procedure TFormTransfers.ApplyTransferException(const ATransferId : TGUID; const AReason : String);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  pData^.State       := tsError;
  pData^.Description := AReason;

  ///
  VSTRefresh();
end;

procedure TFormTransfers.ApplyTransferException(const ALogTransferException : TLogTransferException);
begin
  if not Assigned(ALogTransferException) then
    Exit();
  ///

  ApplyTransferException(ALogTransferException.TransferId, ALogTransferException.LogMessage);
end;

procedure TFormTransfers.OnRequestTransferTask(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask);
begin
  ATask := nil;
  ///

  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  if pData^.State <> tsQueued then
    Exit();

  try
    case pData^.Direction of
      // Server Req File Download
      otdClientIsUploading : ATask := TOptixDownloadTask.Create(pData^.DestinationFilePath);

      // Server Req File Upload
      otdClientIsDownloading : ATask := TOptixUploadTask.Create(pData^.SourceFilePath);
    end;
  except
    on E : EWindowsException do begin
      if Assigned(ATask) then
        FreeAndNil(ATask);
      ///

      pData^.State       := tsError;
      pData^.Description := E.Message;

      ///
      VSTRefresh();
    end;
  end;
end;

procedure TFormTransfers.OnTransferError(Sender : TObject; const ATransferId : TGUID; const AReason : String);
begin
  ApplyTransferException(ATransferId, AReason);
end;

function TFormTransfers.RequestFileDownload(const ARemoteFilePath, ALocalFilePath : String; const AContext : String = '') : TGUID;
begin
  var pNode := VST.AddChild(nil);
  var pData := PTreeData(pNode.GetData);
  ///

  pData^.SourceFilePath      := ARemoteFilePath;
  pData^.DestinationFilePath := ALocalFilePath;
  pData^.Direction           := otdClientIsUploading;
  pData^.Context             := AContext;
  pData^.ImageIndex          := SystemFileIcon(ARemoteFilepath, True);

  SendCommand(TOptixCommandDownloadFile.Create(ARemoteFilePath, pData^.Id));

  ///
  result := pData^.Id;

  ///
  VSTRefresh();
end;

function TFormTransfers.RequestFileUpload(const ALocalFilePath, ARemoteFilePath : String; const AContext : String = '') : TGUID;
begin
  var pNode := VST.AddChild(nil);
  var pData := PTreeData(pNode.GetData);
  ///

  pData^.SourceFilePath      := ALocalFilePath;
  pData^.DestinationFilePath := ARemoteFilePath;
  pData^.Direction           := otdClientIsDownloading;
  pData^.Context             := AContext;
  pData^.ImageIndex          := SystemFileIcon(ALocalFilePath);

  SendCommand(TOptixCommandUploadFile.Create(ARemoteFilePath, pData^.Id));

  ///
  result := pData^.Id;

  ///
  VSTRefresh();
end;

procedure TFormTransfers.UploadaFile1Click(Sender: TObject);
begin
  if not OpenDialog.Execute() then
    Exit();

  var ARemoteFile : String;

  if not InputQuery('Upload File', 'Remote Destination', ARemoteFile) then
    Exit();

  ARemoteFile := ARemoteFile.Trim();

  if ARemoteFile.EndsWith('\') then
    ARemoteFile := ARemoteFile + TPath.GetFileName(OpenDialog.FileName);

  ///
  RequestFileUpload(OpenDialog.FileName, ARemoteFile);
end;

procedure TFormTransfers.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormTransfers.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);

  function GetStateOrder(const AState: TTransferState): Integer;
  begin
    case AState of
      tsProgress      : result := 0;
      tsQueued        : result := 1;
      tsEnded         : result := 2;
      tsError         : result := 3;
      tsCancelRequest : result := 4;
      tsCanceled      : result := 5;
      else
        result := 6;
    end;
  end;

begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);

  if Assigned(pData1) and Assigned(pData2) then begin
    var AOrder1 := GetStateOrder(pData1^.State);
    var AOrder2 := GetStateOrder(pData2^.State);
    ///

    result := AOrder1 - AOrder2;
  end else
    result := 0;
end;

procedure TFormTransfers.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormTransfers.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  ///

  if Kind = TVTImageKind.ikState then begin
    case Column of
      2 : begin
        case pData^.Direction of
          otdClientIsUploading   : ImageIndex := IMAGE_FILE_DOWNLOAD;
          otdClientIsDownloading : ImageIndex := IMAGE_FILE_UPLOAD;
        end;
      end;

      4 : begin
        case pData^.State of
          tsQueued        : ImageIndex := IMAGE_FILE_QUEUE;
          tsProgress      : ImageIndex := IMAGE_FILE_TRANSFERING;
          tsEnded         : ImageIndex := IMAGE_FILE_TRANSFERED;
          tsError         : ImageIndex := IMAGE_FILE_TRANSFER_ERROR;
          tsCancelRequest : ImageIndex := IMAGE_FILE_TRANSFER_CANCEL_REQUEST;
          tsCanceled      : ImageIndex := IMAGE_FILE_TRANSFER_CANCELED;
        end;
      end;
    end;
  end else if ((Kind = TVTImageKind.ikNormal) or (Kind = TVTImageKind.ikSelected)) and (Column = 0) then
    ImageIndex := pData^.ImageIndex;
end;

procedure TFormTransfers.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormTransfers.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  case Column of
    0 : CellText := Format('%s (%s)', [
      TPath.GetFileName(pData^.SourceFilePath),
      TPath.GetDirectoryName(pData^.SourceFilePath)
    ]);
    1 : CellText := Format('%s (%s)', [
      TPath.GetFileName(pData^.DestinationFilePath),
      TPath.GetDirectoryName(pData^.DestinationFilePath)
    ]);
    2 : begin
      case pData^.Direction of
        otdClientIsUploading   : CellText := 'Download';
        otdClientIsDownloading : CellText := 'Upload';
      end;
    end;
    3 : begin
      if pData^.FileSize > 0 then
        CellText := FormatFileSize(pData^.FileSize);
    end;
    4 : begin
      case pData^.State of
        tsQueued : CellText := 'Queued';
        tsProgress : begin
          if pData^.FileSize > 0 then
            CellText := Format('%d%% (%s/%s)', [
              (pData^.WorkCount * 100) div pData^.FileSize,
              FormatFileSize(pData^.WorkCount),
              FormatFileSize(pData^.FileSize)
            ]);
        end;
        tsEnded         : CellText := 'Ended';
        tsError         : CellText := 'Error';
        tsCancelRequest : CellText := 'Cancel Request';
        tsCanceled      : CellText := 'Canceled';
      end;
    end;
    5 : CellText := pData^.Context;
    6 : CellText := pData^.Description;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TFormTransfers.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  var pData := PTreeData(Node.GetData);

  pData^.Id        := TGUID.NewGuid;
  pData^.State     := tsQueued;
  pData^.FileSize  := 0;
  pData^.WorkCount := 0;
end;

procedure TFormTransfers.CancelTransfer1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  if Application.MessageBox(
    'You are about to cancel a transfer. A canceled transfer cannot be resumed. Are you sure?',
    'Cancel Transfer',
    MB_ICONQUESTION + MB_YESNO
  ) = ID_NO then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);

  pData^.State := tsCancelRequest;

  ///
  VSTRefresh();
end;

procedure TFormTransfers.DownloadaFile1Click(Sender: TObject);
begin
  var ARemoteFile : String;

  if not InputQuery('Download File', 'Remote File Path', ARemoteFile) then
    Exit();

  ARemoteFile := ARemoteFile.Trim();

  var ADirectory : String;

  if not SelectDirectory('Select local destination', '', ADirectory) then
    Exit();

  ///
  RequestFileDownload(
    ARemoteFile,
    TFileSystemHelper.UniqueFileName(
      IncludeTrailingPathDelimiter(ADirectory) + TPath.GetFileName(ARemoteFile)
    )
  );
end;

end.
