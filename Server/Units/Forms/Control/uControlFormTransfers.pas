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

unit uControlFormTransfers;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes, System.Types,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,

  VirtualTrees, VirtualTrees.Types, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree,
  VirtualTrees.AncestorVCL,

  __uBaseFormControl__,

  OptixCore.Protocol.FileTransfer, OptixCore.LogNotifier, OptixCore.Protocol.Packet,

  NeoFlat.PopupMenu;
// ---------------------------------------------------------------------------------------------------------------------

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

  TControlFormTransfers = class(TBaseFormControl)
    VST: TVirtualStringTree;
    PopupMenu: TFlatPopupMenu;
    DownloadaFile1: TMenuItem;
    UploadaFile1: TMenuItem;
    OpenDialog: TOpenDialog;
    N1: TMenuItem;
    CancelTransfer1: TMenuItem;
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure DownloadaFile1Click(Sender: TObject);
    procedure UploadaFile1Click(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure PopupMenuPopup(Sender: TObject);
    procedure CancelTransfer1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    {@M}
    function RegisterNewTransfer(const ASourceFilePath, ADestinationFilePath : String; const ADirection : TOptixTransferDirection; const AContext : String = '') : TGUID;

    function GetNodeByTransferId(const ATransferId : TGUID) : PVirtualNode;
  public
    {@M}
    function RequestFileDownload(ARemoteFilePath : String = ''; ALocalFilePath : String = ''; const AContext : String = '') : TGUID; override;
    function RequestFileUpload(ALocalFilePath : String; ARemoteFilePath : String = ''; const AContext : String = '') : TGUID; override;

    procedure ApplyTransferException(const ATransferId : TGUID; const AReason : String); overload;
    procedure ApplyTransferException(const ALogTransferException : TOptixCommandReceiveTransferException); overload;

    procedure OnRequestTransferTask(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask);
    procedure OnTransferError(Sender : TObject; const ATransferId : TGUID; const AReason : String);
    procedure OnTransferBegins(Sender : TObject; const ATransferId : TGUID; const AFileSize : Int64);
    procedure OnTransferUpdate(Sender : TObject; const ATransferId : TGUID; const AWorkCount : Int64; var ACanceled : Boolean);
    procedure OnTransferEnds(Sender : TObject; const ATransferId : TGUID);

    procedure ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean); override;
  end;

var
  ControlFormTransfers: TControlFormTransfers;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.IOUtils, System.Math, System.DateUtils,

  uFormMain, uControlFormFileManager,

  OptixCore.Commands, VCL.FileCtrl, OptixCore.System.FileSystem, OptixCore.Exceptions, Optix.Constants,
  Optix.Helper, OptixCore.Commands.FileSystem;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormTransfers.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  inherited;
  ///

  // -------------------------------------------------------------------------------------------------------------------
  if AOptixPacket is TOptixCommandGetUploadedFileInformation then begin
    var AForms := FormMain.GetControlForms(self, TControlFormFileManager);
    if not Assigned(AForms) then
      Exit();
    try
      var AFileInformation := TOptixCommandGetUploadedFileInformation(AOptixPacket);
      if Assigned(AFileInformation.FileInformation) then
        for var AForm in AForms do
          TControlFormFileManager(AForm).RegisterNewFile(
            ExtractFilePath(AFileInformation.FileName),
            AFileInformation.FileInformation
          );
    finally
      FreeAndNil(AForms);
    end;
  end;
  // -------------------------------------------------------------------------------------------------------------------
end;

procedure TControlFormTransfers.OnTransferBegins(Sender : TObject; const ATransferId : TGUID; const AFileSize : Int64);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  VST.BeginUpdate();
  try
    pData^.State    := tsProgress;
    pData^.FileSize := AFileSize;
  finally
    VST.EndUpdate();
  end;
end;

procedure TControlFormTransfers.OnTransferUpdate(Sender : TObject; const ATransferId : TGUID; const AWorkCount : Int64; var ACanceled : Boolean);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  ACanceled := pData^.State = tsCancelRequest;

  if pData^.State = tsProgress then begin
    VST.BeginUpdate();
    try
      pData^.WorkCount := AWorkCount;
    finally
      VST.EndUpdate();
    end;
  end;
end;

procedure TControlFormTransfers.PopupMenuPopup(Sender: TObject);
begin
  TOptixHelper.HideAllPopupMenuRootItems(TPopupMenu(Sender));
  ///

  UploadaFile1.Visible   := True;
  DownloadaFile1.Visible := True;

  var pNode := VST.FocusedNode;
  if Assigned(pNode) then begin
    var pData := PTreeData(pNode.GetData);
    CancelTransfer1.Visible := (pData^.State = tsQueued) or (pData^.State = tsProgress);
  end;
end;

procedure TControlFormTransfers.OnTransferEnds(Sender : TObject; const ATransferId : TGUID);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  VST.BeginUpdate();
  try
    if pData^.State = tsCancelRequest then
      pData^.State := tsCanceled
    else begin
      pData^.State := tsEnded;

      ///
      SendCommand(TOptixCommandGetUploadedFileInformation.Create(pData^.DestinationFilePath, False));
    end;
  finally
    VST.EndUpdate();
  end;
end;

function TControlFormTransfers.GetNodeByTransferId(const ATransferId : TGUID) : PVirtualNode;
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

procedure TControlFormTransfers.ApplyTransferException(const ATransferId : TGUID; const AReason : String);
begin
  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  VST.BeginUpdate();
  try
    pData^.State       := tsError;
    pData^.Description := AReason;
  finally
    VST.EndUpdate();
  end;
end;

procedure TControlFormTransfers.ApplyTransferException(const ALogTransferException : TOptixCommandReceiveTransferException);
begin
  if not Assigned(ALogTransferException) then
    Exit();
  ///

  ApplyTransferException(ALogTransferException.TransferId, ALogTransferException.LogMessage);
end;

procedure TControlFormTransfers.OnRequestTransferTask(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask);
begin
  ATask := nil;
  ///

  if VST.IsUpdating then
    Exit();

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
      ///

      VST.BeginUpdate();
      try
        pData^.State       := tsError;
        pData^.Description := E.Message;
      finally
        VST.EndUpdate();
      end;
    end;
  end;
end;

procedure TControlFormTransfers.OnTransferError(Sender : TObject; const ATransferId : TGUID; const AReason : String);
begin
  ApplyTransferException(ATransferId, AReason);
end;

function TControlFormTransfers.RegisterNewTransfer(const ASourceFilePath, ADestinationFilePath : String; const ADirection : TOptixTransferDirection; const AContext : String = '') : TGUID;
var pNode : PVirtualNode;
    pData : PTreeData;
begin
  VST.BeginUpdate();
  try
    pNode := VST.AddChild(nil);
    pData := pNode.GetData;

    // Init
    pData^.Id                  := TGUID.NewGuid;
    pData^.State               := tsQueued;
    pData^.FileSize            := 0;
    pData^.WorkCount           := 0;

    // Param
    pData^.SourceFilePath      := ASourceFilePath.Trim();
    pData^.DestinationFilePath := TFileSystemHelper.UniqueFileName(ADestinationFilePath.Trim());
    pData^.Direction           := ADirection;
    pData^.Context             := AContext;
    pData^.ImageIndex          := TOptixHelper.SystemFileIcon(pData^.SourceFilePath, (ADirection = otdClientIsUploading));

    ///
    result := pData^.Id;
  finally
    VST.EndUpdate();
  end;

  ///

  if Assigned(pData) then
    case ADirection of
      otdClientIsUploading   : SendCommand(TOptixCommandDownloadFile.Create(pData^.SourceFilePath, pData^.Id));
      otdClientIsDownloading : SendCommand(TOptixCommandUploadFile.Create(pData^.DestinationFilePath, pData^.Id));
    end;

  ///
  TOptixHelper.ShowForm(self);
end;

function TControlFormTransfers.RequestFileDownload(ARemoteFilePath : String = ''; ALocalFilePath : String = ''; const AContext : String = '') : TGUID;
begin
  if String.IsNullOrWhiteSpace(ARemoteFilePath) then
    if not InputQuery('Download File', 'Remote File Path', ARemoteFilePath) then
      Exit();

  if String.IsNullOrWhiteSpace(ALocalFilePath) then begin
    var ADirectory := '';

    if not SelectDirectory('Select Destination', '', ADirectory) then
      Exit();

    ALocalFilePath := IncludeTrailingPathDelimiter(ADirectory) + TPath.GetFileName(ARemoteFilePath);
  end;

  ///
  result := RegisterNewTransfer(ARemoteFilePath, ALocalFilePath, otdClientIsUploading, AContext);
end;

function TControlFormTransfers.RequestFileUpload(ALocalFilePath : String; ARemoteFilePath : String = ''; const AContext : String = '') : TGUID;
begin
  if String.IsNullOrWhiteSpace(ALocalFilePath) then begin
    if not OpenDialog.Execute() then
      Exit();

    ///
    ALocalFilePath := OpenDialog.FileName;
  end;

  if String.IsNullOrWhiteSpace(ARemoteFilePath) then begin
    if not InputQuery('Upload File', 'Remote Destination (File or Folder "\")', ARemoteFilePath) then
      Exit();
  end;

  if ARemoteFilePath.EndsWith('\') then
    ARemoteFilePath := ARemoteFilePath.Trim() + TPath.GetFileName(OpenDialog.FileName);

  ///
  result := RegisterNewTransfer(ALocalFilePath, ARemoteFilePath, otdClientIsDownloading, AContext);
end;

procedure TControlFormTransfers.UploadaFile1Click(Sender: TObject);
begin
  RequestFileUpload('');
end;

procedure TControlFormTransfers.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if Column <> 4 then
    Exit;

  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit;
  ///

  var AColor := clNone;

  case pData^.State of
    tsQueued        : AColor := COLOR_LIST_GRAY;
    tsProgress      : AColor := COLOR_LIST_BLUE;
    tsEnded         : AColor := COLOR_LIST_GREEN;
    tsError         : AColor := COLOR_LIST_RED;
    tsCancelRequest : AColor := COLOR_LIST_YELLOW;
    tsCanceled      : AColor := COLOR_LIST_ORANGE;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TControlFormTransfers.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
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
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := 0
  else begin
    case Column of
      0 : CompareText(pData1^.SourceFilePath, pData2^.SourceFilePath);
      1 : CompareText(pData1^.DestinationFilePath, pData2^.DestinationFilePath);
      2 : CompareValue(Cardinal(pData1^.Direction), Cardinal(pData2^.Direction));
      3 : CompareValue(pData1^.FileSize, pData2^.FileSize);

      4 : begin
        var AOrder1 := GetStateOrder(pData1^.State);
        var AOrder2 := GetStateOrder(pData2^.State);
        ///

        result := AOrder1 - AOrder2;
      end;

      5 : CompareText(pData1^.Context, pData2^.Context);
      6 : CompareText(pData1^.Description, pData2^.Description);
      7 : CompareText(pData1^.Id.ToString, pData2^.Id.ToString);
    end;
  end;
end;

procedure TControlFormTransfers.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  if Kind = TVTImageKind.ikState then begin
    case Column of
      2 : begin
        case pData^.Direction of
          otdClientIsUploading   : ImageIndex := IMAGE_BLUE_ARROW_LEFT;
          otdClientIsDownloading : ImageIndex := IMAGE_BLUE_ARROW_RIGHT;
        end;
      end;
    end;
  end else if ((Kind = TVTImageKind.ikNormal) or (Kind = TVTImageKind.ikSelected)) and (Column = 0) then
    ImageIndex := pData^.ImageIndex;
end;

procedure TControlFormTransfers.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TControlFormTransfers.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData) then begin
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
          CellText := TOptixHelper.FormatFileSize(pData^.FileSize);
      end;
      4 : begin
        case pData^.State of
          tsQueued : CellText := 'Queued';
          tsProgress : begin
            if pData^.FileSize > 0 then
              CellText := Format('%d%% (%s/%s)', [
                (pData^.WorkCount * 100) div pData^.FileSize,
                TOptixHelper.FormatFileSize(pData^.WorkCount),
                TOptixHelper.FormatFileSize(pData^.FileSize)
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
      7 : CellText := pData^.Id.ToString();
    end;
  end;

  ///
  CellText := TOptixHelper.DefaultIfEmpty(CellText);
end;

procedure TControlFormTransfers.CancelTransfer1Click(Sender: TObject);
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

  VST.BeginUpdate();
  try
    pData^.State := tsCancelRequest;
  finally
    VST.EndUpdate();
  end;
end;

procedure TControlFormTransfers.DownloadaFile1Click(Sender: TObject);
begin
  RequestFileDownload();
end;

procedure TControlFormTransfers.FormDestroy(Sender: TObject);
begin
  VST.Clear();
end;

procedure TControlFormTransfers.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TBaseVirtualTree(Sender).GetNodeAt(Point(X, Y)) = nil then begin
    TBaseVirtualTree(Sender).ClearSelection();

    TBaseVirtualTree(Sender).FocusedNode := nil;
  end;
end;

end.
