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
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees, Vcl.Menus;

type
  TTreeData = record
    SourceFilePath      : String;
    DestinationFilePath : String;
    Direction           : TOptixTransferDirection;
    FileSize            : UInt64;
    Id                  : TGUID;
  end;
  PTreeData = ^TTreeData;

  TFormTransfers = class(TBaseFormControl)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    DownloadaFile1: TMenuItem;
    UploadaFile1: TMenuItem;
    OpenDialog: TOpenDialog;
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure DownloadaFile1Click(Sender: TObject);
    procedure UploadaFile1Click(Sender: TObject);
  private
    {@M}
    function GetNodeByTransferId(const ATransferId : TGUID) : PVirtualNode;
  public
    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String); override;

    {@M}
    function RequestFileDownload(const ARemoteFilePath, ALocalFilePath : String) : TGUID;
    function RequestFileUpload(const ALocalFilePath, ARemoteFilePath : String) : TGUID;

    procedure OnRequestTransferTask(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask);
  end;

var
  FormTransfers: TFormTransfers;

implementation

uses Optix.Func.Commands, Optix.Helper, VCL.FileCtrl, Optix.FileSystem.Helper, System.IOUtils;

{$R *.dfm}

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

procedure TFormTransfers.OnRequestTransferTask(Sender : TObject; const ATransferId : TGUID; var ATask : TOptixTransferTask);
begin
  ATask := nil;
  ///

  var pNode := GetNodeByTransferId(ATransferId);
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PTreeData(pNode.GetData);

  case pData^.Direction of
    // Server Req File Download
    otdClientIsUploading : ATask := TOptixDownloadTask.Create(pData^.DestinationFilePath);

    // Server Req File Upload
    otdClientIsDownloading : ATask := TOptixUploadTask.Create(pData^.SourceFilePath);
  end;
end;

function TFormTransfers.RequestFileDownload(const ARemoteFilePath, ALocalFilePath : String) : TGUID;
begin
  var pNode := VST.AddChild(nil);
  var pData := PTreeData(pNode.GetData);

  pData^.SourceFilePath      := ARemoteFilePath;
  pData^.DestinationFilePath := ALocalFilePath;
  pData^.Direction           := otdClientIsUploading;
  pData^.Id                  := TGUID.NewGuid;

  SendCommand(TOptixDownloadFile.Create(ARemoteFilePath, pData^.Id));

  ///
  result := pData^.Id;

  ///
  VST.Refresh();
end;

function TFormTransfers.RequestFileUpload(const ALocalFilePath, ARemoteFilePath : String) : TGUID;
begin
  var pNode := VST.AddChild(nil);
  var pData := PTreeData(pNode.GetData);

  pData^.SourceFilePath      := ALocalFilePath;
  pData^.DestinationFilePath := ARemoteFilePath;
  pData^.Direction           := otdClientIsDownloading;
  pData^.Id                  := TGUID.NewGuid;

  SendCommand(TOptixUploadFile.Create(ARemoteFilePath, pData^.Id));

  ///
  result := pData^.Id;

  ///
  VST.Refresh();
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

procedure TFormTransfers.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
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
    0 : CellText := pData^.SourceFilePath;
    1 : CellText := pData^.DestinationFilePath;
    2 : begin
      case pData^.Direction of
        otdClientIsUploading   : CellText := '<- Download';
        otdClientIsDownloading : CellText := '-> Upload';
      end;
    end;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

constructor TFormTransfers.Create(AOwner : TComponent; const AUserIdentifier : String);
begin
  inherited;
  ///

  FSpecialForm := True;
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
