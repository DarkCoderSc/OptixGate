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
    - Column Sorting
    - Lock GUI during refresh (Folders), Unlock if : Refresh Success / Refresh Error
}

unit uFormFileManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, __uBaseFormControl__,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.StdCtrls, Vcl.ExtCtrls, XSuperObject, Vcl.Menus,
  Optix.Func.Enum.FileSystem;

type
  TTreeData = record
    DriveInformation : TDriveInformation;
    FileInformation  : TFileInformation;
    ImageIndex       : Integer;
  end;
  PTreeData = ^TTreeData;

  TDisplayMode = (
    dmDrives,
    dmFiles
  );

  TFormFileManager = class(TBaseFormControl)
    VST: TVirtualStringTree;
    EditPath: TEdit;
    PopupMenu: TPopupMenu;
    RefreshDrives1: TMenuItem;
    N1: TMenuItem;
    procedure RefreshDrives1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    FFirstShow : Boolean;

    {@M}
    procedure RefreshDrives(const AList : TDriveList);
    procedure RefreshFiles(const AList : TFileList);
    procedure SetDisplayMode(const AMode : TDisplayMode);
    procedure BrowsePath(const APath : string);
  protected
    {@M}
    function GetContextDescription() : String; override;
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;

    {@C}
    constructor Create(AOwner : TComponent); override;
  end;

var
  FormFileManager: TFormFileManager;

implementation

uses uFormMain, Optix.Func.Commands, Optix.Protocol.Packet, Optix.Helper,
     Optix.FileSystem.Helper, Optix.Constants, Optix.VCL.Helper,
     System.IOUtils;

{$R *.dfm}

constructor TFormFileManager.Create(AOwner : TComponent);
begin
  inherited;
  ///

  FFirstShow := True;

  SetDisplayMode(dmDrives);
end;

procedure TFormFileManager.SetDisplayMode(const AMode : TDisplayMode);
begin
  VST.Clear();
  ///

  EditPath.Visible := AMode = dmFiles;
  EditPath.Clear();

  TOptixVirtualTreesHelper.UpdateColumnVisibility(VST, 'DACL (SSDL)', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VST, 'Access Rights', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VST, 'Creation Date', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VST, 'Last Modified', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VST, 'Last Access', AMode = dmFiles);
end;

procedure TFormFileManager.FormShow(Sender: TObject);
begin
  if FFirstShow then begin
    RefreshDrives1Click(RefreshDrives1);

    FFirstShow := False;
  end;
end;

function TFormFileManager.GetContextDescription() : String;
begin
  var ANodeCount := VST.RootNodeCount;
  ///

  if not EditPath.Visible and (ANodeCount > 0) then
    result := Format('%d drives enumerated.', [ANodeCount])
  else if ANodeCount > 0 then
    result := Format('%s', [EditPath.Text])
end;

procedure TFormFileManager.RefreshDrives1Click(Sender: TObject);
begin
  SendCommand(TOptixRefreshDrives.Create());
end;

procedure TFormFileManager.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);
  var AColor := clNone;

  if Assigned(pData^.FileInformation) then begin
    if faWrite in pData^.FileInformation.Access then
      AColor := COLOR_LIST_BLUE;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormFileManager.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormFileManager.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);

  // File Mode Sorting ---------------------------------------------------------
  if Assigned(pData1^.FileInformation) and Assigned(pData2^.FileInformation) then begin
    case Column of
      0 : begin
        // Always put '..' at the top
        if (pData1^.FileInformation.Name = '..') and
           (pData2^.FileInformation.Name <> '..') then
          Result := -1
        else if (pData2^.FileInformation.Name = '..') and
                (pData1^.FileInformation.Name <> '..') then
          Result := 1
        else if (pData1^.FileInformation.Name = '..') and
                (pData2^.FileInformation.Name = '..') then
          Result := 0
        else begin
          // Separate folders from files
          if pData1^.FileInformation.IsDirectory and
             not pData2^.FileInformation.IsDirectory then
            Result := -1
          else if not pData1^.FileInformation.IsDirectory and
                      pData2^.FileInformation.IsDirectory then
            Result := 1
          else
            Result := CompareText(
              pData1^.FileInformation.Name,
              pData2^.FileInformation.Name
            );
        end;
      end;

      // TODO: Continue
      1 : ;
      2 : ;
      3 : ;
      4 : ;
      5 : ;
      6 : ;
      7 : ;
    end;
  end;

  // TODO: Drives

  // ---------------------------------------------------------------------------

end;

procedure TFormFileManager.BrowsePath(const APath : string);
begin
  SendCommand(TOptixRefreshFiles.Create(APath));
end;

procedure TFormFileManager.VSTDblClick(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);

  if Assigned(pData^.DriveInformation) then
    BrowsePath(pData^.DriveInformation.Letter)
  else if Assigned(pData^.FileInformation) then begin
    var APath := EditPath.Text;

    if pData^.FileInformation.Name = '..' then
      APath := IncludeTrailingPathDelimiter(
        TDirectory.GetParent(ExcludeTrailingPathDelimiter(APath))
      )
    else
      APath := IncludeTrailingPathDelimiter(APath) + pData^.FileInformation.Name;

    ///
    BrowsePath(APath);
  end;
end;

procedure TFormFileManager.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormFileManager.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);

  if Assigned(pData^.DriveInformation) then
    FreeAndNil(pData^.DriveInformation);

  if Assigned(pData^.FileInformation) then
    FreeAndNil(pData^.FileInformation);
end;

procedure TFormFileManager.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if Column <> 0 then
    Exit();

  var pData := PTreeData(Node.GetData);

  if Assigned(pData^.DriveInformation) and (Kind = TVTImageKind.ikState) then begin
    case pData^.DriveInformation.DriveType of
      dtUnknown   : ImageIndex := IMAGE_DRIVE_UNKNOWN;
      dtNoRootDir : ImageIndex := IMAGE_DRIVE_NO_ROOT;
      dtRemovable : ImageIndex := IMAGE_DRIVE_USB;
      dtFixed     : ImageIndex := IMAGE_DRIVE;
      dtRemote    : ImageIndex := IMAGE_DRIVE_NETWORK;
      dtCDROM     : ImageIndex := IMAGE_DRIVE_CD;
      dtRAMDisk   : ImageIndex := IMAGE_DRIVE_HARDWARE;
    end;
  end else if Assigned(pData^.FileInformation) and
  ((Kind = TVTImageKind.ikNormal) or (Kind = TVTImageKind.ikSelected)) then
    ImageIndex := pData^.ImageIndex;
end;

procedure TFormFileManager.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormFileManager.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData^.DriveInformation) then begin
    // Drives ------------------------------------------------------------------
    case Column of
      0 : begin
        if String.IsNullOrEmpty(pData^.DriveInformation.Name) then
          CellText := pData^.DriveInformation.Letter
        else
          CellText := Format('%s (%s)', [
            pData^.DriveInformation.Letter,
            pData^.DriveInformation.Name
          ]);
      end;

      1 : begin
        var ADriveType := DriveTypeToString(pData^.DriveInformation.DriveType);
        if String.IsNullOrEmpty(pData^.DriveInformation.Format) then
          CellText := ADriveType
        else
          CellText := Format('%s (%s)', [
            ADriveType,
            pData^.DriveInformation.Format
          ]);
      end;

      2 : begin
        if pData^.DriveInformation.TotalSize > 0 then
          CellText := Format('%s(%d%%) / %s', [
            FormatFileSize(pData^.DriveInformation.UsedSize),
            pData^.DriveInformation.UsedPercentage,
            FormatFileSize(pData^.DriveInformation.TotalSize)
          ]);
      end;
    end;
  // ---------------------------------------------------------------------------
  end else if Assigned(pData^.FileInformation) then begin
    // Files -------------------------------------------------------------------
    case Column of
      0 : CellText := pData^.FileInformation.Name;

      1 : begin
        if pData^.FileInformation.IsDirectory then
          CellText := 'Directory'
        else
          CellText := pData^.FileInformation.TypeDescription;
      end;

      2 : CellText := FormatFileSize(pData^.FileInformation.Size);
      3 : CellText := FileAccessAttributesToString(pData^.FileInformation.Access);
      4 : CellText := pData^.FileInformation.ACL_SSDL;
    end;

    if pData^.FileInformation.DateAreValid then begin
      case column of
        5 : CellText := DateTimeToStr(pData^.FileInformation.CreatedDate);
        6 : CellText := DateTimeToStr(pData^.FileInformation.LastModifiedDate);
        7 : CellText := DateTimeToStr(pData^.FileInformation.LastAccessDate);
      End;
    end;
  end;
  // ---------------------------------------------------------------------------

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TFormFileManager.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  inherited;
  ///

  var AOptixPacket : TOptixPacket := nil;
  try
    // -------------------------------------------------------------------------
    if AClassName = TDriveList.ClassName then begin
      AOptixPacket := TDriveList.Create(ASerializedPacket);

      RefreshDrives(TDriveList(AOptixPacket));
    end
    // -------------------------------------------------------------------------
    else if AClassName = TFileList.ClassName then begin
      AOptixPacket := TFileList.Create(ASerializedPacket);

      RefreshFiles(TFileList(AOptixPacket));
    end;
    // -------------------------------------------------------------------------
  finally
    if Assigned(AOptixPacket) then
      FreeAndNil(AOptixPacket);
  end;
end;

procedure TFormFileManager.RefreshDrives(const AList : TDriveList);
begin
  SetDisplayMode(dmDrives);
  ///

  if not Assigned(AList) then
    Exit();
  ///

  VST.BeginUpdate();
  try
    for var ADrive in AList.List do begin
      var pNode := VST.AddChild(nil);
      var pData := PTreeData(pNode.GetData);
      ///

      pData^.DriveInformation := TDriveInformation.Create();
      pData^.DriveInformation.Assign(ADrive);
      pData^.FileInformation := nil;
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormFileManager.RefreshFiles(const AList : TFileList);
begin
  SetDisplayMode(dmFiles);
  ///

  if not Assigned(AList) then
    Exit();
  ///

  EditPath.Text := AList.Path;

  VST.BeginUpdate();
  try
    for var AFile in AList.List do begin
      var pNode := VST.AddChild(nil);
      var pData := PTreeData(pNode.GetData);
      ///

      pData^.DriveInformation := nil;
      pData^.FileInformation := TFileInformation.Create();
      pData^.FileInformation.Assign(AFile);

      if AFile.IsDirectory then
        pData^.ImageIndex := SystemFolderIcon()
      else
        pData^.ImageIndex := SystemFileIcon(AFile.Name, True);
    end;
  finally
    VST.SortTree(0, TSortDirection.sdAscending);

    VST.EndUpdate();
  end;
end;

end.
