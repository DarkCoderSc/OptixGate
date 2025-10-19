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
    - Lock GUI during refresh (Folders), Unlock if : Refresh Success / Refresh Error
}

unit uControlFormFileManager;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Buttons, Vcl.Menus, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  VirtualTrees, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  OMultiPanel,

  __uBaseFormControl__,

   VirtualTrees.Types, Optix.FileSystem.Helper, Optix.FileSystem.Enum, Optix.Func.Commands.FileSystem,
   Optix.Protocol.Packet;

// ---------------------------------------------------------------------------------------------------------------------

type
  TFileTreeData = record
    DriveInformation : TDriveInformation;
    FileInformation  : TFileInformation;
    ImageIndex       : Integer;

    {@M}
    function Name() : String;
  end;
  PFileTreeData = ^TFileTreeData;

  TFolderTreeData = record
    Information : TSimpleFolderInformation;
    ImageIndex  : Integer;
  end;
  PFolderTreeData = ^TFolderTreeData;

  TDisplayMode = (
    dmDrives,
    dmFiles
  );

  TControlFormFileManager = class(TBaseFormControl)
    VSTFiles: TVirtualStringTree;
    EditPath: TEdit;
    PopupMenu: TPopupMenu;
    DownloadFile1: TMenuItem;
    UploadToFolder1: TMenuItem;
    PanelActions: TPanel;
    ButtonHome: TSpeedButton;
    ButtonRefresh: TSpeedButton;
    ButtonUpload: TSpeedButton;
    ButtonOptions: TSpeedButton;
    PopupMenuOptions: TPopupMenu;
    ColoredFoldersAccessView1: TMenuItem;
    LabelAccess: TLabel;
    ButtonBack: TSpeedButton;
    ButtonForward: TSpeedButton;
    ButtonGoTo: TSpeedButton;
    MultiPanel: TOMultiPanel;
    VSTFolders: TVirtualStringTree;
    N1: TMenuItem;
    ShowFolderTree1: TMenuItem;
    PopupFoldersTree: TPopupMenu;
    FullExpand1: TMenuItem;
    FullCollapse1: TMenuItem;
    procedure VSTFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFilesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFilesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFilesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTFilesDblClick(Sender: TObject);
    procedure VSTFilesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTFilesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure PopupMenuPopup(Sender: TObject);
    procedure ColoredFoldersAccessView1Click(Sender: TObject);
    procedure DownloadFile1Click(Sender: TObject);
    procedure ButtonHomeClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure UploadToFolder1Click(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonUploadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonForwardClick(Sender: TObject);
    procedure VSTFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ButtonGoToClick(Sender: TObject);
    procedure ShowFolderTree1Click(Sender: TObject);
    procedure VSTFoldersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTFoldersChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFoldersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTFoldersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure VSTFoldersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure FullExpand1Click(Sender: TObject);
    procedure FullCollapse1Click(Sender: TObject);
    procedure VSTFoldersDblClick(Sender: TObject);
    procedure VSTFoldersCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure VSTFoldersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FHistoryCursor : Integer;
    FPathHistory   : TList<String>;

    {@M}
    procedure InsertPathToHistory(APath : String);
    procedure BrowseFromCurrentHistoryLocation();
    function CanNodeFileBeDownloaded(var pData : PFileTreeData) : Boolean;
    function CanFileBeUploadedToNodeDirectory(var pData : PFileTreeData) : Boolean;
    procedure RegisterFoldersInTree(const AParentFolders : TObjectList<TSimpleFolderInformation>;
      const AFolders : TObjectList<TSimpleFolderInformation>);
    procedure DisplayDrives(const AList : TOptixCommandRefreshDrives);
    procedure DisplayFiles(const AList : TOptixCommandRefreshFiles);
    procedure SetDisplayMode(const AMode : TDisplayMode);
    procedure BrowsePath(const APath : string; const APushToHistory : Boolean = True);
    procedure RefreshNavButtons();
    procedure RefreshDrives(const APushToHistory : Boolean = True);
    procedure RefreshFiles();
    function GetFolderImageIndex(const AFolderAccess : TFileAccessAttributes) : Integer;
    function GetNodeByFileName(const AFileName : String) : PVirtualNode;
  protected
    {@M}
    function GetContextDescription() : String; override;
    procedure OnFirstShow(); override;

    function RequestFileDownload(const ARemoteFilePath : String = ''; ALocalFilePath : String = '') : TGUID; reintroduce;
    function RequestFileUpload(ALocalFilePath : String; const ARemoteFilePath : String = ''; const AContext : String = '') : TGUID; reintroduce;
  public
    {@M}
    procedure ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean); override;
    procedure RegisterNewFile(const APath : String; const AFileInformation : TFileInformation);

    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String;
      const ASpecialForm : Boolean = False); override;
  end;

var
  ControlFormFileManager: TControlFormFileManager;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Types, System.DateUtils, System.Math, System.IOUtils, System.StrUtils,

  uFormMain,

  Optix.Func.Commands, Optix.Helper, Optix.Constants, Optix.VCL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

(* TFileTreeData *)

function TFileTreeData.Name() : String;
begin
  result := '';
  ///

  if Assigned(DriveInformation) then
    result := DriveInformation.Letter
  else if Assigned(FileInformation) then
    result := FileInformation.Name;
end;

(* TControlFormFileManager *)

function TControlFormFileManager.GetNodeByFileName(const AFileName : String) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VSTFiles.Nodes do begin
    var pData := PFileTreeData(pNode.GetData);
    ///

    if String.Compare(pData^.Name, AFileName, True) = 0 then begin
      result := pNode;

      break;
    end;
  end;
end;

procedure TControlFormFileManager.RegisterNewFile(const APath : String; const AFileInformation : TFileInformation);
begin
  if not Assigned(AFileInformation) then
    Exit();
  ///

  if String.Compare(
    IncludeTrailingPathDelimiter(APath),
    IncludeTrailingPathDelimiter(EditPath.Text),
    True
  ) <> 0 then
    Exit();

  VSTFiles.BeginUpdate();
  try
    var pNode := GetNodeByFileName(AFileInformation.Name);
    if not Assigned(pNode) then
      pNode := VSTFiles.AddChild(nil);

    var pData := PFileTreeData(pNode.GetData);

    pData^.FileInformation := TFileInformation.Create();
    pData^.FileInformation.Assign(AFileInformation);

    if AFileInformation.IsDirectory then
      pData^.ImageIndex := SystemFolderIcon()
    else
      pData^.ImageIndex := SystemFileIcon(pData^.Name, True);
  finally
    VSTFiles.EndUpdate();
  end;

  ///
  if AFileInformation.IsDirectory then begin
    // TODO: Support directories (Modify "new file/folder" feedback command to have parent folders list)
  end;
end;

function TControlFormFileManager.GetFolderImageIndex(const AFolderAccess : TFileAccessAttributes) : Integer;
begin
  result := -1;
  ///

  if not ColoredFoldersAccessView1.Checked then
    Exit();
  ///

  if (faRead in AFolderAccess) and (faWrite in AFolderAccess) and (faExecute in AFolderAccess) then
    result := IMAGE_FOLDER_FULLACCESS
  else if AFolderAccess = [faRead] then
    result := IMAGE_FOLDER_READONLY
  else if AFolderAccess = [faWrite] then
    result := IMAGE_FOLDER_WRITEONLY
  else if AFolderAccess = [faExecute] then
    result := IMAGE_FOLDER_EXECONLY
  else if AFolderAccess = [] then
    result := IMAGE_FOLDER_DENIED
  else
    result := IMAGE_FOLDER_NORMAL;
end;

procedure TControlFormFileManager.RegisterFoldersInTree(const AParentFolders : TObjectList<TSimpleFolderInformation>;
  const AFolders : TObjectList<TSimpleFolderInformation>);
begin
  TOptixVirtualTreesFolderTreeHelper.UpdateTree<TSimpleFolderInformation>(
    VSTFolders,
    AParentFolders,
    AFolders,
    (
      function (const pData : Pointer) : String
      begin
        result := PFolderTreeData(pData)^.Information.Name
      end
    ),
    (
      function (const AItem: TSimpleFolderInformation): String
      begin
        result := AItem.Name
      end
    ),
    (
      procedure (var pNode, pParentNode : PVirtualNode; const AItem : TSimpleFolderInformation)
      begin
        var pData : PFolderTreeData;
        ///

        if not Assigned(pNode) then begin
          pNode := VSTFolders.AddChild(pParentNode);

          pData := PFolderTreeData(pNode.GetData);

          pData^.Information := TSimpleFolderInformation.Create();
        end else
          pData := PFolderTreeData(pNode.GetData);

        pData^.Information.Assign(AItem);

        if AItem.IsRoot then
          pData^.ImageIndex := SystemFileIcon(AItem.Path)
        else
          pData^.ImageIndex := SystemFolderIcon();
      end
    )
  );
end;

procedure TControlFormFileManager.BrowseFromCurrentHistoryLocation();
begin
  if (FPathHistory.Count > 0) and (FHistoryCursor >= 0) then begin
    var APath := FPathHistory.Items[FHistoryCursor];

    if String.Compare(APath, '\\:DRIVES:\\') = 0 then
      RefreshDrives(False)
    else
      BrowsePath(APath, False);
  end;
end;

procedure TControlFormFileManager.RefreshNavButtons();
begin
  ButtonBack.Enabled    := (FPathHistory.Count > 0) and (FHistoryCursor > 0);
  ButtonForward.Enabled := (FPathHistory.Count > 0) and (FHistoryCursor < FPathHistory.Count -1)
end;

procedure TControlFormFileManager.InsertPathToHistory(APath : String);
begin
  if not Assigned(FPathHistory) then
    Exit();
  ///

  APath := IncludeTrailingPathDelimiter(APath);

  if (FPathHistory.Count = 0) or
     (String.Compare(FPathHistory.Items[FPathHistory.Count-1], APath, True) <> 0) then begin

     if FHistoryCursor < FPathHistory.Count -1 then
      FPathHistory.DeleteRange(FHistoryCursor +1, (FPathHistory.Count -1) - FHistoryCursor);

     FPathHistory.Add(APath);

     ///
     FHistoryCursor := FPathHistory.Count -1;
  end;

  ///
  RefreshNavButtons();
end;

procedure TControlFormFileManager.OnFirstShow();
begin
  inherited;
  ///

  RefreshDrives();
end;

function TControlFormFileManager.RequestFileDownload(const ARemoteFilePath : String = ''; ALocalFilePath : String = '') : TGUID;
begin
  inherited RequestFileDownload(ARemoteFilePath, ALocalFilePath, Format('File Manager (%s)', [EditPath.Text]));
end;

function TControlFormFileManager.RequestFileUpload(ALocalFilePath : String; const ARemoteFilePath : String = ''; const AContext : String = '') : TGUID;
begin
  inherited RequestFileUpload(ALocalFilePath, ARemoteFilePath, Format('File Manager (%s)', [EditPath.Text]));
end;

procedure TControlFormFileManager.RefreshDrives(const APushToHistory : Boolean = True);
begin
  if APushToHistory then
    InsertPathToHistory('\\:DRIVES:\\');
  ///

  SendCommand(TOptixCommandRefreshDrives.Create());
end;

procedure TControlFormFileManager.RefreshFiles();
begin
  BrowsePath(EditPath.Text);
end;

procedure TControlFormFileManager.ColoredFoldersAccessView1Click(Sender: TObject);
begin
  VSTFiles.Refresh();
  VSTFolders.Refresh();
end;

constructor TControlFormFileManager.Create(AOwner : TComponent; const AUserIdentifier : String;
  const ASpecialForm : Boolean = False);
begin
  inherited;
  ///

  FHistoryCursor := 0;
  FPathHistory := TList<String>.Create();

  SetDisplayMode(dmDrives);
end;

procedure TControlFormFileManager.SetDisplayMode(const AMode : TDisplayMode);
begin
  VSTFiles.Clear();
  ///

  EditPath.Visible := AMode = dmFiles;
  EditPath.Clear();

  LabelAccess.Visible := AMode = dmFiles;

  TOptixVirtualTreesHelper.UpdateColumnVisibility(VSTFiles, 'DACL (SSDL)', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VSTFiles, 'Access Rights', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VSTFiles, 'Creation Date', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VSTFiles, 'Last Modified', AMode = dmFiles);
  TOptixVirtualTreesHelper.UpdateColumnVisibility(VSTFiles, 'Last Access', AMode = dmFiles);

  ButtonRefresh.Enabled := AMode = dmFiles;
  ButtonUpload.Enabled  := AMode = dmFiles;

  ///
  RefreshNavButtons();
end;

procedure TControlFormFileManager.ButtonBackClick(Sender: TObject);
begin
  Dec(FHistoryCursor);
  if FHistoryCursor < 0 then
    FHistoryCursor := 0;

  BrowseFromCurrentHistoryLocation();

  RefreshNavButtons();
end;

procedure TControlFormFileManager.ButtonForwardClick(Sender: TObject);
begin
  Inc(FHistoryCursor);
  if FHistoryCursor > FPathHistory.Count -1 then
    FHistoryCursor := FPathHistory.Count -1;

  BrowseFromCurrentHistoryLocation();

  RefreshNavButtons();
end;

procedure TControlFormFileManager.ButtonGoToClick(Sender: TObject);
begin
  var APath := '';

  if not InputQuery('Go To', 'Path:', APath) then
    Exit();

  ///
  BrowsePath(APath);
end;

procedure TControlFormFileManager.ButtonHomeClick(Sender: TObject);
begin
  RefreshDrives();
end;

procedure TControlFormFileManager.ButtonOptionsClick(Sender: TObject);
begin
  var APoint := self.ClientToScreen(
    Point(
      TSpeedButton(Sender).Left,
      TSpeedButton(Sender).Top + TSpeedButton(Sender).Height
    )
  );

  PopupMenuOptions.Popup(APoint.X, APoint.Y);
end;

procedure TControlFormFileManager.ButtonRefreshClick(Sender: TObject);
begin
  if EditPath.Visible then
    RefreshFiles()
  else
    RefreshDrives();
end;

procedure TControlFormFileManager.ButtonUploadClick(Sender: TObject);
begin
  if EditPath.Visible then
    RequestFileUpload('', IncludeTrailingPathDelimiter(EditPath.Text));
end;

procedure TControlFormFileManager.UploadToFolder1Click(Sender: TObject);
begin
  var pNode := VSTFiles.FocusedNode;
  if not Assigned(pNode) then
    Exit();

  var pData := PFileTreeData(pNode.GetData);
  if not CanFileBeUploadedToNodeDirectory(pData) then
    Exit();

  RequestFileUpload('', IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(EditPath.Text) + pData^.FileInformation.Name));
end;

function TControlFormFileManager.GetContextDescription() : String;
begin
  var ANodeCount := VSTFiles.RootNodeCount;
  ///

  if not EditPath.Visible and (ANodeCount > 0) then
    result := Format('%d drives enumerated.', [ANodeCount])
  else if ANodeCount > 0 then
    result := Format('%s', [EditPath.Text])
end;

function TControlFormFileManager.CanNodeFileBeDownloaded(var pData : PFileTreeData) : Boolean;
begin
  result := False;
  ///

  if not Assigned(pData) and Assigned(pData^.FileInformation) and (pData^.FileInformation.IsDirectory) then
    Exit();

  // ?? File is not empty
  // ?? Client has read access
  result := (pData^.FileInformation.Size > 0) and (faRead in pData^.FileInformation.Access);
end;

function TControlFormFileManager.CanFileBeUploadedToNodeDirectory(var pData : PFileTreeData) : Boolean;
begin
  result := False;
  ///

  if not Assigned(pData) and Assigned(pData^.FileInformation) then
    Exit();

  // ?? A folder
  // ?? Client has write access
  result := (pData^.FileInformation.IsDirectory) and (faWrite in pData^.FileInformation.Access);
end;

procedure TControlFormFileManager.PopupMenuPopup(Sender: TObject);
begin
  TOptixVCLHelper.HideAllPopupMenuRootItems(TPopupMenu(Sender));
  ///

  if EditPath.Visible then begin
    var pNode := VSTFiles.FocusedNode;
    if Assigned(pNode) then begin
      var pData := PFileTreeData(pNode.GetData());
      ///

      if Assigned(pData^.FileInformation) then begin
        if pData^.FileInformation.IsDirectory then begin
          UploadToFolder1.Visible := True;
          UploadToFolder1.Enabled := CanFileBeUploadedToNodeDirectory(pData);
        end else begin
          DownloadFile1.Visible   := True;
          DownloadFile1.Enabled   := CanNodeFileBeDownloaded(pData);
        end;
      end;
    end;
  end;
end;

procedure TControlFormFileManager.VSTFilesBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if Column <> 3 then
    Exit();
  ///

  var pData := PFileTreeData(Node.GetData);
  var AColor := clNone;

  if Assigned(pData) and Assigned(pData^.FileInformation) then begin
    var AFolderAccess := pData^.FileInformation.Access;
    ///

    if (faRead in AFolderAccess) and (faWrite in AFolderAccess) and (faExecute in AFolderAccess) then
      AColor := COLOR_FILE_ALL_ACCESS
    else if (AFolderAccess = [faRead]) then
      AColor := COLOR_FILE_READ_ONLY
    else if (AFolderAccess = [faWrite]) then
      AColor := COLOR_FILE_WRITE_ONLY
    else if (AFolderAccess = [faExecute]) then
      AColor := COLOR_FILE_EXECUTE_ONLY
    else if (AFolderAccess = []) then
      AColor := COLOR_FILE_NO_ACCESS;

    if AColor <> clNone then begin
      TargetCanvas.Brush.Color := AColor;

      CellRect.Width := ScaleValue(4);

      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TControlFormFileManager.VSTFilesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormFileManager.VSTFilesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PFileTreeData(Node1.GetData);
  var pData2 := PFileTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then begin
    Result := 0;

    ///
    Exit(); // Avoid too many nested blocks of code in this specific case.
  end;

  // File Mode Sorting -------------------------------------------------------------------------------------------------
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

      1 : Result := CompareText(pData1^.FileInformation.TypeDescription, pData2^.FileInformation.TypeDescription);
      2 : Result := CompareValue(pData1^.FileInformation.Size, pData2^.FileInformation.Size);

      3 : Result := CompareText(
                      AccessSetToReadableString(pData1^.FileInformation.Access),
                      AccessSetToReadableString(pData2^.FileInformation.Access)
                    );

      4 : Result := CompareText(pData1^.FileInformation.ACL_SSDL, pData2^.FileInformation.ACL_SSDL);
      5 : Result := CompareDateTime(pData1^.FileInformation.CreatedDate, pData2^.FileInformation.CreatedDate);
      6 : Result := CompareDateTime(pData1^.FileInformation.LastModifiedDate, pData2^.FileInformation.LastModifiedDate);
      7 : Result := CompareDateTime(pData1^.FileInformation.LastAccessDate, pData2^.FileInformation.LastAccessDate);
    end;
  end else if Assigned(pData1^.DriveInformation) and Assigned(pData2^.DriveInformation) then begin
    case Column of
      0 : Result := CompareText(pData1^.DriveInformation.Letter, pData1^.DriveInformation.Letter);
      1 : Result := CompareText(pData1^.DriveInformation.Format, pData2^.DriveInformation.Format);
      2 : Result := CompareValue(pData1^.DriveInformation.TotalSize, pData2^.DriveInformation.TotalSize);
    end;
  end;
  // -------------------------------------------------------------------------------------------------------------------
end;

procedure TControlFormFileManager.BrowsePath(const APath : string; const APushToHistory : Boolean = True);
begin
  if APushToHistory then
    InsertPathToHistory(APath);
  ///

  SendCommand(TOptixCommandRefreshFiles.Create(APath));
end;

procedure TControlFormFileManager.VSTFilesDblClick(Sender: TObject);
begin
  if VSTFiles.FocusedNode = nil then
    Exit();

  var pData := PFileTreeData(VSTFiles.FocusedNode.GetData);

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

procedure TControlFormFileManager.VSTFilesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormFileManager.VSTFilesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var pData := PFileTreeData(Node.GetData);
  if not Assigned(pData) then
    Exit();
  ///

  if Assigned(pData^.DriveInformation) then
    FreeAndNil(pData^.DriveInformation);

  if Assigned(pData^.FileInformation) then
    FreeAndNil(pData^.FileInformation);
end;

procedure TControlFormFileManager.VSTFilesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PFileTreeData(Node.GetData);
  if not Assigned(pData) or (Column <> 0) then
    Exit();
  ///

//  if Assigned(pData^.DriveInformation) and (Kind = TVTImageKind.ikState) then begin
//    case pData^.DriveInformation.DriveType of
//      dtUnknown   : ImageIndex := IMAGE_DRIVE_UNKNOWN;
//      dtNoRootDir : ImageIndex := IMAGE_DRIVE_NO_ROOT;
//      dtRemovable : ImageIndex := IMAGE_DRIVE_USB;
//      dtFixed     : ImageIndex := IMAGE_DRIVE;
//      dtRemote    : ImageIndex := IMAGE_DRIVE_NETWORK;
//      dtCDROM     : ImageIndex := IMAGE_DRIVE_CD;
//      dtRAMDisk   : ImageIndex := IMAGE_DRIVE_HARDWARE;
//    end;
  if Assigned(pData^.DriveInformation) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    ImageIndex := pData^.ImageIndex
  else if Assigned(pData^.FileInformation) then begin
    case Kind of
      ikNormal, ikSelected: begin
        if (pData^.FileInformation.IsDirectory and not ColoredFoldersAccessView1.Checked) or
            not (pData^.FileInformation.IsDirectory) then
          ImageIndex := pData^.ImageIndex;
      end;

      ikState : begin
        if pData^.FileInformation.IsDirectory and ColoredFoldersAccessView1.Checked then begin
          if (pData^.FileInformation.Name = '..') then
            ImageIndex := IMAGE_FOLDER_PREV
          else
            ImageIndex := GetFolderImageIndex(pData^.FileInformation.Access);
        end;
      end;
    end;
  end;
end;

procedure TControlFormFileManager.VSTFilesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TFileTreeData);
end;

procedure TControlFormFileManager.VSTFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  var pData := PFileTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData) and Assigned(pData^.DriveInformation) then begin
    // Drives ----------------------------------------------------------------------------------------------------------
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
  // -------------------------------------------------------------------------------------------------------------------
  end else if Assigned(pData) and Assigned(pData^.FileInformation) then begin
    // Files -----------------------------------------------------------------------------------------------------------
    if (pData^.FileInformation.Name = '..') then begin
      if Column = 0 then
        CellText := '< .. >';
    end else begin
      case Column of
        0 : CellText := pData^.FileInformation.Name;
        1 : CellText := pData^.FileInformation.TypeDescription;

        2 : begin
          if not pData^.FileInformation.IsDirectory then
            CellText := FormatFileSize(pData^.FileInformation.Size);
        end;

        3 : CellText := AccessSetToReadableString(pData^.FileInformation.Access);
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
  end;
  // -------------------------------------------------------------------------------------------------------------------

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TControlFormFileManager.VSTFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if TBaseVirtualTree(Sender).GetNodeAt(Point(X, Y)) = nil then begin
    TBaseVirtualTree(Sender).ClearSelection();

    TBaseVirtualTree(Sender).FocusedNode := nil;
  end;
end;

procedure TControlFormFileManager.VSTFoldersChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormFileManager.VSTFoldersCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  var pData1 := PFolderTreeData(Node1.GetData);
  var pData2 := PFolderTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := 0
  else
    Result := CompareText(pData1^.Information.Name, pData2^.Information.Name);
end;

procedure TControlFormFileManager.VSTFoldersDblClick(Sender: TObject);
begin
  var pNode := VSTFolders.FocusedNode;
  if not Assigned(pNode) then
    Exit();
  ///

  var pData := PFolderTreeData(pNode.GetData);

  ///
  BrowsePath(pData^.Information.Path);
end;

procedure TControlFormFileManager.VSTFoldersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormFileManager.VSTFoldersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PFolderTreeData(Node.GetData);
  if Assigned(pData) and Assigned(pData^.Information) then
    FreeAndNil(pData^.Information);
end;

procedure TControlFormFileManager.VSTFoldersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PFolderTreeData(Node.GetData);
  if not Assigned(pData) or (Column <> 0) or ((Kind <> ikNormal) and (Kind <> ikSelected)) then
    Exit();
  ///

  ImageIndex := pData^.ImageIndex;
end;

procedure TControlFormFileManager.VSTFoldersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TFolderTreeData);
end;

procedure TControlFormFileManager.VSTFoldersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PFolderTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData) then begin
    case Column of
      0 : CellText := pData^.Information.Name;
    end;
  end;

  CellText := DefaultIfEmpty(CellText);
end;

procedure TControlFormFileManager.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  inherited;
  ///

  // -------------------------------------------------------------------------------------------------------------------
  if AOptixPacket is TOptixCommandRefreshDrives then
    DisplayDrives(TOptixCommandRefreshDrives(AOptixPacket))
  // -------------------------------------------------------------------------------------------------------------------
  else if AOptixPacket is TOptixCommandRefreshFiles then
    DisplayFiles(TOptixCommandRefreshFiles(AOptixPacket));
  // -------------------------------------------------------------------------------------------------------------------
end;

procedure TControlFormFileManager.DisplayDrives(const AList : TOptixCommandRefreshDrives);
begin
  SetDisplayMode(dmDrives);
  ///

  if not Assigned(AList) then
    Exit();
  ///

  var AFolders := TObjectList<TSimpleFolderInformation>.Create(True);

  VSTFiles.BeginUpdate();
  try
    for var ADrive in AList.List do begin
      var pNode := VSTFiles.AddChild(nil);
      var pData := PFileTreeData(pNode.GetData);
      ///

      pData^.DriveInformation := TDriveInformation.Create();
      pData^.DriveInformation.Assign(ADrive);
      pData^.FileInformation := nil;
      pData^.ImageIndex  := SystemFileIcon(IncludeTrailingPathDelimiter(ADrive.Letter));

      ///
      AFolders.Add(TSimpleFolderInformation.Create(ADrive.Letter, ADrive.Letter, [], True));
    end;
  finally
    RegisterFoldersInTree(nil, AFolders);

    VSTFiles.EndUpdate();

    if Assigned(AFolders) then
      FreeAndNil(AFolders);
  end;
end;

procedure TControlFormFileManager.DisplayFiles(const AList : TOptixCommandRefreshFiles);
begin
  SetDisplayMode(dmFiles);
  ///

  if not Assigned(AList) then
    Exit();
  ///

  EditPath.Text := AList.Path;
  LabelAccess.Caption := AccessSetToString(AList.Access);

  ButtonUpload.Enabled := faWrite in AList.Access;

  var AFolders := TObjectList<TSimpleFolderInformation>.Create(True);

  VSTFiles.BeginUpdate();
  try
    for var AFile in AList.Files do begin
      var pNode := VSTFiles.AddChild(nil);
      var pData := PFileTreeData(pNode.GetData);
      ///

      pData^.DriveInformation := nil;
      pData^.FileInformation := TFileInformation.Create();
      pData^.FileInformation.Assign(AFile);

      if AFile.IsDirectory then begin
        pData^.ImageIndex := SystemFolderIcon();

        ///
        if not MatchStr(pData^.FileInformation.Name, ['.', '..']) then
          AFolders.Add(
            TSimpleFolderInformation.Create(
              pData^.FileInformation.Name,
              pData^.FileInformation.Path,
              pData^.FileInformation.Access,
              False
            )
          );
      end else
        pData^.ImageIndex := SystemFileIcon(AFile.Name, True);
    end;
  finally
    RegisterFoldersInTree(AList.ParentFolders, AFolders);

    VSTFiles.EndUpdate();

    ///
    if Assigned(AFolders) then
      FreeAndNil(AFolders);
  end;
end;

procedure TControlFormFileManager.DownloadFile1Click(Sender: TObject);
begin
  var pNode := VSTFiles.FocusedNode;
  if not Assigned(pNode) then
    Exit();

  var pData := PFileTreeData(pNode.GetData);
  if not CanNodeFileBeDownloaded(pData) then
    Exit();

  ///
  RequestFileDownload(IncludeTrailingPathDelimiter(EditPath.Text) + pData^.FileInformation.Name);
end;

procedure TControlFormFileManager.ShowFolderTree1Click(Sender: TObject);
begin
  MultiPanel.PanelCollection.Items[0].Visible := TMenuItem(Sender).Checked;
end;

procedure TControlFormFileManager.FormDestroy(Sender: TObject);
begin
  VSTFiles.Clear();
  ///

  if Assigned(FPathHistory) then
    FreeAndNil(FPathHistory);
end;

procedure TControlFormFileManager.FullCollapse1Click(Sender: TObject);
begin
  VSTFolders.FullCollapse(nil);
end;

procedure TControlFormFileManager.FullExpand1Click(Sender: TObject);
begin
  VSTFolders.FullExpand(nil);
end;

end.
