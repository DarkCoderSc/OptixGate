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
    - Hide / Show certain columns when in display drive / file mode
    - Hide / Show TEdit Path when in display drive / file mode
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
  end;
  PTreeData = ^TTreeData;

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
  private
    {@M}
    procedure RefreshDrives(const AList : TDriveList);
  protected
    {@M}
    function GetContextDescription() : String; override;
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;
  end;

var
  FormFileManager: TFormFileManager;

implementation

uses uFormMain, Optix.Func.Commands, Optix.Protocol.Packet, Optix.Helper,
     Optix.FileSystem.Helper, Optix.Constants;

{$R *.dfm}

procedure TFormFileManager.FormShow(Sender: TObject);
begin
  RefreshDrives1Click(RefreshDrives1);
end;

function TFormFileManager.GetContextDescription() : String;
begin
  ///
end;

procedure TFormFileManager.RefreshDrives1Click(Sender: TObject);
begin
  SendCommand(TOptixRefreshDrives.Create());
end;

procedure TFormFileManager.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
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
  end;
end;

procedure TFormFileManager.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(NodeDataSize);
end;

procedure TFormFileManager.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if Assigned(pData^.DriveInformation) then begin
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
          CellText := Format('%s(%d) / %s', [
            FormatFileSize(pData^.DriveInformation.UsedSize),
            pData^.DriveInformation.UsedPercentage,
            FormatFileSize(pData^.DriveInformation.TotalSize)
          ]);
      end;
    end;
  end else if Assigned(pData^.FileInformation) then begin

  end;

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
    end;
    // -------------------------------------------------------------------------
  finally
    if Assigned(AOptixPacket) then
      FreeAndNil(AOptixPacket);
  end;
end;

procedure TFormFileManager.RefreshDrives(const AList : TDriveList);
begin
  VST.Clear();
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

end.
