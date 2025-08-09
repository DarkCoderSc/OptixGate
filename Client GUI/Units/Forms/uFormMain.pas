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

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.Menus, Optix.Protocol.SessionHandler, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TTreeData = record
    Handler : TOptixSessionHandlerThread;
  end;
  PTreeData = ^TTreeData;

  TFormMain = class(TForm)
    VST: TVirtualStringTree;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    ConnecttoServer1: TMenuItem;
    N1: TMenuItem;
    ImageCollection: TImageCollection;
    VirtualImageList: TVirtualImageList;
    PopupMenu: TPopupMenu;
    RemoveClient1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure Close1Click(Sender: TObject);
    procedure ConnecttoServer1Click(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PopupMenuPopup(Sender: TObject);
    procedure RemoveClient1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses Optix.Thread, Optix.VCL.Helper, Optix.Helper, uFormConnectToServer, Optix.Constants;

{$R *.dfm}

procedure TFormMain.Close1Click(Sender: TObject);
begin
  Close();
end;

procedure TFormMain.ConnecttoServer1Click(Sender: TObject);
begin
  var AForm := TFormConnectToServer.Create(self);
  try
    AForm.ShowModal();
    if AForm.Canceled then
      Exit();
    ///

    VST.beginUpdate();
    try
      var pNode := VST.AddChild(nil);
      var pData := PTreeData(pNode.GetData);

      pData^.Handler := TOptixSessionHandlerThread.Create(AForm.EditServerAddress.Text, AForm.SpinPort.Value);
      pData^.Handler.Retry := True;
      pData^.Handler.RetryDelay := 1000;
      pData^.Handler.Start();
    finally
      VST.EndUpdate();
    end;
  finally
    FreeAndNil(AForm);
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ///
  TOptixThread.SignalHiveAndFlush();
end;

procedure TFormMain.PopupMenuPopup(Sender: TObject);
begin
  RemoveClient1.Visible :=  VST.FocusedNode <> nil;
end;

procedure TFormMain.RemoveClient1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  VST.BeginUpdate();
  try
    VST.DeleteNode(VST.FocusedNode);
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormMain.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormMain.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  if Assigned(pData^.Handler) then
    pData^.Handler.Terminate;
end;

procedure TFormMain.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);

  if (Column <> 0) or ((Kind <> TVTImageKind.ikNormal) and (Kind <> TVTImageKind.ikSelected)) then
    Exit();

  ImageIndex := IMAGE_CLIENT_WAIT;
end;

procedure TFormMain.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormMain.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData^.Handler) then
    Exit();

  CellText := '';

  case Column of
    0 : CellText := pData^.Handler.RemoteAddress;
    1 : CellText := IntToStr(pData^.Handler.RemotePort);
  end;

  CellText := DefaultIfEmpty(CellText);
end;

end.
