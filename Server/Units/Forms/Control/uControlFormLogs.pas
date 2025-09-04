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

unit uControlFormLogs;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Variants, System.Classes, System.SysUtils,

  Winapi.Windows, Winapi.Messages,

  VirtualTrees.Types, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees,
  XSuperObject,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  __uBaseFormControl__,

  Optix.Func.LogNotifier;
// ---------------------------------------------------------------------------------------------------------------------

type
  TTreeData = record
    LogMessage : String;
    Context    : String;
    LogKind    : TLogKind;
    LogDate    : TDateTime;
  end;
  PTreeData = ^TTreeData;

  TControlFormLogs = class(TBaseFormControl)
    VST: TVirtualStringTree;
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
  private
    {@M}
    procedure AddLog(const AMessage, AContext : String; const AKind : TLogKind);
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;
  end;

var
  ControlFormLogs: TControlFormLogs;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math, System.DateUtils,

  uFormMain, uControlFormTransfers,

  Optix.Helper, Optix.Protocol.Packet, Optix.Constants, Optix.VCL.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormLogs.AddLog(const AMessage, AContext : String; const AKind : TLogKind);
begin
  var pNode := VST.AddChild(nil);
  var pData := PTreeData(pNode.GetData);

  pData^.LogDate    := Now;

  pData^.LogMessage := AMessage;
  pData^.Context    := AContext;
  pData^.LogKind    := AKind;

  ///
  VST.Update();
end;

procedure TControlFormLogs.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  inherited;
  ///

  if not Assigned(ASerializedPacket) then
    Exit();
  ///

  var ALogNotifier : TLogNotifier := nil;
  try
    // -----------------------------------------------------------------------------------------------------------------
    if (AClassName = TLogNotifier.ClassName) then
      ALogNotifier := TLogNotifier.Create(ASerializedPacket)
    // -----------------------------------------------------------------------------------------------------------------
    else if (AClassName = TLogTransferException.ClassName) then begin
      ALogNotifier := TLogTransferException.Create(ASerializedPacket);

      // Notify concerned transfer
      var ATransfersForm := TControlFormTransfers(FormMain.GetControlForm(self, TControlFormTransfers));
      if Assigned(aTransfersForm) then
        ATransfersForm.ApplyTransferException(TLogTransferException(ALogNotifier));
    end;
    // -----------------------------------------------------------------------------------------------------------------
  finally
    if Assigned(ALogNotifier) then begin
      AddLog(ALogNotifier.DetailedMessage, ALogNotifier.Context, ALogNotifier.Kind);

      ///
      FreeAndNil(ALogNotifier);
    end;
  end;

  ///
  TOptixVCLHelper.ShowForm(self);
end;

procedure TControlFormLogs.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var pData := PTreeData(Node.GetData);

  var AColor := clNone;

  case pData^.LogKind of
    lkException : AColor := COLOR_LIST_RED;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TControlFormLogs.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormLogs.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Result := 0
  else begin
    case Column of
      0 : Result := CompareText(pData1^.LogMessage, pData2^.LogMessage);
      1 : Result := CompareText(pData1^.Context, pData2^.Context);
      2 : Result := CompareValue(Cardinal(pData1^.LogKind), Cardinal(pData2^.LogKind));
      3 : Result := CompareDateTime(pData1^.LogDate, pData2^.LogDate);
    end;
  end;
end;

procedure TControlFormLogs.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormLogs.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);

  if Column <> 0 then
    Exit();

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected : begin
      case pData^.LogKind of
        lkException : ImageIndex := IMAGE_EXCEPTION;
        // ... //
      end;
    end;
  end;
end;

procedure TControlFormLogs.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TControlFormLogs.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  case Column of
    0 : CellText := pData^.LogMessage;
    1 : CellText := pData^.Context;
    2 : CellText := LogKindToString(pData^.LogKind);
    3 : CellText := DateTimeToStr(pData^.LogDate);
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

end.
