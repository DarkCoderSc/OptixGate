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

unit uControlFormProcessManager;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus,

  VirtualTrees, VirtualTrees.AncestorVCL, VirtualTrees.Types, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree,
  XSuperObject,

  __uBaseFormControl__, uControlFormDumpProcess,

  Optix.Func.Enum.Process, Optix.WinApiEx;
// ---------------------------------------------------------------------------------------------------------------------

type
  TTreeData = record
    ProcessInformation : TProcessInformation;
  end;
  PTreeData = ^TTreeData;

  TFilterKind = (
    fkDifferentArch,
    fkUnreachable
  );
  TFilterKinds = set of TFilterKind;

  TControlFormProcessManager = class(TBaseFormControl)
    PopupMenu: TPopupMenu;
    VST: TVirtualStringTree;
    Refresh1: TMenuItem;
    N1: TMenuItem;
    Exclude1: TMenuItem;
    DifferentArchitecture1: TMenuItem;
    UnreachableProcess1: TMenuItem;
    Options1: TMenuItem;
    ColorBackground1: TMenuItem;
    N2: TMenuItem;
    KillProcess1: TMenuItem;
    N3: TMenuItem;
    Clear1: TMenuItem;
    DumpProcess1: TMenuItem;
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
    procedure DifferentArchitecture1Click(Sender: TObject);
    procedure UnreachableProcess1Click(Sender: TObject);
    procedure ColorBackground1Click(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure KillProcess1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure DumpProcess1Click(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
  private
    FClientArchitecture          : TProcessorArchitecture;
    FRemoteProcessorArchitecture : TProcessorArchitecture;

    {@M}
    procedure Refresh(const AProcessList : TProcessList);
    procedure ApplyFilters(const AFilters : TFilterKinds = []);
    procedure ApplyFilterSettings();
    function GetNodeByProcessId(const AProcessId : Cardinal) : PVirtualNode;
    procedure RemoveProcess(const AProcessId : Cardinal);
    function GetImageIndex(const pData : PTreeData) : Integer;
    procedure RefreshProcess();
  protected
    {@M}
    function GetContextDescription() : String; override;
    procedure RefreshCaption(); override;
    procedure OnFirstShow(); override;
  public
    {@M}
    procedure ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject); override;

    {@C}
    constructor Create(AOwner : TComponent; const AUserIdentifier : String; const AClientArchitecture : TProcessorArchitecture; const ARemoteProcessorArchitecture : TProcessorArchitecture); reintroduce;
  end;

var
  ControlFormProcessManager: TControlFormProcessManager;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math, System.DateUtils,

  uFormMain,

  Optix.Func.Commands, Optix.Helper, Optix.Shared.Types, Optix.Process.Helper, Optix.Constants, Optix.VCL.Helper,
  Optix.Protocol.Packet;
 // ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormProcessManager.OnFirstShow();
begin
  inherited;
  ///

  RefreshProcess();
end;

procedure TControlFormProcessManager.RefreshProcess();
begin
  SendCommand(TOptixCommandRefreshProcess.Create());
end;

function TControlFormProcessManager.GetContextDescription() : String;
begin
  result := inherited;
  ///

  var ACount := VST.RootNodeCount;
  if ACount > 0 then
    result := Format('%d process in list.', [ACount])
  else
    result := 'Process list empty.';
end;

procedure TControlFormProcessManager.RefreshCaption();
begin
  inherited;
  ///

  Caption := Caption + ' - ' + ProcessorArchitectureToString(FRemoteProcessorArchitecture);
end;

function TControlFormProcessManager.GetNodeByProcessId(const AProcessId : Cardinal) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData := PTreeData(pNode.GetData);
    if not Assigned(pData^.ProcessInformation) then
      continue;
    ///

    if pData^.ProcessInformation.Id = AProcessId then begin
      result := pNode;

      break;
    end;
  end;
end;

procedure TControlFormProcessManager.RemoveProcess(const AProcessId : Cardinal);
begin
  var pNode := GetNodeByProcessid(AProcessId);
  if not Assigned(pNode) then
    Exit();
  ///

  VST.DeleteNode(pNode);
end;

procedure TControlFormProcessManager.ApplyFilterSettings();
begin
  var AFilters : TFilterKinds := [];

  if DifferentArchitecture1.Checked then
    Include(AFilters, fkDifferentArch);

  if UnreachableProcess1.Checked then
    Include(AFilters, fkUnreachable);

  ///
  ApplyFilters(AFilters);
end;

procedure TControlFormProcessManager.Clear1Click(Sender: TObject);
begin
  VST.Clear();
end;

procedure TControlFormProcessManager.ColorBackground1Click(Sender: TObject);
begin
  VST.Refresh();
end;

procedure TControlFormProcessManager.ApplyFilters(const AFilters : TFilterKinds = []);
begin
  VST.BeginUpdate();
  try
    for var pNode in VST.Nodes do begin
      var AExclude := False;
      var pData := PTreeData(pNode.GetData);
      try
        if not Assigned(pData^.ProcessInformation) then begin
          AExclude := True;

          ///
          continue;
        end;

        ///
        if (fkDifferentArch in AFilters) then begin
              AExclude := (pData^.ProcessInformation.IsWow64Process <> brError) and
                          (
                            ((FClientArchitecture = pa86_32) and (pData^.ProcessInformation.IsWow64Process = brFalse)) or
                            ((FClientArchitecture = pa86_64) and (pData^.ProcessInformation.IsWow64Process = brTrue))
                          );
        end;

        if AExclude then
          continue;

        if (fkUnreachable in AFilters) then begin
          // Good sign, it is!
          AExclude := (String.IsNullOrEmpty(pData^.ProcessInformation.ImagePath) and (pData^.ProcessInformation.Elevated = esUnknown));
        end;
      finally
        VST.IsVisible[pNode] := not AExclude;
      end;
    end;
  finally
    VST.EndUpdate();
  end;
end;

constructor TControlFormProcessManager.Create(AOwner : TComponent; const AUserIdentifier : String; const AClientArchitecture : TProcessorArchitecture; const ARemoteProcessorArchitecture : TProcessorArchitecture);
begin
  inherited Create(AOwner, AUserIdentifier);
  ///

  FClientArchitecture          := AClientArchitecture;
  FRemoteProcessorArchitecture := ARemoteProcessorArchitecture;
end;

procedure TControlFormProcessManager.DifferentArchitecture1Click(Sender: TObject);
begin
  ApplyFilterSettings();
end;

procedure TControlFormProcessManager.DumpProcess1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  var pData := PTreeData(VST.FocusedNode.GetData);

  //SendCommand(TOptixCommandProcessDump.Create(0, 'c:\temp\process_dump.dmp'));

  var ADialog := TControlFormDumpProcess.Create(
    self,
    pData^.ProcessInformation.Name,
    pData^.ProcessInformation.Id,
    FFormInformation.UserIdentifier,
    GetImageIndex(pData)
  );

  RegisterNewDialogAndShow(ADialog);
end;

procedure TControlFormProcessManager.KillProcess1Click(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  var pData := PTreeData(VST.FocusedNode.GetData);

  if Application.MessageBox('You are about to terminate a process. This action may affect system stability. Do you want to continue?', 'Kill Process', MB_ICONQUESTION + MB_YESNO) = ID_NO then
    Exit();

  ///
  SendCommand(TOptixCommandKillProcess.Create(pData^.ProcessInformation.Id));
end;

procedure TControlFormProcessManager.PopupMenuPopup(Sender: TObject);
begin
  KillProcess1.Visible := VST.FocusedNode <> nil;
  DumpProcess1.Visible := KillProcess1.Visible;
end;

procedure TControlFormProcessManager.Refresh(const AProcessList : TProcessList);
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
    ApplyFilterSettings();

    ///
    VST.EndUpdate();
  end;
end;

procedure TControlFormProcessManager.Refresh1Click(Sender: TObject);
begin
  RefreshProcess();
end;

procedure TControlFormProcessManager.UnreachableProcess1Click(Sender: TObject);
begin
  ApplyFilterSettings();
end;

procedure TControlFormProcessManager.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if not self.ColorBackground1.Checked then
    Exit();
  ///

  var pData := PTreeData(Node.GetData);

  if not Assigned(pData^.ProcessInformation) then
    Exit();

  var AColor := clNone;

  if pData^.ProcessInformation.IsCurrentProcess then
    AColor := COLOR_LIST_LIMY
  else begin
    if pData^.ProcessInformation.IsSystem then
      AColor := COLOR_USER_SYSTEM
    else if pData^.ProcessInformation.Elevated = esElevated then
      AColor := COLOR_USER_ELEVATED;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TControlFormProcessManager.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormProcessManager.VSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

  function GetElevationOrder(const AValue: TElevatedStatus): Integer;
  begin
    case AValue of
      esElevated : result := 0;
      esLimited  : result := 1;
      else
        result := 2;
    end;
  end;

begin
  var pData1 := PTreeData(Node1.GetData);
  var pData2 := PTreeData(Node2.GetData);
  ///

  if (not Assigned(pData1) or not Assigned(pData2)) or
     (not Assigned(pData1^.ProcessInformation) or not Assigned(pData2^.ProcessInformation)) then
    Result := 0
  else begin
    case Column of
      0  : Result := CompareText(pData1^.ProcessInformation.Name, pData2^.ProcessInformation.Name);
      1  : Result := CompareValue(pData1^.ProcessInformation.id, pData2^.ProcessInformation.Id);
      2  : Result := CompareValue(pData1^.ProcessInformation.ParentId, pData2^.ProcessInformation.ParentId);
      3  : Result := CompareValue(pData1^.ProcessInformation.ThreadCount, pData2^.ProcessInformation.ThreadCount);
      4  : Result := CompareText(pData1^.ProcessInformation.Username, pData2^.ProcessInformation.Username);
      5  : Result := CompareText(pData1^.ProcessInformation.Domain, pData2^.ProcessInformation.Domain);
      6  : Result := CompareValue(pData1^.ProcessInformation.SessionId, pData2^.ProcessInformation.SessionId);
      7  : Result := CompareValue(
                                    GetElevationOrder(pData1^.ProcessInformation.Elevated),
                                    GetElevationOrder(pData2^.ProcessInformation.Elevated)
                     );
      8  : Result := CompareDate(pData1^.ProcessInformation.CreatedTime, pData2^.ProcessInformation.CreatedTime);
      9  : Result := CompareText(pData1^.ProcessInformation.CommandLine, pData2^.ProcessInformation.CommandLine);
      10 : Result := CompareText(pData1^.ProcessInformation.ImagePath, pData2^.ProcessInformation.ImagePath);
    end;
  end;
end;

procedure TControlFormProcessManager.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TControlFormProcessManager.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var pData := PTreeData(Node.GetData);
  ///

  if Assigned(pData^.ProcessInformation) then
    FreeAndNil(pData^.ProcessInformation);
end;

function TControlFormProcessManager.GetImageIndex(const pData : PTreeData) : Integer;
begin
  result := -1;
  ///

  if not Assigned(pData) then
    Exit();

  if pData^.ProcessInformation.IsCurrentProcess then
    result := IMAGE_PROCESS_SELF
  else begin
    if (FRemoteProcessorArchitecture = pa86_64) and
       (pData^.ProcessInformation.IsWow64Process <> brError) then begin
      if pData^.ProcessInformation.IsWow64Process = brTrue then
        result := IMAGE_PROCESS_X86_32
      else
        result := IMAGE_PROCESS_X86_64;
    end else
      result := IMAGE_PROCESS;
  end;
end;

procedure TControlFormProcessManager.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var pData := PTreeData(Node.GetData);
  if not Assigned(pData^.ProcessInformation) or (Column <> 0) then
    Exit();
  ///

  case Kind of
    TVTImageKind.ikNormal, TVTImageKind.ikSelected : ImageIndex := GetImageIndex(pData);
  end;
end;

procedure TControlFormProcessManager.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TControlFormProcessManager.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  var pData := PTreeData(Node.GetData);

  CellText := '';

  if not Assigned(pData^.ProcessInformation) then
    Exit();

  case Column of
    0  : begin
      if (FRemoteProcessorArchitecture = pa86_64) and
         (pData^.ProcessInformation.IsWow64Process = brTrue) then begin
        CellText := Format('%s (32 bit)', [pData^.ProcessInformation.Name]);
      end else
        CellText := pData^.ProcessInformation.Name;
    end;

    1  : CellText := FormatInt(pData^.ProcessInformation.Id);
    2  : CellText := FormatInt(pData^.ProcessInformation.ParentId);
    3  : CellText := IntToStr(pData^.ProcessInformation.ThreadCount);
    4  : CellText := pData^.ProcessInformation.Username;
    5  : CellText := pData^.ProcessInformation.Domain;
    6  : CellText := IntToStr(pData^.ProcessInformation.SessionId);
    7  : CellText := ElevatedStatusToString(pData^.ProcessInformation.Elevated);
    8  : CellText := DateTimeToStr(pData^.ProcessInformation.CreatedTime);
    9  : CellText := pData^.ProcessInformation.CommandLine;
    10 : CellText := pData^.ProcessInformation.ImagePath;
  end;

  ///
  CellText := DefaultIfEmpty(CellText);
end;

procedure TControlFormProcessManager.ReceivePacket(const AClassName : String; const ASerializedPacket : ISuperObject);
begin
  inherited;
  ///

  var AOptixPacket : TOptixPacket := nil;
  try
    // -------------------------------------------------------------------------
    if AClassName = TProcessList.ClassName then begin
      AOptixPacket := TProcessList.Create(ASerializedPacket);

      Refresh(TProcessList(AOptixPacket));
    end
    // -------------------------------------------------------------------------
    else if AClassName = TOptixCommandKillProcess.ClassName then begin
      AOptixPacket := TOptixCommandKillProcess.Create(ASerializedPacket);

      RemoveProcess(TOptixCommandKillProcess(AOptixPacket).ProcessId);
    end;
    // -------------------------------------------------------------------------
  finally
    if Assigned(AOptixPacket) then
      FreeAndNil(AOptixPacket);
  end;
end;

end.
