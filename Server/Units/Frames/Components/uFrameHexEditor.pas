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



(*
  TODO:
    - Implement Lazy Loading like I did in an old version of Winja Hex Editor (Extra Tool)
    - Support unicode text for ascii section
    - Select more than one cell
    - Copy / Paste / Cut
*)

unit uFrameHexEditor;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees, VirtualTrees.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TGridSectionKind = (
    gskNone,
    gskHex,
    gskAscii
  );

  TFrameHexEditor = class(TFrame)
    VST: TVirtualStringTree;
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure VSTKeyPress(Sender: TObject; var Key: Char);
    procedure VSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    const
      MINIMUM_PAGE_SIZE = 1024;
      DEFAULT_PAGESIZE  = MINIMUM_PAGE_SIZE * 8;
      ROW_SIZE          = 16;

    var
      FPageSize              : UInt64;
      FBuffer                : Pointer;
      FTotalBufferSize       : UInt64;
      FRealSize              : UInt64;
      FOffsetBase            : UInt64;

      FSelKind               : TGridSectionKind;
      FSelStart              : Int64;
      FSelEnd                : Int64;

      FEditHexCellPos        : Byte;

      FReadOnly              : Boolean;
      FExpandable            : Boolean;
      FSelectFirstCellOnInit : Boolean;

    {@M}
    procedure RefreshHexGrid();
    procedure InitializeGridHeader();
    procedure UnselectAll();
    procedure UpdateSelection(const ASelStart : Int64);
    procedure InvalidateNodes(const AFromSel : Int64; AToSel : Int64 = -1);
    procedure InvalidateSelectedNodes();
    function GetNodeByIndex(const AIndex : Int64) : PVirtualNode;
    procedure UpdateRealSize(const AValue : Int64);
    procedure IncRealSize();
    procedure FreeBytesAndRelocate(const AFreeFrom : UInt64; AFreeTo : UInt64 = 0);
    procedure SetPageSize(const AValue : UInt64);
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure LoadData(const pData : Pointer; const ADataSize : UInt64; const AOffsetBase : UInt64 = 0;
      const AReadOnly : Boolean = False);

    procedure Clear();

    {@G}
    property Data            : Pointer read FBuffer;
    property DataSize        : UInt64  read FRealSize;
    property TotalBufferSize : UInt64  read FTotalBufferSize;
  published
    {@G/S}
    property Expandable            : Boolean read FExpandable            write FExpandable;
    property SelectFirstCellOnInit : Boolean read FSelectFirstCellOnInit write FSelectFirstCellOnInit;
    property PageSize              : UInt64  read FPageSize              write SetPageSize;
  end;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.StrUtils, System.Math, Optix.Constants;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}
procedure TFrameHexEditor.Clear();
begin
  FSelStart := -1;
  FSelEnd   := -1;
  FSelKind  := gskNone;

  if not FReadOnly and Assigned(FBuffer) then begin
    FEditHexCellPos := 0;
    ZeroMemory(FBuffer, FTotalBufferSize);

    if FExpandable then begin
      ReallocMem(FBuffer, FPageSize);

      ///
      FTotalBufferSize := FPageSize;
      FRealSize := 0;

      RefreshHexGrid();
    end;
  end;

  ///
  VST.Refresh();
end;
constructor TFrameHexEditor.Create(AOwner : TComponent);
begin
  inherited;
  ///

  InitializeGridHeader();

  FReadOnly := False;
  FExpandable := True;
  FSelectFirstCellOnInit := True;

  FPageSize        := DEFAULT_PAGESIZE;
  FTotalBufferSize := FPageSize;
  FRealSize        := 0;
  FEditHexCellPos  := 0;
  FOffsetBase      := 0;

  GetMem(FBuffer, FPageSize);
  ZeroMemory(FBuffer, FPageSize);

  FSelKind := gskNone;

  FSelStart := -1;
  FSelEnd   := -1;

  if FSelectFirstCellOnInit then begin
    Inc(FSelStart);
    Inc(FSelEnd);

    FSelKind := gskHex;
  end;

  ///
  RefreshHexGrid();
end;
destructor TFrameHexEditor.Destroy();
begin
  if Assigned(FBuffer) then
    FreeMem(FBuffer, FTotalBufferSize);

  ///
  inherited;
end;

procedure TFrameHexEditor.VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  if not (HitInfo.HitColumn in [1..ROW_SIZE, ROW_SIZE +1..(ROW_SIZE * 2) +1]) or not Assigned(HitInfo.HitNode) then
    Exit();

  var AByteIndex : Byte;
  if HitInfo.HitColumn in [1..ROW_SIZE] then begin
    AByteIndex := HitInfo.HitColumn -1;

    ///
    FSelKind := gskHex;
  end else begin
    AByteIndex := HitInfo.HitColumn - (ROW_SIZE +1);

    ///
    FSelKind := gskAscii;
  end;

  var ACandidate := (HitInfo.HitNode.Index * ROW_SIZE) + AByteIndex;
  UpdateSelection(ACandidate);
end;

procedure TFrameHexEditor.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var AColor := clNone;

  var AByteIndex : Integer := -1;
  var AGridSectionKind := gskNone;

  case Column of
    0 : AColor := RGB(240, 240, 240);

    1..ROW_SIZE : begin
      AByteIndex := Column - 1;

      AGridSectionKind := gskHex;

      if odd(Column mod 2) then
        AColor := RGB(240, 240, 240);
    end;

    ROW_SIZE +1..(ROW_SIZE * 2) +1 : begin
      AGridSectionKind := gskAscii;

      AByteIndex := Column - (ROW_SIZE +1);
    end;
  end;

  if AByteIndex >= 0 then begin
    var ABufferPos : Int64 := (Integer(Node.Index) * ROW_SIZE) + AByteIndex;
    if (ABufferPos >= FSelStart) and (ABufferPos <= FSelEnd) then begin
      if AGridSectionKind = FSelKind then
        AColor := COLOR_LIST_DARKER_BLUE
      else
        AColor := COLOR_LIST_BLUE;
    end;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFrameHexEditor.VSTGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := 0;
end;

procedure TFrameHexEditor.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  CellText := '';
  ///

  if not Assigned(FBuffer) or (FRealSize <= 0) then
    Exit();
  ///

  var pRowOffset := PByte(NativeUInt(FBuffer) + (Node.Index * ROW_SIZE));

  case Column of
    // Offset
    0 : begin
      var AOffset := FOffsetBase + (Node.Index * ROW_SIZE);
      var AWidth := IfThen(AOffset > High(Cardinal), 16, 8);
      CellText := IntToHex(AOffset, AWidth);
    end;

    // Hex
    1..ROW_SIZE : begin
      var pColumnOffset := PByte(NativeUInt(pRowOffset) + Cardinal(Column -1));
      if NativeUInt(pColumnOffset) < NativeUInt(FBuffer) + FRealSize then
        CellText := IntToHex(pColumnOffset^);
    end;

    // Ascii
    ROW_SIZE +1..(ROW_SIZE * 2) +1 : begin
      var pColumnOffset := PByte(NativeUInt(pRowOffset) + Cardinal(Column - (ROW_SIZE +1)));
      if NativeUInt(pColumnOffset) < NativeUInt(FBuffer) + FRealSize then begin
        var AByte := PByte(NativeUInt(pRowOffset) + Cardinal(Column - (ROW_SIZE +1)))^;
        if AByte in [32..126] then
          CellText := Chr(AByte)
        else
          CellText := '.';
      end;
    end;
  end;
end;

procedure TFrameHexEditor.VSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT   : UpdateSelection(FSelStart -1);
    VK_UP     : UpdateSelection(FSelStart - ROW_SIZE);
    VK_RIGHT  : UpdateSelection(FSelStart +1);
    VK_DOWN   : UpdateSelection(FSelStart + ROW_SIZE);
    VK_RETURN : UpdateSelection(FSelStart+1);
    VK_END    : ; // TODO
    VK_HOME   : ; // TODO
  end;

  if FReadOnly or (FSelStart < 0) then
    Exit();
  ///

  case Key of
    VK_BACK : begin
      if FSelStart > 0 then
        FreeBytesAndRelocate(FSelStart-1);
    end;

    VK_DELETE : FreeBytesAndRelocate(FSelStart);
  end;
end;

procedure TFrameHexEditor.VSTKeyPress(Sender: TObject; var Key: Char);
begin
  if FReadOnly then
    Exit();
  ///

  const HEX_CANDIDATES = ['a'..'f', 'A'..'F', '0'..'9'];
  const ASCII_CANDIDATES = [#13, #32..#126];
  ///

  var AIsHex := (FSelKind = gskHex) and CharInSet(Key, HEX_CANDIDATES);
  var AIsAscii := (FSelKind = gskAscii) and CharInSet(Key, ASCII_CANDIDATES);
  if (FSelStart < 0) or (not AIsHex and not AIsAscii) then
    Exit();
  ///

  var ptrByte := PByte(NativeUInt(FBuffer) + UInt64(FSelStart));

  if (FSelStart >= FRealSize) and FExpandable then
    IncRealSize(); // Initialize new cell

  // Hex Values (On Hex Columns)
  if AIsHex then begin
      Inc(FEditHexCellPos);
      ///

      if FEditHexCellPos = 1 then
        ptrByte^ := (ptrByte^ and $0F) or (StrToInt('$' + Key) shl 4)
      else
        ptrByte^ := (ptrByte^ and $F0) or (StrToInt('$' + Key) and $0F);

      InvalidateSelectedNodes();

      if FEditHexCellPos = 2 then begin
        UpdateSelection(FSelStart+1);

        ///
        FEditHexCellPos := 0;
      end else
        InvalidateNodes(FSelStart);
  // Ascii Values (On Ascii Columns)
  end else if AIsAscii then begin
    ptrByte^ := ord(Key);

    UpdateSelection(FSelStart+1);
  end;
end;

procedure TFrameHexEditor.InitializeGridHeader();
begin
  VST.NodeAlignment := naProportional;
  VST.TextMargin    := 0;
  ///

  var AColumns := VST.Header.Columns;
  AColumns.Clear();
  ///

  // Offset
  with AColumns.Add do begin
    Text      := 'Offset (h)';
    Width     := ScaleValue(150);
    Alignment := taCenter;
  end;

  // Hex
  for var I := 0 to ROW_SIZE-1 do begin
    with AColumns.Add do begin
      Text  := IntToHex(I, 2);
      Width := ScaleValue(20);
    end;
  end;

  // Ascii
  for var I := 0 to ROW_SIZE-1 do begin
    with AColumns.Add do begin
      Text    := '';
      Width   := ScaleValue(15);
      Spacing := 0;
      Margin  := 0;

      ///
      if I = 0 then
        Text := 'Ascii';
    end;
  end;
end;

procedure TFrameHexEditor.RefreshHexGrid();
begin
  var ARowCount := Cardinal(ceil((FRealSize +1) / ROW_SIZE));
  ///

  if ARowCount = VST.RootNodeCount then
    Exit();
  ///

  VST.RootNodeCount := ARowCount;
end;

function TFrameHexEditor.GetNodeByIndex(const AIndex : Int64) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    if pNode.Index = AIndex then begin
      result := pNode;

      break;
    end;
  end;
end;

procedure TFrameHexEditor.InvalidateNodes(const AFromSel : Int64; AToSel : Int64 = -1);
begin
  if AToSel = -1 then
    AToSel := AFromSel;
  ///

  var AFromIndex := AFromSel div ROW_SIZE;
  var AToIndex   := AToSel div ROW_SIZE;

  for var pNode in VST.Nodes do begin
    if (pNode.Index >= AFromIndex) and (pNode.Index <= AToIndex) then
      VST.InvalidateNode(pNode)
    else if (pNode.Index > AToIndex) then
      break;
  end;
end;

procedure TFrameHexEditor.InvalidateSelectedNodes();
begin
  if FSelStart >= 0 then
    InvalidateNodes(FSelStart, FSelEnd);
end;

procedure TFrameHexEditor.UnselectAll();
begin
  InvalidateNodes(FSelStart, FSelEnd);
  ///

  FSelStart := -1;
  FSelEnd   := -1;
end;

procedure TFrameHexEditor.FreeBytesAndRelocate(const AFreeFrom : UInt64; AFreeTo : UInt64 = 0);
begin
  if FReadOnly or (AFreeFrom < 0) then
    Exit();

  if AFreeTo <= 0 then
    AFreeTo := AFreeFrom +1;

  if AFreeTo > FRealSize then
    Exit();

  MoveMemory(
    Pointer(NativeUInt(FBuffer) + AFreeFrom),
    Pointer(NativeUInt(FBuffer) + AFreeTo),
    FRealSize - AFreeTo
  );

  Dec(FRealSize, AFreeTo - AFreeFrom);

  FEditHexCellPos := 0;

  ///
  InvalidateNodes(AFreeFrom, FRealSize);
  UpdateSelection(AFreeFrom);
  RefreshHexGrid();
end;

procedure TFrameHexEditor.UpdateRealSize(const AValue : Int64);
begin
  if FReadOnly or (not FExpandable and (AValue > FTotalBufferSize)) or (AValue = FRealSize) then
    Exit();

  if FRealSize +1 > FTotalBufferSize then begin
    Inc(FTotalBufferSize, FPageSize);

    ///
    ReallocMem(FBuffer, FTotalBufferSize);
  end;

  FRealSize := AValue;

  RefreshHexGrid();
end;

procedure TFrameHexEditor.IncRealSize();
begin
  UpdateRealSize(FRealSize +1);
end;

procedure TFrameHexEditor.UpdateSelection(const ASelStart : Int64);
begin
  if (ASelStart <> FSelStart) and (ASelStart >= 0) and (ASelStart < FRealSize +1) then begin
    UnselectAll();
    ///

    FEditHexCellPos := 0;
    FSelStart := ASelStart;
    FSelEnd := FSelStart;

    InvalidateNodes(FSelStart, FSelEnd);

    var pNode := GetNodeByIndex(FSelStart div ROW_SIZE);
    if Assigned(pNode) then
      VST.ScrollIntoView(pNode, False);
  end;
end;

procedure TFrameHexEditor.LoadData(const pData : Pointer; const ADataSize : UInt64; const AOffsetBase : UInt64 = 0;
  const AReadOnly : Boolean = False);
begin
  if not Assigned(pData) or (ADataSize = 0) then
    Exit();
  ///

  if ADataSize > FTotalBufferSize then begin
    ReallocMem(FBuffer, ADataSize);

    FTotalBufferSize := ADataSize;
  end;

  FRealSize   := ADataSize;
  FOffsetBase := AOffsetBase;
  FReadOnly   := AReadOnly;

  CopyMemory(FBuffer, pData, ADataSize);
  ZeroMemory(PByte(NativeUInt(FBuffer) + ADataSize), FTotalBufferSize - ADataSize);

  FEditHexCellPos := 0;
  FSelStart       := -1;
  FSelEnd         := -1;

  RefreshHexGrid();

  ///
  VST.Refresh();
end;

procedure TFrameHexEditor.SetPageSize(const AValue : UInt64);
begin
  if (FPageSize = AValue) then
    Exit();

  if AValue < MINIMUM_PAGE_SIZE then
    FPageSize := MINIMUM_PAGE_SIZE
  else
    FPageSize := AValue;
end;

end.
