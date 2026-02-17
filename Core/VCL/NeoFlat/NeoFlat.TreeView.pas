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
{                   License: (!) CHECK README.md (!)                           }
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
{******************************************************************************}

unit NeoFlat.TreeView;

interface

uses System.Classes, VirtualTrees, VirtualTrees.Types, VCL.Graphics, WinAPI.Windows,
     VCL.ImgList, System.UITypes;

type
  TTreeData = record
    ItemName   : String;
    Index      : Integer;
    ImageIndex : Integer;
  end;
  PTreeData = ^TTreeData;

  TOnItemClick = procedure(Sender : TObject; AIndex : Integer; AItemName : String) of object;

  TFlatVirtualStringTree = class(TCustomVirtualStringTree)
  private
    FOnItemClick : TOnItemClick;
  protected
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure DoNodeClick(const HitInfo: THitInfo); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex): TCustomImageList; override;
  public
    {@M}
    function AddItem(const ACaption : String; const AIndex : Cardinal; AParent : PVirtualNode = nil; const AImageIndex : Integer = -1) : PVirtualNode; overload;
    function AddItem(const ACaption : String; const AIndex : Cardinal; const AImageIndex : Integer = -1) : PVirtualNode; overload;

    {@C}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property Align;
    property Margins;
    property Alignwithmargins;
    property Enabled;
    property Visible;
    property Images;

    {@G/S}
    property OnItemClick : TOnItemClick read FOnItemClick write FOnItemClick;
  end;

implementation

uses NeoFlat.Theme, math, VCL.Forms;

{ TFlatVirtualStringTree.Create }
constructor TFlatVirtualStringTree.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Color := clWhite;
  self.Colors.TreeLineColor := MAIN_ACCENT;

  self.Header.Options := self.Header.Options + [hoAutoResize];

  self.TreeOptions.SelectionOptions := self.TreeOptions.SelectionOptions + [toFullRowSelect];

  self.TreeOptions.PaintOptions := self.TreeOptions.PaintOptions - [
                                                                      toShowRoot,
                                                                      toShowButtons,
                                                                      toUseBlendedSelection,
                                                                      toThemeAware
  ] + [
          toAlwaysHideSelection,
          toHideSelection,
          toHideFocusRect
  ];

  self.Font.Name   := FONT_1;
  self.Font.Color  := MAIN_ACCENT;
  self.Font.Height := -11;

  self.Header.Font.Name   := FONT_1;
  self.Header.Font.Height := -11;

  self.BorderStyle := bsNone;

  self.Header.Columns.Add();

  self.NodeDataSize := SizeOf(TTreeData);

  self.Font.name := FONT_1;

  FOnItemClick := nil;
end;

{ TFlatVirtualStringTree.Destroy }
destructor TFlatVirtualStringTree.Destroy();
begin

  ///
  inherited Destroy();
end;

{ TFlatVirtualStringTree.AddItem }
function TFlatVirtualStringTree.AddItem(const ACaption : String; const AIndex : Cardinal; AParent : PVirtualNode = nil; const AImageIndex : Integer = -1) : PVirtualNode;
var AData : PTreeData;
    ANode : PVirtualNode;
begin
  ANode := self.AddChild(AParent);
  AData := self.GetNodeData(ANode);

  AData^.ItemName   := ACaption;
  AData^.Index      := AIndex;
  AData^.ImageIndex := AImageIndex;

  ///
  result := ANode;
end;

function TFlatVirtualStringTree.AddItem(const ACaption : String; const AIndex : Cardinal; const AImageIndex : Integer = -1) : PVirtualNode;
begin
  result := AddItem(ACaption, AIndex, nil, AImageIndex);
end;

{ TFlatVirtualStringTree.DoGetImageIndex }
function TFlatVirtualStringTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex): TCustomImageList;
var pData : PTreeData;
begin
  result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, ImageIndex);
  ///

  pData := Node.GetData();
  if not Assigned(pData) then
    Exit();

  if Column <> 0 then
    Exit();

  case Kind of
    ikNormal, ikSelected: begin
      ImageIndex := pData^.ImageIndex;
    end;
  end;

end;

{ TFlatVirtualStringTree.DoPaintText }
procedure TFlatVirtualStringTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited DoPaintText(Node, Canvas, Column, TextType);
  ///

  if (Node.ChildCount > 0) then
    Canvas.Font.Color := MAIN_ACCENT
  else
    Canvas.Font.Color := MAIN_ACCENT;
end;


{ TFlatVirtualStringTree.DoBeforeCellPaint }
procedure TFlatVirtualStringTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var ASelected : Boolean;
begin
  inherited DoBeforeCellPaint(Canvas, Node, Column, CellPaintMode, CellRect, ContentRect);
  ///

  {
    Draw Selected Background
  }
  ASelected := (vsSelected in Node.States);
  if ASelected then begin
    Canvas.Brush.Color := MAIN_GRAY;

    Canvas.FillRect(CellRect);
  end;
end;

{ TFlatVirtualStringTree.DoTextDrawing }
procedure TFlatVirtualStringTree.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal);
begin
  inherited;
end;

{ TFlatVirtualStringTree.DoNodeClick }
procedure TFlatVirtualStringTree.DoNodeClick(const HitInfo: THitInfo);
var AData : PTreeData;
begin
  inherited;

  if HitInfo.HitNode.ChildCount > 0 then
    self.Expanded[HitInfo.HitNode] := not (vsExpanded in HitInfo.HitNode.States)
  else if Assigned(FOnItemClick) then begin
    AData := self.GetNodeData(HitInfo.HitNode);

    if Assigned(AData) then
      FOnItemClick(self, AData^.Index, AData^.ItemName);
  end;
end;

{ TFlatVirtualStringTree.DoGetText }
procedure TFlatVirtualStringTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var AData : PTreeData;
begin
  inherited;
  ///

  AData := self.GetNodeData(pEventArgs.Node);
  if Assigned(AData) then begin
    if pEventArgs.Column = 0 then
      pEventArgs.CellText := AData^.ItemName;
  end;
end;

end.
