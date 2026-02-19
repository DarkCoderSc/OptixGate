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

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.UITypes,

  WinAPI.Windows,

  VCL.Graphics, VCL.ImgList,

  VirtualTrees, VirtualTrees.Types;
// ---------------------------------------------------------------------------------------------------------------------

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
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect;
      DrawFormat: Cardinal); override;
    procedure DoNodeClick(const HitInfo: THitInfo); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: System.UITypes.TImageIndex): TCustomImageList; override;
  public
    {@M}
    function AddItem(const ACaption : String; const AIndex : Cardinal; AParent : PVirtualNode = nil;
      const AImageIndex : Integer = -1) : PVirtualNode; overload;
    function AddItem(const ACaption : String; const AIndex : Cardinal;
      const AImageIndex : Integer = -1) : PVirtualNode; overload;

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

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math,

  VCL.Forms,

  NeoFlat.Theme;
// ---------------------------------------------------------------------------------------------------------------------

constructor TFlatVirtualStringTree.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  Color := clWhite;
  Colors.TreeLineColor := MAIN_ACCENT;

  Header.Options := Header.Options + [hoAutoResize];

  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];

  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [
    toShowRoot,
    toShowButtons,
    toUseBlendedSelection,
    toThemeAware
  ] + [
    toAlwaysHideSelection,
    toHideSelection,
    toHideFocusRect
  ];

  Font.Name   := FONT_1;
  Font.Color  := MAIN_ACCENT;
  Font.Height := -11;

  Header.Font.Name   := FONT_1;
  Header.Font.Height := -11;

  BorderStyle := bsNone;

  Header.Columns.Add();

  NodeDataSize := SizeOf(TTreeData);

  Font.name := FONT_1;

  FOnItemClick := nil;
end;

destructor TFlatVirtualStringTree.Destroy();
begin

  ///
  inherited Destroy();
end;

function TFlatVirtualStringTree.AddItem(const ACaption : String; const AIndex : Cardinal; AParent : PVirtualNode = nil;
  const AImageIndex : Integer = -1) : PVirtualNode;
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

function TFlatVirtualStringTree.AddItem(const ACaption : String; const AIndex : Cardinal;
  const AImageIndex : Integer = -1) : PVirtualNode;
begin
  result := AddItem(ACaption, AIndex, nil, AImageIndex);
end;

function TFlatVirtualStringTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: System.UITypes.TImageIndex): TCustomImageList;
var pData : PTreeData;
begin
  result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, ImageIndex);
  ///

  pData := Node.GetData();
  if not Assigned(pData) then
    Exit;

  if Column <> 0 then
    Exit;

  case Kind of
    ikNormal, ikSelected: begin
      ImageIndex := pData^.ImageIndex;
    end;
  end;

end;

procedure TFlatVirtualStringTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  inherited DoPaintText(Node, Canvas, Column, TextType);
  ///

  if (Node.ChildCount > 0) then
    Canvas.Font.Color := MAIN_ACCENT
  else
    Canvas.Font.Color := MAIN_ACCENT;
end;

procedure TFlatVirtualStringTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var ASelected : Boolean;
begin
  inherited DoBeforeCellPaint(Canvas, Node, Column, CellPaintMode, CellRect, ContentRect);
  ///

  ASelected := (vsSelected in Node.States);
  if ASelected then begin
    Canvas.Brush.Color := MAIN_GRAY;

    Canvas.FillRect(CellRect);
  end;
end;

procedure TFlatVirtualStringTree.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect;
  DrawFormat: Cardinal);
begin
  inherited;
end;

procedure TFlatVirtualStringTree.DoNodeClick(const HitInfo: THitInfo);
var AData : PTreeData;
begin
  inherited;

  if HitInfo.HitNode.ChildCount > 0 then
    Expanded[HitInfo.HitNode] := not (vsExpanded in HitInfo.HitNode.States)
  else if Assigned(FOnItemClick) then begin
    AData := GetNodeData(HitInfo.HitNode);

    if Assigned(AData) then
      FOnItemClick(self, AData^.Index, AData^.ItemName);
  end;
end;

procedure TFlatVirtualStringTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var AData : PTreeData;
begin
  inherited;
  ///

  AData := GetNodeData(pEventArgs.Node);
  if Assigned(AData) then begin
    if pEventArgs.Column = 0 then
      pEventArgs.CellText := AData^.ItemName;
  end;
end;

end.
