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


unit uControlFormContentReader;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils, System.Variants, System.Classes,

  Winapi.Windows, Winapi.Messages,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, Vcl.Menus,
  Vcl.ComCtrls, Vcl.StdCtrls,

  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees, VirtualTrees.Types,

  __uBaseFormControl__,

  Optix.Protocol.Packet, Optix.Func.Commands.ContentReader;
// ---------------------------------------------------------------------------------------------------------------------

type
  TControlFormContentReader = class(TBaseFormControl)
    Pages: TPageControl;
    TabHexView: TTabSheet;
    TabStrings: TTabSheet;
    RichHex: TRichEdit;
    PanelActions: TPanel;
    ButtonBack: TSpeedButton;
    ButtonForward: TSpeedButton;
    TabHexTable: TTabSheet;
    ButtonDownload: TSpeedButton;
    StatusBar: TStatusBar;
    ButtonBrowsePage: TSpeedButton;
    VST: TVirtualStringTree;
    PopupRichHex: TPopupMenu;
    Copy1: TMenuItem;
    SelectAll1: TMenuItem;
    N3: TMenuItem;
    RichStrings: TRichEdit;
    PopupRichStrings: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    N1: TMenuItem;
    StringKind1: TMenuItem;
    Ansi1: TMenuItem;
    Unicode1: TMenuItem;
    MinimumLength1: TMenuItem;
    NoMinimum1: TMenuItem;
    Custom1: TMenuItem;
    ButtonUpdatePageSize: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonForwardClick(Sender: TObject);
    procedure ButtonBrowsePageClick(Sender: TObject);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure SelectAll1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Ansi1Click(Sender: TObject);
    procedure Unicode1Click(Sender: TObject);
    procedure NoMinimum1Click(Sender: TObject);
    procedure PopupRichStringsPopup(Sender: TObject);
    procedure Custom1Click(Sender: TObject);
    procedure ButtonUpdatePageSizeClick(Sender: TObject);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    FCurrentPage              : TOptixCommandContentReaderPage;
    FMinExtractedStringLength : Cardinal;

    {@M}
    procedure UpdateFormElements();
    procedure BrowsePage(APageNumber : UInt64);
    procedure InitializeHexGrid();
  protected
    {@M}
    procedure RefreshCaption(); override;
    procedure RefreshExtractedStrings();
  public
    {@M}
    procedure ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean); override;
  end;

var
  ControlFormContentReader: TControlFormContentReader;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Math,

  Optix.Helper, Optix.FileSystem.Helper;
// ---------------------------------------------------------------------------------------------------------------------

{$R *.dfm}

procedure TControlFormContentReader.RefreshExtractedStrings();
begin
  if not Assigned(FCurrentPage) or not Assigned(FCurrentPage.Data) then
    Exit();
  ///

  var AStringKinds : TContentFormater.TStringKinds;

  if Ansi1.Checked then
    Include(AStringKinds, skAnsi);

  if Unicode1.Checked then
    Include(AStringKinds, skUnicode);

  RichStrings.Text := TContentFormater.ExtractStrings(
    FCurrentPage.Data,
    FCurrentPage.DataSize,
    FMinExtractedStringLength,
    AStringKinds
  );
end;

procedure TControlFormContentReader.InitializeHexGrid();
begin
  var ARowCount := Cardinal(ceil(FCurrentPage.DataSize / 16));
  ///

  if ARowCount = VST.RootNodeCount then
    Exit();
  ///

  VST.RootNodeCount := ARowCount;
end;

procedure TControlFormContentReader.MenuItem1Click(Sender: TObject);
begin
  RichStrings.SelectAll;
end;

procedure TControlFormContentReader.MenuItem3Click(Sender: TObject);
begin
  RichStrings.CopyToClipboard;
end;

procedure TControlFormContentReader.NoMinimum1Click(Sender: TObject);
begin
  FMinExtractedStringLength := 0;

  RefreshExtractedStrings();

  ///
  TMenuItem(Sender).Checked := True;
end;

procedure TControlFormContentReader.PopupRichStringsPopup(Sender: TObject);
begin
  Custom1.Checked := FMinExtractedStringLength > 0;
end;

procedure TControlFormContentReader.Ansi1Click(Sender: TObject);
begin
  RefreshExtractedStrings();
end;

procedure TControlFormContentReader.BrowsePage(APageNumber : UInt64);
begin
  if not Assigned(FCurrentPage) then
    Exit();
  ///

  if APageNumber > FCurrentPage.PageCount  then
    APageNumber := FCurrentPage.PageCount;
  ///

  SendCommand(TOptixCommandBrowseContentReader.Create(APageNumber));
end;

procedure TControlFormContentReader.RefreshCaption();
begin
  inherited;
  ///

  if Assigned(FCurrentPage) then
    if not String.IsNullOrWhiteSpace(FCurrentPage.FilePath) then
      Caption := Format('%s - File: "%s"', [
        Caption,
        FCurrentPage.FilePath
      ]);
end;

procedure TControlFormContentReader.SelectAll1Click(Sender: TObject);
begin
  RichHex.SelectAll();
end;

procedure TControlFormContentReader.ButtonBackClick(Sender: TObject);
begin
  if Assigned(FCurrentPage) then
    BrowsePage(FCurrentPage.PageNumber -1);
end;

procedure TControlFormContentReader.ButtonBrowsePageClick(Sender: TObject);
begin
  if not Assigned(FCurrentPage) then
    Exit();
  ///

  var AValue : String;

  if not InputQuery('Browse Page', Format('Enter page number(1 to %d)', [
    FCurrentPage.PageCount
  ]), AValue) then
    Exit();
  ///

  var APageNumber : UInt64;
  if not TryStrToUInt64(AValue, APageNumber) then
    Exit();
  ///

  if APageNumber = 0 then
    Inc(APageNumber)
  else if APageNumber > FCurrentPage.PageCount then
    APageNumber := FCurrentPage.PageCount;

  BrowsePage(APageNumber -1);
end;

procedure TControlFormContentReader.ButtonDownloadClick(Sender: TObject);
begin
  if Assigned(FCurrentPage) then
    RequestFileDownload(FCurrentPage.FilePath, '', 'Content Reader');
end;

procedure TControlFormContentReader.ButtonForwardClick(Sender: TObject);
begin
  if Assigned(FCurrentPage) then
    BrowsePage(FCurrentPage.PageNumber +1);
end;

procedure TControlFormContentReader.ButtonUpdatePageSizeClick(Sender: TObject);
begin
  var AValue := '';
  if not InputQuery(
    'Update Page Size',
    Format('New page size (min: %d):', [TContentReader.MIN_PAGE_SIZE]),
    AValue) then
      Exit();
  ///

  var APageSize : UInt64;
  if not TryStrToUInt64(AValue, APageSize) then
    Exit();

  if APageSize < TContentReader.MIN_PAGE_SIZE then
    APageSize := TContentReader.MIN_PAGE_SIZE
  else if APageSize > TContentReader.MAX_PAGE_SIZE then
    APageSize := TContentReader.MAX_PAGE_SIZE;

  ///
  SendCommand(TOptixCommandBrowseContentReader.Create(0, APageSize));
end;

procedure TControlFormContentReader.Copy1Click(Sender: TObject);
begin
  RichHex.CopyToClipboard;
end;

procedure TControlFormContentReader.Custom1Click(Sender: TObject);
begin
  var AValue := '';
  if not InputQuery('Minimum String Length', 'Minimum:', AValue) then
    Exit();
  ///

  if not TryStrToUInt(AValue, FMinExtractedStringLength) then
    Exit();

  RefreshExtractedStrings();

  ///
  TMenuItem(Sender).Checked := True;
end;

procedure TControlFormContentReader.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  ///

  SendCommand(TOptixCommandCloseContentReader.Create());

  if Assigned(FCurrentPage) then
    FreeAndNil(FCurrentPage);
end;

procedure TControlFormContentReader.FormCreate(Sender: TObject);
begin
  FCurrentPage := nil;

  FMinExtractedStringLength := 0;

  ///
  UpdateFormElements();
end;

procedure TControlFormContentReader.Unicode1Click(Sender: TObject);
begin
  RefreshExtractedStrings();
end;

procedure TControlFormContentReader.UpdateFormElements();
begin
  StatusBar.Panels.Items[0].Text := '';
  StatusBar.Panels.Items[1].Text := '';
  ///

  for var I := 0 to PanelActions.ControlCount -1 do
    if PanelActions.Controls[I] is TSpeedButton then
      TSpeedButton(PanelActions.Controls[I]).Enabled := False;

  if not Assigned(FCurrentPage) then
    Exit();
  ///

  ButtonDownload.Enabled       := not String.IsNullOrWhiteSpace(FCurrentPage.FilePath);
  ButtonBack.Enabled           := FCurrentPage.PageNumber > 0;
  ButtonForward.Enabled        := FCurrentPage.PageNumber < FCurrentPage.PageCount -1;
  ButtonBrowsePage.Enabled     := FCurrentPage.PageCount > 1;
  ButtonUpdatePageSize.Enabled := True;
  ///

  StatusBar.Panels.Items[0].Text := Format('Page: %d / %d', [
    FCurrentPage.PageNumber +1,
    FCurrentPage.PageCount
  ]);

  StatusBar.Panels.Items[1].Text := Format('Data Size: %s (Page Size: %s), Total Size: %s', [
    FormatFileSize(FCurrentPage.DataSize),
    FormatFileSize(FCurrentPage.PageSize),
    FormatFileSize(FCurrentPage.TotalSize)
  ]);

  ///
  RefreshCaption();
end;

procedure TControlFormContentReader.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  var AColor := clNone;

  case Column of
    0 : AColor := clBlack;

    1..16 : begin
      if odd(Column mod 2) then
        AColor := clBlack;
    end;

    17 : AColor := clBlack;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TControlFormContentReader.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
const ROW_SIZE = 16; // TODO: Customize
begin
  CellText := '';
  ///

  if not Assigned(FCurrentPage) or not Assigned(FCurrentPage.Data) then
    Exit();
  ///

  var pRowOffset := PByte(NativeUInt(FCurrentPage.Data) + (Node.Index * ROW_SIZE));

  case Column of
    // Offset

    // TODO: When Delphi CE 13
//    0 : CellText := IntToHex(
//      FCurrentPage.PageOffset + (Node.Index * ROW_SIZE),
//      (if FCurrentPage.TotalSize > High(Integer) then 16 else 8)
//    );
    // ---

    0 : begin
      var AWidth := IfThen(FCurrentPage.TotalSize > High(Integer), 16, 8);
      CellText := IntToHex(FCurrentPage.PageOffset + (Node.Index * ROW_SIZE), AWidth);
    end;

    // Hex
    1..ROW_SIZE : begin
      var pColumnOffset := PByte(NativeUInt(pRowOffset) + (* CAST Removes Warning *) Cardinal(Column -1));
      if NativeUInt(pColumnOffset) < NativeUInt(FCurrentPage.Data) + FCurrentPage.DataSize then
        CellText := IntToHex(pColumnOffset^);
    end;

    // Ascii
    ROW_SIZE +1 : begin
      var ARemainingBytes := FCurrentPage.DataSize - (Node.Index * ROW_SIZE);
      if ARemainingBytes > ROW_SIZE then
        ARemainingBytes := ROW_SIZE;
      ///

      CellText := TContentFormater.OutputPrintableChar(pRowOffset, ARemainingBytes);
    end;
  end;
end;

procedure TControlFormContentReader.ReceivePacket(const AOptixPacket : TOptixPacket; var AHandleMemory : Boolean);
begin
  // -------------------------------------------------------------------------------------------------------------------
  if AOptixPacket is TOptixCommandContentReaderPage then begin
    var ACommand := TOptixCommandContentReaderPage(AOptixPacket);
    if not Assigned(ACommand.Data) then
      Exit();
    ///

    RichHex.Text := TContentFormater.ToHexTable(
      TOptixCommandContentReaderPage(AOptixPacket).Data,
      TOptixCommandContentReaderPage(AOptixPacket).DataSize,
      TOptixCommandContentReaderPage(AOptixPacket).PageOffset
    );

    AHandleMemory := True;

    if Assigned(FCurrentPage) then
      FCurrentPage.Free;
    ///

    FCurrentPage := TOptixCommandContentReaderPage(AOptixPacket);

    InitializeHexGrid();

    VST.Refresh;

    RefreshExtractedStrings();

    ///
    UpdateFormElements();
  end;
  // -------------------------------------------------------------------------------------------------------------------
end;

end.
