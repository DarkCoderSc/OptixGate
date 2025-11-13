object ControlFormContentReader: TControlFormContentReader
  Left = 0
  Top = 0
  Caption = 'Content Reader'
  ClientHeight = 619
  ClientWidth = 824
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object Pages: TPageControl
    AlignWithMargins = True
    Left = 4
    Top = 34
    Width = 816
    Height = 562
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = TabHexTable
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 806
    ExplicitHeight = 530
    object TabHexTable: TTabSheet
      Caption = 'Hex Table (Beta)'
      ImageIndex = 2
      object VST: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 800
        Height = 519
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        AccessibleName = '0F'
        Align = alClient
        BackGroundImageTransparent = True
        BorderStyle = bsNone
        Color = clWhite
        Colors.UnfocusedColor = clWindowText
        DefaultNodeHeight = 19
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Header.AutoSizeIndex = -1
        Header.DefaultHeight = 25
        Header.MainColumn = 1
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Images = FormMain.VirtualImageList
        Indent = 0
        ParentFont = False
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toHideSelection, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toAlwaysHideSelection]
        TreeOptions.SelectionOptions = [toDisableDrawSelection]
        OnBeforeCellPaint = VSTBeforeCellPaint
        OnGetText = VSTGetText
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Alignment = taCenter
            Position = 0
            Text = 'Offset (h)'
            Width = 150
          end
          item
            Alignment = taCenter
            Position = 1
            Text = '00'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 2
            Text = '01'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 3
            Text = '02'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 4
            Text = '03'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 5
            Text = '04'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 6
            Text = '05'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 7
            Text = '06'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 8
            Text = '07'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 9
            Text = '08'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 10
            Text = '09'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 11
            Text = '0A'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 12
            Text = '0B'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 13
            Text = '0C'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 14
            Text = '0D'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 15
            Text = '0E'
            Width = 30
          end
          item
            Alignment = taCenter
            Position = 16
            Text = '0F'
            Width = 30
          end
          item
            Position = 17
            Width = 150
          end>
      end
    end
    object TabHexView: TTabSheet
      Caption = 'Plain Hex'
      object RichHex: TRichEdit
        Left = 0
        Top = 0
        Width = 800
        Height = 519
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PlainText = True
        PopupMenu = PopupRichHex
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabStrings: TTabSheet
      Caption = 'Strings'
      ImageIndex = 1
      object RichStrings: TRichEdit
        Left = 0
        Top = 0
        Width = 800
        Height = 519
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PlainText = True
        PopupMenu = PopupRichStrings
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object PanelActions: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 816
    Height = 30
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 806
    object ButtonBack: TSpeedButton
      AlignWithMargins = True
      Left = 36
      Top = 4
      Width = 30
      Height = 22
      Hint = 'Browse Previous Page'
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 60
      ImageName = 'button-arrow-left'
      Images = FormMain.VirtualImageList
      SelectedImageName = '-1'
      OnClick = ButtonBackClick
      ExplicitLeft = 34
    end
    object ButtonForward: TSpeedButton
      AlignWithMargins = True
      Left = 70
      Top = 4
      Width = 30
      Height = 22
      Hint = 'Browse Next Page'
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 61
      ImageName = 'button-arrow-right'
      Images = FormMain.VirtualImageList
      OnClick = ButtonForwardClick
      ExplicitLeft = 68
    end
    object ButtonDownload: TSpeedButton
      AlignWithMargins = True
      Left = 2
      Top = 4
      Width = 30
      Height = 22
      Hint = 'Download File'
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 71
      ImageName = 'button-download'
      Images = FormMain.VirtualImageList
      SelectedImageName = '-1'
      OnClick = ButtonDownloadClick
      ExplicitLeft = 34
    end
    object ButtonBrowsePage: TSpeedButton
      AlignWithMargins = True
      Left = 104
      Top = 4
      Width = 30
      Height = 22
      Hint = 'Go To Page'
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 72
      ImageName = 'execute'
      Images = FormMain.VirtualImageList
      OnClick = ButtonBrowsePageClick
      ExplicitLeft = 198
    end
    object ButtonUpdatePageSize: TSpeedButton
      AlignWithMargins = True
      Left = 138
      Top = 4
      Width = 30
      Height = 22
      Hint = 'Update Page Size'
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 73
      ImageName = 'document-update'
      Images = FormMain.VirtualImageList
      OnClick = ButtonUpdatePageSizeClick
      ExplicitLeft = 184
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 600
    Width = 824
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Alignment = taRightJustify
        Width = 50
      end>
    ExplicitTop = 568
    ExplicitWidth = 814
  end
  object PopupRichHex: TPopupMenu
    Left = 332
    Top = 333
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      OnClick = SelectAll1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      OnClick = Copy1Click
    end
  end
  object PopupRichStrings: TPopupMenu
    OnPopup = PopupRichStringsPopup
    Left = 500
    Top = 333
    object MenuItem1: TMenuItem
      Caption = 'Select All'
      OnClick = MenuItem1Click
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object MenuItem3: TMenuItem
      Caption = 'Copy'
      OnClick = MenuItem3Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object StringKind1: TMenuItem
      Caption = 'String Kind'
      object Ansi1: TMenuItem
        AutoCheck = True
        Caption = 'Show ANSI'
        Checked = True
        OnClick = Ansi1Click
      end
      object Unicode1: TMenuItem
        AutoCheck = True
        Caption = 'Show Unicode'
        Checked = True
        OnClick = Unicode1Click
      end
    end
    object MinimumLength1: TMenuItem
      Caption = 'Minimum Length'
      object NoMinimum1: TMenuItem
        Caption = 'No Minimum'
        Checked = True
        RadioItem = True
        OnClick = NoMinimum1Click
      end
      object Custom1: TMenuItem
        Caption = 'Custom'
        RadioItem = True
        OnClick = Custom1Click
      end
    end
  end
end
