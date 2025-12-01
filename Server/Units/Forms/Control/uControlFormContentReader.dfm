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
  OnDestroy = FormDestroy
  TextHeight = 15
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
    TabOrder = 0
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
    object ButtonToggleStrings: TSpeedButton
      AlignWithMargins = True
      Left = 172
      Top = 4
      Width = 30
      Height = 22
      Hint = 'Update Page Size'
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 74
      ImageName = 'control-rich-text-edit-eye'
      Images = FormMain.VirtualImageList
      OnClick = ButtonToggleStringsClick
      ExplicitTop = 1
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
  object MultiPanel: TOMultiPanel
    Left = 0
    Top = 30
    Width = 824
    Height = 570
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = PanelHex
        Position = 0.500000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = PanelStrings
        Position = 1.000000000000000000
        Visible = False
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 814
    ExplicitHeight = 538
    DesignSize = (
      824
      570)
    object PanelHex: TPanel
      Left = 0
      Top = 0
      Width = 824
      Height = 285
      Anchors = []
      BevelOuter = bvNone
      TabOrder = 0
    end
    object PanelStrings: TPanel
      Left = 0
      Top = 288
      Width = 824
      Height = 282
      Anchors = []
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      object RichStrings: TRichEdit
        Left = 0
        Top = 0
        Width = 824
        Height = 282
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
        ExplicitWidth = 814
        ExplicitHeight = 266
      end
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
