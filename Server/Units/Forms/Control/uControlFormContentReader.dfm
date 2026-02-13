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
  object PanelMain: TFlatPanel
    Left = 0
    Top = 0
    Width = 824
    Height = 600
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = 13554645
    BorderColor = clBlack
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 814
    ExplicitHeight = 568
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
      TabOrder = 0
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
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
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
    object PanelActions: TFlatPanel
      Left = 0
      Top = 0
      Width = 824
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 2
      Color = 13554645
      BorderColor = clBlack
      Align = alTop
      Caption = 'PanelActions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 6
      ExplicitWidth = 814
      object ButtonBack: TFlatButton
        Left = 4
        Top = 4
        Width = 32
        Height = 20
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Align = alLeft
        ImageIndex = 17
        Enabled = True
        Caption = '<-'
        Value = 0
        OnClick = ButtonBackClick
        Busy = False
        ExplicitHeight = 27
      end
      object ButtonForward: TFlatButton
        AlignWithMargins = True
        Left = 40
        Top = 4
        Width = 32
        Height = 20
        Margins.Left = 4
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Align = alLeft
        ImageIndex = 17
        Enabled = True
        Caption = '->'
        Value = 0
        OnClick = ButtonForwardClick
        Busy = False
        ExplicitLeft = 68
        ExplicitHeight = 27
      end
      object Shape1: TShape
        AlignWithMargins = True
        Left = 80
        Top = 4
        Width = 1
        Height = 20
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 4
        Margins.Bottom = 0
        Align = alLeft
        ExplicitLeft = 72
        ExplicitHeight = 27
      end
      object ButtonDownload: TFlatButton
        AlignWithMargins = True
        Left = 89
        Top = 4
        Width = 80
        Height = 20
        Margins.Left = 4
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Align = alLeft
        ImageIndex = 17
        Enabled = True
        Caption = 'Download'
        Value = 0
        OnClick = ButtonDownloadClick
        Busy = False
        ExplicitHeight = 22
      end
      object ButtonBrowsePage: TFlatButton
        AlignWithMargins = True
        Left = 186
        Top = 4
        Width = 80
        Height = 20
        Margins.Left = 4
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Align = alLeft
        ImageIndex = 17
        Enabled = True
        Caption = 'Go To Page...'
        Value = 0
        OnClick = ButtonBrowsePageClick
        Busy = False
        ExplicitHeight = 22
      end
      object ButtonUpdatePageSize: TFlatButton
        AlignWithMargins = True
        Left = 270
        Top = 4
        Width = 80
        Height = 20
        Margins.Left = 4
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Align = alLeft
        ImageIndex = 17
        Enabled = True
        Caption = 'Set Page Size'
        Value = 0
        OnClick = ButtonUpdatePageSizeClick
        Busy = False
        ExplicitHeight = 22
      end
      object Shape2: TShape
        AlignWithMargins = True
        Left = 177
        Top = 4
        Width = 1
        Height = 20
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 4
        Margins.Bottom = 0
        Align = alLeft
        ExplicitLeft = 178
        ExplicitHeight = 22
      end
      object Shape3: TShape
        AlignWithMargins = True
        Left = 358
        Top = 4
        Width = 1
        Height = 20
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 4
        Margins.Bottom = 0
        Align = alLeft
        ExplicitHeight = 22
      end
      object ButtonOptions: TFlatButton
        AlignWithMargins = True
        Left = 367
        Top = 4
        Width = 80
        Height = 20
        Margins.Left = 4
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Align = alLeft
        ImageIndex = 17
        Enabled = True
        Caption = 'Options'
        Value = 0
        OnClick = ButtonOptionsClick
        Busy = False
        ExplicitLeft = 494
        ExplicitHeight = 22
      end
    end
  end
  object PopupRichStrings: TFlatPopupMenu
    OwnerDraw = True
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
  object PopupMenuOptions: TFlatPopupMenu
    OwnerDraw = True
    Left = 272
    Top = 214
    object ShowStrings1: TMenuItem
      AutoCheck = True
      Caption = 'Show Strings'
      OnClick = ShowStrings1Click
    end
  end
end
