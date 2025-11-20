object ControlFormRegistryEditor: TControlFormRegistryEditor
  Left = 0
  Top = 0
  Caption = 'Registry Editor'
  ClientHeight = 282
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  TextHeight = 15
  object Notebook: TNotebook
    Left = 0
    Top = 54
    Width = 381
    Height = 174
    Align = alClient
    PageIndex = 2
    TabOrder = 0
    ExplicitWidth = 371
    ExplicitHeight = 142
    object TPage
      Left = 0
      Top = 0
      Caption = 'SZ'
      object PanelSZ: TPanel
        Left = 0
        Top = 0
        Width = 381
        Height = 174
        Align = alClient
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 8
        Padding.Right = 8
        Padding.Bottom = 8
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 365
          Height = 15
          Align = alTop
          Caption = 'Value:'
          ExplicitWidth = 31
        end
        object EditSZ: TEdit
          AlignWithMargins = True
          Left = 8
          Top = 27
          Width = 365
          Height = 23
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          TabOrder = 0
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'MSZ'
      object PanelMSZ: TPanel
        Left = 0
        Top = 0
        Width = 381
        Height = 174
        Align = alClient
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 8
        Padding.Right = 8
        Padding.Bottom = 8
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 8
          Width = 365
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 4
          Align = alTop
          Caption = 'Value:'
          ExplicitWidth = 31
        end
        object RichMSZ: TRichEdit
          Left = 8
          Top = 27
          Width = 365
          Height = 139
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'DQWord'
      ExplicitWidth = 371
      ExplicitHeight = 142
      object PanelQDword: TPanel
        Left = 0
        Top = 0
        Width = 381
        Height = 174
        Align = alClient
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 8
        Padding.Right = 8
        Padding.Bottom = 8
        TabOrder = 0
        ExplicitWidth = 371
        ExplicitHeight = 142
        object Label3: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 8
          Width = 365
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 4
          Align = alTop
          Caption = 'Value:'
          ExplicitWidth = 31
        end
        object EditQDword: TEdit
          Left = 8
          Top = 27
          Width = 365
          Height = 23
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          TabOrder = 0
          Text = '0'
          OnKeyPress = EditQDwordKeyPress
          ExplicitWidth = 355
        end
        object GroupBase: TGroupBox
          AlignWithMargins = True
          Left = 8
          Top = 58
          Width = 365
          Height = 79
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Base'
          Padding.Left = 16
          Padding.Top = 8
          Padding.Right = 16
          Padding.Bottom = 16
          TabOrder = 1
          ExplicitWidth = 355
          object RadioBaseDecimal: TRadioButton
            Left = 18
            Top = 25
            Width = 329
            Height = 17
            Align = alTop
            Caption = 'Decimal'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = RadioBaseDecimalClick
            ExplicitWidth = 319
          end
          object RadioBaseHexadecimal: TRadioButton
            AlignWithMargins = True
            Left = 18
            Top = 50
            Width = 329
            Height = 17
            Margins.Left = 0
            Margins.Top = 8
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Hexadecimal'
            TabOrder = 1
            OnClick = RadioBaseHexadecimalClick
            ExplicitWidth = 319
          end
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Binary'
      object PanelBinary: TPanel
        Left = 0
        Top = 0
        Width = 381
        Height = 174
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
  end
  object PanelFooter: TPanel
    Left = 0
    Top = 228
    Width = 381
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 196
    ExplicitWidth = 371
    object ButtonAction: TButton
      AlignWithMargins = True
      Left = 298
      Top = 4
      Width = 75
      Height = 27
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Action'
      TabOrder = 0
      OnClick = ButtonActionClick
      ExplicitLeft = 288
    end
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 219
      Top = 4
      Width = 75
      Height = 27
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
      ExplicitLeft = 209
    end
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 381
    Height = 54
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    TabOrder = 2
    ExplicitWidth = 371
    object LabelName: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 365
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Name:'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 35
    end
    object EditName: TEdit
      Left = 8
      Top = 27
      Width = 365
      Height = 23
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 355
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 263
    Width = 381
    Height = 19
    Panels = <
      item
        Width = 500
      end>
    ExplicitTop = 231
    ExplicitWidth = 371
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 152
    object Switch1: TMenuItem
      Caption = 'Switch'
      object String1: TMenuItem
        Caption = 'String (SZ)'
        OnClick = String1Click
      end
      object MultiLineString1: TMenuItem
        Caption = 'Multi Line String (MSZ)'
        OnClick = MultiLineString1Click
      end
      object DWORD1: TMenuItem
        Caption = 'DWORD'
        OnClick = DWORD1Click
      end
      object QWORD1: TMenuItem
        Caption = 'QWORD'
        OnClick = QWORD1Click
      end
      object Binary1: TMenuItem
        Caption = 'Binary'
        OnClick = Binary1Click
      end
    end
  end
end
