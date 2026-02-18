object FormListen: TFormListen
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Listen'
  ClientHeight = 326
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 285
    Width = 398
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 253
    ExplicitWidth = 388
    object ButtonConnect: TButton
      Left = 152
      Top = 6
      Width = 83
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = ButtonConnectClick
    end
    object ButtonCancel: TButton
      Left = 63
      Top = 6
      Width = 83
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PanelClient: TPanel
    Left = 50
    Top = 0
    Width = 348
    Height = 285
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitWidth = 338
    ExplicitHeight = 253
    object Label1: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 170
      Width = 332
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Server Port:'
      ExplicitWidth = 60
    end
    object LabelCertificate: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 242
      Width = 332
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Server Certificate:'
      ExplicitWidth = 92
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 332
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'IP Version:'
      ExplicitWidth = 54
    end
    object SpinPort: TSpinEdit
      Left = 8
      Top = 185
      Width = 332
      Height = 24
      Align = alTop
      MaxLength = 5
      MaxValue = 65535
      MinValue = 0
      TabOrder = 0
      Value = 2801
      OnChange = SpinPortChange
      ExplicitWidth = 322
    end
    object ComboCertificate: TComboBox
      Left = 8
      Top = 257
      Width = 332
      Height = 23
      Align = alTop
      Style = csDropDownList
      TabOrder = 1
      ExplicitWidth = 322
    end
    object CheckBoxAutoStart: TCheckBox
      AlignWithMargins = True
      Left = 8
      Top = 217
      Width = 332
      Height = 17
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Automatically Start The Server.'
      Checked = True
      State = cbChecked
      TabOrder = 2
      ExplicitWidth = 322
    end
    object ComboIpVersion: TComboBox
      Left = 8
      Top = 23
      Width = 332
      Height = 23
      Align = alTop
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'IPv4'
      Items.Strings = (
        'IPv4'
        'IPv6')
      ExplicitWidth = 322
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 46
      Width = 332
      Height = 116
      Align = alTop
      Caption = 'Bind Address:'
      Padding.Left = 8
      Padding.Top = 8
      Padding.Right = 8
      Padding.Bottom = 8
      TabOrder = 4
      ExplicitWidth = 322
      object RadioBindAll: TRadioButton
        Left = 10
        Top = 25
        Width = 312
        Height = 17
        Align = alTop
        Caption = 'All Interfaces (Default)'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioBindAllClick
        ExplicitWidth = 302
      end
      object RadioBindLocal: TRadioButton
        Left = 10
        Top = 42
        Width = 312
        Height = 17
        Align = alTop
        Caption = 'Local host only'
        TabOrder = 1
        OnClick = RadioBindLocalClick
        ExplicitWidth = 302
      end
      object RadioBindCustom: TRadioButton
        Left = 10
        Top = 59
        Width = 312
        Height = 17
        Align = alTop
        Caption = 'Custom:'
        TabOrder = 2
        OnClick = RadioBindCustomClick
        ExplicitWidth = 302
      end
      object EditServerBindAddress: TEdit
        AlignWithMargins = True
        Left = 28
        Top = 84
        Width = 294
        Height = 23
        Margins.Left = 18
        Margins.Top = 8
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        TabOrder = 3
        ExplicitWidth = 284
      end
    end
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 50
    Height = 285
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 2
    ExplicitHeight = 253
    object Image: TVirtualImage
      Left = 8
      Top = 8
      Width = 34
      Height = 269
      Align = alClient
      Center = True
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 40
      ImageName = 'servers-connection'
      ExplicitLeft = 10
      ExplicitTop = 10
    end
  end
end
