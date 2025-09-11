object FormListen: TFormListen
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Listen'
  ClientHeight = 185
  ClientWidth = 333
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
    Top = 144
    Width = 333
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 112
    ExplicitWidth = 323
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
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
  object PanelClient: TPanel
    Left = 50
    Top = 0
    Width = 283
    Height = 144
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitWidth = 273
    ExplicitHeight = 112
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 267
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Bind Address :'
      ExplicitWidth = 75
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 54
      Width = 267
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
      Top = 101
      Width = 267
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Server Certificate:'
      ExplicitWidth = 92
    end
    object SpinPort: TSpinEdit
      Left = 8
      Top = 69
      Width = 267
      Height = 24
      Align = alTop
      MaxLength = 5
      MaxValue = 65535
      MinValue = 0
      TabOrder = 0
      Value = 2801
      OnChange = SpinPortChange
      ExplicitWidth = 257
    end
    object EditServerBindAddress: TEdit
      Left = 8
      Top = 23
      Width = 267
      Height = 23
      Align = alTop
      TabOrder = 1
      Text = '0.0.0.0'
      ExplicitWidth = 257
    end
    object ComboCertificate: TComboBox
      Left = 8
      Top = 116
      Width = 267
      Height = 23
      Align = alTop
      Style = csDropDownList
      TabOrder = 2
      ExplicitWidth = 257
    end
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 50
    Height = 144
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 2
    ExplicitHeight = 112
    object Image: TVirtualImage
      Left = 8
      Top = 8
      Width = 34
      Height = 128
      Align = alClient
      Center = True
      ImageCollection = FormMain.ImageCollectionDark
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 40
      ImageName = 'servers-connection'
      ExplicitWidth = 32
      ExplicitHeight = 32
    end
  end
end
