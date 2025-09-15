object FormConnectToServer: TFormConnectToServer
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Client'
  ClientHeight = 196
  ClientWidth = 311
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
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 50
    Height = 155
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    ExplicitHeight = 123
    object Image: TVirtualImage
      Left = 8
      Top = 8
      Width = 34
      Height = 139
      Margins.Top = 8
      Align = alClient
      Center = True
      ImageCollection = FormMain.ImageCollectionDark
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 0
      ImageName = 'computers-network-green'
      ExplicitWidth = 32
      ExplicitHeight = 32
    end
  end
  object PanelClient: TPanel
    Left = 50
    Top = 0
    Width = 261
    Height = 155
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitWidth = 251
    ExplicitHeight = 123
    object Label1: TLabel
      Left = 8
      Top = 46
      Width = 245
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Server Address :'
      ExplicitWidth = 83
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 92
      Width = 245
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
      Top = 139
      Width = 245
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Client Certificate:'
      ExplicitWidth = 91
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 245
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'IP Version:'
      ExplicitWidth = 54
    end
    object EditServerAddress: TEdit
      Left = 8
      Top = 61
      Width = 245
      Height = 23
      Align = alTop
      TabOrder = 0
      Text = '127.0.0.1'
      ExplicitWidth = 235
    end
    object SpinPort: TSpinEdit
      Left = 8
      Top = 107
      Width = 245
      Height = 24
      Align = alTop
      MaxLength = 5
      MaxValue = 65535
      MinValue = 0
      TabOrder = 1
      Value = 2801
      OnChange = SpinPortChange
      ExplicitWidth = 235
    end
    object ComboCertificate: TComboBox
      Left = 8
      Top = 154
      Width = 245
      Height = 23
      Align = alTop
      Style = csDropDownList
      TabOrder = 2
      ExplicitWidth = 235
    end
    object ComboIpVersion: TComboBox
      Left = 8
      Top = 23
      Width = 245
      Height = 23
      Align = alTop
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'IPv4'
      OnChange = ComboIpVersionChange
      Items.Strings = (
        'IPv4'
        'IPv6')
      ExplicitWidth = 235
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 155
    Width = 311
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 123
    ExplicitWidth = 301
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
end
