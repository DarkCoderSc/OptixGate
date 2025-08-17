object FormConnectToServer: TFormConnectToServer
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Client'
  ClientHeight = 199
  ClientWidth = 321
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
    Height = 158
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    ExplicitHeight = 126
    object Image: TVirtualImage
      Left = 8
      Top = 8
      Width = 34
      Height = 142
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
    Width = 271
    Height = 158
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitWidth = 261
    ExplicitHeight = 126
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 83
      Height = 15
      Align = alTop
      Caption = 'Server Address :'
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 54
      Width = 60
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Server Port:'
    end
    object LabelCertificate: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 101
      Width = 91
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Client Certificate:'
    end
    object EditServerAddress: TEdit
      Left = 8
      Top = 23
      Width = 255
      Height = 23
      Align = alTop
      TabOrder = 0
      Text = '127.0.0.1'
      ExplicitWidth = 245
    end
    object SpinPort: TSpinEdit
      Left = 8
      Top = 69
      Width = 255
      Height = 24
      Align = alTop
      MaxLength = 5
      MaxValue = 65535
      MinValue = 0
      TabOrder = 1
      Value = 2801
      OnChange = SpinPortChange
      ExplicitWidth = 245
    end
    object ComboCertificate: TComboBox
      Left = 8
      Top = 116
      Width = 255
      Height = 23
      Align = alTop
      Style = csDropDownList
      TabOrder = 2
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 158
    Width = 321
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 126
    ExplicitWidth = 311
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
