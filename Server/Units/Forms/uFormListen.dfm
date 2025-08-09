object FormListen: TFormListen
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Listen'
  ClientHeight = 140
  ClientWidth = 298
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
    Top = 99
    Width = 298
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -23
    ExplicitWidth = 321
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
    Width = 248
    Height = 99
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitLeft = 48
    ExplicitHeight = 138
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 232
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Bind Address :'
      ExplicitTop = 16
      ExplicitWidth = 75
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 54
      Width = 232
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Server Port:'
      ExplicitLeft = 6
      ExplicitTop = 48
    end
    object SpinPort: TSpinEdit
      Left = 8
      Top = 69
      Width = 232
      Height = 24
      Align = alTop
      MaxValue = 65535
      MinValue = 0
      TabOrder = 0
      Value = 2801
      ExplicitWidth = 255
    end
    object EditServerBindAddress: TEdit
      Left = 8
      Top = 23
      Width = 232
      Height = 23
      Align = alTop
      TabOrder = 1
      Text = '0.0.0.0'
      ExplicitLeft = 6
      ExplicitTop = 28
    end
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 50
    Height = 99
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 2
    ExplicitHeight = 75
    object Image: TVirtualImage
      Left = 8
      Top = 8
      Width = 34
      Height = 83
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
