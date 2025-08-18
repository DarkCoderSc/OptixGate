object FormGenerateNewCertificate: TFormGenerateNewCertificate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Generate New Certificate'
  ClientHeight = 194
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 8
    Top = 145
    Width = 384
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 113
    ExplicitWidth = 374
    object ButtonGenerate: TButton
      Left = 152
      Top = 6
      Width = 83
      Height = 25
      Caption = 'Generate'
      TabOrder = 0
      OnClick = ButtonGenerateClick
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
    Left = 58
    Top = 8
    Width = 334
    Height = 137
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitWidth = 324
    ExplicitHeight = 105
    object Label2: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 100
      Width = 318
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Common Name:'
      ExplicitWidth = 89
    end
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 318
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Country Name:'
      ExplicitWidth = 81
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 54
      Width = 318
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Organization Name:'
      ExplicitWidth = 106
    end
    object EditCN: TEdit
      Left = 8
      Top = 115
      Width = 318
      Height = 23
      Align = alTop
      TabOrder = 0
      Text = 'localhost'
      ExplicitWidth = 308
    end
    object EditC: TEdit
      Left = 8
      Top = 23
      Width = 318
      Height = 23
      Align = alTop
      TabOrder = 1
      Text = 'FR'
      ExplicitWidth = 308
    end
    object EditO: TEdit
      Left = 8
      Top = 69
      Width = 318
      Height = 23
      Align = alTop
      TabOrder = 2
      Text = 'DarkCoderSc'
      ExplicitWidth = 308
    end
  end
  object PanelLeft: TPanel
    Left = 8
    Top = 8
    Width = 50
    Height = 137
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 2
    ExplicitHeight = 105
    object Image: TVirtualImage
      Left = 8
      Top = 8
      Width = 34
      Height = 121
      Align = alClient
      Center = True
      ImageCollection = FormMain.ImageCollectionDark
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 58
      ImageName = 'application-certificate-filled'
      ExplicitWidth = 32
      ExplicitHeight = 32
    end
  end
end
