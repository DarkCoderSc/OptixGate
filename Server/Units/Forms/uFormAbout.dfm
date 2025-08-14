object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 385
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object ImageLogo: TVirtualImage
    Left = 128
    Top = 8
    Width = 80
    Height = 80
    ImageWidth = 0
    ImageHeight = 0
    ImageIndex = -1
  end
  object LabelName: TLabel
    Left = 152
    Top = 119
    Width = 148
    Height = 21
    Caption = 'Optix Gate (GPL v3)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelDarkCoderSc: TLabel
    Left = 96
    Top = 144
    Width = 202
    Height = 15
    Caption = 'DarkCoderSc (Jean-Pierre LESUEUR)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ButtonClose: TButton
    Left = 168
    Top = 336
    Width = 89
    Height = 25
    Caption = 'OK.'
    TabOrder = 0
    OnClick = ButtonCloseClick
  end
  object Disclaimer: TRichEdit
    Left = 8
    Top = 165
    Width = 401
    Height = 148
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
