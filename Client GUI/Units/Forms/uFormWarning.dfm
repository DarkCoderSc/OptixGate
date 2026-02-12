object FormWarning: TFormWarning
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #9888#65039' Optix Gate Warning '#9888#65039
  ClientHeight = 584
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PanelFooter: TPanel
    Left = 0
    Top = 543
    Width = 728
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    ExplicitTop = 511
    ExplicitWidth = 718
    object ButtonAcceptTheRisk: TButton
      Left = 192
      Top = 6
      Width = 110
      Height = 25
      Caption = 'Accept the Risk'
      Enabled = False
      ImageIndex = 14
      ImageName = 'warning'
      Images = FormMain.VirtualImageList
      TabOrder = 0
      OnClick = ButtonAcceptTheRiskClick
    end
    object ButtonCancel: TButton
      Left = 63
      Top = 6
      Width = 110
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 728
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 718
    object ImageWarning: TVirtualImage
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 64
      Height = 57
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      Center = True
      ImageCollection = FormMain.ImageCollectionDark
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 14
      ImageName = 'warning'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 41
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 80
      Top = 8
      Width = 639
      Height = 45
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      Caption = 
        'You are about to give a remote system direct access to your comp' +
        'uter, your files, and potentially sensitive data. This level of ' +
        'access can lead to system damage, data loss, or unauthorized act' +
        'ivity. Continue only if you are absolutely certain the connectio' +
        'n is safe.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 53456
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Transparent = True
      WordWrap = True
      StyleElements = [seClient, seBorder]
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 65
    Width = 728
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Remote Control User Agreement and Disclaimer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    ExplicitWidth = 718
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 728
      Height = 1
      Align = alTop
    end
  end
  object PanelBody: TPanel
    Left = 0
    Top = 97
    Width = 728
    Height = 446
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 718
    ExplicitHeight = 414
    object PanelAcceptSentence: TPanel
      Left = 0
      Top = 397
      Width = 728
      Height = 49
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      ExplicitTop = 365
      ExplicitWidth = 718
      object LabelAccept: TLabel
        Left = 24
        Top = 0
        Width = 263
        Height = 15
        Caption = 'If you agree, please type the acceptance sentence:'
      end
      object EditAccept: TEdit
        Left = 16
        Top = 20
        Width = 121
        Height = 23
        TabOrder = 0
      end
    end
    object RichAgreement: TRichEdit
      Left = 0
      Top = 0
      Width = 728
      Height = 397
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      StyleElements = [seClient, seBorder]
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerTimer
    Left = 304
    Top = 264
  end
end
