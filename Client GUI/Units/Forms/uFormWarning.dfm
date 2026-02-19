object FormWarning: TFormWarning
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'WARNING (Optix Gate)'
  ClientHeight = 428
  ClientWidth = 540
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 14
  object PanelFooter: TFlatPanel
    Left = 0
    Top = 398
    Width = 540
    Height = 30
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    BorderTop = 1
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clWhite
    BorderColor = clBlack
    object ButtonAcceptTheRisk: TFlatButton
      Left = 192
      Top = 6
      Width = 95
      Height = 20
      Caption = 'Accept the Risk'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ShowHint = True
      Images = FormMain.VirtualImageList
      ImageIndex = -1
      Value = 0
      OnClick = ButtonAcceptTheRiskClick
      Busy = False
    end
    object ButtonCancel: TFlatButton
      Left = 63
      Top = 6
      Width = 95
      Height = 20
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ShowHint = True
      ImageIndex = -1
      Value = 0
      OnClick = ButtonCancelClick
      Busy = False
    end
  end
  object PanelHeader: TFlatPanel
    Left = 0
    Top = 0
    Width = 540
    Height = 89
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 1
    Color = clWhite
    BorderColor = clBlack
    object ImageWarning: TSkSvg
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 53
      Height = 81
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      Svg.Source = 
        '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
        's="http://www.w3.org/2000/svg">'#13#10'<path d="M13.7273 13.3C13.9072 ' +
        '13.3 14.0732 13.2034 14.162 13.0469C14.2509 12.8905 14.2488 12.6' +
        '983 14.1566 12.5438L8.42937 2.94384C8.33916 2.79263 8.17606 2.70' +
        '001 7.99998 2.70001C7.8239 2.70001 7.6608 2.79263 7.57059 2.9438' +
        '4L1.84331 12.5438C1.75114 12.6983 1.74908 12.8905 1.83791 13.046' +
        '9C1.92675 13.2034 2.09279 13.3 2.27271 13.3H13.7273Z" fill="#F5D' +
        'A58" stroke="url(#paint0_linear_189_3688)" stroke-linejoin="roun' +
        'd"/>'#13#10'<path d="M7.99998 4.17592L12.8467 12.3H3.15322L7.99998 4.1' +
        '7592Z" stroke="white" stroke-linejoin="round" style="mix-blend-m' +
        'ode:color-burn"/>'#13#10'<path d="M7.99998 4.17592L12.8467 12.3H3.1532' +
        '2L7.99998 4.17592Z" stroke="white" stroke-linejoin="round" style' +
        '="mix-blend-mode:overlay"/>'#13#10'<path d="M8.792 6.182L8.584 9.59H7.' +
        '336L7.128 6.182H8.792ZM7.96 10.262C8.18933 10.262 8.384 10.3447 ' +
        '8.544 10.51C8.704 10.67 8.784 10.862 8.784 11.086C8.784 11.31 8.' +
        '704 11.5047 8.544 11.67C8.384 11.83 8.18933 11.91 7.96 11.91C7.7' +
        '3067 11.91 7.536 11.83 7.376 11.67C7.216 11.5047 7.136 11.31 7.1' +
        '36 11.086C7.136 10.862 7.216 10.67 7.376 10.51C7.536 10.3447 7.7' +
        '3067 10.262 7.96 10.262Z" fill="#C07D2C"/>'#13#10'<defs>'#13#10'<linearGradi' +
        'ent id="paint0_linear_189_3688" x1="7.99998" y1="3.20001" x2="7.' +
        '99998" y2="12.8" gradientUnits="userSpaceOnUse">'#13#10'<stop stop-col' +
        'or="#E1B129"/>'#13#10'<stop offset="1" stop-color="#BF7C2B"/>'#13#10'</linea' +
        'rGradient>'#13#10'</defs>'#13#10'</svg>'
      ExplicitHeight = 65
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 69
      Top = 8
      Width = 463
      Height = 73
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
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = True
      WordWrap = True
      StyleElements = [seClient, seBorder]
      ExplicitWidth = 461
      ExplicitHeight = 45
    end
  end
  object PanelBody: TFlatPanel
    Left = 0
    Top = 89
    Width = 540
    Height = 309
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clWhite
    BorderColor = clWhite
    object PanelAcceptSentence: TFlatPanel
      Left = 0
      Top = 260
      Width = 540
      Height = 49
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
      TabOrder = 0
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = clWhite
      BorderColor = clBlack
      object LabelAccept: TLabel
        Left = 24
        Top = 0
        Width = 251
        Height = 13
        Caption = 'If you agree, please type the acceptance sentence:'
      end
      object EditAccept: TFlatEdit
        Left = 16
        Top = 20
        Width = 121
        Height = 23
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Status = cStatusNormal
        Validators = []
        ShowBorder = True
      end
    end
    object PanelRichAgreement: TFlatPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 524
      Height = 244
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      Caption = 'PanelRichAgreement'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Padding.Left = 1
      Padding.Top = 1
      Padding.Right = 1
      Padding.Bottom = 1
      BorderTop = 1
      BorderLeft = 1
      BorderRight = 1
      BorderBottom = 1
      Color = 13554645
      BorderColor = clBlack
      object RichAgreement: TRichEdit
        Left = 1
        Top = 1
        Width = 522
        Height = 242
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        StyleElements = [seClient, seBorder]
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerTimer
    Left = 304
    Top = 264
  end
  object FlatWindow1: TFlatWindow
    BorderWidth = 2
    CaptionHeight = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    CaptionAlign = taLeftJustify
    ShowBorder = True
    BorderStyle = bsDialog
    Background = 13554645
    Border = clBlack
    Caption = clRed
    Left = 216
    Top = 169
  end
end
