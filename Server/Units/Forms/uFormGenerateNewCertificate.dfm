object FormGenerateNewCertificate: TFormGenerateNewCertificate
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Generate New Certificate'
  ClientHeight = 179
  ClientWidth = 323
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 14
  object PanelClient: TFlatPanel
    Left = 75
    Top = 0
    Width = 248
    Height = 149
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = 13554645
    BorderColor = clBlack
    object Label2: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 96
      Width = 240
      Height = 13
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Common Name:'
      ExplicitWidth = 75
    end
    object Label1: TLabel
      Left = 0
      Top = 8
      Width = 240
      Height = 13
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Country Name:'
      ExplicitWidth = 73
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 52
      Width = 240
      Height = 13
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Organization Name:'
      ExplicitWidth = 95
    end
    object EditCN: TFlatEdit
      Left = 0
      Top = 109
      Width = 240
      Height = 23
      Align = alTop
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
      Text = 'localhost'
      Status = cStatusNormal
      Validators = []
      ShowBorder = True
    end
    object EditC: TFlatEdit
      Left = 0
      Top = 21
      Width = 240
      Height = 23
      Align = alTop
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
      TabOrder = 1
      Text = 'FR'
      Status = cStatusNormal
      Validators = []
      ShowBorder = True
    end
    object EditO: TFlatEdit
      Left = 0
      Top = 65
      Width = 240
      Height = 23
      Align = alTop
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
      TabOrder = 2
      Text = 'DarkCoderSc'
      Status = cStatusNormal
      Validators = []
      ShowBorder = True
    end
  end
  object PanelLeft: TFlatPanel
    Left = 0
    Top = 0
    Width = 75
    Height = 149
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Padding.Right = 8
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = 13554645
    BorderColor = clBlack
    object SkSvg1: TSkSvg
      Left = 0
      Top = 0
      Width = 67
      Height = 149
      Align = alClient
      Svg.Source = 
        '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
        's="http://www.w3.org/2000/svg">'#13#10'<mask id="path-1-outside-1_501_' +
        '8514" maskUnits="userSpaceOnUse" x="2" y="8.5" width="12" height' +
        '="8" fill="black">'#13#10'<rect fill="white" x="2" y="8.5" width="12" ' +
        'height="8"/>'#13#10'<path d="M5.5 10.5L3 15L5 14L6 15L6.5 11L5.5 10.5Z' +
        '"/>'#13#10'<path d="M10.5 10.5L13 15L11 14L10 15L9.5 11L10.5 10.5Z"/>'#13 +
        #10'</mask>'#13#10'<path d="M5.5 10.5L3 15L5 14L6 15L6.5 11L5.5 10.5Z" fi' +
        'll="url(#paint0_linear_501_8514)"/>'#13#10'<path d="M10.5 10.5L13 15L1' +
        '1 14L10 15L9.5 11L10.5 10.5Z" fill="url(#paint1_linear_501_8514)' +
        '"/>'#13#10'<path d="M3 15L2.12584 14.5144C1.91292 14.8976 1.97448 15.3' +
        '749 2.2777 15.6916C2.58092 16.0083 3.05506 16.0905 3.44721 15.89' +
        '44L3 15ZM5.5 10.5L5.94721 9.60557C5.46834 9.36614 4.88585 9.5463' +
        '4 4.62584 10.0144L5.5 10.5ZM6.5 11L7.49228 11.124C7.54487 10.703' +
        '3 7.32644 10.2952 6.94721 10.1056L6.5 11ZM6 15L5.29289 15.7071C5' +
        '.56296 15.9772 5.96328 16.0698 6.32454 15.9459C6.6858 15.8219 6.' +
        '9449 15.503 6.99228 15.124L6 15ZM5 14L5.70711 13.2929L5.19745 12' +
        '.7832L4.55279 13.1056L5 14ZM13 15L12.5528 15.8944C12.9449 16.090' +
        '5 13.4191 16.0083 13.7223 15.6916C14.0255 15.3749 14.0871 14.897' +
        '6 13.8742 14.5144L13 15ZM10.5 10.5L11.3742 10.0144C11.1141 9.546' +
        '34 10.5317 9.36614 10.0528 9.60557L10.5 10.5ZM9.5 11L9.05279 10.' +
        '1056C8.67356 10.2952 8.45513 10.7033 8.50772 11.124L9.5 11ZM10 1' +
        '5L9.00772 15.124C9.0551 15.503 9.3142 15.8219 9.67546 15.9459C10' +
        '.0367 16.0698 10.437 15.9772 10.7071 15.7071L10 15ZM11 14L11.447' +
        '2 13.1056L10.8025 12.7832L10.2929 13.2929L11 14ZM3.87416 15.4856' +
        'L6.37416 10.9856L4.62584 10.0144L2.12584 14.5144L3.87416 15.4856' +
        'ZM5.05279 11.3944L6.05279 11.8944L6.94721 10.1056L5.94721 9.6055' +
        '7L5.05279 11.3944ZM5.50772 10.876L5.00772 14.876L6.99228 15.124L' +
        '7.49228 11.124L5.50772 10.876ZM6.70711 14.2929L5.70711 13.2929L4' +
        '.29289 14.7071L5.29289 15.7071L6.70711 14.2929ZM4.55279 13.1056L' +
        '2.55279 14.1056L3.44721 15.8944L5.44721 14.8944L4.55279 13.1056Z' +
        'M13.8742 14.5144L11.3742 10.0144L9.62584 10.9856L12.1258 15.4856' +
        'L13.8742 14.5144ZM10.0528 9.60557L9.05279 10.1056L9.94721 11.894' +
        '4L10.9472 11.3944L10.0528 9.60557ZM8.50772 11.124L9.00772 15.124' +
        'L10.9923 14.876L10.4923 10.876L8.50772 11.124ZM10.7071 15.7071L1' +
        '1.7071 14.7071L10.2929 13.2929L9.29289 14.2929L10.7071 15.7071ZM' +
        '10.5528 14.8944L12.5528 15.8944L13.4472 14.1056L11.4472 13.1056L' +
        '10.5528 14.8944Z" fill="url(#paint2_linear_501_8514)" mask="url(' +
        '#path-1-outside-1_501_8514)"/>'#13#10'<path d="M8 1L11.2139 2.16978L12' +
        '.924 5.13176L12.3301 8.5L9.7101 10.6985H6.2899L3.66987 8.5L3.075' +
        '96 5.13176L4.78606 2.16978L8 1Z" fill="url(#paint3_radial_501_85' +
        '14)"/>'#13#10'<path d="M8.17101 0.530154C8.06055 0.489949 7.93945 0.48' +
        '9949 7.82899 0.530154L4.61505 1.69993C4.50459 1.74014 4.41182 1.' +
        '81798 4.35305 1.91978L2.64295 4.88176C2.58417 4.98356 2.56314 5.' +
        '10282 2.58356 5.21858L3.17747 8.58682C3.19788 8.70259 3.25843 8.' +
        '80746 3.34848 8.88302L5.96851 11.0815C6.05856 11.157 6.17235 11.' +
        '1985 6.2899 11.1985H9.7101C9.82765 11.1985 9.94144 11.157 10.031' +
        '5 11.0815L12.6515 8.88302C12.7416 8.80746 12.8021 8.70259 12.822' +
        '5 8.58682L13.4164 5.21858C13.4369 5.10282 13.4158 4.98356 13.357' +
        '1 4.88176L11.647 1.91978C11.5882 1.81798 11.4954 1.74014 11.3849' +
        ' 1.69993L8.17101 0.530154Z" stroke="url(#paint4_linear_501_8514)' +
        '" stroke-opacity="0.992157" stroke-linejoin="round"/>'#13#10'<path d="' +
        'M5.12808 2.57738L8 1.53209L10.8719 2.57738L12.4 5.22416L11.8693 ' +
        '8.23396L9.52812 10.1985H6.47188L4.13068 8.23396L3.59997 5.22416L' +
        '5.12808 2.57738Z" stroke="white" stroke-opacity="0.5" stroke-lin' +
        'ejoin="round"/>'#13#10'<defs>'#13#10'<linearGradient id="paint0_linear_501_8' +
        '514" x1="8" y1="11" x2="8" y2="14.5" gradientUnits="userSpaceOnU' +
        'se">'#13#10'<stop stop-color="#D30101"/>'#13#10'<stop offset="1" stop-color=' +
        '"#F90000"/>'#13#10'</linearGradient>'#13#10'<linearGradient id="paint1_linea' +
        'r_501_8514" x1="8" y1="11" x2="8" y2="14.5" gradientUnits="userS' +
        'paceOnUse">'#13#10'<stop stop-color="#D30101"/>'#13#10'<stop offset="1" stop' +
        '-color="#F90000"/>'#13#10'</linearGradient>'#13#10'<linearGradient id="paint' +
        '2_linear_501_8514" x1="12.125" y1="14.5" x2="5.5" y2="10.5" grad' +
        'ientUnits="userSpaceOnUse">'#13#10'<stop stop-color="#F41212"/>'#13#10'<stop' +
        ' offset="1" stop-color="#C50000"/>'#13#10'</linearGradient>'#13#10'<radialGr' +
        'adient id="paint3_radial_501_8514" cx="0" cy="0" r="1" gradientU' +
        'nits="userSpaceOnUse" gradientTransform="translate(8 6) rotate(9' +
        '0) scale(5)">'#13#10'<stop stop-color="#FC2E33"/>'#13#10'<stop offset="1" st' +
        'op-color="#DD080E"/>'#13#10'</radialGradient>'#13#10'<linearGradient id="pai' +
        'nt4_linear_501_8514" x1="3" y1="1" x2="13" y2="11" gradientUnits' +
        '="userSpaceOnUse">'#13#10'<stop stop-color="#F91616"/>'#13#10'<stop offset="' +
        '1" stop-color="#B80B0B"/>'#13#10'</linearGradient>'#13#10'</defs>'#13#10'</svg>'
      ExplicitLeft = 8
      ExplicitTop = 40
      ExplicitWidth = 50
      ExplicitHeight = 50
    end
  end
  object PanelBottom: TFlatPanel
    Left = 0
    Top = 149
    Width = 323
    Height = 30
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    BorderTop = 1
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = 13554645
    BorderColor = clBlack
    object ButtonGenerate: TFlatButton
      Left = 208
      Top = 10
      Width = 80
      Height = 20
      Caption = 'Generate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ShowHint = True
      ImageIndex = -1
      Value = 0
      OnClick = ButtonGenerateClick
      Busy = False
    end
    object ButtonCancel: TFlatButton
      Left = 122
      Top = 10
      Width = 80
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
    Caption = clBlack
    Left = 83
    Top = 80
  end
end
