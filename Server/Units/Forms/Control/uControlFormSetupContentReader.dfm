object ControlFormSetupContentReader: TControlFormSetupContentReader
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New Content Reader'
  ClientHeight = 88
  ClientWidth = 336
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
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PanelMain: TFlatPanel
    Left = 0
    Top = 0
    Width = 336
    Height = 88
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = 13554645
    BorderColor = clBlack
    Align = alClient
    Caption = 'PanelMain'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    object PanelBottom: TFlatPanel
      Left = 8
      Top = 85
      Width = 330
      Height = 35
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = 13554645
      BorderColor = clBlack
      Align = alBottom
      Caption = 'PanelBottom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ExplicitTop = 53
      ExplicitWidth = 320
      object ButtonCancel: TFlatButton
        Left = 136
        Top = 6
        Width = 65
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
      object ButtonStart: TFlatButton
        Left = 207
        Top = 12
        Width = 65
        Height = 20
        Caption = 'Start'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ShowHint = True
        ImageIndex = -1
        Value = 0
        OnClick = ButtonStartClick
        Busy = False
      end
    end
    object PanelForm: TFlatPanel
      Left = 57
      Top = 8
      Width = 281
      Height = 77
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = 13554645
      BorderColor = clBlack
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Padding.Left = 16
      ExplicitWidth = 271
      ExplicitHeight = 45
      object LabelPageSize: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 40
        Width = 265
        Height = 13
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'File Path:'
        ExplicitWidth = 45
      end
      object Label2: TLabel
        Left = 16
        Top = 0
        Width = 265
        Height = 13
        Align = alTop
        Caption = 'File Path:'
        ExplicitWidth = 45
      end
      object EditPath: TFlatEdit
        Left = 16
        Top = 13
        Width = 265
        Height = 19
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
        Text = 'C:\Temp\test.png'
        Status = cStatusNormal
        Validators = []
        ShowBorder = True
        ExplicitWidth = 255
      end
      object EditPageSize: TFlatEdit
        Left = 16
        Top = 53
        Width = 265
        Height = 19
        Align = alTop
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '1024'
        Status = cStatusNormal
        Validators = []
        ShowBorder = True
        ExplicitWidth = 255
      end
    end
    object PanelLeft: TFlatPanel
      Left = 8
      Top = 8
      Width = 49
      Height = 77
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = 13554645
      BorderColor = clBlack
      Align = alLeft
      Caption = 'PanelLeft'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      ExplicitHeight = 45
      object SkSvg1: TSkSvg
        Left = 0
        Top = 0
        Width = 49
        Height = 77
        Align = alClient
        Svg.Source = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path d="M6 13.5H0.5V3.5H6C6.27' +
          '614 3.5 6.5 3.72386 6.5 4V4.5H7H9H9.5V4C9.5 3.72386 9.72386 3.5 ' +
          '10 3.5H15.5V13.5H10C9.34689 13.5 8.79127 13.9174 8.58535 14.5H7.' +
          '41465C7.20873 13.9174 6.65311 13.5 6 13.5Z" fill="#6EA0C9"/>'#13#10'<p' +
          'ath d="M6 13.5H0.5V3.5H6C6.27614 3.5 6.5 3.72386 6.5 4V4.5H7H9H9' +
          '.5V4C9.5 3.72386 9.72386 3.5 10 3.5H15.5V13.5H10C9.34689 13.5 8.' +
          '79127 13.9174 8.58535 14.5H7.41465C7.20873 13.9174 6.65311 13.5 ' +
          '6 13.5Z" fill="url(#paint0_linear_99_12639)"/>'#13#10'<path d="M6 13.5' +
          'H0.5V3.5H6C6.27614 3.5 6.5 3.72386 6.5 4V4.5H7H9H9.5V4C9.5 3.723' +
          '86 9.72386 3.5 10 3.5H15.5V13.5H10C9.34689 13.5 8.79127 13.9174 ' +
          '8.58535 14.5H7.41465C7.20873 13.9174 6.65311 13.5 6 13.5Z" strok' +
          'e="url(#paint1_linear_99_12639)"/>'#13#10'<path d="M10.4142 2.5H14.5V1' +
          '1.5H10.4142C9.53452 11.5 8.68536 11.7961 8 12.3332C7.31464 11.79' +
          '61 6.46548 11.5 5.58579 11.5H1.5V2.5H5.58579C6.35868 2.5 7.09993' +
          ' 2.80703 7.64645 3.35355L8 3.70711L8.35355 3.35355C8.90007 2.807' +
          '03 9.64132 2.5 10.4142 2.5Z" fill="url(#paint2_linear_99_12639)"' +
          ' stroke="url(#paint3_linear_99_12639)"/>'#13#10'<path fill-rule="eveno' +
          'dd" clip-rule="evenodd" d="M3 4.5C3 4.22386 3.22386 4 3.5 4H4.79' +
          '18C5.33515 4 5.87105 4.12651 6.35704 4.3695L6.72361 4.55279C6.97' +
          '06 4.67628 7.07071 4.97662 6.94721 5.22361C6.82372 5.4706 6.5233' +
          '8 5.57071 6.27639 5.44721L5.90983 5.26393C5.56269 5.09036 5.1799' +
          '1 5 4.7918 5H3.5C3.22386 5 3 4.77614 3 4.5Z" fill="black" fill-o' +
          'pacity="0.33"/>'#13#10'<path fill-rule="evenodd" clip-rule="evenodd" d' +
          '="M3 8.5C3 8.22386 3.22386 8 3.5 8H4.7918C5.33515 8 5.87105 8.12' +
          '651 6.35704 8.3695L6.72361 8.55279C6.9706 8.67628 7.07071 8.9766' +
          '2 6.94721 9.22361C6.82372 9.4706 6.52338 9.57071 6.27639 9.44721' +
          'L5.90983 9.26393C5.56269 9.09036 5.17991 9 4.7918 9H3.5C3.22386 ' +
          '9 3 8.77614 3 8.5Z" fill="black" fill-opacity="0.33"/>'#13#10'<path fi' +
          'll-rule="evenodd" clip-rule="evenodd" d="M3 6.5C3 6.22386 3.2238' +
          '6 6 3.5 6H4.7918C5.33515 6 5.87105 6.12651 6.35704 6.3695L6.7236' +
          '1 6.55279C6.9706 6.67628 7.07071 6.97662 6.94721 7.22361C6.82372' +
          ' 7.4706 6.52338 7.57071 6.27639 7.44721L5.90983 7.26393C5.56269 ' +
          '7.09036 5.17991 7 4.7918 7H3.5C3.22386 7 3 6.77614 3 6.5Z" fill=' +
          '"black" fill-opacity="0.33"/>'#13#10'<path fill-rule="evenodd" clip-ru' +
          'le="evenodd" d="M13.0001 4.5C13.0001 4.22386 12.7763 4 12.5001 4' +
          'H11.2083C10.665 4 10.1291 4.12651 9.64308 4.3695L9.27652 4.55279' +
          'C9.02953 4.67628 8.92941 4.97662 9.05291 5.22361C9.1764 5.4706 9' +
          '.47674 5.57071 9.72373 5.44721L10.0903 5.26393C10.4374 5.09036 1' +
          '0.8202 5 11.2083 5H12.5001C12.7763 5 13.0001 4.77614 13.0001 4.5' +
          'Z" fill="black" fill-opacity="0.33"/>'#13#10'<path fill-rule="evenodd"' +
          ' clip-rule="evenodd" d="M13.0001 8.5C13.0001 8.22386 12.7763 8 1' +
          '2.5001 8H11.2083C10.665 8 10.1291 8.12651 9.64308 8.3695L9.27652' +
          ' 8.55279C9.02953 8.67628 8.92941 8.97662 9.05291 9.22361C9.1764 ' +
          '9.4706 9.47674 9.57071 9.72373 9.44721L10.0903 9.26393C10.4374 9' +
          '.09036 10.8202 9 11.2083 9H12.5001C12.7763 9 13.0001 8.77614 13.' +
          '0001 8.5Z" fill="black" fill-opacity="0.33"/>'#13#10'<path fill-rule="' +
          'evenodd" clip-rule="evenodd" d="M13.0001 6.5C13.0001 6.22386 12.' +
          '7763 6 12.5001 6H11.2083C10.665 6 10.1291 6.12651 9.64308 6.3695' +
          'L9.27652 6.55279C9.02953 6.67628 8.92941 6.97662 9.05291 7.22361' +
          'C9.1764 7.4706 9.47674 7.57071 9.72373 7.44721L10.0903 7.26393C1' +
          '0.4374 7.09036 10.8202 7 11.2083 7H12.5001C12.7763 7 13.0001 6.7' +
          '7614 13.0001 6.5Z" fill="black" fill-opacity="0.33"/>'#13#10'<defs>'#13#10'<' +
          'linearGradient id="paint0_linear_99_12639" x1="1.33333" y1="3.75' +
          '" x2="6.81895" y2="17.4031" gradientUnits="userSpaceOnUse">'#13#10'<st' +
          'op offset="0.322074" stop-color="#A9CEF6"/>'#13#10'<stop offset="1" st' +
          'op-color="#3E77B5"/>'#13#10'</linearGradient>'#13#10'<linearGradient id="pai' +
          'nt1_linear_99_12639" x1="16" y1="15" x2="9.28497" y2="-0.917099"' +
          ' gradientUnits="userSpaceOnUse">'#13#10'<stop stop-color="#012E5B"/>'#13#10 +
          '<stop offset="1" stop-color="#6399C6"/>'#13#10'</linearGradient>'#13#10'<lin' +
          'earGradient id="paint2_linear_99_12639" x1="1" y1="8" x2="15" y2' +
          '="8" gradientUnits="userSpaceOnUse">'#13#10'<stop offset="0.357143" st' +
          'op-color="#F8F8F8"/>'#13#10'<stop offset="0.504464" stop-color="#D0D0D' +
          '0"/>'#13#10'<stop offset="0.647321" stop-color="#F8F8F8"/>'#13#10'</linearGr' +
          'adient>'#13#10'<linearGradient id="paint3_linear_99_12639" x1="15" y1=' +
          '"13" x2="8.64507" y2="-1.37884" gradientUnits="userSpaceOnUse">'#13 +
          #10'<stop stop-color="#363636"/>'#13#10'<stop offset="1" stop-color="#F2F' +
          '2F2"/>'#13#10'</linearGradient>'#13#10'</defs>'#13#10'</svg>'
        ExplicitLeft = 8
        ExplicitTop = 8
        ExplicitHeight = 80
      end
    end
  end
end
