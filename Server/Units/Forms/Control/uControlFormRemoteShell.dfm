object ControlFormRemoteShell: TControlFormRemoteShell
  Left = 0
  Top = 0
  Caption = 'Remote Shell'
  ClientHeight = 294
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object PanelMain: TFlatPanel
    Left = 0
    Top = 0
    Width = 680
    Height = 294
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
    TabOrder = 0
    ExplicitWidth = 670
    ExplicitHeight = 262
    object OMultiPanel: TOMultiPanel
      AlignWithMargins = True
      Left = 4
      Top = 33
      Width = 672
      Height = 257
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      PanelType = ptVertical
      PanelCollection = <
        item
          Control = PanelInstances
          Position = 1.000000000000000000
          Visible = True
          Index = 0
        end>
      MinPosition = 0.020000000000000000
      SplitterColor = 13554645
      SplitterHoverColor = clHighlight
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 662
      ExplicitHeight = 225
      object PanelInstances: TFlatPanel
        Left = 0
        Top = 0
        Width = 672
        Height = 257
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        BorderTop = 2
        BorderLeft = 2
        BorderRight = 2
        BorderBottom = 2
        Color = clWhite
        BorderColor = clBlack
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Padding.Left = 2
        Padding.Top = 2
        Padding.Right = 2
        Padding.Bottom = 2
      end
    end
    object PanelActions: TFlatPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 672
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 0
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = clBlack
      BorderColor = clBlack
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ExplicitWidth = 662
      object ButtonUploadFileToCwd: TFlatImageButton
        AlignWithMargins = True
        Left = 122
        Top = 2
        Width = 23
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 1
        Margins.Bottom = 0
        Align = alLeft
        Visible = False
        Background = 13554645
        ImageList = FormMain.VirtualImageList
        ImageIndex = 22
        Value = 0
        ExplicitLeft = 121
        ExplicitTop = 1
        ExplicitHeight = 23
      end
      object ButtonRenameInstance: TFlatImageButton
        AlignWithMargins = True
        Left = 98
        Top = 2
        Width = 23
        Height = 21
        Hint = 'Rename the current shell session.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 1
        Margins.Bottom = 0
        Align = alLeft
        Background = 13554645
        ImageList = FormMain.VirtualImageList
        ImageIndex = 21
        OnClick = ButtonRenameInstanceClick
        Value = 0
        ExplicitLeft = 97
        ExplicitTop = 1
        ExplicitHeight = 23
      end
      object ButtonBreakInstance: TFlatImageButton
        AlignWithMargins = True
        Left = 74
        Top = 2
        Width = 23
        Height = 21
        Hint = 
          'Send a break signal (Ctrl+C) to the current remote shell session' +
          '. This interrupts the currently running command.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 1
        Margins.Bottom = 0
        Align = alLeft
        Background = 13554645
        ImageList = FormMain.VirtualImageList
        ImageIndex = 20
        OnClick = ButtonBreakInstanceClick
        Value = 0
        ExplicitLeft = 73
        ExplicitTop = 1
        ExplicitHeight = 23
      end
      object ButtonCloseInstance: TFlatImageButton
        AlignWithMargins = True
        Left = 50
        Top = 2
        Width = 23
        Height = 21
        Hint = 
          'Close the current remote shell instance. You will no longer be a' +
          'ble to send commands or receive data for this session, but its c' +
          'ontents will remain accessible.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 1
        Margins.Bottom = 0
        Align = alLeft
        Background = 13554645
        ImageList = FormMain.VirtualImageList
        ImageIndex = 19
        OnClick = ButtonCloseInstanceClick
        Value = 0
        ExplicitLeft = 49
        ExplicitTop = 1
        ExplicitHeight = 23
      end
      object ButtonDeleteInstance: TFlatImageButton
        AlignWithMargins = True
        Left = 26
        Top = 2
        Width = 23
        Height = 21
        Hint = 
          'Delete the current remote shell instance. This action will close' +
          ' the session and permanently remove its contents.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 1
        Margins.Bottom = 0
        Align = alLeft
        Background = 13554645
        ImageList = FormMain.VirtualImageList
        ImageIndex = 18
        OnClick = ButtonDeleteInstanceClick
        Value = 0
        ExplicitLeft = 25
        ExplicitTop = 1
        ExplicitHeight = 23
      end
      object ButtonNewInstance: TFlatImageButton
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 23
        Height = 21
        Hint = 'Request a new remote shell instance.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 1
        Margins.Bottom = 0
        Align = alLeft
        Background = 13554645
        ImageList = FormMain.VirtualImageList
        ImageIndex = 17
        OnClick = ButtonNewInstanceClick
        Value = 0
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitHeight = 23
      end
      object ButtonSaveOutput: TFlatImageButton
        AlignWithMargins = True
        Left = 146
        Top = 2
        Width = 23
        Height = 21
        Hint = 
          'Save the current remote shell session content. This allows you t' +
          'o keep a record of the commands executed and their corresponding' +
          ' output.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 1
        Margins.Bottom = 0
        Align = alLeft
        Background = 13554645
        ImageList = FormMain.VirtualImageList
        ImageIndex = 23
        OnClick = ButtonSaveOutputClick
        Value = 0
        ExplicitLeft = 170
      end
      object PanelComboInstance: TFlatPanel
        Left = 170
        Top = 2
        Width = 500
        Height = 21
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
        TabOrder = 0
        ExplicitWidth = 490
        object ComboInstance: TFlatComboBox
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 496
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 0
          Align = alTop
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          ItemIndex = -1
          OnChange = ComboInstanceChange
          Status = cStatusNormal
          Validators = []
          ExplicitWidth = 486
        end
      end
    end
  end
  object ActionList: TActionList
    Left = 280
    Top = 144
    object NewShellInstance1: TAction
      Caption = 'Start New Shell Instance'
      ShortCut = 16462
      OnExecute = NewShellInstance1Execute
    end
    object BreakActiveShellInstance1: TAction
      Caption = 'Break Active Shell Instance'
      ShortCut = 16451
      OnExecute = BreakActiveShellInstance1Execute
    end
    object SaveInstanceOutputToFile1: TAction
      ShortCut = 16467
      OnExecute = SaveInstanceOutputToFile1Execute
    end
  end
  object SD: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Plain text file|.txt'
    Left = 396
    Top = 153
  end
end
