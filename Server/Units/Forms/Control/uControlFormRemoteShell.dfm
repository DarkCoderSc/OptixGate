object ControlFormRemoteShell: TControlFormRemoteShell
  Left = 0
  Top = 0
  Caption = 'Remote Shell'
  ClientHeight = 421
  ClientWidth = 908
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PanelActions: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 35
    Height = 413
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 307
    object ButtonNewInstance: TSpeedButton
      Left = 0
      Top = 0
      Width = 35
      Height = 35
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alTop
      ImageIndex = 56
      ImageName = 'application-console-close-window-filled'
      Images = FormMain.VirtualImageList
      OnClick = ButtonNewInstanceClick
      ExplicitLeft = 2
    end
    object ButtonBreak: TSpeedButton
      AlignWithMargins = True
      Left = 0
      Top = 39
      Width = 35
      Height = 35
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      ImageIndex = 57
      ImageName = 'debug-step-over'
      Images = FormMain.VirtualImageList
      OnClick = ButtonBreakClick
      ExplicitLeft = 2
      ExplicitTop = 35
    end
  end
  object PanelMain: TFlatPanel
    Left = 43
    Top = 0
    Width = 865
    Height = 421
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
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    ExplicitLeft = 44
    ExplicitTop = -4
    object PanelVST: TFlatPanel
      Left = 4
      Top = 4
      Width = 201
      Height = 413
      BorderTop = 2
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
      Color = 13554645
      BorderColor = clBlack
      Align = alLeft
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 421
    end
    object PanelPages: TFlatPanel
      AlignWithMargins = True
      Left = 209
      Top = 4
      Width = 652
      Height = 413
      Margins.Left = 4
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      BorderTop = 2
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
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
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ExplicitLeft = 212
    end
  end
  object PopupTabs: TPopupMenu
    OnPopup = PopupTabsPopup
    Left = 392
    Top = 136
    object erminateInstance1: TMenuItem
      Caption = 'Terminate Instance'
      OnClick = erminateInstance1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CloseTabTerminate1: TMenuItem
      Caption = 'Close Tab (Terminate)'
      OnClick = CloseTabTerminate1Click
    end
    object RenameTab1: TMenuItem
      Caption = 'Rename Tab'
      OnClick = RenameTab1Click
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
  end
end
