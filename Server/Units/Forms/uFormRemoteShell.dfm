object FormRemoteShell: TFormRemoteShell
  Left = 0
  Top = 0
  Caption = 'Remote Shell'
  ClientHeight = 347
  ClientWidth = 698
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
  object Pages: TPageControl
    AlignWithMargins = True
    Left = 47
    Top = 4
    Width = 647
    Height = 339
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Images = FormMain.VirtualImageList
    TabOrder = 0
    OnChange = PagesChange
    OnContextPopup = PagesContextPopup
    ExplicitWidth = 637
    ExplicitHeight = 307
  end
  object PanelActions: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 35
    Height = 339
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
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
