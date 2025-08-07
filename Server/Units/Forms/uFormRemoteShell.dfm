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
  Menu = MainMenu
  Position = poOwnerFormCenter
  TextHeight = 15
  object Pages: TPageControl
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 682
    Height = 312
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Images = FormMain.VirtualImageList
    TabOrder = 0
    OnContextPopup = PagesContextPopup
    ExplicitWidth = 672
    ExplicitHeight = 280
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 328
    Width = 698
    Height = 19
    Panels = <>
    ExplicitTop = 296
    ExplicitWidth = 688
  end
  object MainMenu: TMainMenu
    Left = 280
    Top = 144
    object File1: TMenuItem
      Caption = 'Shell'
      object NewInstance1: TMenuItem
        Caption = 'Start New Instance'
        OnClick = NewInstance1Click
      end
    end
  end
  object PopupTabs: TPopupMenu
    Left = 392
    Top = 136
    object erminateInstance1: TMenuItem
      Caption = 'Terminate Instance'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CloseTabTerminate1: TMenuItem
      Caption = 'Close Tab (Terminate)'
    end
    object RenameTab1: TMenuItem
      Caption = 'Rename Tab'
    end
  end
end
