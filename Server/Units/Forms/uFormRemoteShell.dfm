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
    Height = 331
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 48
    ExplicitTop = 13
  end
  object MainMenu: TMainMenu
    Left = 280
    Top = 144
    object File1: TMenuItem
      Caption = 'File'
      object NewInstance1: TMenuItem
        Caption = 'New Instance'
        OnClick = NewInstance1Click
      end
    end
  end
end
