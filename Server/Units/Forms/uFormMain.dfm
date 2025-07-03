object FormMain: TFormMain
  Left = 0
  Top = 0
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  Caption = 'Optix Gate'
  ClientHeight = 686
  ClientWidth = 1015
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 32
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 1015
    Height = 667
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BackGroundImageTransparent = True
    BorderStyle = bsNone
    Color = clWhite
    DefaultNodeHeight = 40
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 25
    Header.Height = 41
    Header.MaxHeight = 20000
    Header.MinHeight = 20
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Indent = 36
    Margin = 8
    PopupMenu = PopupMenu
    TabOrder = 0
    TextMargin = 8
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitHeight = 686
    Columns = <
      item
        MaxWidth = 20000
        MinWidth = 20
        Position = 0
        Spacing = 6
        Text = 'Remote Address'
        Width = 200
      end
      item
        MaxWidth = 20000
        MinWidth = 20
        Position = 1
        Spacing = 6
        Text = 'Username'
        Width = 220
      end
      item
        MaxWidth = 20000
        MinWidth = 20
        Position = 2
        Spacing = 6
        Text = 'Machine'
        Width = 220
      end
      item
        MaxWidth = 20000
        MinWidth = 20
        Position = 3
        Spacing = 6
        Text = 'Windows Version'
        Width = 400
      end
      item
        MaxWidth = 20000
        MinWidth = 20
        Position = 4
        Spacing = 6
        Text = 'Spawn Since'
        Width = 260
      end
      item
        MaxWidth = 20000
        MinWidth = 20
        Position = 5
        Spacing = 6
        Text = 'Process Id'
        Width = 280
      end
      item
        Position = 6
        Text = 'Process Arch'
        Width = 210
      end
      item
        MaxWidth = 20000
        MinWidth = 20
        Position = 7
        Spacing = 6
        Text = 'Elevated Status'
        Width = 200
      end>
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 1334
    Width = 2030
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <
      item
        Width = 260
      end>
  end
  object MainMenu: TMainMenu
    Left = 240
    Top = 272
    object File1: TMenuItem
      Caption = 'File'
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
    object Server1: TMenuItem
      Caption = 'Server'
      object Start1: TMenuItem
        Caption = 'Start'
        OnClick = Start1Click
      end
    end
    object About1: TMenuItem
      Caption = 'About'
    end
  end
  object PopupMenu: TPopupMenu
    Left = 448
    Top = 272
    object ProcessManager1: TMenuItem
      Caption = 'Process Manager'
    end
    object ServiceManager1: TMenuItem
      Caption = 'Service Manager'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object FileManager1: TMenuItem
      Caption = 'File Manager'
    end
    object SearchForFiles1: TMenuItem
      Caption = 'Search For Files'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object RegistryManager1: TMenuItem
      Caption = 'Registry Manager'
    end
    object RegistrySearch1: TMenuItem
      Caption = 'Registry Search'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object RemoteShell1: TMenuItem
      Caption = 'Remote Shell'
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Privesc1: TMenuItem
      Caption = 'Privesc'
      object FodHelper1: TMenuItem
        Caption = 'Elevate (FodHelper)'
      end
      object SYSTEMTaskScheduler1: TMenuItem
        Caption = 'SYSTEM (Task Scheduler)'
      end
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object CodeInjection1: TMenuItem
      Caption = 'Code Injection'
    end
    object FuncInEngine1: TMenuItem
      Caption = 'Func-In Engine'
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object erminate1: TMenuItem
      Caption = 'Terminate'
    end
  end
end
