object FormServers: TFormServers
  Left = 0
  Top = 0
  Caption = 'Servers / Listeners'
  ClientHeight = 227
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 673
    Height = 227
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BackGroundImageTransparent = True
    BorderStyle = bsNone
    Color = clWhite
    Colors.UnfocusedColor = clWindowText
    DefaultNodeHeight = 19
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 25
    Header.MainColumn = 1
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 0
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnChange = VSTChange
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Address (Interface)'
        Width = 200
      end
      item
        Position = 1
        Text = 'Port'
        Width = 90
      end
      item
        Position = 2
        Text = 'IP Version'
        Width = 100
      end
      item
        Position = 3
        Text = 'Status'
        Width = 120
      end
      item
        Position = 4
        Text = 'Server Certificate'
        Width = 250
      end
      item
        Position = 5
        Text = 'Status Message'
        Width = 200
      end>
  end
  object MainMenu: TMainMenu
    Left = 104
    Top = 96
    object Server1: TMenuItem
      Caption = 'Server'
      object New1: TMenuItem
        Caption = 'New'
        ShortCut = 16462
        OnClick = New1Click
      end
    end
  end
  object PopupMenu: TPopupMenu
    Left = 240
    Top = 72
    object Start1: TMenuItem
      Caption = 'Start'
      OnClick = Start1Click
    end
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object AutoStart1: TMenuItem
      AutoCheck = True
      Caption = 'Auto Start'
      Checked = True
    end
  end
end
