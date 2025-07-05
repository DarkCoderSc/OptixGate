object FormProcessManager: TFormProcessManager
  Left = 0
  Top = 0
  Caption = 'Process Manager'
  ClientHeight = 368
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  ShowInTaskBar = True
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 476
    Height = 368
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BackGroundImageTransparent = True
    BorderStyle = bsNone
    Color = clWhite
    DefaultNodeHeight = 19
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 25
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnFocusChanged = VSTFocusChanged
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitWidth = 579
    ExplicitHeight = 425
    Columns = <
      item
        Position = 0
        Text = 'Name'
        Width = 160
      end
      item
        Position = 1
        Text = 'Id'
        Width = 70
      end
      item
        Position = 2
        Text = 'Parent Id'
        Width = 70
      end
      item
        Position = 3
        Text = 'Thread Count'
        Width = 110
      end
      item
        Position = 4
        Text = 'Username'
        Width = 110
      end
      item
        Position = 5
        Text = 'Domain'
        Width = 110
      end
      item
        Position = 6
        Text = 'Session Id'
        Width = 80
      end
      item
        Position = 7
        Text = 'Elevated'
        Width = 90
      end
      item
        Position = 8
        Text = 'Created Date'
        Width = 115
      end
      item
        Position = 9
        Text = 'Image Path'
        Width = 200
      end>
  end
  object PopupMenu: TPopupMenu
    Left = 131
    Top = 150
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh1Click
    end
  end
end
