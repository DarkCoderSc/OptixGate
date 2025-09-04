object ControlFormControlForms: TControlFormControlForms
  Left = 0
  Top = 0
  Caption = 'Control Forms'
  ClientHeight = 271
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 455
    Height = 271
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
    StateImages = FormMain.VirtualImageList
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnDblClick = VSTDblClick
    OnFocusChanged = VSTFocusChanged
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Title'
        Width = 190
      end
      item
        Position = 1
        Text = 'Class'
        Width = 110
      end
      item
        Position = 2
        Text = 'State'
        Width = 100
      end
      item
        Position = 3
        Text = 'Created Date'
        Width = 130
      end
      item
        Position = 4
        Text = 'Last Received Data'
        Width = 130
      end
      item
        Position = 5
        Text = 'Extended Information'
        Width = 250
      end
      item
        Position = 6
        Text = 'GUID'
        Width = 100
      end>
  end
  object PopupMenu: TPopupMenu
    OnChange = PopupMenuChange
    Left = 160
    Top = 96
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Show1: TMenuItem
      Caption = 'Show'
      OnClick = Show1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Purge1: TMenuItem
      Caption = 'Purge'
      OnClick = Purge1Click
    end
  end
  object TimerRefresh: TTimer
    Enabled = False
    OnTimer = TimerRefreshTimer
    Left = 256
    Top = 96
  end
end
