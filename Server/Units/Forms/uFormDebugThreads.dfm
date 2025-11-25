object FormDebugThreads: TFormDebugThreads
  Left = 0
  Top = 0
  Caption = 'Optix Thread Manager'
  ClientHeight = 290
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnMouseDown = FormMouseDown
  OnShow = FormShow
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 690
    Height = 290
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
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 1
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnCompareNodes = VSTCompareNodes
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Id'
        Width = 130
      end
      item
        Position = 1
        Text = 'Class'
        Width = 200
      end
      item
        Position = 2
        Text = 'Running'
        Width = 100
      end
      item
        Position = 3
        Text = 'Created Time'
        Width = 100
      end
      item
        Position = 4
        Text = 'Priority'
        Width = 100
      end>
  end
  object TimerRefresh: TTimer
    Enabled = False
    OnTimer = TimerRefreshTimer
    Left = 120
    Top = 120
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 240
    Top = 120
    object Terminate1: TMenuItem
      Caption = 'Terminate'
      OnClick = Terminate1Click
    end
  end
end
