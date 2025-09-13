object ControlFormTasks: TControlFormTasks
  Left = 0
  Top = 0
  Caption = 'Tasks'
  ClientHeight = 229
  ClientWidth = 774
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 774
    Height = 229
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
    Header.SortColumn = 2
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    OnMouseDown = VSTMouseDown
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Id'
        Width = 240
      end
      item
        Position = 1
        Text = 'Class Name'
        Width = 130
      end
      item
        Position = 2
        Text = 'State'
        Width = 100
      end
      item
        Position = 3
        Text = 'Created'
        Width = 100
      end
      item
        Position = 4
        Text = 'Ended'
        Width = 100
      end
      item
        Position = 5
        Text = 'Description'
        Width = 300
      end>
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 160
    Top = 64
    object Action1: TMenuItem
    end
  end
end
