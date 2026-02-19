object FormDebugThreads: TFormDebugThreads
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'Threads'
  ClientHeight = 165
  ClientWidth = 651
  Color = clBlack
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 14
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 651
    Height = 165
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BackGroundImageTransparent = True
    BorderStyle = bsNone
    Color = clWhite
    Colors.UnfocusedColor = clWindowText
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 25
    Header.Height = 18
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 1
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnCompareNodes = VSTCompareNodes
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitWidth = 667
    ExplicitHeight = 268
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
    Left = 128
    Top = 80
  end
  object PopupMenu: TFlatPopupMenu
    OwnerDraw = True
    OnPopup = PopupMenuPopup
    Left = 256
    Top = 64
    object Terminate1: TMenuItem
      Caption = 'Terminate'
      OnClick = Terminate1Click
    end
  end
  object FlatWindow1: TFlatWindow
    BorderWidth = 2
    CaptionHeight = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    CaptionAlign = taLeftJustify
    ShowBorder = True
    BorderStyle = bsSizeable
    Background = 13554645
    Border = clBlack
    Caption = clBlack
    Left = 336
    Top = 80
  end
end
