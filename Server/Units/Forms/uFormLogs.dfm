object FormLogs: TFormLogs
  Left = 0
  Top = 0
  Caption = 'Session Logs'
  ClientHeight = 344
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 577
    Height = 344
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
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitWidth = 565
    Columns = <
      item
        Position = 0
        Text = 'Message'
        Width = 250
      end
      item
        Position = 1
        Text = 'Context'
        Width = 110
      end
      item
        Position = 2
        Text = 'Kind'
        Width = 80
      end
      item
        Position = 3
        Text = 'When'
        Width = 120
      end>
  end
end
