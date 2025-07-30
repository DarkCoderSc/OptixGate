object FormTasks: TFormTasks
  Left = 0
  Top = 0
  Caption = 'Tasks'
  ClientHeight = 337
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 702
    Height = 337
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
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = FormMain.VirtualImageList
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnChange = VSTChange
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitWidth = 553
    Columns = <
      item
        Position = 0
        Text = 'Id'
        Width = 200
      end
      item
        Position = 1
        Text = 'Display Name'
        Width = 130
      end
      item
        Position = 2
        Text = 'State'
        Width = 100
      end
      item
        Position = 3
        Text = 'Started'
        Width = 100
      end
      item
        Position = 4
        Text = 'Ended'
        Width = 100
      end>
  end
end
