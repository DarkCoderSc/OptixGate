object ControlFormRegistryManager: TControlFormRegistryManager
  Left = 0
  Top = 0
  Caption = 'Registry Manager'
  ClientHeight = 431
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object OMultiPanel: TOMultiPanel
    Left = 0
    Top = 31
    Width = 711
    Height = 400
    PanelCollection = <
      item
        Control = VSTKeys
        Position = 0.333333333333333300
        Visible = True
        Index = 0
      end
      item
        Control = VSTValues
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    DesignSize = (
      711
      400)
    object VSTKeys: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 237
      Height = 400
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = []
      BackGroundImageTransparent = True
      BorderStyle = bsNone
      Color = clWhite
      Colors.UnfocusedColor = clWindowText
      DefaultNodeHeight = 19
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 25
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoHeaderClickAutoSort]
      Header.SortColumn = 0
      Images = FormMain.ImageSystem
      PopupMenu = PopupKeys
      StateImages = FormMain.VirtualImageList
      TabOrder = 0
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
      OnChange = VSTKeysChange
      OnCompareNodes = VSTKeysCompareNodes
      OnDblClick = VSTKeysDblClick
      OnFocusChanged = VSTKeysFocusChanged
      OnFreeNode = VSTKeysFreeNode
      OnGetText = VSTKeysGetText
      OnGetImageIndex = VSTKeysGetImageIndex
      OnGetNodeDataSize = VSTKeysGetNodeDataSize
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          Position = 0
          Text = 'Name'
          Width = 237
        end>
    end
    object VSTValues: TVirtualStringTree
      Left = 240
      Top = 0
      Width = 471
      Height = 400
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = []
      BackGroundImageTransparent = True
      BorderStyle = bsNone
      Color = clWhite
      Colors.UnfocusedColor = clWindowText
      DefaultNodeHeight = 19
      Header.AutoSizeIndex = -1
      Header.DefaultHeight = 25
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.SortColumn = 0
      Images = FormMain.VirtualImageList
      PopupMenu = PopupValues
      TabOrder = 1
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
      OnChange = VSTValuesChange
      OnCompareNodes = VSTValuesCompareNodes
      OnDblClick = VSTValuesDblClick
      OnFocusChanged = VSTValuesFocusChanged
      OnFreeNode = VSTValuesFreeNode
      OnGetText = VSTValuesGetText
      OnGetImageIndex = VSTValuesGetImageIndex
      OnGetNodeDataSize = VSTValuesGetNodeDataSize
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          Position = 0
          Text = 'Name'
          Width = 150
        end
        item
          Position = 1
          Text = 'Type'
          Width = 120
        end
        item
          Position = 2
          Text = 'Data'
          Width = 200
        end>
    end
  end
  object EditPath: TEdit
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 703
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    BevelOuter = bvRaised
    ReadOnly = True
    TabOrder = 1
  end
  object MainMenu: TMainMenu
    Left = 408
    Top = 191
    object Registry1: TMenuItem
      Caption = 'Registry'
      object Refresh1: TMenuItem
        Caption = 'Refresh'
        ShortCut = 16466
        OnClick = Refresh1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object CreateKey1: TMenuItem
        Caption = 'Create Key'
        OnClick = CreateKey1Click
      end
      object GoTo1: TMenuItem
        Caption = 'Go To'
        OnClick = GoTo1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object HideUnenumerableKeys1: TMenuItem
        AutoCheck = True
        Caption = 'Hide Unenumerable Keys'
        Checked = True
        OnClick = HideUnenumerableKeys1Click
      end
    end
  end
  object PopupKeys: TPopupMenu
    OnPopup = PopupKeysPopup
    Left = 64
    Top = 167
    object FullExpand1: TMenuItem
      Caption = 'Full Expand'
      OnClick = FullExpand1Click
    end
    object FullCollapse1: TMenuItem
      Caption = 'Full Collapse'
      OnClick = FullCollapse1Click
    end
    object FullCollapse2: TMenuItem
      Caption = '-'
    end
    object CreateSubKey1: TMenuItem
      Caption = 'Create Sub Key'
      OnClick = CreateSubKey1Click
    end
    object DeleteSelectedKey1: TMenuItem
      Caption = 'Delete Selected Key'
      OnClick = DeleteSelectedKey1Click
    end
  end
  object PopupValues: TPopupMenu
    OnPopup = PopupValuesPopup
    Left = 520
    Top = 191
    object Refresh2: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh2Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object New1: TMenuItem
      Caption = 'New'
      object NewKey1: TMenuItem
        Caption = 'Key'
        OnClick = NewKey1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object NewStringValue1: TMenuItem
        Caption = 'String (SZ)'
        OnClick = NewStringValue1Click
      end
      object NewMultiLineStringValue1: TMenuItem
        Caption = 'Multi Line String (MSZ)'
        OnClick = NewMultiLineStringValue1Click
      end
      object NewDWORDValue1: TMenuItem
        Caption = 'DWORD (4 Byte)'
        OnClick = NewDWORDValue1Click
      end
      object NewQWORDValue1: TMenuItem
        Caption = 'QWORD (8 Bytes)'
        OnClick = NewQWORDValue1Click
      end
      object NewBinaryValue1: TMenuItem
        Caption = 'Binary'
        OnClick = NewBinaryValue1Click
      end
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object EditSelectedValue1: TMenuItem
      Caption = 'Edit Selected'
      OnClick = EditSelectedValue1Click
    end
  end
end
