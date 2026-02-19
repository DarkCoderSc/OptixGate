object ControlFormRegistryManager: TControlFormRegistryManager
  Left = 0
  Top = 0
  Caption = 'Registry Manager'
  ClientHeight = 311
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PanelMain: TFlatPanel
    Left = 0
    Top = 0
    Width = 537
    Height = 311
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = 13554645
    BorderColor = clBlack
    ExplicitWidth = 547
    object OMultiPanel: TOMultiPanel
      AlignWithMargins = True
      Left = 4
      Top = 29
      Width = 529
      Height = 278
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      PanelCollection = <
        item
          Control = PanelVSTKeys
          Position = 0.330000000000000000
          Visible = True
          Index = 0
        end
        item
          Control = PanelVSTValues
          Position = 1.000000000000000000
          Visible = True
          Index = 1
        end>
      MinPosition = 0.020000000000000000
      SplitterHoverColor = clHighlight
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 539
      DesignSize = (
        529
        278)
      object PanelVSTKeys: TFlatPanel
        Left = 0
        Top = 0
        Width = 175
        Height = 278
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Anchors = []
        Padding.Left = 2
        Padding.Top = 2
        Padding.Right = 2
        Padding.Bottom = 2
        BorderTop = 2
        BorderLeft = 2
        BorderRight = 2
        BorderBottom = 2
        Color = 13554645
        BorderColor = clBlack
        object VSTKeys: TVirtualStringTree
          Left = 2
          Top = 2
          Width = 171
          Height = 274
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BackGroundImageTransparent = True
          BorderStyle = bsNone
          Color = clWhite
          Colors.UnfocusedColor = clWindowText
          DefaultNodeHeight = 17
          Header.AutoSizeIndex = 0
          Header.DefaultHeight = 25
          Header.Height = 17
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoHeaderClickAutoSort]
          Header.SortColumn = 0
          Images = FormMain.ImageSystem
          PopupMenu = PopupKeys
          StateImages = FormMain.VirtualImageList
          TabOrder = 0
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toUseBlendedImages, toFullVertGridLines]
          TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
          OnCompareNodes = VSTKeysCompareNodes
          OnDblClick = VSTKeysDblClick
          OnFreeNode = VSTKeysFreeNode
          OnGetText = VSTKeysGetText
          OnGetImageIndex = VSTKeysGetImageIndex
          OnGetNodeDataSize = VSTKeysGetNodeDataSize
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coEditable, coStyleColor]
              Position = 0
              Text = 'Name'
              Width = 171
            end>
        end
      end
      object PanelVSTValues: TFlatPanel
        Left = 178
        Top = 0
        Width = 351
        Height = 278
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Anchors = []
        Padding.Left = 2
        Padding.Top = 2
        Padding.Right = 2
        Padding.Bottom = 2
        BorderTop = 2
        BorderLeft = 2
        BorderRight = 2
        BorderBottom = 2
        Color = 13554645
        BorderColor = clBlack
        object VSTValues: TVirtualStringTree
          Left = 2
          Top = 2
          Width = 347
          Height = 274
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BackGroundImageTransparent = True
          BorderStyle = bsNone
          Color = clWhite
          Colors.UnfocusedColor = clWindowText
          DefaultNodeHeight = 17
          Header.AutoSizeIndex = -1
          Header.DefaultHeight = 25
          Header.Height = 17
          Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
          Header.SortColumn = 0
          Images = FormMain.VirtualImageList
          PopupMenu = PopupValues
          TabOrder = 0
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toUseBlendedImages, toFullVertGridLines]
          TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
          OnCompareNodes = VSTValuesCompareNodes
          OnDblClick = VSTValuesDblClick
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
    end
    object PanelPath: TFlatPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 529
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 0
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Padding.Left = 1
      Padding.Top = 1
      Padding.Right = 1
      Padding.Bottom = 1
      BorderTop = 2
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
      Color = 13554645
      BorderColor = clBlack
      ExplicitWidth = 519
      object EditPath: TFlatEdit
        Left = 1
        Top = 1
        Width = 527
        Height = 19
        Align = alClient
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 0
        Status = cStatusNormal
        Validators = []
        ShowBorder = True
        ExplicitWidth = 517
      end
    end
  end
  object PopupKeys: TFlatPopupMenu
    OwnerDraw = True
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
    object RenameSelectedKey1: TMenuItem
      Caption = 'Rename Selected Key'
      OnClick = RenameSelectedKey1Click
    end
    object DeleteSelectedKey1: TMenuItem
      Caption = 'Delete Selected Key'
      OnClick = DeleteSelectedKey1Click
    end
  end
  object PopupValues: TFlatPopupMenu
    OwnerDraw = True
    OnPopup = PopupValuesPopup
    Left = 304
    Top = 151
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
    object RenameSelectedValue1: TMenuItem
      Caption = 'Rename Selected Value'
      OnClick = RenameSelectedValue1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object DeleteSelectedValue1: TMenuItem
      Caption = 'Delete Selected Value'
      OnClick = DeleteSelectedValue1Click
    end
  end
  object MainMenu: TFlatPopupMenu
    OwnerDraw = True
    Left = 366
    Top = 101
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
    object N6: TMenuItem
      Caption = '-'
    end
    object HideUnenumerableKeys1: TMenuItem
      Caption = 'Hide Unenumerable Keys'
      Checked = True
      OnClick = HideUnenumerableKeys1Click
    end
  end
end
