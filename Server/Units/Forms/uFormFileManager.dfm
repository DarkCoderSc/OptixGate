object FormFileManager: TFormFileManager
  Left = 0
  Top = 0
  Caption = 'File Manager'
  ClientHeight = 370
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 477
    Height = 347
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
    Images = FormMain.ImageSystem
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
        Text = 'Name'
        Width = 150
      end
      item
        Position = 1
        Text = 'Type'
        Width = 100
      end
      item
        Position = 2
        Text = 'Size'
        Width = 100
      end
      item
        Position = 3
        Text = 'Access Rights'
        Width = 100
      end
      item
        Position = 4
        Text = 'DACL (SSDL)'
        Width = 200
      end
      item
        Position = 5
        Text = 'Creation Date'
        Width = 150
      end
      item
        Position = 6
        Text = 'Last Modified'
        Width = 150
      end
      item
        Position = 7
        Text = 'Last Access'
        Width = 150
      end>
  end
  object EditPath: TEdit
    Left = 0
    Top = 347
    Width = 477
    Height = 23
    Align = alBottom
    ReadOnly = True
    TabOrder = 1
    ExplicitTop = 315
    ExplicitWidth = 467
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 240
    Top = 128
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ShowDrives1: TMenuItem
      Caption = 'Show Drives (Home)'
      OnClick = ShowDrives1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object ColoredFoldersAccessView1: TMenuItem
        AutoCheck = True
        Caption = 'Colored Folders (Access View)'
        Checked = True
        OnClick = ColoredFoldersAccessView1Click
      end
    end
  end
end
