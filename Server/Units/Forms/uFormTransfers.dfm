object FormTransfers: TFormTransfers
  Left = 0
  Top = 0
  Caption = 'Transfers'
  ClientHeight = 269
  ClientWidth = 694
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 694
    Height = 269
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
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.SortColumn = 0
    Images = FormMain.ImageSystem
    PopupMenu = PopupMenu
    StateImages = FormMain.VirtualImageList
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    OnInitNode = VSTInitNode
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Source File'
        Width = 120
      end
      item
        Position = 1
        Text = 'Destination File'
        Width = 120
      end
      item
        Position = 2
        Text = 'Direction'
        Width = 100
      end
      item
        Position = 3
        Text = 'File Size'
        Width = 100
      end
      item
        Position = 4
        Text = 'State'
        Width = 90
      end
      item
        Position = 5
        Text = 'Context'
        Width = 100
      end
      item
        Position = 6
        Text = 'Description'
        Width = 250
      end>
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 232
    Top = 96
    object DownloadaFile1: TMenuItem
      Caption = 'Download a File'
      OnClick = DownloadaFile1Click
    end
    object UploadaFile1: TMenuItem
      Caption = 'Upload a File'
      OnClick = UploadaFile1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CancelTransfer1: TMenuItem
      Caption = 'Cancel Transfer'
      OnClick = CancelTransfer1Click
    end
  end
  object OpenDialog: TOpenDialog
    Left = 320
    Top = 88
  end
end
