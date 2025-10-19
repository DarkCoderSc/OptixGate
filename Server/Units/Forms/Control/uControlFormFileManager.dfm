object ControlFormFileManager: TControlFormFileManager
  Left = 0
  Top = 0
  Caption = 'File Manager'
  ClientHeight = 437
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  TextHeight = 15
  object EditPath: TEdit
    AlignWithMargins = True
    Left = 4
    Top = 30
    Width = 648
    Height = 23
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    ReadOnly = True
    TabOrder = 0
    ExplicitWidth = 638
  end
  object PanelActions: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 648
    Height = 30
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 638
    object ButtonHome: TSpeedButton
      AlignWithMargins = True
      Left = 0
      Top = 4
      Width = 30
      Height = 22
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 51
      ImageName = 'home'
      Images = FormMain.VirtualImageList
      OnClick = ButtonHomeClick
      ExplicitLeft = 4
      ExplicitTop = 2
    end
    object ButtonRefresh: TSpeedButton
      AlignWithMargins = True
      Left = 102
      Top = 4
      Width = 30
      Height = 22
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 52
      ImageName = 'symbol-refresh'
      Images = FormMain.VirtualImageList
      OnClick = ButtonRefreshClick
      ExplicitLeft = 42
      ExplicitTop = 2
    end
    object ButtonUpload: TSpeedButton
      AlignWithMargins = True
      Left = 170
      Top = 4
      Width = 30
      Height = 22
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 50
      ImageName = 'folder-open-filled-arrow-up-filled'
      Images = FormMain.VirtualImageList
      OnClick = ButtonUploadClick
      ExplicitLeft = 118
      ExplicitTop = 8
    end
    object ButtonOptions: TSpeedButton
      AlignWithMargins = True
      Left = 204
      Top = 4
      Width = 30
      Height = 22
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 53
      ImageName = 'gear-filled'
      Images = FormMain.VirtualImageList
      OnClick = ButtonOptionsClick
      ExplicitLeft = 104
      ExplicitTop = 2
    end
    object LabelAccess: TLabel
      AlignWithMargins = True
      Left = 619
      Top = 10
      Width = 21
      Height = 15
      Margins.Left = 8
      Margins.Top = 10
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alRight
      Caption = '___'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object ButtonBack: TSpeedButton
      AlignWithMargins = True
      Left = 34
      Top = 4
      Width = 30
      Height = 22
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 60
      ImageName = 'button-arrow-left'
      Images = FormMain.VirtualImageList
      SelectedImageName = '-1'
      OnClick = ButtonBackClick
    end
    object ButtonForward: TSpeedButton
      AlignWithMargins = True
      Left = 68
      Top = 4
      Width = 30
      Height = 22
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 61
      ImageName = 'button-arrow-right'
      Images = FormMain.VirtualImageList
      OnClick = ButtonForwardClick
    end
    object ButtonGoTo: TSpeedButton
      AlignWithMargins = True
      Left = 136
      Top = 4
      Width = 30
      Height = 22
      Margins.Left = 2
      Margins.Top = 4
      Margins.Right = 2
      Margins.Bottom = 4
      Align = alLeft
      ImageIndex = 67
      ImageName = 'control-edit'
      Images = FormMain.VirtualImageList
      OnClick = ButtonGoToClick
      ExplicitLeft = 122
    end
  end
  object MultiPanel: TOMultiPanel
    Left = 0
    Top = 57
    Width = 656
    Height = 380
    PanelCollection = <
      item
        Control = VSTFolders
        Position = 0.300000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = VSTFiles
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 666
    DesignSize = (
      656
      380)
    object VSTFiles: TVirtualStringTree
      Left = 200
      Top = 0
      Width = 456
      Height = 380
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BackGroundImageTransparent = True
      BorderStyle = bsNone
      Color = clWhite
      Colors.UnfocusedColor = clWindowText
      DefaultNodeHeight = 19
      Header.AutoSizeIndex = -1
      Header.DefaultHeight = 25
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.SortColumn = 0
      Images = FormMain.ImageSystem
      PopupMenu = PopupMenu
      StateImages = FormMain.VirtualImageList
      TabOrder = 1
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
      OnBeforeCellPaint = VSTFilesBeforeCellPaint
      OnChange = VSTFilesChange
      OnCompareNodes = VSTFilesCompareNodes
      OnDblClick = VSTFilesDblClick
      OnFocusChanged = VSTFilesFocusChanged
      OnFreeNode = VSTFilesFreeNode
      OnGetText = VSTFilesGetText
      OnGetImageIndex = VSTFilesGetImageIndex
      OnGetNodeDataSize = VSTFilesGetNodeDataSize
      OnMouseDown = VSTFilesMouseDown
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
    object VSTFolders: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 197
      Height = 380
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
      PopupMenu = PopupFoldersTree
      StateImages = FormMain.VirtualImageList
      TabOrder = 0
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
      OnChange = VSTFoldersChange
      OnCompareNodes = VSTFoldersCompareNodes
      OnDblClick = VSTFoldersDblClick
      OnFocusChanged = VSTFoldersFocusChanged
      OnFreeNode = VSTFoldersFreeNode
      OnGetText = VSTFoldersGetText
      OnGetImageIndex = VSTFoldersGetImageIndex
      OnGetNodeDataSize = VSTFoldersGetNodeDataSize
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          Position = 0
          Text = 'Name'
          Width = 197
        end>
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 240
    Top = 128
    object DownloadFile1: TMenuItem
      Caption = 'Download File'
      OnClick = DownloadFile1Click
    end
    object UploadToFolder1: TMenuItem
      Caption = 'Upload To Folder'
      OnClick = UploadToFolder1Click
    end
  end
  object PopupMenuOptions: TPopupMenu
    Left = 384
    Top = 128
    object ColoredFoldersAccessView1: TMenuItem
      AutoCheck = True
      Caption = 'Colored Folders (Access View)'
      OnClick = ColoredFoldersAccessView1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ShowFolderTree1: TMenuItem
      AutoCheck = True
      Caption = 'Show Folder Tree'
      Checked = True
      OnClick = ShowFolderTree1Click
    end
  end
  object PopupFoldersTree: TPopupMenu
    Left = 256
    Top = 241
    object FullExpand1: TMenuItem
      Caption = 'Full Expand'
      OnClick = FullExpand1Click
    end
    object FullCollapse1: TMenuItem
      Caption = 'Full Collapse'
      OnClick = FullCollapse1Click
    end
  end
end
