object ControlFormFileManager: TControlFormFileManager
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
  OnDestroy = FormDestroy
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 57
    Width = 477
    Height = 313
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
    Header.SortColumn = 0
    Images = FormMain.ImageSystem
    PopupMenu = PopupMenu
    StateImages = FormMain.VirtualImageList
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnDblClick = VSTDblClick
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
    AlignWithMargins = True
    Left = 4
    Top = 30
    Width = 469
    Height = 23
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    ReadOnly = True
    TabOrder = 1
    ExplicitWidth = 459
  end
  object PanelActions: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 469
    Height = 30
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 459
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
      Visible = False
      OnClick = ButtonRefreshClick
      ExplicitLeft = 42
      ExplicitTop = 2
    end
    object ButtonUpload: TSpeedButton
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
      ImageIndex = 50
      ImageName = 'folder-open-filled-arrow-up-filled'
      Images = FormMain.VirtualImageList
      Visible = False
      OnClick = ButtonUploadClick
      ExplicitLeft = 118
      ExplicitTop = 8
    end
    object ButtonOptions: TSpeedButton
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
      ImageIndex = 53
      ImageName = 'gear-filled'
      Images = FormMain.VirtualImageList
      OnClick = ButtonOptionsClick
      ExplicitLeft = 104
      ExplicitTop = 2
    end
    object LabelAccess: TLabel
      AlignWithMargins = True
      Left = 440
      Top = 10
      Width = 21
      Height = 20
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
      ExplicitHeight = 15
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
    Left = 104
    Top = 136
    object ColoredFoldersAccessView1: TMenuItem
      AutoCheck = True
      Caption = 'Colored Folders (Access View)'
      Checked = True
      OnClick = ColoredFoldersAccessView1Click
    end
  end
end
