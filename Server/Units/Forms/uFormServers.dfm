object FormServers: TFormServers
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Servers / Listeners'
  ClientHeight = 208
  ClientWidth = 637
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 14
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 637
    Height = 208
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
    Header.MainColumn = 1
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 0
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
    Columns = <
      item
        Position = 0
        Text = 'Address (Interface)'
        Width = 200
      end
      item
        Position = 1
        Text = 'Port'
        Width = 90
      end
      item
        Position = 2
        Text = 'IP Version'
        Width = 100
      end
      item
        Position = 3
        Text = 'Status'
        Width = 120
      end
      item
        Position = 4
        Text = 'Auto Start'
        Width = 110
      end
      item
        Position = 5
        Text = 'Server Certificate'
        Width = 250
      end
      item
        Position = 6
        Text = 'Status Message'
        Width = 200
      end>
  end
  object PopupMenu: TFlatPopupMenu
    OwnerDraw = True
    OnPopup = PopupMenuPopup
    Left = 240
    Top = 72
    object Start1: TMenuItem
      Caption = 'Start'
      OnClick = Start1Click
    end
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object AutoStart1: TMenuItem
      AutoCheck = True
      Caption = 'Auto Start'
      Checked = True
      OnClick = AutoStart1Click
    end
    object Certificate1: TMenuItem
      Caption = 'Certificate'
      OnClick = Certificate1Click
    end
  end
  object MainMenu: TFlatPopupMenu
    OwnerDraw = True
    Left = 336
    Top = 72
    object New1: TMenuItem
      Caption = 'New'
      ShortCut = 16462
      OnClick = New1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Certificates1: TMenuItem
      Caption = 'Certificates'
      OnClick = Certificates1Click
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
    MenuDropDown = MainMenu
    ShowBorder = True
    BorderStyle = bsSizeable
    Background = 13554645
    Border = clBlack
    Caption = clBlack
    Left = 144
    Top = 80
  end
end
