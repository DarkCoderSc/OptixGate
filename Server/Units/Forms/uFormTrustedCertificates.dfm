object FormTrustedCertificates: TFormTrustedCertificates
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Trusted Certificates'
  ClientHeight = 151
  ClientWidth = 363
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
  TextHeight = 14
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 363
    Height = 151
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BackGroundImageTransparent = True
    BorderStyle = bsNone
    Color = clWhite
    Colors.UnfocusedColor = clWindowText
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 25
    Header.Height = 18
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 0
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnCompareNodes = VSTCompareNodes
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitWidth = 424
    ExplicitHeight = 218
    Columns = <
      item
        Position = 0
        Text = 'Fingerprint'
        Width = 363
      end>
  end
  object PopupMenu: TFlatPopupMenu
    OwnerDraw = True
    OnPopup = PopupMenuPopup
    Left = 248
    Top = 48
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
  end
  object MainMenu: TFlatPopupMenu
    OwnerDraw = True
    Left = 152
    Top = 48
    object AddTrustedCertificate1: TMenuItem
      Caption = 'Add Trusted Certificate'
      OnClick = AddTrustedCertificate1Click
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
    Left = 56
    Top = 48
  end
end
