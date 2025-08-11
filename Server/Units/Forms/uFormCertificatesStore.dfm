object FormCertificatesStore: TFormCertificatesStore
  Left = 0
  Top = 0
  Caption = 'Certificates Store'
  ClientHeight = 316
  ClientWidth = 626
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 626
    Height = 316
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
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Country (C)'
        Width = 90
      end
      item
        Position = 1
        Text = 'Organization Name (O)'
        Width = 130
      end
      item
        Position = 2
        Text = 'Common Name (CN)'
        Width = 130
      end
      item
        Position = 3
        Text = 'Fingerprint (Sha512)'
        Width = 250
      end>
  end
  object MainMenu: TMainMenu
    Left = 280
    Top = 152
    object File1: TMenuItem
      Caption = 'Certificate'
      object GeneratenewCertificate1: TMenuItem
        Caption = 'Generate New'
        OnClick = GeneratenewCertificate1Click
      end
      object ImportRecommended1: TMenuItem
        Caption = 'Import (Recommended)'
      end
    end
  end
end
