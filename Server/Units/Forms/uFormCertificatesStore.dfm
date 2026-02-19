object FormCertificatesStore: TFormCertificatesStore
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Certificates Store'
  ClientHeight = 169
  ClientWidth = 501
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
    Width = 501
    Height = 169
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
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 3
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toSelectNextNodeOnRemoval]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnCompareNodes = VSTCompareNodes
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
  object OD: TOpenDialog
    Left = 320
    Top = 80
  end
  object SD: TSaveDialog
    DefaultExt = 'pem'
    Left = 392
    Top = 96
  end
  object PopupMenu: TFlatPopupMenu
    OwnerDraw = True
    OnPopup = PopupMenuPopup
    Left = 104
    Top = 96
    object ExportCertificate1: TMenuItem
      Caption = 'Export Certificate'
      OnClick = ExportCertificate1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ExportPublicKey1: TMenuItem
      Caption = 'Export Public Key'
      OnClick = ExportPublicKey1Click
    end
    object ExportPrivateKey1: TMenuItem
      Caption = 'Export Private Key'
      OnClick = ExportPrivateKey1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CopySelectedFingerprint1: TMenuItem
      Caption = 'Copy Selected Fingerprint'
      OnClick = CopySelectedFingerprint1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object RemoveCertificate1: TMenuItem
      Caption = 'Remove Certificate'
      OnClick = RemoveCertificate1Click
    end
  end
  object MainMenu: TFlatPopupMenu
    OwnerDraw = True
    Left = 224
    Top = 80
    object GenerateNew1: TMenuItem
      Caption = 'Generate New'
      ShortCut = 16462
      OnClick = GenerateNew1Click
    end
    object Import1: TMenuItem
      Caption = 'Import'
      ShortCut = 16457
      OnClick = Import1Click
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
    Left = 464
    Top = 104
  end
end
