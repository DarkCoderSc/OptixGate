object FrameRemoteShellInstance: TFrameRemoteShellInstance
  Left = 0
  Top = 0
  Width = 816
  Height = 417
  Align = alClient
  TabOrder = 0
  object OMultiPanel: TOMultiPanel
    Left = 0
    Top = 0
    Width = 816
    Height = 417
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = Shell
        Position = 0.920000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = PanelCommand
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    object Shell: TRichEdit
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 816
      Height = 384
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      EnableURLs = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      StyleElements = [seClient, seBorder]
      OnLinkClick = ShellLinkClick
    end
    object PanelCommand: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 387
      Width = 816
      Height = 30
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      BevelOuter = bvNone
      TabOrder = 1
      object Command: TRichEdit
        Left = 0
        Top = 0
        Width = 756
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        EnableURLs = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        OnChange = CommandChange
        OnKeyUp = CommandKeyUp
        ExplicitHeight = 39
      end
      object ButtonSend: TButton
        AlignWithMargins = True
        Left = 760
        Top = 0
        Width = 56
        Height = 30
        Margins.Left = 4
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        Default = True
        Enabled = False
        ImageAlignment = iaCenter
        ImageIndex = 54
        ImageName = 'message'
        Images = FormMain.VirtualImageList
        TabOrder = 1
        OnClick = ButtonSendClick
        ExplicitHeight = 39
      end
    end
  end
end
