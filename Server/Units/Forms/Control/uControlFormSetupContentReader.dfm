object ControlFormSetupContentReader: TControlFormSetupContentReader
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New Content Reader'
  ClientHeight = 218
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PanelClient: TPanel
    Left = 50
    Top = 0
    Width = 346
    Height = 177
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    ExplicitWidth = 336
    ExplicitHeight = 145
    object Label1: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 330
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'File Path:'
      ExplicitWidth = 48
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 54
      Width = 330
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Page Size:'
      ExplicitWidth = 52
    end
    object EditPath: TEdit
      Left = 8
      Top = 23
      Width = 330
      Height = 23
      Align = alTop
      TabOrder = 0
      Text = 'c:\temp\cports.exe'
      ExplicitWidth = 320
    end
    object SpinPageSize: TSpinEdit
      Left = 8
      Top = 69
      Width = 330
      Height = 24
      Align = alTop
      MaxValue = 2147483647
      MinValue = 128
      TabOrder = 1
      Value = 1024
      ExplicitWidth = 320
    end
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 50
    Height = 177
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitHeight = 145
    object Image: TVirtualImage
      Left = 8
      Top = 8
      Width = 34
      Height = 161
      Align = alClient
      Center = True
      ImageCollection = FormMain.ImageCollectionDark
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 70
      ImageName = 'check-source-code'
      ExplicitWidth = 32
      ExplicitHeight = 32
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 177
    Width = 396
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 145
    ExplicitWidth = 386
    object ButtonStart: TButton
      Left = 152
      Top = 6
      Width = 83
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = ButtonStartClick
    end
    object ButtonCancel: TButton
      Left = 63
      Top = 6
      Width = 83
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
end
