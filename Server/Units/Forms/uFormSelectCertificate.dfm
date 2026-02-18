object FormSelectCertificate: TFormSelectCertificate
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select Certificate'
  ClientHeight = 110
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 69
    Width = 398
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 37
    ExplicitWidth = 388
    object ButtonValidate: TButton
      Left = 152
      Top = 6
      Width = 83
      Height = 25
      Caption = 'Validate'
      TabOrder = 0
      OnClick = ButtonValidateClick
    end
    object ButtonCancel: TButton
      Left = 63
      Top = 6
      Width = 83
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 398
    Height = 69
    Align = alClient
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    ExplicitWidth = 388
    ExplicitHeight = 37
    object LabelCertificate: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 17
      Width = 380
      Height = 15
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Server Certificate:'
      ExplicitWidth = 92
    end
    object ComboCertificate: TComboBox
      Left = 9
      Top = 32
      Width = 380
      Height = 23
      Align = alTop
      Style = csDropDownList
      TabOrder = 0
      ExplicitWidth = 370
    end
  end
end
