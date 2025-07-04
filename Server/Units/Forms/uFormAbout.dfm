object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 636
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object ImageLogo: TVirtualImage
    Left = 128
    Top = 8
    Width = 105
    Height = 105
    ImageCollection = FormMain.ImageCollection
    ImageWidth = 0
    ImageHeight = 0
    ImageIndex = 4
    ImageName = 'optix'
  end
  object LabelName: TLabel
    Left = 152
    Top = 119
    Width = 81
    Height = 21
    Caption = 'Optix Gate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelDarkCoderSc: TLabel
    Left = 96
    Top = 144
    Width = 202
    Height = 15
    Caption = 'DarkCoderSc (Jean-Pierre LESUEUR)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelDisclaimer: TLabel
    Left = 8
    Top = 184
    Width = 457
    Height = 345
    Caption = 
      'This software is provided for educational purposes and general i' +
      'nformational use only. While it may be applicable in real-world ' +
      'scenarios and can be used in legally compliant contexts, the sof' +
      'tware is provided "as is" without any warranties, express or imp' +
      'lied, including but not limited to warranties of merchantability' +
      ', fitness for a particular purpose, or non-infringement.'#13#10#13#10'The ' +
      'developer makes no representations or guarantees regarding the a' +
      'ccuracy, reliability, suitability, or effectiveness of the softw' +
      'are when used in any specific application or environment, includ' +
      'ing but not limited to professional, commercial, or legal contex' +
      'ts.'#13#10#13#10'By using this software, you acknowledge and agree that:'#13#10 +
      #13#10'- You are solely responsible for how the software is used and ' +
      'for ensuring its suitability for your specific needs.'#13#10'- You ass' +
      'ume all risks associated with its use, including any direct, ind' +
      'irect, incidental, or consequential damages that may result.'#13#10'- ' +
      'The developer shall not be held liable for any loss, damage, or ' +
      'legal claims arising from your use or misuse of the software, wh' +
      'ether in an educational, personal, or professional capacity.'#13#10#13#10 +
      'Use of this software constitutes acceptance of this disclaimer. ' +
      'If you do not agree to these terms, you should not use the softw' +
      'are.'
    WordWrap = True
  end
  object ButtonClose: TButton
    Left = 152
    Top = 552
    Width = 121
    Height = 33
    Caption = 'OK.'
    TabOrder = 0
    OnClick = ButtonCloseClick
  end
end
