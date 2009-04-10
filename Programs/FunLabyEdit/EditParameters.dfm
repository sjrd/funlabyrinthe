object FormParameters: TFormParameters
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Attributs d'#39'un joueur'
  ClientHeight = 289
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelParams: TLabel
    Left = 16
    Top = 16
    Width = 98
    Height = 13
    Caption = 'Attributs du joueur :'
  end
  object ValueListParams: TValueListEditor
    Left = 16
    Top = 32
    Width = 393
    Height = 209
    KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
    TabOrder = 0
    TitleCaptions.Strings = (
      'Attribut'
      'Valeur')
    OnValidate = ValueListParamsValidate
    ColWidths = (
      150
      237)
  end
  object ButtonOK: TButton
    Left = 232
    Top = 248
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 328
    Top = 248
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 2
  end
end
