object FormParamsLabMax: TFormParamsLabMax
  Left = 224
  Top = 165
  HelpContext = 3
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Param'#232'tres sp'#233'ciaux'
  ClientHeight = 279
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PlanEtage: TImage
    Left = 0
    Top = 0
    Width = 210
    Height = 210
    OnMouseDown = PlanEtageMouseDown
  end
  object LabelTerrain1: TLabel
    Left = 222
    Top = 8
    Width = 90
    Height = 13
    Caption = 'Terrain par d'#233'faut :'
  end
  object LabelTerrain2: TLabel
    Left = 222
    Top = 88
    Width = 108
    Height = 13
    Caption = 'Terrain de quadrillage :'
  end
  object LabelTerrain3: TLabel
    Left = 222
    Top = 168
    Width = 110
    Height = 13
    Caption = 'Terrain de de ceinture :'
  end
  object BoutonOK: TBitBtn
    Left = 36
    Top = 224
    Width = 97
    Height = 41
    TabOrder = 0
    Kind = bkOK
  end
  object BoutonAnnuler: TBitBtn
    Left = 156
    Top = 224
    Width = 97
    Height = 41
    TabOrder = 1
    Kind = bkCancel
  end
  object ComboTerrain1: TComboBox
    Left = 222
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = ComboTerrain1Change
    Items.Strings = (
      'Herbe'
      'Eau'
      'Trous'
      'Murs'
      'Sortie')
  end
  object ComboTerrain2: TComboBox
    Left = 222
    Top = 104
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = ComboTerrain2Change
    Items.Strings = (
      '(Pas de quadrillage)'
      'Murs'
      'Eau'
      'Herbe'
      'Trous'
      'Sortie')
  end
  object BoutonAide: TBitBtn
    Left = 276
    Top = 224
    Width = 97
    Height = 41
    HelpContext = 3
    TabOrder = 4
    Kind = bkHelp
  end
  object ComboTerrain3: TComboBox
    Left = 222
    Top = 184
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = ComboTerrain3Change
    Items.Strings = (
      '(Pas de ceinture)'
      'Murs'
      'Eau'
      'Herbe'
      'Trous'
      'Sortie')
  end
end
