object FormParamsLab: TFormParamsLab
  Left = 222
  Top = 181
  HelpContext = 2
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Param'#232'tres du labyrinthe '#224' cr'#233'er'
  ClientHeight = 297
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelBord: TLabel
    Left = 40
    Top = 208
    Width = 145
    Height = 13
    Caption = 'Terrain ext'#233'rieur au labyrinthe :'
  end
  object BoutonOK: TBitBtn
    Left = 44
    Top = 240
    Width = 97
    Height = 41
    TabOrder = 0
    Kind = bkOK
  end
  object BoutonAnnuler: TBitBtn
    Left = 164
    Top = 240
    Width = 97
    Height = 41
    TabOrder = 1
    Kind = bkCancel
  end
  object ComboBord: TComboBox
    Left = 240
    Top = 200
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'Sortie'
      'Eau'
      'Herbe'
      'Murs'
      'Ciel'
      'Trous')
  end
  object BoutonAide: TBitBtn
    Left = 284
    Top = 240
    Width = 97
    Height = 41
    HelpContext = 2
    TabOrder = 3
    Kind = bkHelp
  end
  object GroupeType: TGroupBox
    Left = 32
    Top = 16
    Width = 361
    Height = 169
    Caption = 'Types des zones'
    TabOrder = 4
    object LabelTerrain1: TLabel
      Left = 32
      Top = 52
      Width = 90
      Height = 13
      Caption = 'Terrain par d'#233'faut :'
    end
    object LabelTerrain2: TLabel
      Left = 32
      Top = 80
      Width = 108
      Height = 13
      Caption = 'Terrain de quadrillage :'
    end
    object LabelTerrain3: TLabel
      Left = 32
      Top = 108
      Width = 95
      Height = 13
      Caption = 'Terrain de ceinture :'
    end
    object BoutonTypesIdentiques: TRadioButton
      Left = 16
      Top = 24
      Width = 153
      Height = 17
      Caption = 'Zones identiques'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = BoutonTypesIdentiquesClick
    end
    object BoutonTypesDifferents: TRadioButton
      Left = 16
      Top = 136
      Width = 177
      Height = 17
      Caption = 'Zones diff'#233'rentes'
      TabOrder = 1
      OnClick = BoutonTypesDifferentsClick
    end
    object ComboTerrain1: TComboBox
      Left = 208
      Top = 44
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'Herbe'
        'Eau'
        'Trous'
        'Murs'
        'Cases de fin')
    end
    object ComboTerrain2: TComboBox
      Left = 208
      Top = 76
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        '(Pas de quadrillage)'
        'Murs'
        'Eau'
        'Herbe'
        'Trous'
        'Cases de fin')
    end
    object ComboTerrain3: TComboBox
      Left = 208
      Top = 108
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      Items.Strings = (
        '(Pas de ceinture)'
        'Murs'
        'Eau'
        'Herbe'
        'Trous'
        'Cases de fin')
    end
  end
end
