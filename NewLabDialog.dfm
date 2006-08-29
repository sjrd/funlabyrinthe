object FormNouveau: TFormNouveau
  Left = 269
  Top = 169
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Nouveau labyrinthe'
  ClientHeight = 225
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelColonnes: TLabel
    Left = 36
    Top = 20
    Width = 102
    Height = 13
    Caption = 'Colonnes (en zones) :'
  end
  object LabelLignes: TLabel
    Left = 36
    Top = 52
    Width = 89
    Height = 13
    Caption = 'Lignes (en zones) :'
  end
  object LabelEtages: TLabel
    Left = 36
    Top = 84
    Width = 92
    Height = 13
    Caption = 'Nombre d'#39#233'tage(s) :'
  end
  object EditColonnes: TSpinEdit
    Left = 164
    Top = 12
    Width = 81
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 0
    Value = 1
  end
  object EditLignes: TSpinEdit
    Left = 164
    Top = 44
    Width = 81
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object EditEtages: TSpinEdit
    Left = 164
    Top = 76
    Width = 81
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 2
    Value = 1
  end
  object BoutonOK: TBitBtn
    Left = 56
    Top = 180
    Width = 81
    Height = 33
    TabOrder = 3
    Kind = bkOK
  end
  object BoutonAnnuler: TBitBtn
    Left = 144
    Top = 180
    Width = 81
    Height = 33
    TabOrder = 4
    Kind = bkCancel
  end
  object GroupeTypeLab: TRadioGroup
    Left = 32
    Top = 108
    Width = 217
    Height = 57
    Caption = 'Type de labyrinthe'
    ItemIndex = 0
    Items.Strings = (
      'Normal'
      'Personnalis'#233)
    TabOrder = 5
  end
end
