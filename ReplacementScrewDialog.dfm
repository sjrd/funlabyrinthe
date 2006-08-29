object FormNewCase: TFormNewCase
  Left = 241
  Top = 176
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Nouvelle case de remplacement'
  ClientHeight = 249
  ClientWidth = 289
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
  object ImageHerbe: TImage
    Left = 25
    Top = 8
    Width = 30
    Height = 30
  end
  object ImageEau: TImage
    Left = 81
    Top = 8
    Width = 30
    Height = 30
  end
  object ImageMur: TImage
    Left = 137
    Top = 8
    Width = 30
    Height = 30
  end
  object ImageTrou: TImage
    Left = 193
    Top = 8
    Width = 30
    Height = 30
  end
  object ImageBlocArgent: TImage
    Left = 249
    Top = 8
    Width = 30
    Height = 30
  end
  object ImageBlocOr: TImage
    Left = 25
    Top = 48
    Width = 30
    Height = 30
  end
  object ImageNord: TImage
    Left = 25
    Top = 88
    Width = 30
    Height = 30
  end
  object ImageEst: TImage
    Left = 81
    Top = 88
    Width = 30
    Height = 30
  end
  object ImageSud: TImage
    Left = 137
    Top = 88
    Width = 30
    Height = 30
  end
  object ImageOuest: TImage
    Left = 193
    Top = 88
    Width = 30
    Height = 30
  end
  object ImageTeleporteur: TImage
    Left = 25
    Top = 128
    Width = 30
    Height = 30
  end
  object ImageBouton: TImage
    Left = 81
    Top = 128
    Width = 30
    Height = 30
  end
  object ImageBoutonEnfonce: TImage
    Left = 137
    Top = 128
    Width = 30
    Height = 30
  end
  object ImageTresor: TImage
    Left = 193
    Top = 128
    Width = 30
    Height = 30
  end
  object ImageBouee: TImage
    Left = 249
    Top = 128
    Width = 30
    Height = 30
  end
  object ImagePlanche: TImage
    Left = 81
    Top = 168
    Width = 30
    Height = 30
  end
  object ImageCleArgent: TImage
    Left = 137
    Top = 168
    Width = 30
    Height = 30
  end
  object ImageCleOr: TImage
    Left = 193
    Top = 168
    Width = 30
    Height = 30
  end
  object ImageFauxMur: TImage
    Left = 81
    Top = 48
    Width = 30
    Height = 30
  end
  object ImageBarque: TImage
    Left = 137
    Top = 48
    Width = 30
    Height = 30
  end
  object ImageCarrefour: TImage
    Left = 249
    Top = 88
    Width = 30
    Height = 30
  end
  object ImageMoulinDirect: TImage
    Left = 193
    Top = 48
    Width = 30
    Height = 30
  end
  object ImageMoulinIndirect: TImage
    Left = 249
    Top = 48
    Width = 30
    Height = 30
  end
  object BoutonHerbe: TRadioButton
    Tag = 48
    Left = 9
    Top = 16
    Width = 14
    Height = 14
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object BoutonEau: TRadioButton
    Tag = 49
    Left = 65
    Top = 16
    Width = 14
    Height = 14
    TabOrder = 1
  end
  object BoutonMur: TRadioButton
    Tag = 50
    Left = 121
    Top = 16
    Width = 14
    Height = 14
    TabOrder = 2
  end
  object BoutonTrou: TRadioButton
    Tag = 51
    Left = 177
    Top = 16
    Width = 14
    Height = 14
    TabOrder = 3
  end
  object BoutonBlocArgent: TRadioButton
    Tag = 52
    Left = 233
    Top = 16
    Width = 14
    Height = 14
    TabOrder = 4
  end
  object BoutonBlocOr: TRadioButton
    Tag = 53
    Left = 9
    Top = 56
    Width = 14
    Height = 14
    TabOrder = 5
  end
  object BoutonNord: TRadioButton
    Tag = 54
    Left = 9
    Top = 96
    Width = 14
    Height = 14
    TabOrder = 6
  end
  object BoutonEst: TRadioButton
    Tag = 55
    Left = 65
    Top = 96
    Width = 14
    Height = 14
    TabOrder = 7
  end
  object BoutonSud: TRadioButton
    Tag = 56
    Left = 121
    Top = 96
    Width = 14
    Height = 14
    TabOrder = 8
  end
  object BoutonOuest: TRadioButton
    Tag = 57
    Left = 177
    Top = 96
    Width = 14
    Height = 14
    TabOrder = 9
  end
  object BoutonTeleporteur: TRadioButton
    Tag = 3
    Left = 9
    Top = 136
    Width = 14
    Height = 14
    TabOrder = 10
  end
  object BoutonBouton: TRadioButton
    Tag = 4
    Left = 65
    Top = 136
    Width = 14
    Height = 14
    TabOrder = 11
  end
  object BoutonBoutonEnfonce: TRadioButton
    Tag = 64
    Left = 121
    Top = 136
    Width = 14
    Height = 14
    TabOrder = 12
  end
  object BoutonTresor: TRadioButton
    Tag = 59
    Left = 177
    Top = 136
    Width = 14
    Height = 14
    TabOrder = 13
  end
  object BoutonBouee: TRadioButton
    Tag = 67
    Left = 233
    Top = 136
    Width = 14
    Height = 14
    TabOrder = 14
  end
  object BoutonPlanche: TRadioButton
    Tag = 68
    Left = 65
    Top = 176
    Width = 14
    Height = 14
    TabOrder = 15
  end
  object BoutonCleArgent: TRadioButton
    Tag = 69
    Left = 121
    Top = 176
    Width = 14
    Height = 14
    TabOrder = 16
  end
  object BoutonCleOr: TRadioButton
    Tag = 70
    Left = 177
    Top = 176
    Width = 14
    Height = 14
    TabOrder = 17
  end
  object BoutonOK: TBitBtn
    Left = 56
    Top = 208
    Width = 81
    Height = 33
    TabOrder = 18
    Kind = bkOK
  end
  object BoutonAnnuler: TBitBtn
    Left = 152
    Top = 208
    Width = 81
    Height = 33
    TabOrder = 19
    Kind = bkCancel
  end
  object BoutonFauxMur: TRadioButton
    Tag = 63
    Left = 65
    Top = 56
    Width = 14
    Height = 14
    TabOrder = 20
  end
  object BoutonBarque: TRadioButton
    Tag = 5
    Left = 121
    Top = 56
    Width = 14
    Height = 14
    TabOrder = 21
  end
  object BoutonCarrefour: TRadioButton
    Tag = 58
    Left = 233
    Top = 96
    Width = 14
    Height = 14
    Caption = 'BoutonCarrefour'
    TabOrder = 22
  end
  object BoutonMoulinDirect: TRadioButton
    Tag = 224
    Left = 177
    Top = 56
    Width = 14
    Height = 14
    Caption = 'BoutonMoulinDirect'
    TabOrder = 23
  end
  object BoutonMoulinIndirect: TRadioButton
    Tag = 225
    Left = 233
    Top = 56
    Width = 14
    Height = 14
    Caption = 'BoutonMoulinIndirect'
    TabOrder = 24
  end
end
