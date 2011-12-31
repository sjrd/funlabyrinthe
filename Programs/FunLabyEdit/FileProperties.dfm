object FormFileProperties: TFormFileProperties
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Propri'#233't'#233's du fichier'
  ClientHeight = 577
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 17
  object LabelTitle: TLabel
    Left = 21
    Top = 21
    Width = 36
    Height = 17
    Caption = 'Titre :'
  end
  object LabelDescription: TLabel
    Left = 21
    Top = 84
    Width = 77
    Height = 17
    Caption = 'Description :'
  end
  object LabelDifficulty: TLabel
    Left = 21
    Top = 324
    Width = 60
    Height = 17
    Caption = 'Difficult'#233' :'
  end
  object LabelAuthor: TLabel
    Left = 21
    Top = 387
    Width = 50
    Height = 17
    Caption = 'Auteur :'
  end
  object LabelKind: TLabel
    Left = 21
    Top = 262
    Width = 45
    Height = 17
    Caption = 'Genre :'
  end
  object LabelVersion: TLabel
    Left = 21
    Top = 449
    Width = 53
    Height = 17
    Caption = 'Version :'
  end
  object EditTitle: TEdit
    Left = 21
    Top = 42
    Width = 388
    Height = 25
    TabOrder = 0
  end
  object EditDescription: TMemo
    Left = 21
    Top = 105
    Width = 388
    Height = 137
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object EditDifficulty: TEdit
    Left = 21
    Top = 345
    Width = 388
    Height = 25
    TabOrder = 3
  end
  object EditAuthor: TEdit
    Left = 21
    Top = 408
    Width = 388
    Height = 25
    TabOrder = 4
  end
  object ButtonOK: TButton
    Left = 73
    Top = 523
    Width = 127
    Height = 33
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 230
    Top = 523
    Width = 127
    Height = 33
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 7
  end
  object EditKind: TEdit
    Left = 21
    Top = 282
    Width = 388
    Height = 25
    TabOrder = 2
  end
  object EditVersion: TEdit
    Left = 21
    Top = 471
    Width = 388
    Height = 25
    TabOrder = 5
  end
end
