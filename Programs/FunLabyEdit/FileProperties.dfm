object FormFileProperties: TFormFileProperties
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Propri'#233't'#233's du fichier'
  ClientHeight = 385
  ClientWidth = 329
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
  object LabelTitle: TLabel
    Left = 16
    Top = 16
    Width = 29
    Height = 13
    Caption = 'Titre :'
  end
  object LabelDescription: TLabel
    Left = 16
    Top = 64
    Width = 60
    Height = 13
    Caption = 'Description :'
  end
  object LabelDifficulty: TLabel
    Left = 16
    Top = 248
    Width = 49
    Height = 13
    Caption = 'Difficult'#233' :'
  end
  object LabelAuthor: TLabel
    Left = 16
    Top = 296
    Width = 40
    Height = 13
    Caption = 'Auteur :'
  end
  object LabelKind: TLabel
    Left = 16
    Top = 200
    Width = 36
    Height = 13
    Caption = 'Genre :'
  end
  object EditTitle: TEdit
    Left = 16
    Top = 32
    Width = 297
    Height = 21
    TabOrder = 0
  end
  object EditDescription: TMemo
    Left = 16
    Top = 80
    Width = 297
    Height = 105
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object EditDifficulty: TEdit
    Left = 16
    Top = 264
    Width = 297
    Height = 21
    TabOrder = 3
  end
  object EditAuthor: TEdit
    Left = 16
    Top = 312
    Width = 297
    Height = 21
    TabOrder = 4
  end
  object ButtonOK: TButton
    Left = 56
    Top = 344
    Width = 97
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 176
    Top = 344
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 6
  end
  object EditKind: TEdit
    Left = 16
    Top = 216
    Width = 297
    Height = 21
    TabOrder = 2
  end
end
