object FormFileProperties: TFormFileProperties
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Propri'#233't'#233's du fichier'
  ClientHeight = 361
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
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
    Top = 200
    Width = 49
    Height = 13
    Caption = 'Difficult'#233' :'
  end
  object LabelAuthor: TLabel
    Left = 16
    Top = 248
    Width = 40
    Height = 13
    Caption = 'Auteur :'
  end
  object LabelAuthorID: TLabel
    Left = 16
    Top = 296
    Width = 181
    Height = 13
    Caption = 'ID sur le site (ou 0 si pas enregistr'#233') :'
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
    Top = 216
    Width = 297
    Height = 21
    TabOrder = 2
  end
  object EditAuthor: TEdit
    Left = 16
    Top = 264
    Width = 297
    Height = 21
    TabOrder = 3
  end
  object EditAuthorID: TEdit
    Left = 224
    Top = 288
    Width = 89
    Height = 21
    TabOrder = 4
    OnExit = EditAuthorIDExit
  end
  object ButtonOK: TButton
    Left = 56
    Top = 320
    Width = 97
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 176
    Top = 320
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 6
  end
end
