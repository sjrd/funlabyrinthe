object FormDescription: TFormDescription
  Left = 217
  Top = 163
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Description du labyrinthe'
  ClientHeight = 217
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MemoDescription: TMemo
    Left = 16
    Top = 8
    Width = 345
    Height = 153
    TabOrder = 0
  end
  object BoutonOK: TButton
    Left = 76
    Top = 176
    Width = 97
    Height = 33
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object BoutonAnnuler: TButton
    Left = 204
    Top = 176
    Width = 97
    Height = 33
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 2
  end
end
