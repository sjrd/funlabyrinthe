object FormNumeroCase: TFormNumeroCase
  Left = 276
  Top = 197
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Choisissez le num'#233'ro de la case'
  ClientHeight = 97
  ClientWidth = 273
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object LabelNumero: TLabel
    Left = 24
    Top = 28
    Width = 127
    Height = 13
    Caption = 'Num'#233'ro de la case (1 '#224' x) :'
  end
  object EditNumero: TSpinEdit
    Left = 184
    Top = 23
    Width = 65
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 0
    Value = 1
  end
  object BoutonOK: TButton
    Left = 92
    Top = 60
    Width = 89
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
end
