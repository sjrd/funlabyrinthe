object FormAscenseur: TFormAscenseur
  Left = 245
  Top = 208
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Placer un ascenseur'
  ClientHeight = 113
  ClientWidth = 305
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
  object LabelDebut: TLabel
    Left = 16
    Top = 16
    Width = 182
    Height = 13
    Caption = 'Etage du bas de la cage d'#39'ascenseur :'
  end
  object LabelFin: TLabel
    Left = 16
    Top = 48
    Width = 186
    Height = 13
    Caption = 'Etage du haut de la cage d'#39'ascenseur :'
  end
  object BoutonOK: TButton
    Left = 104
    Top = 72
    Width = 97
    Height = 33
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object EditDebut: TSpinEdit
    Left = 216
    Top = 8
    Width = 73
    Height = 22
    EditorEnabled = False
    MaxValue = 1
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object EditFin: TSpinEdit
    Left = 216
    Top = 40
    Width = 73
    Height = 22
    EditorEnabled = False
    MaxValue = 1
    MinValue = 1
    TabOrder = 2
    Value = 1
  end
end
