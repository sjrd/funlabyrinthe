object FormAddMap: TFormAddMap
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Ajouter une carte'
  ClientHeight = 161
  ClientWidth = 321
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
  object LabelID: TLabel
    Left = 16
    Top = 16
    Width = 245
    Height = 13
    Caption = 'ID de la carte (lettres non accentu'#233'es et chiffres) :'
  end
  object LabelDimensions: TLabel
    Left = 16
    Top = 67
    Width = 113
    Height = 13
    Caption = 'Dimensions (en cases) :'
  end
  object LabelZoneSize: TLabel
    Left = 16
    Top = 91
    Width = 137
    Height = 13
    Caption = 'Taile d'#39'une zone (en cases) :'
  end
  object EditID: TEdit
    Left = 16
    Top = 32
    Width = 289
    Height = 21
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 56
    Top = 120
    Width = 97
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 168
    Top = 120
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 4
  end
  object EditDimensions: TMaskEdit
    Left = 184
    Top = 59
    Width = 121
    Height = 21
    EditMask = '!999x999x99;1;0'
    MaxLength = 10
    TabOrder = 1
    Text = '  7x  7x 1'
  end
  object EditZoneSize: TMaskEdit
    Left = 184
    Top = 83
    Width = 121
    Height = 21
    EditMask = '!99x99;1;0'
    MaxLength = 5
    TabOrder = 2
    Text = ' 7x 7'
  end
end
