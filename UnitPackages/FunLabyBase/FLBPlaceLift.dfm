object FormPlaceLift: TFormPlaceLift
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Placement d'#39'un ascenseur'
  ClientHeight = 257
  ClientWidth = 345
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
  object LabelFloorList: TLabel
    Left = 16
    Top = 16
    Width = 222
    Height = 13
    Caption = 'S'#233'lectionnez les '#233'tages o'#249' placer l'#39'ascenseur :'
  end
  object ListBoxFloorList: TListBox
    Left = 16
    Top = 32
    Width = 313
    Height = 177
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
  end
  object ButtonOK: TBitBtn
    Left = 104
    Top = 216
    Width = 105
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object ButtonCancel: TBitBtn
    Left = 224
    Top = 216
    Width = 105
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
