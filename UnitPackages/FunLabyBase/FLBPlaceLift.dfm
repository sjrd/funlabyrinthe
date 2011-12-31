object FormPlaceLift: TFormPlaceLift
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Placement d'#39'un ascenseur'
  ClientHeight = 336
  ClientWidth = 451
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
  object LabelFloorList: TLabel
    Left = 21
    Top = 21
    Width = 277
    Height = 17
    Caption = 'S'#233'lectionnez les '#233'tages o'#249' placer l'#39'ascenseur :'
  end
  object ListBoxFloorList: TListBox
    Left = 21
    Top = 42
    Width = 409
    Height = 231
    ItemHeight = 17
    MultiSelect = True
    TabOrder = 0
  end
  object ButtonOK: TBitBtn
    Left = 136
    Top = 282
    Width = 137
    Height = 33
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object ButtonCancel: TBitBtn
    Left = 293
    Top = 282
    Width = 137
    Height = 33
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 2
  end
end
