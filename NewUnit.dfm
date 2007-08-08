object FormCreateNewUnit: TFormCreateNewUnit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Cr'#233'er une nouvelle unit'#233
  ClientHeight = 257
  ClientWidth = 361
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
  object LabelUnitType: TLabel
    Left = 16
    Top = 16
    Width = 66
    Height = 13
    Caption = 'Type d'#39'unit'#233' :'
  end
  object LabelDescription: TLabel
    Left = 192
    Top = 16
    Width = 60
    Height = 13
    Caption = 'Description :'
  end
  object ListBoxUnitType: TListBox
    Left = 16
    Top = 32
    Width = 153
    Height = 169
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBoxUnitTypeClick
  end
  object MemoDescription: TMemo
    Left = 192
    Top = 32
    Width = 153
    Height = 169
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ButtonOK: TBitBtn
    Left = 160
    Top = 216
    Width = 89
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object ButtonCancel: TBitBtn
    Left = 256
    Top = 216
    Width = 89
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
end
