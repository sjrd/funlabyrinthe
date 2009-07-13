object FormObjects: TFormObjects
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Objets d'#39'un joueur'
  ClientHeight = 281
  ClientWidth = 425
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
  object LabelObjects: TLabel
    Left = 16
    Top = 16
    Width = 88
    Height = 13
    Caption = 'Objets du joueur :'
  end
  object ListViewObjects: TListView
    Left = 16
    Top = 32
    Width = 393
    Height = 201
    Columns = <>
    IconOptions.Arrangement = iaLeft
    ReadOnly = True
    SmallImages = ObjectsImages
    TabOrder = 0
    ViewStyle = vsList
    OnCustomDrawItem = ListViewObjectsCustomDrawItem
  end
  object ButtonOK: TButton
    Left = 328
    Top = 240
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ObjectsImages: TImageList
    Height = 30
    Width = 30
    Left = 128
    Top = 136
  end
end
