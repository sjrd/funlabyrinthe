object FormObjects: TFormObjects
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Objets d'#39'un joueur'
  ClientHeight = 362
  ClientWidth = 546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    546
    362)
  PixelsPerInch = 120
  TextHeight = 17
  object LabelObjects: TLabel
    Left = 21
    Top = 21
    Width = 113
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Objets du joueur :'
  end
  object ListViewObjects: TListView
    Left = 21
    Top = 42
    Width = 514
    Height = 263
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    IconOptions.Arrangement = iaLeft
    ReadOnly = True
    RowSelect = True
    SmallImages = ObjectsImages
    TabOrder = 0
    ViewStyle = vsList
    OnCustomDrawItem = ListViewObjectsCustomDrawItem
  end
  object ButtonOK: TButton
    Left = 429
    Top = 314
    Width = 106
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
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
