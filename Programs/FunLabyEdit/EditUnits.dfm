object FormEditUnits: TFormEditUnits
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Unit'#233's utilis'#233'es'
  ClientHeight = 347
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
  object LabelAvailableUnits: TLabel
    Left = 21
    Top = 21
    Width = 115
    Height = 17
    Caption = 'Unit'#233's disponibles :'
  end
  object ButtonAddUnit: TSpeedButton
    Left = 209
    Top = 42
    Width = 33
    Height = 33
    Caption = '>'
    OnClick = ButtonAddUnitClick
  end
  object ButtonRemoveUnit: TSpeedButton
    Left = 209
    Top = 84
    Width = 33
    Height = 32
    Caption = '<'
    OnClick = ButtonRemoveUnitClick
  end
  object LabelUsedUnits: TLabel
    Left = 251
    Top = 21
    Width = 95
    Height = 17
    Caption = 'Unit'#233's utilis'#233'es :'
  end
  object ButtonOK: TButton
    Left = 199
    Top = 293
    Width = 106
    Height = 33
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 324
    Top = 293
    Width = 106
    Height = 33
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 1
  end
  object ListBoxAvailableUnits: TListBox
    Left = 21
    Top = 42
    Width = 179
    Height = 231
    ItemHeight = 13
    ItemHeight = 17
    MultiSelect = True
    Sorted = True
    TabOrder = 2
  end
  object ListBoxUsedUnits: TListBox
    Left = 251
    Top = 42
    Width = 179
    Height = 231
    DragMode = dmAutomatic
    ItemHeight = 17
    MultiSelect = True
    TabOrder = 3
    OnDragDrop = ListBoxUsedUnitsDragDrop
    OnDragOver = ListBoxUsedUnitsDragOver
    OnMouseDown = ListBoxUsedUnitsMouseDown
  end
end
