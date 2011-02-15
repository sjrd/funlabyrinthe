object FormEditUnits: TFormEditUnits
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Unit'#233's utilis'#233'es'
  ClientHeight = 265
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
  object LabelAvailableUnits: TLabel
    Left = 16
    Top = 16
    Width = 92
    Height = 13
    Caption = 'Unit'#233's disponibles :'
  end
  object ButtonAddUnit: TSpeedButton
    Left = 160
    Top = 32
    Width = 25
    Height = 25
    Caption = '>'
    OnClick = ButtonAddUnitClick
  end
  object ButtonRemoveUnit: TSpeedButton
    Left = 160
    Top = 64
    Width = 25
    Height = 25
    Caption = '<'
    OnClick = ButtonRemoveUnitClick
  end
  object LabelUsedUnits: TLabel
    Left = 192
    Top = 16
    Width = 78
    Height = 13
    Caption = 'Unit'#233's utilis'#233'es :'
  end
  object ButtonOK: TButton
    Left = 152
    Top = 224
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 248
    Top = 224
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 1
  end
  object ListBoxAvailableUnits: TListBox
    Left = 16
    Top = 32
    Width = 137
    Height = 177
    ItemHeight = 13
    MultiSelect = True
    Sorted = True
    TabOrder = 2
  end
  object ListBoxUsedUnits: TListBox
    Left = 192
    Top = 32
    Width = 137
    Height = 177
    ItemHeight = 13
    MultiSelect = True
    Sorted = True
    TabOrder = 3
  end
end
