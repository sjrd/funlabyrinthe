object FormEditUnits: TFormEditUnits
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Modifier les unit'#233's'
  ClientHeight = 233
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    417
    233)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelUnits: TLabel
    Left = 16
    Top = 16
    Width = 74
    Height = 13
    Caption = 'Liste des unit'#233's'
  end
  object ListBoxUnits: TListBox
    Left = 16
    Top = 32
    Width = 209
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object ButtonOK: TBitBtn
    Left = 216
    Top = 192
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkOK
  end
  object ButtonCancel: TBitBtn
    Left = 312
    Top = 192
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkCancel
  end
  object ButtonAdd: TButton
    Left = 240
    Top = 32
    Width = 161
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ajouter une unit'#233
    TabOrder = 3
    OnClick = ButtonAddClick
  end
  object ButtonRemove: TButton
    Left = 240
    Top = 72
    Width = 161
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Retirer cette unit'#233
    TabOrder = 4
    OnClick = ButtonRemoveClick
  end
  object ButtonEditParams: TButton
    Left = 240
    Top = 112
    Width = 161
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Param'#232'tres de l'#39'unit'#233'...'
    TabOrder = 5
    OnClick = ButtonEditParamsClick
  end
  object OpenUnitDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Ajouter une unit'#233' existante'
    Left = 16
    Top = 192
  end
end
