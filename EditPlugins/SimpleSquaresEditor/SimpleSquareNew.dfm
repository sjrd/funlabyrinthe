object FormNewSimpleSquare: TFormNewSimpleSquare
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Nouveau composant'
  ClientHeight = 420
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    430
    420)
  PixelsPerInch = 120
  TextHeight = 17
  object LabelComponentClass: TLabel
    Left = 21
    Top = 94
    Width = 178
    Height = 17
    Caption = 'Type de composant '#224' cr'#233'er :'
  end
  object LabelID: TLabel
    Left = 21
    Top = 31
    Width = 69
    Height = 17
    Caption = 'Identifiant :'
  end
  object LabelName: TLabel
    Left = 21
    Top = 63
    Width = 38
    Height = 17
    Caption = 'Nom :'
  end
  object EditID: TEdit
    Left = 136
    Top = 21
    Width = 273
    Height = 25
    TabOrder = 0
  end
  object EditName: TEdit
    Left = 136
    Top = 52
    Width = 273
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object ListBoxComponentClass: TListBox
    Left = 21
    Top = 115
    Width = 388
    Height = 242
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 17
    TabOrder = 2
    OnDblClick = ListBoxComponentClassDblClick
  end
  object ButtonOK: TButton
    Left = 157
    Top = 366
    Width = 116
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 293
    Top = 366
    Width = 116
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 4
  end
end
