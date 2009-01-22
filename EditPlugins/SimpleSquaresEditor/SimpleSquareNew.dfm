object FormNewSimpleSquare: TFormNewSimpleSquare
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Nouveau composant'
  ClientHeight = 321
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    329
    321)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelComponentClass: TLabel
    Left = 16
    Top = 72
    Width = 138
    Height = 13
    Caption = 'Type de composant '#224' cr'#233'er :'
  end
  object LabelID: TLabel
    Left = 16
    Top = 24
    Width = 57
    Height = 13
    Caption = 'Identifiant :'
  end
  object LabelName: TLabel
    Left = 16
    Top = 48
    Width = 28
    Height = 13
    Caption = 'Nom :'
  end
  object EditID: TEdit
    Left = 104
    Top = 16
    Width = 209
    Height = 21
    TabOrder = 0
  end
  object EditName: TEdit
    Left = 104
    Top = 40
    Width = 209
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object ListBoxComponentClass: TListBox
    Left = 16
    Top = 88
    Width = 297
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
    OnDblClick = ListBoxComponentClassDblClick
  end
  object ButtonOK: TButton
    Left = 120
    Top = 280
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 224
    Top = 280
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 4
  end
end
