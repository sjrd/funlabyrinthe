object FormBougeAscenseur: TFormBougeAscenseur
  Left = 481
  Top = 29
  ActiveControl = BoutonOK
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Ascenseur'
  ClientHeight = 105
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object LabelEtage: TLabel
    Left = 16
    Top = 24
    Width = 129
    Height = 13
    Caption = #192' quel '#233'tage veux-tu aller ?'
  end
  object EditEtage: TSpinEdit
    Left = 160
    Top = 17
    Width = 81
    Height = 26
    AutoSelect = False
    EditorEnabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 10
    MaxValue = 1
    MinValue = 0
    ParentFont = False
    TabOrder = 0
    Value = 1
  end
  object BoutonOK: TButton
    Left = 84
    Top = 64
    Width = 89
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
