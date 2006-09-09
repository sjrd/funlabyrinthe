object FormNumber: TFormNumber
  Left = 481
  Top = 29
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Choix d'#39'un nombre'
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
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPrompt: TLabel
    Left = 16
    Top = 24
    Width = 32
    Height = 13
    Caption = 'Invite :'
  end
  object EditValue: TSpinEdit
    Left = 176
    Top = 17
    Width = 65
    Height = 26
    AutoSelect = False
    EditorEnabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 10
    MaxValue = 0
    MinValue = 0
    ParentFont = False
    TabOrder = 0
    Value = 0
  end
  object ButtonOK: TButton
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
