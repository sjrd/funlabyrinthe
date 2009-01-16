object FormCreateNewSourceFile: TFormCreateNewSourceFile
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Cr'#233'er un nouveau fichier source'
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelSourceFileType: TLabel
    Left = 16
    Top = 16
    Width = 113
    Height = 13
    Caption = 'Type de fichier source :'
  end
  object LabelDescription: TLabel
    Left = 192
    Top = 16
    Width = 60
    Height = 13
    Caption = 'Description :'
  end
  object ListBoxSourceFileType: TListBox
    Left = 16
    Top = 32
    Width = 153
    Height = 169
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBoxSourceFileTypeClick
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
  object SaveSourceFileDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Veuillez choisir un nom de fichier source'
    Left = 16
    Top = 208
  end
end
