object FormCreateNewSourceFile: TFormCreateNewSourceFile
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Cr'#233'er un nouveau fichier source'
  ClientHeight = 336
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 17
  object LabelSourceFileType: TLabel
    Left = 21
    Top = 21
    Width = 143
    Height = 17
    Caption = 'Type de fichier source :'
  end
  object LabelDescription: TLabel
    Left = 251
    Top = 21
    Width = 77
    Height = 17
    Caption = 'Description :'
  end
  object ListBoxSourceFileType: TListBox
    Left = 21
    Top = 42
    Width = 200
    Height = 221
    ItemHeight = 17
    TabOrder = 0
    OnClick = ListBoxSourceFileTypeClick
  end
  object MemoDescription: TMemo
    Left = 251
    Top = 42
    Width = 200
    Height = 221
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ButtonOK: TBitBtn
    Left = 209
    Top = 282
    Width = 117
    Height = 33
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object ButtonCancel: TBitBtn
    Left = 335
    Top = 282
    Width = 116
    Height = 33
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object SaveSourceFileDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Veuillez choisir un nom de fichier source'
    OnCanClose = SaveSourceFileDialogCanClose
    Left = 64
    Top = 208
  end
end
