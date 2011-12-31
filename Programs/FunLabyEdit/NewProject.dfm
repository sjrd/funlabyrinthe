object FormNewProject: TFormNewProject
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Cr'#233'er un nouveau projet'
  ClientHeight = 200
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object Label1: TLabel
    Left = 21
    Top = 21
    Width = 97
    Height = 17
    Caption = 'Titre du projet :'
  end
  object LabelFileName: TLabel
    Left = 21
    Top = 84
    Width = 242
    Height = 17
    Caption = 'Nom du fichier projet (sans extension) :'
  end
  object EditTitle: TEdit
    Left = 21
    Top = 42
    Width = 545
    Height = 25
    TabOrder = 0
    OnChange = EditTitleChange
  end
  object EditFileName: TJvValidateEdit
    Left = 21
    Top = 105
    Width = 545
    Height = 25
    Alignment = taLeftJustify
    CheckChars = '01234567890'
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    DisplayFormat = dfCheckChars
    TabOrder = 1
    OnChange = EditFileNameChange
  end
  object ButtonOK: TButton
    Left = 282
    Top = 146
    Width = 138
    Height = 33
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 429
    Top = 146
    Width = 137
    Height = 33
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
end
