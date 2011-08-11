object FormNewProject: TFormNewProject
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Cr'#233'er un nouveau projet'
  ClientHeight = 153
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 76
    Height = 13
    Caption = 'Titre du projet :'
  end
  object LabelFileName: TLabel
    Left = 16
    Top = 64
    Width = 190
    Height = 13
    Caption = 'Nom du fichier projet (sans extension) :'
  end
  object EditTitle: TEdit
    Left = 16
    Top = 32
    Width = 417
    Height = 21
    TabOrder = 0
    OnChange = EditTitleChange
  end
  object EditFileName: TJvValidateEdit
    Left = 16
    Top = 80
    Width = 417
    Height = 21
    Alignment = taLeftJustify
    CheckChars = '01234567890'
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    DisplayFormat = dfCheckChars
    TabOrder = 1
    OnChange = EditFileNameChange
  end
  object ButtonOK: TButton
    Left = 216
    Top = 112
    Width = 105
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 328
    Top = 112
    Width = 105
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
end
