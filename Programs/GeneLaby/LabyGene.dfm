object FormPrincipale: TFormPrincipale
  Left = 198
  Top = 118
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'G'#233'n'#233'rateur de Labyrinthes'
  ClientHeight = 304
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000009900000000000
    0009900000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000C0030000DFFB0000DFFB0000D8030000DE7B0000DE7B
    0000C01B0000DFFB0000DFFB0000C0030000FFFF0000FFFF0000FFFF0000}
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 286
    Height = 263
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'G'#233'n'#233'ral'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 24
        Top = 24
        Width = 140
        Height = 13
        Caption = 'Nombre de lignes (en zones) :'
      end
      object Label2: TLabel
        Left = 24
        Top = 56
        Width = 156
        Height = 13
        Caption = 'Nombre de colonnes (en zones) :'
      end
      object Label3: TLabel
        Left = 24
        Top = 136
        Width = 91
        Height = 13
        Caption = 'Nom du labyrinthe :'
      end
      object Label5: TLabel
        Left = 24
        Top = 88
        Width = 86
        Height = 13
        Caption = 'Nombre d'#39#233'tages :'
      end
      object EditNomFichier: TEdit
        Left = 128
        Top = 128
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'LabyGene'
      end
      object EditLignes: TSpinEdit
        Left = 192
        Top = 16
        Width = 57
        Height = 22
        MaxValue = 10
        MinValue = 1
        TabOrder = 1
        Value = 3
        OnChange = EditLignesChange
      end
      object EditColonnes: TSpinEdit
        Left = 192
        Top = 48
        Width = 57
        Height = 22
        MaxValue = 10
        MinValue = 1
        TabOrder = 2
        Value = 3
        OnChange = EditColonnesChange
      end
      object EditEtages: TSpinEdit
        Left = 192
        Top = 80
        Width = 57
        Height = 22
        MaxValue = 10
        MinValue = 1
        TabOrder = 3
        Value = 3
        OnChange = EditEtagesChange
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Avanc'#233
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label4: TLabel
        Left = 13
        Top = 72
        Width = 180
        Height = 13
        Caption = 'Coefficient de blocage des directions :'
      end
      object Label7: TLabel
        Left = 17
        Top = 200
        Width = 168
        Height = 13
        Caption = 'Coefficient d'#39'interdiction de boucle :'
      end
      object Label8: TLabel
        Left = 11
        Top = 102
        Width = 166
        Height = 13
        Caption = 'P'#233'riode de blocage des directions :'
      end
      object Label6: TLabel
        Left = 16
        Top = 168
        Width = 165
        Height = 13
        Caption = 'Coefficient d'#39'interdiction d'#39'escalier :'
      end
      object EditBlocage: TSpinEdit
        Left = 200
        Top = 64
        Width = 57
        Height = 22
        Increment = 5
        MaxValue = 100
        MinValue = 0
        TabOrder = 0
        Value = 60
      end
      object CBCarrefours: TCheckBox
        Left = 16
        Top = 132
        Width = 129
        Height = 17
        Caption = 'Croisements autoris'#233's'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object EditBlocEscaliers: TSpinEdit
        Left = 200
        Top = 159
        Width = 57
        Height = 22
        Increment = 5
        MaxValue = 100
        MinValue = 0
        TabOrder = 2
        Value = 80
      end
      object EditBlocBoucles: TSpinEdit
        Left = 200
        Top = 191
        Width = 57
        Height = 22
        Increment = 20
        MaxValue = 100
        MinValue = 0
        TabOrder = 3
        Value = 80
      end
      object EditPeriode: TSpinEdit
        Left = 200
        Top = 96
        Width = 57
        Height = 22
        MaxValue = 1000
        MinValue = 1
        TabOrder = 4
        Value = 7
      end
      object RGModulation: TRadioGroup
        Left = 8
        Top = 8
        Width = 257
        Height = 41
        Caption = 'Modulation du coefficient de blocage'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Arc Tangente'
          'Sinuso'#239'de')
        TabOrder = 5
        OnClick = RGModulationClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 263
    Width = 286
    Height = 41
    Align = alBottom
    TabOrder = 1
    object BExe: TButton
      Left = 10
      Top = 8
      Width = 73
      Height = 25
      Caption = 'Ex'#233'cuter'
      Default = True
      TabOrder = 0
      OnClick = BExeClick
    end
    object BAide: TButton
      Left = 106
      Top = 8
      Width = 73
      Height = 25
      Caption = 'Aide'
      TabOrder = 1
      OnClick = BAideClick
    end
    object BQuitter: TButton
      Left = 202
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Quitter'
      Enabled = False
      TabOrder = 2
      OnClick = BQuitterClick
    end
  end
end
