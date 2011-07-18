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
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000B0B11A2FB6C434AFBBD558F1BFDA67FFBBD537FFC0DD71FFB6CC3DFFD3CE
    BEFFD3CFB8FFE0DECDFFDCD9C7FFDEDCCAF1E7E5D9AFECEAE02F00000000B5C4
    2B2FB6CC22EFC0DE62FFCFF1CEFFC8E695FFBFDB60FFBBDB5BFFBFDD67FFD4CF
    BEFFDFDCCBFFE5E3D4FFE6E3D6FFE2E0D1FFE3E0D2FFEAE8DEEFE5E3D52FB8D2
    39AFB3BC22FFB0B325FFBDD967FFBED75DFFBFCB6CFFBCCA57FFB6CD38FFE7E4
    DEFFE7E5D8FFE9E7DBFFE3E1D2FFE9E7DBFFECEAE1FFEEECE3FFEDEBE1AFBFDB
    56F1B0B92FFFAB940DFFAFAE1FFFB9CA41FF878351FF39220EFF230000FF3402
    02FF643533FFBCA99EFFE3E1D2FFE5E3D5FFE7E5DAFFEEECE3FFEFEDE6F1BAD9
    54FFB6C928FFAFAD1FFFAC9618FF554810FF160000FF2E0000FF410000FF5100
    00FF5C0000FF610000FFA16D66FFE0DDCDFFE8E6D9FFF0EEE7FFF0EFE8FFB6C8
    33FFB0A827FFA98C12FF7C6016FF110000FF2E0000FF460000FF5D0000FF6F00
    00FF7B0000FF810000FF7F0000FFC19F92FFE3E0D2FFEBE9E0FFF1F0E8FFB3BC
    2EFFAB9019FFA37918FF2C1D04FF240000FF400000FF5C0000FF750000FF8B00
    00FF9A0000FFA20000FFA00000FFA42F2CFFE0DECEFFEBE9DEFFF5F4EFFFC4D7
    53FFB38C1AFFAC6F08FF110200FF300000FF4F0000FF6E0000FF890000FFA200
    00FFB70000FFC00000FFBD0000FFAC0000FFE4DECAFFECE7D6FFFDF8F3FF1E97
    49FF1E9E4AFF189A48FF150201FF390000FF590000FF790000FF970000FFB500
    00FFCE0000FFDF0000FFD90000FFC10000FF5D999CFF295451FF69A59EFF36AC
    56FF2FAB51FF239A46FF1D2310FF3B0000FF5D0000FF7D0000FF9E0000FFBD00
    00FFDC0000FFF80000FFEA0000FFB21A18FF6CA1A1FF3E6C66FF79B0AEFF40AE
    5CFF299D4AFF209843FF217434FF390000FF5A0000FF7A0000FF9B0000FFB700
    00FFD30000FFE50000FFDD0000FF6B5A5CFF508482FF427068FF689F96FF269C
    47FF269A46FF1D9541FF2EA44EFF345228FF510000FF700000FF8D0000FFA800
    00FFBD0000FFC70000FF862F2FFF568989FF528A92FF457D82FF2B5552FF24A0
    48F11C9541FF209844FF2AA44BFF29A34CFF36773AFF532310FF790000FF9000
    00FF8A1618FF78777CFF42716CFF629A9BFF659D9FFF5D989AFF39655CF137A9
    52AF1D9843FF2FA14FFF1D9843FF219A46FF34A452FF299C4AFF2C9E49FF2F50
    51FF5A969CFF58949DFF477675FF85C2C9FF80B3B5FF487670FF416B64AF30A1
    4E2F19983FEF2BA04EFF2CA04CFF18923EFF2EA550FF32A150FF2D9B48FF486B
    6CFF214C4EFF3D7176FF538689FF3F7575FF457A79FF336161EF22453E2F0000
    000025A0492F30A852AF2B9D4BF1259A48FF32A651FF39A356FF239A44FF5B82
    86FF2F5B59FF447F87FF59959AFF305650F1355E59AF4870652F00000000C003
    0000800100000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000080010000C0030000}
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
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
      object Label1: TLabel
        Left = 24
        Top = 24
        Width = 178
        Height = 16
        Caption = 'Nombre de lignes (en zones) :'
      end
      object Label2: TLabel
        Left = 24
        Top = 56
        Width = 197
        Height = 16
        Caption = 'Nombre de colonnes (en zones) :'
      end
      object Label3: TLabel
        Left = 24
        Top = 136
        Width = 114
        Height = 16
        Caption = 'Nom du labyrinthe :'
      end
      object Label5: TLabel
        Left = 24
        Top = 88
        Width = 111
        Height = 16
        Caption = 'Nombre d'#39#233'tages :'
      end
      object EditNomFichier: TEdit
        Left = 128
        Top = 128
        Width = 121
        Height = 24
        TabOrder = 0
        Text = 'LabyGene'
      end
      object EditLignes: TSpinEdit
        Left = 192
        Top = 16
        Width = 57
        Height = 26
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
        Height = 26
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
        Height = 26
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
        Width = 227
        Height = 16
        Caption = 'Coefficient de blocage des directions :'
      end
      object Label7: TLabel
        Left = 17
        Top = 200
        Width = 209
        Height = 16
        Caption = 'Coefficient d'#39'interdiction de boucle :'
      end
      object Label8: TLabel
        Left = 11
        Top = 102
        Width = 213
        Height = 16
        Caption = 'P'#233'riode de blocage des directions :'
      end
      object Label6: TLabel
        Left = 16
        Top = 168
        Width = 208
        Height = 16
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
