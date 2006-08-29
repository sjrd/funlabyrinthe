object FormPrincipale: TFormPrincipale
  Left = 255
  Top = 129
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'FunLabyrinthe'
  ClientHeight = 289
  ClientWidth = 270
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
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 0
    Width = 270
    Height = 270
    Align = alClient
    Stretch = True
  end
  object Barre: TStatusBar
    Left = 0
    Top = 270
    Width = 270
    Height = 19
    Panels = <
      item
        Width = 55
      end
      item
        Width = 65
      end
      item
        Width = 86
      end
      item
        Width = 91
      end>
    SizeGrip = False
  end
  object BigMenu: TMainMenu
    Left = 72
    Top = 120
    object MenuFichier: TMenuItem
      Caption = '&Fichier'
      object MenuNouveau: TMenuItem
        Caption = 'Nouvelle Partie'
        ShortCut = 113
        OnClick = MenuNouveauClick
      end
      object MenuRecommencer: TMenuItem
        Caption = 'Recommencer'
        Enabled = False
        ShortCut = 114
        OnClick = MenuRecommencerClick
      end
      object MenuCharger: TMenuItem
        Caption = 'Charger'
        ShortCut = 16451
        OnClick = MenuChargerClick
      end
      object MenuEnregistrer: TMenuItem
        Caption = 'Enregistrer'
        Enabled = False
        ShortCut = 16467
        OnClick = MenuEnregistrerClick
      end
      object Sep1: TMenuItem
        Caption = '-'
      end
      object MenuDescription: TMenuItem
        Caption = 'Description'
        Enabled = False
        OnClick = MenuDescriptionClick
      end
      object MenuProprietes: TMenuItem
        Caption = 'Propri'#233't'#233's'
        Enabled = False
        object MenuPropLabyrinthe: TMenuItem
          Caption = 'Labyrinthe'
          OnClick = MenuPropLabyrintheClick
        end
        object MenuPropJoueur: TMenuItem
          Caption = 'Joueur'
          OnClick = MenuPropJoueurClick
        end
      end
      object Sep2: TMenuItem
        Caption = '-'
      end
      object MenuQuitter: TMenuItem
        Caption = 'Quitter'
        ShortCut = 16465
        OnClick = MenuQuitterClick
      end
    end
    object MenuOptions: TMenuItem
      Caption = '&Options'
      object MenuIndices: TMenuItem
        Caption = 'Montrer les indices'
        OnClick = MenuIndicesClick
      end
    end
    object MenuAide: TMenuItem
      Caption = '&Aide'
      object MenuRubrAide: TMenuItem
        Caption = 'Rubriques d'#39'aide'
        ShortCut = 112
        OnClick = MenuRubrAideClick
      end
      object Sep3: TMenuItem
        Caption = '-'
      end
      object MenuAPropos: TMenuItem
        Caption = 'A propos...'
        OnClick = MenuAProposClick
      end
    end
  end
  object Ouvrir: TOpenDialog
    DefaultExt = 'lab'
    Filter = 'Labyrinthes (*.lab)|*.lab;*.url'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofNoNetworkButton]
    Left = 104
    Top = 120
  end
  object Sauver: TSaveDialog
    DefaultExt = 'lab'
    Filter = 'Labyrinthes (*.lab)|*.lab'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofCreatePrompt, ofNoNetworkButton]
    Title = 'Enregistrer la partie'
    Left = 136
    Top = 120
  end
  object AboutDialog: TSdAboutDialog
    ProgramIcon.Data = {
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
    ProgramName = 'FunLabyrinthe'
    ProgramVersion = '4.5'
    AuthorName = 'S'#233'bastien Jean Robert Doeraene'
    AuthorEMail = 'sjrd@redaction-developpez.com'
    WebSite = 'http://sjrd.developpez.com/programmes/funlaby/'
    Left = 168
    Top = 120
  end
end
