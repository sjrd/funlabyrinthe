object FormPrincipale: TFormPrincipale
  Left = 193
  Top = 145
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Editeur de labyrinthes'
  ClientHeight = 228
  ClientWidth = 492
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
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 264
    Top = 0
    Width = 210
    Height = 210
    Enabled = False
    Stretch = True
  end
  object LabelEtage: TLabel
    Left = 213
    Top = 184
    Width = 34
    Height = 13
    Caption = 'Etage :'
  end
  object ImageHerbe: TSvDropImage
    Tag = 48
    Left = 16
    Top = 24
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageEau: TSvDropImage
    Tag = 49
    Left = 56
    Top = 24
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageMur: TSvDropImage
    Tag = 50
    Left = 96
    Top = 24
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageTrou: TSvDropImage
    Tag = 51
    Left = 136
    Top = 24
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageBlocArgent: TSvDropImage
    Tag = 52
    Left = 176
    Top = 24
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageBlocOr: TSvDropImage
    Tag = 53
    Left = 216
    Top = 24
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageNord: TSvDropImage
    Tag = 54
    Left = 16
    Top = 64
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageEst: TSvDropImage
    Tag = 55
    Left = 56
    Top = 64
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageSud: TSvDropImage
    Tag = 56
    Left = 96
    Top = 64
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageOuest: TSvDropImage
    Tag = 57
    Left = 136
    Top = 64
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageCarrefour: TSvDropImage
    Tag = 58
    Left = 176
    Top = 64
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageTeleporteur: TSvDropImage
    Tag = 3
    Left = 216
    Top = 64
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageMoulinDirect: TSvDropImage
    Tag = 224
    Left = 16
    Top = 104
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageMoulinIndirect: TSvDropImage
    Tag = 225
    Left = 56
    Top = 104
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageEscalierM: TSvDropImage
    Tag = 62
    Left = 96
    Top = 104
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageEscalierD: TSvDropImage
    Tag = 60
    Left = 136
    Top = 104
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageAscenseur: TSvDropImage
    Tag = 61
    Left = 176
    Top = 104
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageBouton: TSvDropImage
    Tag = 4
    Left = 216
    Top = 104
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageBoutonEnfonce: TSvDropImage
    Tag = 64
    Left = 16
    Top = 144
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageBouee: TSvDropImage
    Tag = 67
    Left = 56
    Top = 144
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImagePlanche: TSvDropImage
    Tag = 68
    Left = 96
    Top = 144
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageCleArgent: TSvDropImage
    Tag = 69
    Left = 136
    Top = 144
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageCleOr: TSvDropImage
    Tag = 70
    Left = 176
    Top = 144
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageFauxMur: TSvDropImage
    Tag = 63
    Left = 216
    Top = 144
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageBarque: TSvDropImage
    Tag = 193
    Left = 16
    Top = 184
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageDepart: TSvDropImage
    Tag = 65
    Left = 56
    Top = 184
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object ImageTresor: TSvDropImage
    Tag = 59
    Left = 96
    Top = 184
    Width = 30
    Height = 30
    DropControl = Image
    OnDrop = PoseImage
  end
  object Horizontal: TScrollBar
    Left = 264
    Top = 211
    Width = 211
    Height = 16
    Enabled = False
    LargeChange = 7
    Max = 0
    PageSize = 0
    TabOrder = 0
    OnScroll = HorizontalScroll
  end
  object Vertical: TScrollBar
    Left = 475
    Top = 0
    Width = 16
    Height = 210
    Enabled = False
    Kind = sbVertical
    LargeChange = 7
    Max = 0
    PageSize = 0
    TabOrder = 1
    OnScroll = VerticalScroll
  end
  object Etage: TSpinButton
    Left = 475
    Top = 211
    Width = 16
    Height = 16
    DownGlyph.Data = {
      DE000000424DDE00000000000000360000002800000009000000060000000100
      180000000000A800000000000000000000000000000000000000008080008080
      0080800080800080800080800080800080800080800000808000808000808000
      8080000000008080008080008080008080000080800080800080800000000000
      0000000000808000808000808000008080008080000000000000000000000000
      0000000080800080800000808000000000000000000000000000000000000000
      0000008080000080800080800080800080800080800080800080800080800080
      8000}
    Enabled = False
    TabOrder = 2
    UpGlyph.Data = {
      DE000000424DDE00000000000000360000002800000009000000060000000100
      180000000000A800000000000000000000000000000000000000008080008080
      0080800080800080800080800080800080800080800000808000000000000000
      0000000000000000000000000000008080650080800080800000000000000000
      000000000000000080800080806C008080008080008080000000000000000000
      0080800080800080807200808000808000808000808000000000808000808000
      8080008080740080800080800080800080800080800080800080800080800080
      806F}
    OnDownClick = EtageDownClick
    OnUpClick = EtageUpClick
  end
  object EditEtage: TSpinEdit
    Left = 213
    Top = 200
    Width = 44
    Height = 22
    EditorEnabled = False
    Enabled = False
    MaxValue = 1
    MinValue = 1
    TabOrder = 3
    Value = 1
    OnChange = EditEtageChange
  end
  object BigMenu: TMainMenu
    Left = 328
    Top = 104
    object MenuFichier: TMenuItem
      Caption = '&Fichier'
      object MenuNouveau: TMenuItem
        Caption = 'Nouveau'
        ShortCut = 16462
        OnClick = MenuNouveauClick
      end
      object MenuOuvrir: TMenuItem
        Caption = 'Ouvrir'
        ShortCut = 16463
        OnClick = MenuOuvrirClick
      end
      object MenuEnregistrer: TMenuItem
        Caption = 'Enregistrer'
        Enabled = False
        ShortCut = 16467
        OnClick = MenuEnregistrerClick
      end
      object MenuEnregSous: TMenuItem
        Caption = 'Enregistrer sous'
        Enabled = False
        OnClick = MenuEnregSousClick
      end
      object MenuFermer: TMenuItem
        Caption = 'Fermer'
        Enabled = False
        OnClick = MenuFermerClick
      end
      object Sep1: TMenuItem
        Caption = '-'
      end
      object MenuQuitter: TMenuItem
        Caption = 'Quitter'
        ShortCut = 16465
        OnClick = MenuQuitterClick
      end
    end
    object MenuEdition: TMenuItem
      Caption = '&Edition'
      Visible = False
      object MenuBoutons: TMenuItem
        Caption = 'Actions des boutons'
        OnClick = MenuBoutonsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuNom: TMenuItem
        Caption = 'Nom'
        OnClick = MenuNomClick
      end
      object MenuDescription: TMenuItem
        Caption = 'Description'
        OnClick = MenuDescriptionClick
      end
    end
    object MenuAide: TMenuItem
      Caption = '&Aide'
      object MenuRubrAide: TMenuItem
        Caption = 'Rubriques d'#39'aide'
        ShortCut = 112
        OnClick = MenuRubrAideClick
      end
      object Sep2: TMenuItem
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
    Filter = 'Labyrinthes (*.lab)|*.lab'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofNoNetworkButton]
    Title = 'Ouvrir un labyrinthe existant'
    Left = 360
    Top = 104
  end
  object Sauver: TSaveDialog
    DefaultExt = 'lab'
    Filter = 'Labyrinthes (*.lab)|*.lab'
    Options = [ofOverwritePrompt, ofNoChangeDir, ofCreatePrompt, ofNoNetworkButton]
    Title = 'Enregistrer le labyrinthe'
    Left = 392
    Top = 104
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
    ProgramName = 'EditLabyrinthe'
    ProgramVersion = '4.5'
    AuthorName = 'S'#233'bastien Jean Robert Doeraene'
    AuthorEMail = 'sjrd@redaction-developpez.com'
    WebSite = 'http://sjrd.developpez.com/programmes/funlaby/'
    Left = 424
    Top = 104
  end
end
