object FormModifieBoutons: TFormModifieBoutons
  Left = 95
  Top = 8
  HelpContext = 5
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Actions des boutons'
  ClientHeight = 473
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Carte: TImage
    Left = 0
    Top = 0
    Width = 210
    Height = 210
    Stretch = True
    OnMouseDown = CarteMouseDown
  end
  object LabelNo: TLabel
    Left = 432
    Top = 16
    Width = 103
    Height = 24
    Caption = 'Bouton(s) n'#176
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -20
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LabelEtage: TLabel
    Left = 52
    Top = 245
    Width = 34
    Height = 13
    Caption = 'Etage :'
  end
  object LabelMessage: TLabel
    Left = 16
    Top = 284
    Width = 247
    Height = 13
    Caption = 'Texte de l'#39'information que fait appara'#238'tre ce bouton :'
  end
  object LabelRemplacement: TLabel
    Left = 240
    Top = 8
    Width = 125
    Height = 13
    Caption = 'Case(s) de remplacement :'
  end
  object LabelStyle: TLabel
    Left = 432
    Top = 56
    Width = 80
    Height = 13
    Caption = 'Style du bouton :'
  end
  object Sep1: TBevel
    Left = 416
    Top = 0
    Width = 9
    Height = 473
    Shape = bsLeftLine
  end
  object Sep3: TBevel
    Left = 0
    Top = 272
    Width = 673
    Height = 9
    Shape = bsTopLine
  end
  object Sep4: TBevel
    Left = 418
    Top = 136
    Width = 256
    Height = 9
    Shape = bsTopLine
  end
  object Sep5: TBevel
    Left = 416
    Top = 329
    Width = 257
    Height = 7
    Shape = bsTopLine
  end
  object LabelSon: TLabel
    Left = 432
    Top = 144
    Width = 77
    Height = 13
    Caption = #201'mettre un son :'
  end
  object Sep6: TBevel
    Left = 417
    Top = 216
    Width = 256
    Height = 9
    Shape = bsTopLine
  end
  object LabelBord: TLabel
    Left = 432
    Top = 224
    Width = 185
    Height = 13
    Caption = 'Remplacer l'#39'ext'#233'rieur du labyrinthe par :'
  end
  object LabelNomBouton: TLabel
    Left = 432
    Top = 80
    Width = 79
    Height = 13
    Caption = 'Nom du bouton :'
  end
  object LabelCouleur: TLabel
    Left = 432
    Top = 280
    Width = 148
    Height = 13
    Caption = 'Changer la couleur du pion en :'
  end
  object Horizontal: TScrollBar
    Left = 0
    Top = 211
    Width = 210
    Height = 16
    LargeChange = 7
    PageSize = 0
    TabOrder = 0
    OnScroll = HorizontalScroll
  end
  object Vertical: TScrollBar
    Left = 211
    Top = 0
    Width = 16
    Height = 210
    Kind = sbVertical
    LargeChange = 7
    PageSize = 0
    TabOrder = 1
    OnScroll = VerticalScroll
  end
  object BoutonsEtage: TSpinButton
    Left = 211
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
    TabOrder = 2
    UpGlyph.Data = {
      DE000000424DDE00000000000000360000002800000009000000060000000100
      180000000000A800000000000000000000000000000000000000008080008080
      0080800080800080800080800080800080800080806500808000000000000000
      0000000000000000000000000000008080640080800080800000000000000000
      0000000000000000808000808006008080008080008080000000000000000000
      0080800080800080805600808000808000808000808000000000808000808000
      8080008080000080800080800080800080800080800080800080800080800080
      8020}
    OnDownClick = BoutonsEtageDownClick
    OnUpClick = BoutonsEtageUpClick
  end
  object EditNo: TSpinEdit
    Left = 548
    Top = 8
    Width = 113
    Height = 34
    EditorEnabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -20
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxValue = 45
    MinValue = 0
    ParentFont = False
    TabOrder = 3
    Value = 1
    OnChange = EditNoChange
  end
  object EditEtage: TSpinEdit
    Left = 92
    Top = 240
    Width = 73
    Height = 22
    MaxValue = 1
    MinValue = 1
    TabOrder = 4
    Value = 1
    OnChange = EditEtageChange
  end
  object CasesRemps: TListView
    Left = 236
    Top = 24
    Width = 173
    Height = 205
    Columns = <>
    IconOptions.Arrangement = iaLeft
    IconOptions.WrapText = False
    ReadOnly = True
    TabOrder = 5
    ViewStyle = vsSmallIcon
  end
  object BoutonAjouter: TButton
    Left = 237
    Top = 240
    Width = 81
    Height = 25
    Caption = 'Ajouter...'
    TabOrder = 6
    OnClick = BoutonAjouterClick
  end
  object BoutonSupprimer: TButton
    Left = 329
    Top = 240
    Width = 81
    Height = 25
    Caption = 'Supprimer'
    TabOrder = 7
    OnClick = BoutonSupprimerClick
  end
  object EditMessage: TMemo
    Left = 16
    Top = 304
    Width = 377
    Height = 65
    Lines.Strings = (
      'Information')
    ScrollBars = ssVertical
    TabOrder = 8
    WordWrap = False
    OnExit = SaveMessage
  end
  object CheckDesactiver: TCheckBox
    Left = 432
    Top = 104
    Width = 233
    Height = 17
    Caption = 'D'#233'sactiver le bouton lorsqu'#39'on passe dessus'
    TabOrder = 9
    OnClick = CheckDesactiverClick
  end
  object GroupMessage: TRadioGroup
    Left = 16
    Top = 376
    Width = 377
    Height = 65
    Caption = 'Type de l'#39'information'
    ItemIndex = 0
    Items.Strings = (
      'Message (toujours affich'#233')'
      
        'Indice (affich'#233' seulement si l'#39'option "Montrer les indices" est ' +
        'activ'#233'e)'
      
        'Impasse (pour les obstacles ; n'#39'est pas affich'#233' si on vient d'#39'un' +
        'e fl'#232'che)')
    TabOrder = 10
    OnClick = GroupMessageClick
  end
  object CheckUnique: TCheckBox
    Left = 14
    Top = 452
    Width = 369
    Height = 17
    Caption = 
      'L'#39'information doit '#234'tre montr'#233'e seulement au premier passage sur' +
      ' le bouton'
    TabOrder = 11
    OnClick = SaveMessage
  end
  object ComboStyle: TComboBox
    Left = 548
    Top = 48
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 12
    OnChange = ComboStyleChange
    Items.Strings = (
      'Poussoir'
      'Commutateur'
      'Borne Info'
      'Bouton cach'#233
      'Actions perso'
      'Objet perso'
      'Obstacle perso'
      'Direction perso')
  end
  object BoutonOK: TBitBtn
    Left = 480
    Top = 416
    Width = 137
    Height = 41
    TabOrder = 13
    Kind = bkOK
  end
  object BoutonAnnuler: TBitBtn
    Left = 480
    Top = 360
    Width = 137
    Height = 41
    TabOrder = 14
    Kind = bkCancel
  end
  object CheckUniqueSon: TCheckBox
    Left = 432
    Top = 192
    Width = 233
    Height = 17
    Caption = #201'mettre le son seulement la premi'#232're fois'
    TabOrder = 15
    OnClick = ComboSonChange
  end
  object ComboBord: TComboBox
    Left = 432
    Top = 240
    Width = 177
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 16
    OnChange = ComboBordChange
    Items.Strings = (
      '(Ne pas changer)'
      'Sortie'
      'Eau'
      'Murs'
      'Trous'
      'Herbe'
      'Ciel')
  end
  object EditNomBouton: TComboBox
    Left = 548
    Top = 72
    Width = 113
    Height = 21
    ItemHeight = 13
    Sorted = True
    TabOrder = 17
    Text = 'EditNomBouton'
  end
  object ComboSon: TComboBox
    Left = 432
    Top = 160
    Width = 233
    Height = 21
    ItemHeight = 13
    TabOrder = 18
    Text = 'ComboSon'
  end
  object ComboCouleur: TComboBox
    Left = 432
    Top = 296
    Width = 177
    Height = 21
    ItemHeight = 13
    TabOrder = 19
    OnChange = ComboCouleurChange
    Items.Strings = (
      '(Ne pas changer)'
      'Bleu'
      'Rouge'
      'Vert'
      'Jaune'
      'Blanc'
      'Noir'
      'Invisible')
  end
end
