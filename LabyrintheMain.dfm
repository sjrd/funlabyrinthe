object FormMain: TFormMain
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
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 0
    Width = 270
    Height = 270
    Align = alClient
    Center = True
  end
  object StatusBar: TStatusBar
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
    Left = 40
    Top = 120
    object BigMenuFile: TMenuItem
      Caption = '&Fichier'
      object MenuNewGame: TMenuItem
        Caption = 'Nouvelle Partie'
        ShortCut = 113
        OnClick = MenuNewGameClick
      end
      object MenuReloadGame: TMenuItem
        Caption = 'Recommencer'
        Enabled = False
        ShortCut = 114
        OnClick = MenuReloadGameClick
      end
      object MenuLoadGame: TMenuItem
        Caption = 'Charger'
        ShortCut = 16451
        OnClick = MenuLoadGameClick
      end
      object MenuSaveGame: TMenuItem
        Caption = 'Enregistrer'
        Enabled = False
        ShortCut = 16467
        OnClick = MenuSaveGameClick
      end
      object Sep1: TMenuItem
        Caption = '-'
      end
      object MenuDescription: TMenuItem
        Caption = 'Description'
        Enabled = False
        OnClick = MenuDescriptionClick
      end
      object MenuProperties: TMenuItem
        Caption = 'Propri'#233't'#233's'
        Enabled = False
        object MenuMapProperties: TMenuItem
          Caption = 'Carte'
          OnClick = MenuMapPropertiesClick
        end
        object MenuPlayerProperties: TMenuItem
          Caption = 'Joueur'
          OnClick = MenuPlayerPropertiesClick
        end
      end
      object Sep2: TMenuItem
        Caption = '-'
      end
      object MenuExit: TMenuItem
        Caption = 'Quitter'
        ShortCut = 16465
        OnClick = MenuExitClick
      end
    end
    object BigMenuOptions: TMenuItem
      Caption = '&Options'
      object MenuViewSize: TMenuItem
        Caption = 'Taille de la vue'
        Enabled = False
        OnClick = MenuViewSizeClick
      end
    end
    object BigMenuHelp: TMenuItem
      Caption = '&Aide'
      object MenuHelpTopics: TMenuItem
        Caption = 'Rubriques d'#39'aide'
        ShortCut = 112
        OnClick = MenuHelpTopicsClick
      end
      object Sep3: TMenuItem
        Caption = '-'
      end
      object MenuAbout: TMenuItem
        Caption = #192' propos...'
        OnClick = MenuAboutClick
      end
    end
  end
  object NewGameDialog: TOpenDialog
    DefaultExt = 'flp'
    Filter = 'Projet FunLabyrinthe (*.flp)|*.flp;*.url'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Nouvelle partie'
    Left = 72
    Top = 120
  end
  object SaveGameDialog: TSaveDialog
    DefaultExt = 'flg'
    Filter = 'Projet FunLabyrinthe (*.flp)|*.flp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Enregistrer la partie'
    Left = 136
    Top = 120
  end
  object LoadGameDialog: TOpenDialog
    DefaultExt = 'flp'
    Filter = 'Projet FunLabyrinthe (*.flp)|*.flp'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Charger une partie'
    Left = 104
    Top = 120
  end
  object TimerUpdateImage: TTimer
    Enabled = False
    Interval = 100
    OnTimer = UpdateImage
    Left = 168
    Top = 120
  end
end
