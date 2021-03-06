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
  Font.Height = -14
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
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object StatusBar: TStatusBar
    Left = 0
    Top = 270
    Width = 270
    Height = 19
    Panels = <>
    SizeGrip = False
  end
  object PaintBox: TPaintBox32
    Left = 0
    Top = 0
    Width = 270
    Height = 270
    Align = alClient
    TabOrder = 1
    OnPaintBuffer = PaintBoxPaintBuffer
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
      object MenuPlayerObjects: TMenuItem
        Caption = 'Vos objets'
        Enabled = False
        OnClick = MenuPlayerObjectsClick
      end
      object Sep2: TMenuItem
        Caption = '-'
      end
      object MenuOpenProjectManager: TMenuItem
        Caption = 'Gestionnaire de projets...'
        OnClick = MenuOpenProjectManagerClick
      end
      object Sep4: TMenuItem
        Caption = '-'
      end
      object MenuExit: TMenuItem
        Caption = 'Quitter'
        ShortCut = 16465
        OnClick = MenuExitClick
      end
    end
    object BigMenuScreenshot: TMenuItem
      Caption = '&Screenshot'
      Visible = False
      object MenuScreenshotToClipboard: TMenuItem
        Caption = 'Vers presse-papier'
        ShortCut = 16499
        OnClick = MenuScreenshotToClipboardClick
      end
      object MenuScreenshotToFile: TMenuItem
        Caption = 'Vers fichier...'
        ShortCut = 8307
        OnClick = MenuScreenshotToFileClick
      end
      object MenuScreenshotToFileAuto: TMenuItem
        Caption = 'Screenshot rapide'
        ShortCut = 115
        OnClick = MenuScreenshotToFileAutoClick
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
      object MenuVersionCheck: TMenuItem
        Caption = 'Mise '#224' jour automatique'
        OnClick = MenuVersionCheckClick
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
  object OptionsStorage: TJvAppXMLFileStorage
    StorageOptions.BooleanStringTrueValues = 'true, yes, y'
    StorageOptions.BooleanStringFalseValues = 'false, no, n'
    StorageOptions.InvalidCharReplacement = '_'
    FileName = 'FunLabyrinthe\FunLabyrinthe.xml'
    Location = flUserFolder
    RootNodeName = 'configuration'
    SubStorages = <>
    Left = 40
    Top = 184
  end
  object SaveScreenshotDialog: TSaveDialog
    DefaultExt = 'png'
    Filter = 'Image Portable Network Graphic (*.png)|*.png'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Enregistrer le screenshot'
    Left = 136
    Top = 184
  end
end
