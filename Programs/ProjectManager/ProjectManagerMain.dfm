object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Gestionnaire de projets'
  ClientHeight = 482
  ClientWidth = 737
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
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
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object ActionMainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 737
    Height = 30
    UseSystemFont = False
    ActionManager = ActionManager
    Caption = 'ActionMainMenuBar'
    Color = clMenuBar
    ColorMap.HighlightColor = clWhite
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
  end
  object ActionToolBar: TActionToolBar
    Left = 0
    Top = 30
    Width = 737
    Height = 26
    ActionManager = ActionManager
    Caption = 'ActionToolBar'
    Color = clMenuBar
    ColorMap.HighlightColor = clWhite
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object PageControl: TPageControl
    Left = 0
    Top = 56
    Width = 737
    Height = 426
    ActivePage = TabProjects
    Align = alClient
    TabOrder = 2
    object TabProjects: TTabSheet
      Caption = 'Projets'
      object ListViewProjects: TListView
        Left = 0
        Top = 0
        Width = 729
        Height = 395
        Align = alClient
        Columns = <
          item
            Caption = 'Nom du fichier'
            Width = 200
          end
          item
            Caption = 'Titre'
            Width = 200
          end
          item
            Caption = 'Version install'#233'e'
            Width = 100
          end
          item
            Caption = 'Version disponible'
            Width = 100
          end
          item
            Caption = 'Genre'
            Width = 100
          end
          item
            Caption = 'Difficult'#233
            Width = 100
          end
          item
            Caption = 'Auteur'
            Width = 150
          end>
        Groups = <
          item
            Header = 'Vos projets'
            GroupID = 0
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
            ExtendedImage = -1
          end
          item
            Header = 'Projets install'#233's sur cet ordinateur'
            GroupID = 1
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
            ExtendedImage = -1
          end
          item
            Header = 'Projets disponibles sur Internet'
            GroupID = 2
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
            ExtendedImage = -1
          end>
        GroupView = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = ListViewPopupActionBar
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = ListViewProjectsSelectItem
      end
    end
    object TabLibrary: TTabSheet
      Caption = 'Biblioth'#232'que'
      ImageIndex = 1
      object ListViewLibrary: TListView
        Left = 0
        Top = 0
        Width = 729
        Height = 395
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            AutoSize = True
            Caption = 'Fichier'
          end
          item
            Caption = 'SHA1 local'
            Width = 100
          end
          item
            Caption = 'SHA1 distant'
            Width = 100
          end
          item
            Caption = #201'tat'
            Width = 100
          end
          item
            Caption = 'Action'
            Width = 150
          end>
        Groups = <
          item
            Header = 'Fichiers modifi'#233's localement'
            GroupID = 0
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
            ExtendedImage = -1
          end
          item
            Header = 'Fichiers pouvant '#234'tre ajout'#233's ou mis '#224' jour'
            GroupID = 1
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
            ExtendedImage = -1
          end
          item
            Header = 'Fichiers '#224' jour'
            GroupID = 2
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
            ExtendedImage = -1
          end>
        MultiSelect = True
        GroupView = True
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnItemChecked = ListViewLibraryItemChecked
      end
    end
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = ActionRefresh
                Caption = '&Rafra'#238'chir'
                ImageIndex = 0
                ShortCut = 116
              end
              item
                Action = ActionExit
                Caption = '&Quitter'
                ImageIndex = 1
              end>
            Caption = '&Fichier'
          end
          item
            Items = <
              item
                Action = ActionInstall
                Caption = '&Installer / Mettre '#224' jour'
                ImageIndex = 2
              end
              item
                Action = ActionExport
                Caption = '&Exporter en zip'
                ImageIndex = 3
              end
              item
                Action = ActionOwnProject
                Caption = '&Consid'#233'rer comme mon projet'
              end
              item
                Caption = '-'
              end
              item
                Action = ActionRun
                Caption = '&Lancer le jeu'
                ImageIndex = 4
              end>
            Caption = '&Projet'
          end
          item
            Items = <
              item
                Action = ActionApplyLibraryChanges
                Caption = '&Mettre '#224' jour'
                ImageIndex = 5
              end>
            Caption = '&Biblioth'#232'que'
          end>
        ActionBar = ActionMainMenuBar
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = ActionRefresh
            Caption = '&Rafra'#238'chir'
            ImageIndex = 0
            ShortCut = 116
          end
          item
            Action = ActionExit
            Caption = '&Quitter'
            ImageIndex = 1
          end
          item
            Caption = '-'
          end
          item
            Action = ActionInstall
            Caption = '&Installer / Mettre '#224' jour'
            ImageIndex = 2
          end
          item
            Action = ActionExport
            Caption = '&Exporter en zip'
            ImageIndex = 3
          end
          item
            Caption = '-'
          end
          item
            Action = ActionRun
            Caption = '&Lancer le jeu'
            ImageIndex = 4
          end
          item
            Caption = '-'
          end
          item
            Action = ActionApplyLibraryChanges
            Caption = '&Mettre '#224' jour'
            ImageIndex = 5
          end>
        ActionBar = ActionToolBar
      end>
    Images = Images
    Left = 88
    Top = 136
    StyleName = 'Platform Default'
    object ActionRefresh: TAction
      Category = 'File'
      Caption = 'Rafra'#238'chir'
      ImageIndex = 0
      ShortCut = 116
      OnExecute = ActionRefreshExecute
    end
    object ActionExit: TAction
      Category = 'File'
      Caption = 'Quitter'
      ImageIndex = 1
      OnExecute = ActionExitExecute
    end
    object ActionInstall: TAction
      Category = 'Project'
      Caption = 'Installer / Mettre '#224' jour'
      Enabled = False
      Hint = 'Installer ou mettre '#224' jour le projet s'#233'lectionn'#233
      ImageIndex = 2
      OnExecute = ActionInstallExecute
    end
    object ActionExport: TAction
      Category = 'Project'
      Caption = 'Exporter en zip'
      Enabled = False
      Hint = 'Cr'#233'er un zip de ce projet, qui peut '#234'tre publi'#233' sur le site'
      ImageIndex = 3
      OnExecute = ActionExportExecute
    end
    object ActionOwnProject: TAction
      Category = 'Project'
      Caption = 'Consid'#233'rer comme mon projet'
      Enabled = False
      Hint = 'Consid'#233'rer ce projet comme mon projet'
      OnExecute = ActionOwnProjectExecute
    end
    object ActionRun: TAction
      Category = 'Project'
      Caption = 'Lancer le jeu'
      Enabled = False
      Hint = 'Lancer le jeu'
      ImageIndex = 4
      OnExecute = ActionRunExecute
    end
    object ActionApplyLibraryChanges: TAction
      Category = 'Library'
      Caption = 'Mettre '#224' jour'
      Hint = 'Appliquer les mises '#224' jour s'#233'lectionn'#233'es pour la biblioth'#232'que'
      ImageIndex = 5
      OnExecute = ActionApplyLibraryChangesExecute
    end
  end
  object Images: TImageList
    Left = 88
    Top = 192
    Bitmap = {
      494C01010600C800840010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006D332700853C130095440D0096450D00873D1200703425000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D6FFEF0000946300009C630000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007037
      2A0070372A00CD772700E8AD7000F3CCA100F4CDA300E9B17600D07C2C006F35
      29006F3529000000000000000000000000000000000000000000000000000000
      000000000000D6FFEF00006B4A0000845A00004A3100007B5200FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000086411D00C062
      0B00F0C29200FFFEFA00FDFAF600F5E3D100F5E2D000FDF8F400FFFFFD00F2C9
      9E00C66911007B3A210000000000000000000000000000000000000000000000
      00009CFFDE00006B4A0008A5730042D6A50010AD7B00004A3100006B4A00D6FF
      EF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C451C00C1610700F7DB
      BD00FFFEFE00E0A46B00CE6D1300C75C0000C9610000CE6E1200DE9D5F00FDFA
      F700FAE5CC00C6680D006F3528000000000000000000000000000000000063F7
      C6000052390018B584004AE7B50031CE940021BD8C0008A5730000523900005A
      42009CFFDE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C451C00ECBD8B00FFFF
      FF00DA8F4300C6560000FFFFFF00DC975100C75B0000CA620000C75B0000D583
      3300FDFAF800F3CB9F006F35280000000000000000000000000052EFBD000052
      390021B584004AE7B50039D69C0029BD8C0018B5840008A57300009463000052
      3900004A310063FFCE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A04D1000CE772100FFFDFB00E8B6
      8400D06B0400D06B0700FFFFFF00FFFFFF00E1A87000C95E0000CA630000C75B
      0000DFA06100FFFFFF00CF7B2800703525000000000042D6A5000052390021B5
      84004AE7B50039D69C0029C6940021BD840010AD7B00009C630000946300007B
      5200004A3100004A31005AF7BD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AF550700E5AA6F00FFFFFF00DD8F
      3F00DA812800D87B1E00FFFFFF00FFFFFF00FFFFFF00EAC19800CC670800C95F
      0000CE6E0D00FDFAF600E9B175007035250029C69400005A420029BD8C0042DE
      A50031CE940029C6940021BD840010AD7B0008A573000094630000845A000073
      4A00006B4A00004A31000039290042DEA5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BB5F0A00F0CAA100FCF4ED00E193
      4300E2924200DF8A3400FFFFFF00FFFFFF00FFFFFF00FFFFFF00F3DEC600CF70
      1700C95F0000F5E3D000F3CEA40070352500004A3100009C630018B5840021BD
      840021BD840018B5840010AD7B0008A57300009C630000845A00007B5200006B
      4A00005A420000523900004A3100006B4A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C1650F00F2CDA600FDF7F000E9A1
      5800E9A05600E6994A00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EBC39B00CD6A
      0D00C9610000F6E6D400F3CCA100703525000094630000734A00004A310010AD
      7B0029C6940021B5840008A573000094630000845A00007B520000734A000063
      4200003121000063420000734A0031CE9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C1640D00EEBC8800FFFFFF00F1B8
      7C00F0AE6900EEA75F00FFFFFF00FFFFFF00FFFFFF00E7B17A00CE690200C961
      0000CF711100FEFCFA00E7AC6D0070352500000000000000000000734A0018B5
      840042D6A50031CE9C0031CE940010AD7B00007B5200006B4A00006342000052
      390000312100D6FFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BF600600E5A05900FFFDFA00FBE0
      C400F8BA7B00F4B47100FFFFFF00FFFFFF00E8AB6D00D87B1D00D2741400C85C
      0000E2AA7100FFFFFE00CC75200070352500000000000000000000734A0021BD
      8C004AE7B50042DEA5004AE7B5004AE7B50031CE9C0008A5730000734A000052
      390000312100D6FFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3620400FAD9B800FFFF
      FF00FEDCB800F7B87700FFFFFF00EBAD6D00E08D3700DA822800D06B0500DB92
      4A00FFFFFF00EFC08C006B342C00000000000000000000000000006B4A0029C6
      94005AF7BD004AE7B5004AE7B5004AE7B50052EFBD0052EFBD004AE7B500009C
      630000312100BDFFE70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3620400E79E5500FEEB
      D700FFFFFF00FBDFC300F1B57800E89F5500E2944500DE914200E8B78600FFFF
      FF00F6D8B700BE5F06006B342C00000000000000000000000000006B4A0031CE
      940063FFCE005AF7BD005AF7BD005AF7BD005AF7BD005AF7BD008CFFDE0021BD
      8C0000312100BDFFE70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6670C00E69E
      5500FAD9B600FFFBF600FFFFFF00FEF8F200FDF6EF00FFFFFF00FEF9F200ECB8
      8400BE5F090075382600000000000000000000000000000000000063420042D6
      A500D6FFEF009CFFDE009CFFDE009CFFDE009CFFDE009CFFDE00D6FFEF0029C6
      940000312100B5FFE70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C060
      0500C0600500E49F5A00EEBA8600F2CAA000F0C59900E4A76800CC741E00783A
      2700783A27000000000000000000000000000000000000000000005A420021BD
      840063FFCE005AF7C6005AF7C6005AF7C6005AF7C6005AF7C60063FFCE0010AD
      7B0000312100D6FFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B65C0A00B8601200B9611300B25A0F00A24F0E008E451A000000
      000000000000000000000000000000000000000000000000000031CE94000039
      2900004A3100004A3100004A3100004A3100004A3100004A3100004A31000039
      290021BD84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A400C2A6A400C2A6A40000000000000000000000000000000000000000000000
      00000000000001079F000313A9000418AE000419AE000313A9000108A0000000
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000C2A6A400FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00C2A6A40000000000000000000000000000000000000000000104
      9D00041CB1000730C0000734C4000735C5000735C5000734C3000731C100041F
      B30001069E00000000000000000000000000078DBE0025A1D10072C7E70085D7
      FA0066CDF90065CDF90065CDF90065CDF90065CDF80065CDF90065CDF80066CE
      F90039ADD800078DBE000000000000000000078DBE0063CBF800078DBE00A3E1
      FB0066CDF90065CDF80065CDF90065CDF90065CDF80065CDF90065CDF80066CD
      F8003AADD800ACE7F500078DBE00000000000000000000000000C2A6A400FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00D8EBD600018A0200018A0200D8EBD600FEFC
      FB00FEFCFB00C2A6A400000000000000000000000000000000000109A100052B
      C3000735C7000733C2000732C2000732C2000732C2000732C2000733C3000735
      C400062DBE00020CA4000000000000000000078DBE004CBCE70039A8D100A0E2
      FB006FD4FA006FD4F9006ED4FA006FD4F9006FD4FA006FD4FA006FD4FA006ED4
      F9003EB1D90084D7EB00078DBE0000000000078DBE006AD1F900078DBE00A8E5
      FC006FD4FA006FD4F9006ED4FA006FD4F9006FD4FA006FD4FA006FD4FA006ED4
      F9003EB1D900B1EAF500078DBE00000000000000000000000000C2A6A400FEFB
      F700FEFBF700018A0200D8EAD200018A0200D8EAD200D8EAD200018A0200FEFB
      F700FEFBF700C2A6A40000000000000000000000000001049B00052BCA000636
      D8000431CD000027C400032EC1000732C2000732C2000430C1000027BF00042F
      C1000735C400072EBE0001069E0000000000078DBE0072D6FA00078DBE00AEEA
      FC0079DCFB0079DCFB0079DCFB0079DCFB0079DCFB007ADCFB0079DCFA0079DC
      FA0044B5D900AEF1F900078DBE0000000000078DBE0072D6FA00078DBE00AEEA
      FC0079DCFB0079DCFB0079DCFB0079DCFB0079DCFB007ADCFB0079DCFA0079DC
      FA0044B5D900B6EEF600078DBE00000000000000000000000000C2A6A400FEF9
      F400FEF9F400018A0200018A0200D8E8D000FEF9F400FEF9F400D8E8D000FEF9
      F400FEF9F400C2A6A400000000000000000000000000031ABA000537E7000331
      DD00123DD8006480E0001840CB00002CC100022DC0000F38C4006580D9001B43
      C700052FC1000735C500051FB30000000000078DBE0079DDFB001899C7009ADF
      F30092E7FB0084E4FB0083E4FC0083E4FC0084E4FC0083E4FC0083E4FB0084E5
      FC0048B9DA00B3F4F900078DBE0000000000078DBE0079DDFB00078DBE00B5EE
      FD0083E4FB0044B181000A8313000C8D170044B0810079DBE90083E4FB0084E5
      FC0048B9DA00BBF2F600078DBE00000000000000000000000000C2A6A400FEF7
      F000FEF7F000018A0200018A0200018A0200FEF7F000FEF7F000FEF7F000FEF7
      F000FEF7F000C2A6A400000000000000000001049E000430E4000436F100002A
      E4005070E900FFFFFF00B7C4F1000D36CA00042DC300A2B2E800FFFFFF006984
      DA000026BE000733C3000731C1000108A000078DBE0082E3FC0043B7DC0065C3
      E000ACF0FD008DEBFC008DEBFC008DEBFD008DEBFD008DEBFC008DEBFD000C85
      18004CBBDA00B6F7F9006DCAE000078DBE00078DBE0081E2F900078DBD00BAF3
      FD008DEBFC008DEBFC0053BE96000D9718000C981800279747008AE9F8008DEB
      FC004CBBDA00BEF4F700078DBE00000000000000000000000000C2A6A400FEF5
      EC00FEF5EC00FEF5EC00FEF5EC00FEF5EC00018A0200018A0200018A0200FEF5
      EC00FEF5EC00C2A6A4000000000000000000020FAF000336FA000335F8000232
      EE000A35E8008CA2F200FFFFFF00B4C2F100A9B8ED00FFFFFF00A7B7E900133A
      C400052FC1000732C2000734C4000313AA00078DBE008AEAFC0077DCF300229C
      C600FDFFFF00C8F7FE00C9F7FE00C9F7FE00C9F7FE00C8F7FE000C8518003CBC
      5D000C851800DEF9FB00D6F6F900078DBE00078DBE0089EAFB00078DBD00FFFF
      FF00C9F7FE00C8F7FE00C9F7FE0073C396000E9D1B000C96170033994600C8F7
      FE009BD5E700DEF9FB00078DBE00000000000000000000000000C2A6A400FEF3
      E900FEF3E900D8E3C700FEF3E900FEF3E900D8E3C700018A0200018A0200FEF3
      E900FEF3E900C2A6A40000000000000000000619BC001747FE00093AFC000435
      F8000131F000002BE80091A5F400FFFFFF00FFFFFF00ABBAEF00062FC500022D
      C0000732C2000732C2000736C5000419AE00078DBE0093F0FE0093F0FD001697
      C500078DBE00078DBE00078DBE00078DBE00078DBE000C85180052D97F0062ED
      970041C465000C851800078DBE00078DBE00078DBE0093F0FE00078DBE00078D
      BE00078DBE00078DBE00078DBE0007868A000E9B1A000FA71C0008822200078A
      AF00078DBE00078DBE00078DBE00000000000000000000000000C2A6A400FFF1
      E500FFF1E500018A0200D9E2C300D9E2C300018A0200D9E2C300018A0200FFF1
      E500FFF1E500C2A6A40000000000000000000B1DBE004168FE001C49FC000335
      FB000031F9000531F200A4B5F700FFFFFF00FFFFFF00B9C6F2000D36D000002C
      C6000732C2000732C2000736C5000418AD00078DBE009BF5FE009AF6FE009AF6
      FE009BF5FD009BF6FE009AF6FE009BF5FE000C85180046CE6C0059E4880058E1
      880061EB940040C165000C85180000000000078DBE009BF5FE009AF6FE009AF6
      FE009BF5FD009BF6FE009AF6FE0076D4C1001698260016AF26000C94180064C5
      A7000989BA000000000000000000000000000000000000000000C2A6A400FFF0
      E200FFF0E200D9E1C100018A0200018A0200D9E1C100DDCFC200DDCFC200DDCF
      C200DDCFC200C2A6A40000000000000000000613B4005B7CFC00486CFD000133
      FB00113CFB00A1B4FE00FFFFFF00A4B6F80092A7F500FFFFFF00B6C4F2001A41
      D300042FC8000732C4000734C3000212A900078DBE00FEFEFE00A0FBFF00A0FB
      FE00A0FBFE00A1FAFE00A1FBFE000C8518000C8518000C8518000C85180056E1
      840047CD6E000C8518000C8518000C851800078DBE00FEFEFE00A0FBFF00A0FB
      FE00A0FBFE00A1FAFE00A1FBFE0086E2D5001F9E340025BB3D0014A4230045AC
      6F000989BA000000000000000000000000000000000000000000C2A6A400FFEE
      DE00FFEEDE00FFEEDE00FFEEDE00FFEEDE00FFEEDE00C5B5A900C3B4A800C2B3
      A700C1B2A600C2A6A40000000000000000000003A0004A6AF3008FA6FF001F46
      FB004C6FFC00FFFFFF00A7B8FE000733F600002AED008CA2F600FFFFFF00627F
      E7000028D0000734CC000730C30000069F0000000000078DBE00FEFEFE00A5FE
      FF00A5FEFF00A5FEFF00078CB60043B7DC0043B7DC0043B7DC000C8518004EDD
      790036BA54000C851800000000000000000000000000078DBE00FEFEFE00A5FE
      FF000C8518000C8518000C8518000C85180027A9420034C5520023B539000C85
      18000C8518000C8518000C851800000000000000000000000000C2A6A400FFEC
      DA00FFECDA00FFECDA00FFECDA00FFECDA00FFECDA00B0A29600B0A29600B0A2
      9600B0A29600C2A6A4000000000000000000000000001A2FCB0099AFFF008BA2
      FE00214DFB004D71FC000E3DFB000030FB000031F7000636F1004C6EF100103C
      E3000432DB000636D700041CB500000000000000000000000000078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000000000000C85180040D0
      65000C8518000000000000000000000000000000000000000000078DBE00078D
      BE00078CAE000C8518002095320050D97B004BD575003ECB620032C1500026B8
      3E00159E24000C85180000000000000000000000000000000000C2A6A400FFEA
      D700FFEAD700FFEAD700FFEAD700FFEAD700C9B9AC00FBF8F400FBF8F400E6DA
      D900C2A6A400000000000000000000000000000000000004A000415EEC00B8C7
      FF009CAFFD003A5CFC000A3AFB000335FB000335FB000133F900052FF2000635
      EB000537E900052CCD0000049C00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C8518002AB743002DBA
      49000C8518000000000000000000000000000000000000000000000000000000
      000000000000000000000C85180032AE4E005CE68E004FD8780043D068002EBA
      4B000C8518000000000000000000000000000000000000000000C2A6A400FFE8
      D300FFE8D300FFE8D300FFE8D300FFE8D300C9B9AC00FBF8F400DFCEC700C2A6
      A4000000000000000000000000000000000000000000000000000309A5004260
      EC00A9BBFF00BDCAFF008EA5FE006483FD005073FC004A6EFD003961FD001444
      F900042CD7000109A20000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C85180021B538000C85
      1800000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C8518003EBE600061EA93004ED677000C85
      1800000000000000000000000000000000000000000000000000C2A6A400FFE6
      D000FFE6D000FFE6D000FFE6D000FFE6D000C9B9AC00DFCEC700C2A6A4000000
      0000000000000000000000000000000000000000000000000000000000000004
      A0001E32CD005876F600859EFE008BA3FF007994FE005376FC00234AF000051E
      C50001049C000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C8518000C8518000C8518000C8518000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000C85180046C86C000C8518000000
      0000000000000000000000000000000000000000000000000000C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000004A0000917B6001022C3000D1FC2000311B40001059F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C8518000C8518000C8518000C85180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C851800000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00F81FFC7F00000000E007F81F00000000
      C003F00F000000008001E007000000008001C003000000000000800100000000
      0000000000000000000000000000000000000000000000000000C00300000000
      0000C003000000008001C003000000008001C00300000000C003C00300000000
      E007C00300000000F81FC00700000000C003F81F80038003C003E00700030001
      C003C00300010001C003800100010001C003800100010001C003000000000001
      C003000000000001C003000000000001C003000000010007C003000000000007
      C003000080038001C0038001C3C7C003C0078001FF87FC07C00FC003FF8FFE0F
      C01FE007FE1FFF1FC03FF81FF87FFFBF00000000000000000000000000000000
      000000000000}
  end
  object ListViewPopupActionBar: TPopupActionBar
    Images = Images
    Left = 224
    Top = 208
    object MenuInstall: TMenuItem
      Action = ActionInstall
    end
    object MenuExport: TMenuItem
      Action = ActionExport
    end
    object MenuOwnProject: TMenuItem
      Action = ActionOwnProject
    end
    object MenuSep1: TMenuItem
      Caption = '-'
    end
    object MenuRun: TMenuItem
      Action = ActionRun
    end
  end
  object Grabber: TIdHTTP
    OnWork = GrabberWork
    OnWorkBegin = GrabberWorkBegin
    OnWorkEnd = GrabberWorkEnd
    AllowCookies = False
    HandleRedirects = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.Accept = 'text/xml, application/zip, application/x-gzip, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 312
    Top = 152
  end
  object AbZipper: TAbZipper
    AutoSave = False
    DOSMode = False
    StoreOptions = [soStripDrive, soRemoveDots, soRecurse]
    Left = 88
    Top = 288
  end
  object ExportDialog: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Archive zip (*.zip)|*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing, ofDontAddToRecent]
    Title = 'Exporter le projet'
    Left = 88
    Top = 344
  end
  object AbUnZipper: TAbUnZipper
    ExtractOptions = [eoCreateDirs, eoRestorePath]
    Left = 160
    Top = 288
  end
  object ProgressDialog: TJvProgressDialog
    Caption = 'T'#233'l'#233'chargement des fichiers'
    ShowCancel = False
    Left = 432
    Top = 224
  end
  object IdAntiFreeze: TIdAntiFreeze
    Left = 408
    Top = 152
  end
end
