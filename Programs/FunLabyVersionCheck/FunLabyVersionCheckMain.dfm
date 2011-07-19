object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'V'#233'rification des nouvelles versions de FunLabyrinthe'
  ClientHeight = 369
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    417
    369)
  PixelsPerInch = 120
  TextHeight = 13
  object GroupFrequency: TRadioGroup
    Left = 16
    Top = 136
    Width = 385
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'V'#233'rifications automatiques'
    ItemIndex = 0
    Items.Strings = (
      'V'#233'rifier tous les jours'
      'V'#233'rifier toutes les semaines'
      'Ne jamais v'#233'rifier')
    TabOrder = 0
  end
  object GroupProxy: TJvGroupBox
    Left = 16
    Top = 232
    Width = 385
    Height = 121
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Utiliser un proxy pour acc'#233'der '#224' Internet'
    TabOrder = 1
    Checkable = True
    PropagateEnable = True
    object LabelProxyServer: TLabel
      Left = 16
      Top = 24
      Width = 76
      Height = 13
      Caption = 'Serveur proxy :'
    end
    object LabelProxyPort: TLabel
      Left = 16
      Top = 48
      Width = 82
      Height = 13
      Caption = 'Num'#233'ro de port :'
    end
    object LabelProxyUsername: TLabel
      Left = 16
      Top = 72
      Width = 86
      Height = 13
      Caption = 'Nom d'#39'utilisateur :'
    end
    object LabelProxyPassword: TLabel
      Left = 16
      Top = 96
      Width = 71
      Height = 13
      Caption = 'Mot de passe :'
    end
    object EditProxyServer: TEdit
      Left = 160
      Top = 16
      Width = 209
      Height = 21
      TabOrder = 1
      TextHint = 'Adresse de votre serveur proxy'
    end
    object EditProxyPort: TEdit
      Left = 160
      Top = 40
      Width = 209
      Height = 21
      TabOrder = 2
      TextHint = 'Port utilis'#233' par votre serveur proxy'
    end
    object EditProxyUsername: TEdit
      Left = 160
      Top = 64
      Width = 209
      Height = 21
      TabOrder = 3
      TextHint = 'Votre nom d'#39'utilisateur pour le proxy'
    end
    object EditProxyPassword: TEdit
      Left = 160
      Top = 88
      Width = 209
      Height = 21
      PasswordChar = '*'
      TabOrder = 4
      TextHint = 'Votre mot de passe pour le proxy'
    end
  end
  object GroupCurrentVersion: TGroupBox
    Left = 16
    Top = 16
    Width = 385
    Height = 105
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = #201'tat actuel de votre version de FunLabyrinthe'
    TabOrder = 2
    object LabelCurrentVersion: TLabel
      Left = 16
      Top = 24
      Width = 158
      Height = 13
      Caption = 'Votre version de FunLabyrinthe :'
    end
    object LabelAvailableVersion: TLabel
      Left = 16
      Top = 43
      Width = 153
      Height = 13
      Caption = 'Version disponible sur Internet :'
    end
    object TextCurrentVersion: TStaticText
      Left = 240
      Top = 24
      Width = 129
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = '5.0.0.0'
      TabOrder = 0
    end
    object TextAvailableVersion: TStaticText
      Left = 240
      Top = 39
      Width = 129
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = '?'
      TabOrder = 1
    end
    object LinkGoToDownloadPage: TLinkLabel
      Left = 16
      Top = 83
      Width = 377
      Height = 24
      Caption = 
        '<a href="http://funlabyrinthe.game-host.org/download/">Aller '#224' l' +
        'a page de t'#233'l'#233'chargement de la derni'#232're version</a>'
      TabOrder = 2
      UseVisualStyle = True
      OnLinkClick = LinkGoToDownloadPageLinkClick
    end
    object ButtonCheckNow: TButton
      Left = 244
      Top = 53
      Width = 126
      Height = 24
      Caption = 'V'#233'rifier maintenant'
      TabOrder = 3
      OnClick = ButtonCheckNowClick
    end
  end
  object VersionInfoGrabber: TIdHTTP
    AllowCookies = False
    HandleRedirects = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.Accept = 'text/xml, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 312
    Top = 152
  end
  object OptionsStorage: TJvAppXMLFileStorage
    StorageOptions.BooleanStringTrueValues = 'true, yes, y'
    StorageOptions.BooleanStringFalseValues = 'false, no, n'
    StorageOptions.InvalidCharReplacement = '_'
    FileName = 'FunLabyrinthe\VersionCheckConfig.xml'
    Location = flUserFolder
    RootNodeName = 'configuration'
    SubStorages = <>
    Left = 216
    Top = 152
  end
end
