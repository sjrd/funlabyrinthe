object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'V'#233'rification des nouvelles versions de FunLabyrinthe'
  ClientHeight = 483
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    545
    483)
  PixelsPerInch = 120
  TextHeight = 17
  object GroupFrequency: TRadioGroup
    Left = 21
    Top = 178
    Width = 503
    Height = 106
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
    Left = 21
    Top = 303
    Width = 503
    Height = 159
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Utiliser un proxy pour acc'#233'der '#224' Internet'
    TabOrder = 1
    Checkable = True
    PropagateEnable = True
    object LabelProxyServer: TLabel
      Left = 21
      Top = 31
      Width = 98
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Serveur proxy :'
    end
    object LabelProxyPort: TLabel
      Left = 21
      Top = 63
      Width = 107
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Num'#233'ro de port :'
    end
    object LabelProxyUsername: TLabel
      Left = 21
      Top = 94
      Width = 110
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Nom d'#39'utilisateur :'
    end
    object LabelProxyPassword: TLabel
      Left = 21
      Top = 126
      Width = 89
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Mot de passe :'
    end
    object EditProxyServer: TEdit
      Left = 209
      Top = 21
      Width = 274
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 1
      TextHint = 'Adresse de votre serveur proxy'
    end
    object EditProxyPort: TEdit
      Left = 209
      Top = 52
      Width = 274
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
      TextHint = 'Port utilis'#233' par votre serveur proxy'
    end
    object EditProxyUsername: TEdit
      Left = 209
      Top = 84
      Width = 274
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 3
      TextHint = 'Votre nom d'#39'utilisateur pour le proxy'
    end
    object EditProxyPassword: TEdit
      Left = 209
      Top = 115
      Width = 274
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      PasswordChar = '*'
      TabOrder = 4
      TextHint = 'Votre mot de passe pour le proxy'
    end
  end
  object GroupCurrentVersion: TGroupBox
    Left = 21
    Top = 21
    Width = 503
    Height = 137
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = #201'tat actuel de votre version de FunLabyrinthe'
    TabOrder = 2
    object LabelCurrentVersion: TLabel
      Left = 21
      Top = 31
      Width = 201
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Votre version de FunLabyrinthe :'
    end
    object LabelAvailableVersion: TLabel
      Left = 21
      Top = 56
      Width = 192
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Version disponible sur Internet :'
    end
    object TextCurrentVersion: TStaticText
      Left = 314
      Top = 31
      Width = 169
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taRightJustify
      AutoSize = False
      Caption = '5.0.0.0'
      TabOrder = 0
    end
    object TextAvailableVersion: TStaticText
      Left = 314
      Top = 51
      Width = 169
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taRightJustify
      AutoSize = False
      Caption = '?'
      TabOrder = 1
    end
    object LinkGoToDownloadPage: TLinkLabel
      Left = 21
      Top = 109
      Width = 377
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        '<a href="http://www.funlabyrinthe.com/download/">Aller '#224' la page' +
        ' de t'#233'l'#233'chargement de la derni'#232're version</a>'
      TabOrder = 2
      UseVisualStyle = True
      OnLinkClick = LinkGoToDownloadPageLinkClick
    end
    object ButtonCheckNow: TButton
      Left = 319
      Top = 69
      Width = 165
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
    Left = 384
    Top = 216
  end
  object OptionsStorage: TJvAppXMLFileStorage
    StorageOptions.BooleanStringTrueValues = 'true, yes, y'
    StorageOptions.BooleanStringFalseValues = 'false, no, n'
    StorageOptions.InvalidCharReplacement = '_'
    FileName = 'FunLabyrinthe\VersionCheckConfig.xml'
    Location = flUserFolder
    RootNodeName = 'configuration'
    SubStorages = <>
    Left = 256
    Top = 216
  end
end
