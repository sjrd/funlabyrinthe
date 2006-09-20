object FormAddMap: TFormAddMap
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Ajouter une carte'
  ClientHeight = 241
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelID: TLabel
    Left = 16
    Top = 16
    Width = 245
    Height = 13
    Caption = 'ID de la carte (lettres non accentu'#233'es et chiffres) :'
  end
  object ButtonBrowse: TSpeedButton
    Left = 280
    Top = 76
    Width = 25
    Height = 25
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FF00FF078DBE
      078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
      BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
      CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
      39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
      D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
      DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
      1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
      DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
      EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
      77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
      E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
      8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
      9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
      BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
      FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
      FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
    OnClick = ButtonBrowseClick
  end
  object LabelDimensions: TLabel
    Left = 32
    Top = 144
    Width = 113
    Height = 13
    Caption = 'Dimensions (en cases) :'
  end
  object LabelZoneSize: TLabel
    Left = 32
    Top = 168
    Width = 137
    Height = 13
    Caption = 'Taile d'#39'une zone (en cases) :'
  end
  object EditID: TEdit
    Left = 16
    Top = 32
    Width = 289
    Height = 21
    TabOrder = 0
  end
  object RadioExistingMap: TRadioButton
    Left = 16
    Top = 64
    Width = 265
    Height = 17
    Caption = 'Charger une carte existante :'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RadioExistingMapClick
  end
  object EditFileName: TEdit
    Left = 32
    Top = 80
    Width = 249
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object RadioNewMap: TRadioButton
    Left = 16
    Top = 112
    Width = 265
    Height = 17
    Caption = 'Cr'#233'er une nouvelle carte'
    TabOrder = 3
    OnClick = RadioNewMapClick
  end
  object ButtonOK: TButton
    Left = 56
    Top = 200
    Width = 97
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 168
    Top = 200
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 7
  end
  object EditDimensions: TMaskEdit
    Left = 184
    Top = 136
    Width = 121
    Height = 21
    Enabled = False
    EditMask = '!999x999x99;1;0'
    MaxLength = 10
    TabOrder = 4
    Text = '  7x  7x 1'
  end
  object EditZoneSize: TMaskEdit
    Left = 184
    Top = 160
    Width = 121
    Height = 21
    Enabled = False
    EditMask = '!99x99;1;0'
    MaxLength = 5
    TabOrder = 5
    Text = ' 7x 7'
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'flm'
    Filter = 'Cartes FunLabyrinthe (*.flm)|*.flm'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Charger une carte existante'
    Left = 16
    Top = 200
  end
end
