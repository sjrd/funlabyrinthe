object FormPlan: TFormPlan
  Left = 549
  Top = 119
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Plan du labyrinthe'
  ClientHeight = 361
  ClientWidth = 228
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000999999999999000092
    2222222229000092222222222900009999229999990000922222222229000092
    2222222229000092299999922900009229B3222229000092293B222229000099
    999999999900000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000C0030000C0030000C0030000C0030000C0030000C003
    0000C0030000C0030000C0030000C0030000FFFF0000FFFF0000FFFF0000}
  OldCreateOrder = True
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 0
    Width = 210
    Height = 210
    Stretch = True
    OnMouseDown = ImageMouseDown
  end
  object LabelEtage: TLabel
    Left = 57
    Top = 240
    Width = 34
    Height = 13
    Caption = 'Etage :'
  end
  object Horizontal: TScrollBar
    Left = 0
    Top = 211
    Width = 211
    Height = 16
    LargeChange = 7
    Max = 0
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
    Max = 0
    PageSize = 0
    TabOrder = 1
    OnScroll = VerticalScroll
  end
  object Etage: TSpinButton
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
    Left = 97
    Top = 232
    Width = 73
    Height = 22
    EditorEnabled = False
    MaxValue = 1
    MinValue = 1
    TabOrder = 3
    Value = 1
    OnChange = EditEtageChange
  end
  object GroupInfos: TGroupBox
    Left = 30
    Top = 264
    Width = 169
    Height = 89
    Caption = 'Informations'
    TabOrder = 4
    object LabelCase: TLabel
      Left = 16
      Top = 20
      Width = 30
      Height = 13
      Caption = 'Case :'
    end
    object LabelCode: TLabel
      Left = 16
      Top = 68
      Width = 31
      Height = 13
      Caption = 'Code :'
    end
    object LabelZone: TLabel
      Left = 16
      Top = 44
      Width = 31
      Height = 13
      Caption = 'Zone :'
    end
  end
end
