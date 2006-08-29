object FormCodesCases: TFormCodesCases
  Left = 316
  Top = 404
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Codes des cases'
  ClientHeight = 282
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TTabControl
    Left = 0
    Top = 0
    Width = 567
    Height = 282
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Cases communes'
      'Boutons'
      'T'#233'l'#233'porteurs'
      'Locomotion')
    TabIndex = 0
    OnChange = PagesChange
    object Grille: TStringGrid
      Left = 4
      Top = 24
      Width = 559
      Height = 254
      Align = alClient
      DefaultColWidth = 110
      FixedCols = 0
      RowCount = 10
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
      ParentFont = False
      ScrollBars = ssNone
      TabOrder = 0
      OnDblClick = GrilleDblClick
    end
  end
end
