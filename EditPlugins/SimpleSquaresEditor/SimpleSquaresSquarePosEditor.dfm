inherited FrameSquarePosEditor: TFrameSquarePosEditor
  Width = 281
  Height = 421
  ExplicitWidth = 281
  ExplicitHeight = 421
  object GroupBoxSquarePos: TGroupBox
    Left = 0
    Top = 0
    Width = 281
    Height = 421
    Caption = 'Position de la case'
    TabOrder = 0
    object LabelMap: TLabel
      Left = 40
      Top = 110
      Width = 40
      Height = 16
      Caption = 'Carte :'
    end
    object LabelAbsolutePosX: TLabel
      Left = 40
      Top = 140
      Width = 58
      Height = 16
      Caption = 'Abscisse :'
    end
    object LabelAbsolutePosY: TLabel
      Left = 40
      Top = 170
      Width = 65
      Height = 16
      Caption = 'Ordonn'#233'e :'
    end
    object LabelAbolutePosZ: TLabel
      Left = 40
      Top = 200
      Width = 41
      Height = 16
      Caption = #201'tage :'
    end
    object ButtonCurrentSquare: TRadioButton
      Left = 20
      Top = 30
      Width = 241
      Height = 21
      Caption = 'Case courante (o'#249' se trouve l'#39'effet)'
      TabOrder = 0
    end
    object ButtonAbsolutePos: TRadioButton
      Left = 20
      Top = 80
      Width = 241
      Height = 21
      Caption = 'Une case en position absolue'
      TabOrder = 1
    end
    object ButtonResetSquarePos: TButton
      Left = 20
      Top = 350
      Width = 241
      Height = 51
      Caption = 
        'Utiliser la case s'#233'lectionn'#233'e dans la fen'#234'tre Cartes (menu Voir|' +
        'Cartes)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      WordWrap = True
      OnClick = ButtonResetSquarePosClick
    end
    object EditMapID: TComboBox
      Left = 100
      Top = 106
      Width = 161
      Height = 24
      ItemHeight = 16
      TabOrder = 2
      OnEnter = EditAbsolutePosEnter
    end
    object EditAbsolutePosX: TSpinEdit
      Left = 180
      Top = 136
      Width = 81
      Height = 26
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnEnter = EditAbsolutePosEnter
    end
    object EditAbsolutePosY: TSpinEdit
      Left = 180
      Top = 166
      Width = 81
      Height = 26
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnEnter = EditAbsolutePosEnter
    end
    object EditAbsolutePosZ: TSpinEdit
      Left = 180
      Top = 196
      Width = 81
      Height = 26
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnEnter = EditAbsolutePosEnter
    end
  end
end
