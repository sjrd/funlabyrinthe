inherited FrameReplaceSquareActionEditor: TFrameReplaceSquareActionEditor
  Width = 621
  Height = 461
  Constraints.MinHeight = 380
  Constraints.MinWidth = 321
  ExplicitWidth = 621
  ExplicitHeight = 461
  object GroupBoxReplaceBy: TGroupBox
    Left = 320
    Top = 20
    Width = 281
    Height = 421
    Caption = 'Modifications pour cette case'
    Constraints.MinHeight = 361
    Constraints.MinWidth = 281
    TabOrder = 0
    DesignSize = (
      281
      421)
    object CheckBoxReplaceObstacle: TCheckBox
      Tag = 3
      Left = 20
      Top = 270
      Width = 241
      Height = 21
      Caption = 'Modifier l'#39'obstacle :'
      TabOrder = 6
    end
    object CheckBoxReplaceTool: TCheckBox
      Tag = 2
      Left = 20
      Top = 190
      Width = 241
      Height = 21
      Caption = 'Modifier l'#39'outil :'
      TabOrder = 4
    end
    object CheckBoxReplaceEffect: TCheckBox
      Tag = 1
      Left = 20
      Top = 110
      Width = 241
      Height = 21
      Caption = 'Modifier l'#39'effet :'
      TabOrder = 2
    end
    object CheckBoxReplaceField: TCheckBox
      Left = 20
      Top = 30
      Width = 241
      Height = 21
      Caption = 'Modifier le terrain :'
      TabOrder = 0
    end
    object ButtonCopySelectedSquare: TButton
      Left = 20
      Top = 350
      Width = 241
      Height = 51
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Imiter la case s'#233'lectionn'#233'e dans la fen'#234'tre Cartes (menu Voir|Ca' +
        'rtes)'
      TabOrder = 8
      WordWrap = True
      OnClick = ButtonCopySelectedSquareClick
    end
    object ComboBoxField: TFLComponentComboBox
      Left = 20
      Top = 50
      Width = 241
      Height = 38
      TabOrder = 1
    end
    object ComboBoxEffect: TFLComponentComboBox
      Tag = 1
      Left = 20
      Top = 130
      Width = 241
      Height = 38
      TabOrder = 3
      UseNil = True
    end
    object ComboBoxTool: TFLComponentComboBox
      Tag = 2
      Left = 20
      Top = 210
      Width = 241
      Height = 38
      TabOrder = 5
      UseNil = True
    end
    object ComboBoxObstacle: TFLComponentComboBox
      Tag = 3
      Left = 20
      Top = 290
      Width = 241
      Height = 38
      TabOrder = 7
      UseNil = True
    end
  end
  object GroupBoxSquarePos: TGroupBox
    Left = 20
    Top = 20
    Width = 281
    Height = 421
    Caption = 'Case '#224' modifier'
    TabOrder = 1
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
