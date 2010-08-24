inherited FrameReplaceSquareActionEditor: TFrameReplaceSquareActionEditor
  Width = 497
  Height = 369
  Constraints.MinHeight = 304
  Constraints.MinWidth = 257
  ExplicitWidth = 497
  ExplicitHeight = 369
  object GroupBoxReplaceBy: TGroupBox
    Left = 256
    Top = 16
    Width = 225
    Height = 337
    Caption = 'Modifications pour cette case'
    Constraints.MinHeight = 289
    Constraints.MinWidth = 225
    TabOrder = 0
    DesignSize = (
      225
      337)
    object CheckBoxReplaceObstacle: TCheckBox
      Tag = 3
      Left = 16
      Top = 216
      Width = 193
      Height = 17
      Caption = 'Modifier l'#39'obstacle :'
      TabOrder = 6
    end
    object CheckBoxReplaceTool: TCheckBox
      Tag = 2
      Left = 16
      Top = 152
      Width = 193
      Height = 17
      Caption = 'Modifier l'#39'outil :'
      TabOrder = 4
    end
    object CheckBoxReplaceEffect: TCheckBox
      Tag = 1
      Left = 16
      Top = 88
      Width = 193
      Height = 17
      Caption = 'Modifier l'#39'effet :'
      TabOrder = 2
    end
    object CheckBoxReplaceField: TCheckBox
      Left = 16
      Top = 24
      Width = 193
      Height = 17
      Caption = 'Modifier le terrain :'
      TabOrder = 0
    end
    object ButtonCopySelectedSquare: TButton
      Left = 16
      Top = 280
      Width = 193
      Height = 41
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Imiter la case s'#233'lectionn'#233'e dans la fen'#234'tre Cartes (menu Voir|Ca' +
        'rtes)'
      TabOrder = 8
      WordWrap = True
      OnClick = ButtonCopySelectedSquareClick
    end
    object ComboBoxField: TFLComponentComboBox
      Left = 16
      Top = 40
      Width = 193
      Height = 38
      TabOrder = 1
    end
    object ComboBoxEffect: TFLComponentComboBox
      Tag = 1
      Left = 16
      Top = 104
      Width = 193
      Height = 38
      TabOrder = 3
      UseNil = True
    end
    object ComboBoxTool: TFLComponentComboBox
      Tag = 2
      Left = 16
      Top = 168
      Width = 193
      Height = 38
      TabOrder = 5
      UseNil = True
    end
    object ComboBoxObstacle: TFLComponentComboBox
      Tag = 3
      Left = 16
      Top = 232
      Width = 193
      Height = 38
      TabOrder = 7
      UseNil = True
    end
  end
  object GroupBoxSquarePos: TGroupBox
    Left = 16
    Top = 16
    Width = 225
    Height = 337
    Caption = 'Case '#224' modifier'
    TabOrder = 1
    object LabelMap: TLabel
      Left = 32
      Top = 88
      Width = 34
      Height = 13
      Caption = 'Carte :'
    end
    object LabelAbsolutePosX: TLabel
      Left = 32
      Top = 112
      Width = 48
      Height = 13
      Caption = 'Abscisse :'
    end
    object LabelAbsolutePosY: TLabel
      Left = 32
      Top = 136
      Width = 55
      Height = 13
      Caption = 'Ordonn'#233'e :'
    end
    object LabelAbolutePosZ: TLabel
      Left = 32
      Top = 160
      Width = 35
      Height = 13
      Caption = #201'tage :'
    end
    object ButtonCurrentSquare: TRadioButton
      Left = 16
      Top = 24
      Width = 193
      Height = 17
      Caption = 'Case courante (o'#249' se trouve l'#39'effet)'
      TabOrder = 0
    end
    object ButtonAbsolutePos: TRadioButton
      Left = 16
      Top = 64
      Width = 193
      Height = 17
      Caption = 'Une case en position absolue'
      TabOrder = 1
    end
    object ButtonResetSquarePos: TButton
      Left = 16
      Top = 280
      Width = 193
      Height = 41
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
      Left = 80
      Top = 85
      Width = 129
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      OnEnter = EditAbsolutePosEnter
    end
    object EditAbsolutePosX: TSpinEdit
      Left = 144
      Top = 109
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnEnter = EditAbsolutePosEnter
    end
    object EditAbsolutePosY: TSpinEdit
      Left = 144
      Top = 133
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnEnter = EditAbsolutePosEnter
    end
    object EditAbsolutePosZ: TSpinEdit
      Left = 144
      Top = 157
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnEnter = EditAbsolutePosEnter
    end
  end
end
