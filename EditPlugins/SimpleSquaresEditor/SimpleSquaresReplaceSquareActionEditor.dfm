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
    TabOrder = 1
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
  inline FrameSquarePosEditor: TFrameSquarePosEditor
    Left = 20
    Top = 20
    Width = 281
    Height = 421
    TabOrder = 0
    TabStop = True
    ExplicitLeft = 20
    ExplicitTop = 20
    inherited GroupBoxSquarePos: TGroupBox
      Caption = 'Case '#224' modifier'
    end
  end
end
