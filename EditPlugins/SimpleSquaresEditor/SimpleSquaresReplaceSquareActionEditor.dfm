inherited FrameReplaceSquareActionEditor: TFrameReplaceSquareActionEditor
  Width = 257
  Height = 393
  Constraints.MinHeight = 304
  Constraints.MinWidth = 257
  object ButtonResetSquarePos: TButton
    Left = 16
    Top = 16
    Width = 225
    Height = 25
    Hint = 
      'Res'#233'lectionner la case concern'#233'e depuis le visualisateur de cart' +
      'es'
    Caption = 'Res'#233'lectionner la case concern'#233'e'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = ButtonResetSquarePosClick
  end
  object GroupBoxReplaceBy: TGroupBox
    Left = 16
    Top = 56
    Width = 225
    Height = 321
    Caption = 'Case de remplacement'
    Constraints.MinHeight = 289
    Constraints.MinWidth = 225
    TabOrder = 1
    DesignSize = (
      225
      321)
    object LabelFieldID: TLabel
      Left = 16
      Top = 56
      Width = 41
      Height = 13
      Caption = 'Terrain :'
    end
    object LabelEffectID: TLabel
      Left = 16
      Top = 120
      Width = 31
      Height = 13
      Caption = 'Effet :'
    end
    object LabelToolID: TLabel
      Left = 16
      Top = 184
      Width = 29
      Height = 13
      Caption = 'Outil :'
    end
    object LabelObstacleID: TLabel
      Left = 16
      Top = 248
      Width = 49
      Height = 13
      Caption = 'Obstacle :'
    end
    object EditEffectID: TComboBoxEx
      Left = 16
      Top = 136
      Width = 193
      Height = 36
      ItemsEx = <>
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 2
      Images = SquaresImages
      DropDownCount = 24
    end
    object EditFieldID: TComboBoxEx
      Left = 16
      Top = 72
      Width = 193
      Height = 36
      ItemsEx = <>
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 1
      Images = SquaresImages
      DropDownCount = 24
    end
    object EditToolID: TComboBoxEx
      Left = 16
      Top = 200
      Width = 193
      Height = 36
      ItemsEx = <>
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 3
      Images = SquaresImages
      DropDownCount = 24
    end
    object EditObstacleID: TComboBoxEx
      Left = 16
      Top = 264
      Width = 193
      Height = 36
      ItemsEx = <>
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 4
      Images = SquaresImages
      DropDownCount = 24
    end
    object ButtonCopySelectedSquare: TButton
      Left = 16
      Top = 24
      Width = 193
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Imiter la case s'#233'lectionn'#233'e'
      TabOrder = 0
      OnClick = ButtonCopySelectedSquareClick
    end
  end
  object SquaresImages: TImageList
    Height = 30
    Width = 30
    Left = 160
    Top = 192
  end
end
