inherited FrameChangeEffectEnabledActionEditor: TFrameChangeEffectEnabledActionEditor
  Width = 273
  Height = 233
  ExplicitWidth = 273
  ExplicitHeight = 233
  object RadioGroupEnabledValue: TRadioGroup
    Left = 16
    Top = 16
    Width = 241
    Height = 65
    Caption = 'Activer ou d'#233'sactiver'
    Items.Strings = (
      'D'#233'sactiver l'#39'effet'
      'Activer l'#39'effet')
    TabOrder = 0
  end
  object GroupBoxEffect: TGroupBox
    Left = 16
    Top = 96
    Width = 241
    Height = 121
    Caption = 'Effet '#224' activer ou d'#233'sactiver'
    TabOrder = 1
    object EditEffectID: TComboBoxEx
      Left = 32
      Top = 67
      Width = 193
      Height = 36
      ItemsEx = <>
      ItemHeight = 16
      TabOrder = 0
      OnEnter = EditEffectIDEnter
      Images = SquaresImages
      DropDownCount = 24
    end
    object ButtonCurrentEffect: TRadioButton
      Left = 16
      Top = 24
      Width = 209
      Height = 17
      Caption = 'Cet effet'
      TabOrder = 1
    end
    object ButtonAnyEffect: TRadioButton
      Left = 16
      Top = 48
      Width = 209
      Height = 17
      Caption = 'Un autre effet :'
      TabOrder = 2
    end
  end
  object SquaresImages: TImageList
    Height = 30
    Width = 30
    Left = 152
    Top = 168
  end
end
