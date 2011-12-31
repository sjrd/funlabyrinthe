inherited FrameChangeEffectEnabledActionEditor: TFrameChangeEffectEnabledActionEditor
  Width = 341
  Height = 291
  ExplicitWidth = 341
  ExplicitHeight = 291
  object RadioGroupEnabledValue: TRadioGroup
    Left = 20
    Top = 20
    Width = 301
    Height = 81
    Caption = 'Activer ou d'#233'sactiver'
    Items.Strings = (
      'D'#233'sactiver l'#39'effet'
      'Activer l'#39'effet')
    TabOrder = 0
  end
  object GroupBoxEffect: TGroupBox
    Left = 20
    Top = 120
    Width = 301
    Height = 151
    Caption = 'Effet '#224' activer ou d'#233'sactiver'
    TabOrder = 1
    object ButtonCurrentEffect: TRadioButton
      Left = 20
      Top = 30
      Width = 261
      Height = 21
      Caption = 'Cet effet'
      TabOrder = 0
    end
    object ButtonAnyEffect: TRadioButton
      Left = 20
      Top = 60
      Width = 261
      Height = 21
      Caption = 'Un autre effet :'
      TabOrder = 1
    end
    object ComboBoxEffect: TFLComponentComboBox
      Left = 40
      Top = 84
      Width = 241
      Height = 38
      TabOrder = 2
      OnChange = ComboBoxEffectChange
      OnEnter = ComboBoxEffectEnter
      OnFilterComponent = ComboBoxEffectFilterComponent
    end
  end
end
