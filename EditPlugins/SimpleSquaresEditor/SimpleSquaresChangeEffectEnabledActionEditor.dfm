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
    object ButtonCurrentEffect: TRadioButton
      Left = 16
      Top = 24
      Width = 209
      Height = 17
      Caption = 'Cet effet'
      TabOrder = 0
    end
    object ButtonAnyEffect: TRadioButton
      Left = 16
      Top = 48
      Width = 209
      Height = 17
      Caption = 'Un autre effet :'
      TabOrder = 1
    end
    object ComboBoxEffect: TFLComponentComboBox
      Left = 32
      Top = 67
      Width = 193
      Height = 38
      TabOrder = 2
      OnChange = ComboBoxEffectChange
      OnEnter = ComboBoxEffectEnter
      OnFilterComponent = ComboBoxEffectFilterComponent
    end
  end
end
