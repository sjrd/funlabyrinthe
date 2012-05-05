inherited FrameAddRemovePluginActionEditor: TFrameAddRemovePluginActionEditor
  Width = 345
  Height = 234
  ExplicitWidth = 345
  ExplicitHeight = 234
  object LabelPluginID: TLabel
    Left = 24
    Top = 160
    Width = 157
    Height = 16
    Caption = 'Plugin '#224' ajouter ou retirer :'
  end
  object RadioGroupKind: TRadioGroup
    Left = 20
    Top = 20
    Width = 301
    Height = 117
    Caption = 'Ajouter ou retirer'
    Items.Strings = (
      'Ajouter le plugin'
      'Retirer le plugin'
      'Inverser le plugin')
    TabOrder = 0
  end
  object EditPluginID: TComboBox
    Left = 24
    Top = 184
    Width = 297
    Height = 24
    ItemHeight = 16
    TabOrder = 1
  end
end
