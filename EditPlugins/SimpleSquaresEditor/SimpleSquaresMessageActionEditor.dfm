inherited FrameMessageActionEditor: TFrameMessageActionEditor
  Width = 249
  Height = 321
  Constraints.MinHeight = 321
  Constraints.MinWidth = 249
  ExplicitWidth = 249
  ExplicitHeight = 321
  DesignSize = (
    249
    321)
  object LabelText: TStaticText
    Left = 16
    Top = 40
    Width = 99
    Height = 17
    Caption = 'Texte du message :'
    TabOrder = 2
  end
  object EditText: TMemo
    Left = 16
    Top = 56
    Width = 217
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'EditText')
    TabOrder = 0
  end
  object CheckBoxOnlyFirstTime: TCheckBox
    Left = 16
    Top = 16
    Width = 217
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Afficher seulement au premier passage'
    TabOrder = 1
  end
end
