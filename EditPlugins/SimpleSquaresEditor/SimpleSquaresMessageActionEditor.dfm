inherited FrameMessageActionEditor: TFrameMessageActionEditor
  Width = 311
  Height = 401
  Constraints.MinHeight = 401
  Constraints.MinWidth = 311
  ExplicitWidth = 311
  ExplicitHeight = 401
  DesignSize = (
    311
    401)
  object LabelText: TStaticText
    Left = 20
    Top = 50
    Width = 118
    Height = 20
    Caption = 'Texte du message :'
    TabOrder = 2
  end
  object EditText: TMemo
    Left = 20
    Top = 70
    Width = 271
    Height = 311
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'EditText')
    TabOrder = 0
  end
  object CheckBoxOnlyFirstTime: TCheckBox
    Left = 20
    Top = 20
    Width = 271
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Afficher seulement au premier passage'
    TabOrder = 1
  end
end
