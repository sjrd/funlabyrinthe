object FrameMessageActionEditor: TFrameMessageActionEditor
  Left = 0
  Top = 0
  Width = 249
  Height = 337
  Constraints.MinHeight = 321
  Constraints.MinWidth = 249
  TabOrder = 0
  TabStop = True
  Visible = False
  DesignSize = (
    249
    337)
  object LabelDialogTitle: TLabel
    Left = 16
    Top = 168
    Width = 140
    Height = 13
    Caption = 'Titre de la bo'#238'te de dialogue :'
  end
  object LabelText: TLabel
    Left = 16
    Top = 216
    Width = 95
    Height = 13
    Caption = 'Texte du message :'
  end
  object RadioGroupKind: TRadioGroup
    Left = 16
    Top = 16
    Width = 217
    Height = 137
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Type de message'
    Items.Strings = (
      'Message'
      'Indice'
      'Impasse'
      'Gagn'#233' !'
      'Perdu !'
      'Personnalis'#233)
    TabOrder = 0
  end
  object EditDialogTitle: TEdit
    Left = 16
    Top = 184
    Width = 217
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'EditDialogTitle'
  end
  object EditText: TMemo
    Left = 16
    Top = 232
    Width = 217
    Height = 90
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'EditText')
    TabOrder = 2
  end
  object CheckBoxOnlyFirstTime: TCheckBox
    Left = 16
    Top = 351
    Width = 15
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Afficher seulement au premier passage'
    TabOrder = 3
  end
end
