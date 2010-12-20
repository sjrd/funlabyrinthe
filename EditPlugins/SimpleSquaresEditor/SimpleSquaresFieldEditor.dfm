inherited FrameFieldEditor: TFrameFieldEditor
  Width = 441
  Height = 217
  ExplicitWidth = 441
  ExplicitHeight = 217
  DesignSize = (
    441
    217)
  object LabelMessageText: TLabel
    Left = 16
    Top = 120
    Width = 304
    Height = 13
    Caption = 
      'Message '#224' afficher si le joueur ne peut pas venir sur le terrain' +
      ' :'
  end
  object ButtonAlways: TRadioButton
    Left = 16
    Top = 16
    Width = 409
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Toujours autoriser le joueur '#224' venir sur ce terrain'
    TabOrder = 0
  end
  object ButtonNever: TRadioButton
    Tag = 1
    Left = 16
    Top = 40
    Width = 417
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Toujours emp'#234'cher le joueur de venir sur ce terrain'
    TabOrder = 1
  end
  object ButtonPlayerAction: TRadioButton
    Tag = 2
    Left = 16
    Top = 64
    Width = 409
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Autoriser le joueur '#224' venir sur ce terrain uniquement s'#39'il peut ' +
      'effectuer l'#39'action :'
    TabOrder = 2
  end
  object EditPlayerAction: TEdit
    Left = 48
    Top = 88
    Width = 209
    Height = 21
    TabOrder = 3
    Text = 'EditPlayerAction'
  end
  object EditMessageText: TMemo
    Left = 16
    Top = 136
    Width = 409
    Height = 65
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'EditMessageText')
    TabOrder = 4
  end
end
