inherited FrameFieldEditor: TFrameFieldEditor
  Width = 551
  Height = 271
  ExplicitWidth = 551
  ExplicitHeight = 271
  DesignSize = (
    551
    271)
  object LabelMessageText: TLabel
    Left = 20
    Top = 150
    Width = 366
    Height = 16
    Caption = 
      'Message '#224' afficher si le joueur ne peut pas venir sur le terrain' +
      ' :'
  end
  object ButtonAlways: TRadioButton
    Left = 20
    Top = 20
    Width = 511
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Toujours autoriser le joueur '#224' venir sur ce terrain'
    TabOrder = 0
  end
  object ButtonNever: TRadioButton
    Tag = 1
    Left = 20
    Top = 50
    Width = 521
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Toujours emp'#234'cher le joueur de venir sur ce terrain'
    TabOrder = 1
  end
  object ButtonPlayerAction: TRadioButton
    Tag = 2
    Left = 20
    Top = 80
    Width = 511
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Autoriser le joueur '#224' venir sur ce terrain uniquement s'#39'il peut ' +
      'effectuer l'#39'action :'
    TabOrder = 2
  end
  object EditPlayerAction: TEdit
    Left = 60
    Top = 110
    Width = 261
    Height = 24
    TabOrder = 3
    Text = 'EditPlayerAction'
  end
  object EditMessageText: TMemo
    Left = 20
    Top = 170
    Width = 511
    Height = 81
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'EditMessageText')
    TabOrder = 4
  end
end
