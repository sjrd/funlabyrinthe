inherited FrameObstacleEditor: TFrameObstacleEditor
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
    Width = 356
    Height = 16
    Caption = 'Message '#224' afficher dans le cas o'#249' l'#39'obstacle n'#39'est pas d'#233'truit :'
  end
  object ButtonAlways: TRadioButton
    Left = 20
    Top = 20
    Width = 511
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Toujours d'#233'truire cet obstacle lorsqu'#39'on pousse dessus'
    TabOrder = 0
  end
  object ButtonNever: TRadioButton
    Tag = 1
    Left = 20
    Top = 50
    Width = 521
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Ne jamais d'#233'truire cet obstacle'
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
      'D'#233'truire cet obstacle uniquement si le joueur peut effectuer l'#39'a' +
      'ction :'
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
