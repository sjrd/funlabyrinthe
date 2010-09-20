inherited FrameObstacleEditor: TFrameObstacleEditor
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
    Width = 297
    Height = 13
    Caption = 'Message '#224' afficher dans le cas o'#249' l'#39'obstacle n'#39'est pas d'#233'truit :'
  end
  object ButtonAlways: TRadioButton
    Left = 16
    Top = 16
    Width = 409
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Toujours d'#233'truire cet obstacle lorsqu'#39'on pousse dessus'
    TabOrder = 0
  end
  object ButtonNever: TRadioButton
    Tag = 1
    Left = 16
    Top = 40
    Width = 417
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Ne jamais d'#233'truire cet obstacle'
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
      'D'#233'truire cet obstacle uniquement si le joueur peut effectuer l'#39'a' +
      'ction :'
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
