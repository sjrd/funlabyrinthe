inherited FrameObjectEditor: TFrameObjectEditor
  Width = 371
  Height = 281
  Constraints.MinHeight = 281
  Constraints.MinWidth = 371
  ExplicitWidth = 371
  ExplicitHeight = 281
  DesignSize = (
    371
    281)
  object LabelFindMessage: TLabel
    Left = 20
    Top = 20
    Width = 278
    Height = 16
    Caption = 'Message affich'#233' quand un joueur trouve l'#39'objet :'
  end
  object LabelMinimumCount: TLabel
    Left = 40
    Top = 220
    Width = 199
    Height = 16
    Caption = 'Nombre minimum d'#39'objets requis :'
  end
  object EditFindMessage: TMemo
    Left = 20
    Top = 40
    Width = 331
    Height = 91
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'EditFindMessage')
    TabOrder = 0
  end
  object CheckBoxHandleAction: TCheckBox
    Left = 20
    Top = 150
    Width = 331
    Height = 21
    Caption = 'Cet objet permet d'#39'effectuer une action :'
    TabOrder = 1
  end
  object EditHandledAction: TEdit
    Left = 40
    Top = 180
    Width = 311
    Height = 24
    TabOrder = 2
    Text = 'EditHandledAction'
  end
  object EditMinimumCount: TSpinEdit
    Left = 270
    Top = 210
    Width = 81
    Height = 26
    MaxValue = 10000
    MinValue = 1
    TabOrder = 3
    Value = 1
  end
  object CheckBoxDecrementOnUse: TCheckBox
    Left = 40
    Top = 240
    Width = 311
    Height = 21
    Caption = 'Les objets sont retir'#233's quand utilis'#233's'
    TabOrder = 4
  end
end
