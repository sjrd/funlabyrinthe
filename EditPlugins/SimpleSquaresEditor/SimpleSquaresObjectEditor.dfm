inherited FrameObjectEditor: TFrameObjectEditor
  Width = 297
  Height = 225
  Constraints.MinHeight = 225
  Constraints.MinWidth = 297
  DesignSize = (
    297
    225)
  object LabelFindMessage: TLabel
    Left = 16
    Top = 16
    Width = 234
    Height = 13
    Caption = 'Message affich'#233' quand un joueur trouve l'#39'objet :'
  end
  object LabelMinimumCount: TLabel
    Left = 32
    Top = 176
    Width = 160
    Height = 13
    Caption = 'Nombre minimum d'#39'objets requis :'
  end
  object EditFindMessage: TMemo
    Left = 16
    Top = 32
    Width = 265
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'EditFindMessage')
    TabOrder = 0
  end
  object CheckBoxHandleAction: TCheckBox
    Left = 16
    Top = 120
    Width = 265
    Height = 17
    Caption = 'Cet objet permet d'#39'effectuer une action :'
    TabOrder = 1
  end
  object EditHandledAction: TEdit
    Left = 32
    Top = 144
    Width = 249
    Height = 21
    TabOrder = 2
    Text = 'EditHandledAction'
  end
  object EditMinimumCount: TSpinEdit
    Left = 216
    Top = 168
    Width = 65
    Height = 22
    MaxValue = 10000
    MinValue = 1
    TabOrder = 3
    Value = 1
  end
  object CheckBoxDecrementOnUse: TCheckBox
    Left = 32
    Top = 192
    Width = 249
    Height = 17
    Caption = 'Les objets sont retir'#233's quand utilis'#233's'
    TabOrder = 4
  end
end
