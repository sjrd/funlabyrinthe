inherited FramePlayerColorActionEditor: TFramePlayerColorActionEditor
  Width = 321
  Height = 291
  ExplicitWidth = 321
  ExplicitHeight = 291
  object LabelColor: TStaticText
    Left = 20
    Top = 20
    Width = 262
    Height = 20
    Caption = 'Remplacer la couleur du pion du joueur par :'
    TabOrder = 0
  end
  object ListBoxColor: TColorBox
    Left = 20
    Top = 40
    Width = 281
    Height = 22
    NoneColorColor = clWindow
    Selected = clBlue
    Style = [cbCustomColor, cbCustomColors]
    ItemHeight = 16
    TabOrder = 1
    OnGetColors = ListBoxColorGetColors
  end
end
