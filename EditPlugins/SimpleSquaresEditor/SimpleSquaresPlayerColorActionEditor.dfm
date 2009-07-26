inherited FramePlayerColorActionEditor: TFramePlayerColorActionEditor
  Width = 257
  Height = 233
  ExplicitWidth = 257
  ExplicitHeight = 233
  object LabelColor: TStaticText
    Left = 16
    Top = 16
    Width = 216
    Height = 17
    Caption = 'Remplacer la couleur du pion du joueur par :'
    TabOrder = 0
  end
  object ListBoxColor: TColorBox
    Left = 16
    Top = 32
    Width = 225
    Height = 22
    NoneColorColor = clWindow
    Selected = clBlue
    Style = [cbCustomColor, cbCustomColors]
    ItemHeight = 16
    TabOrder = 1
    OnGetColors = ListBoxColorGetColors
  end
end
