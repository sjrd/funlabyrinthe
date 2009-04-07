inherited FramePlayerColorActionEditor: TFramePlayerColorActionEditor
  Left = 0
  Top = 0
  Width = 257
  Height = 233
  TabOrder = 0
  TabStop = True
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
    Selected = clBlue
    Style = [cbStandardColors, cbExtendedColors, cbCustomColor]
    ItemHeight = 16
    TabOrder = 1
  end
end
