object FrameDeactivateEffectActionEditor: TFrameDeactivateEffectActionEditor
  Left = 0
  Top = 0
  Width = 257
  Height = 153
  TabOrder = 0
  TabStop = True
  Visible = False
  object LabelEffectID: TLabel
    Left = 16
    Top = 16
    Width = 226
    Height = 13
    Caption = 'D'#233'sactiver l'#39'effet courant, et le remplacer par :'
  end
  object EditEffectID: TComboBoxEx
    Left = 16
    Top = 32
    Width = 193
    Height = 36
    ItemsEx = <>
    ItemHeight = 16
    TabOrder = 0
    Images = SquaresImages
    DropDownCount = 24
  end
  object SquaresImages: TImageList
    Height = 30
    Width = 30
    Left = 136
    Top = 40
  end
end
