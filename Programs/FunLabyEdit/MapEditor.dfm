object FrameMapEditor: TFrameMapEditor
  Left = 0
  Top = 0
  Width = 670
  Height = 421
  TabOrder = 0
  TabStop = True
  object SplitterSquares: TSplitter
    Left = 150
    Top = 0
    Height = 421
    MinSize = 100
  end
  object SplitterPlayers: TSplitter
    Left = 517
    Top = 0
    Height = 421
    Align = alRight
    MinSize = 100
  end
  object SquaresContainer: TCategoryButtons
    Left = 0
    Top = 0
    Width = 150
    Height = 421
    Align = alLeft
    ButtonFlow = cbfVertical
    ButtonHeight = 36
    ButtonWidth = 38
    ButtonOptions = [boGradientFill, boBoldCaptions, boCaptionOnlyBorder]
    Images = SquaresImages
    Categories = <
      item
        Caption = 'Terrains'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Effets'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Outils'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Obstacles'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Cases'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Joueurs'
        Color = 15252386
        Collapsed = False
        Items = <>
      end>
    Color = clWhite
    RegularButtonColor = 15660791
    SelectedButtonColor = 15711942
    ShowHint = True
    TabOrder = 0
    OnButtonClicked = SquaresContainerButtonClicked
    OnDrawIcon = SquaresContainerDrawIcon
  end
  object PlayersContainer: TCategoryButtons
    Left = 520
    Top = 0
    Width = 150
    Height = 421
    Align = alRight
    ButtonFlow = cbfVertical
    ButtonOptions = [boFullSize, boGradientFill, boShowCaptions, boBoldCaptions, boCaptionOnlyBorder]
    Categories = <>
    Color = clWhite
    RegularButtonColor = 15660791
    SelectedButtonColor = 15711942
    ShowHint = True
    TabOrder = 1
    OnButtonClicked = PlayersContainerButtonClicked
  end
  object PanelCenter: TPanel
    Left = 153
    Top = 0
    Width = 364
    Height = 421
    Align = alClient
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    inline MapViewer: TFrameBaseMapViewer
      Left = 0
      Top = 0
      Width = 360
      Height = 417
      Align = alClient
      Constraints.MinHeight = 407
      Constraints.MinWidth = 360
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 360
      ExplicitHeight = 417
      inherited MapTabSet: TTabSet
        Width = 360
        ExplicitWidth = 360
      end
      inherited ScrollBoxMap: TScrollBox
        Width = 360
        Height = 290
        ExplicitWidth = 360
        ExplicitHeight = 290
      end
      inherited PanelMapInfos: TPanel
        Top = 311
        Width = 360
        ExplicitTop = 311
        ExplicitWidth = 360
        inherited LabelFloor: TLabel
          Left = 256
          ExplicitLeft = 256
        end
        inherited EditFloor: TSpinEdit
          Left = 296
          ExplicitLeft = 296
        end
      end
    end
  end
  object SquaresImages: TImageList
    Height = 30
    Width = 30
    Left = 248
    Top = 248
  end
end
