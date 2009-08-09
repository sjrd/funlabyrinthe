object FrameMapEditor: TFrameMapEditor
  Left = 0
  Top = 0
  Width = 720
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
    Categories = <
      item
        Caption = 'Joueurs'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
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
        Caption = 'Neutres'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Composants suppl'#233'mentaires'
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
  object PanelCenter: TPanel
    Left = 153
    Top = 0
    Width = 364
    Height = 421
    Align = alClient
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 1
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
        inherited StaticPosition: TStaticText
          Anchors = [akLeft, akTop, akRight]
        end
        inherited StaticField: TStaticText
          Anchors = [akLeft, akTop, akRight]
        end
        inherited StaticEffect: TStaticText
          Anchors = [akLeft, akTop, akRight]
        end
        inherited StaticTool: TStaticText
          Anchors = [akLeft, akTop, akRight]
        end
        inherited StaticObstacle: TStaticText
          Anchors = [akLeft, akTop, akRight]
        end
        inherited EditFloor: TSpinEdit
          Left = 296
          ExplicitLeft = 296
        end
      end
    end
  end
  object PanelRight: TPanel
    Left = 520
    Top = 0
    Width = 200
    Height = 421
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    inline FrameInspector: TFrameInspector
      Left = 0
      Top = 0
      Width = 200
      Height = 421
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 200
      ExplicitHeight = 421
      inherited Inspector: TJvInspector
        Width = 200
        Height = 267
        ExplicitTop = 25
        ExplicitWidth = 200
        ExplicitHeight = 267
      end
      inherited PanelCollectionEditor: TPanel
        Top = 292
        Width = 200
        ExplicitTop = 292
        ExplicitWidth = 200
        inherited PanelCollectionEditorTitle: TPanel
          Width = 200
          ExplicitWidth = 200
        end
        inherited ToolBarCollectionEditor: TToolBar
          Width = 200
          ExplicitWidth = 200
        end
        inherited ListBoxCollectionItems: TListBox
          Width = 200
          ExplicitWidth = 200
        end
      end
      inherited PanelInspectorTitle: TPanel
        Width = 200
        ExplicitTop = 0
        ExplicitWidth = 200
      end
    end
  end
end
