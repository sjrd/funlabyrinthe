object FrameMapEditor: TFrameMapEditor
  Left = 0
  Top = 0
  Width = 670
  Height = 421
  TabOrder = 0
  TabStop = True
  object SplitterScrews: TSplitter
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
  object ScrewsContainer: TCategoryButtons
    Left = 0
    Top = 0
    Width = 150
    Height = 421
    Align = alLeft
    ButtonFlow = cbfVertical
    ButtonHeight = 36
    ButtonWidth = 38
    ButtonOptions = [boGradientFill, boBoldCaptions, boCaptionOnlyBorder]
    Images = ScrewsImages
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
    OnButtonClicked = ScrewsContainerButtonClicked
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
    object MapTabSet: TTabSet
      Left = 0
      Top = 0
      Width = 360
      Height = 21
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Style = tsModernTabs
      TabPosition = tpTop
      OnChange = MapTabSetChange
    end
    object PanelMapInfos: TPanel
      Left = 0
      Top = 311
      Width = 360
      Height = 106
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        360
        106)
      object LabelPosition: TLabel
        Left = 8
        Top = 8
        Width = 44
        Height = 13
        Caption = 'Position :'
      end
      object LabelField: TLabel
        Left = 8
        Top = 32
        Width = 41
        Height = 13
        Caption = 'Terrain :'
      end
      object LabelEffect: TLabel
        Left = 8
        Top = 48
        Width = 31
        Height = 13
        Caption = 'Effet :'
      end
      object LabelObstacle: TLabel
        Left = 8
        Top = 80
        Width = 49
        Height = 13
        Caption = 'Obstacle :'
      end
      object LabelFloor: TLabel
        Left = 256
        Top = 16
        Width = 35
        Height = 13
        Anchors = [akTop, akRight]
        Caption = #201'tage :'
      end
      object LabelTool: TLabel
        Left = 8
        Top = 64
        Width = 29
        Height = 13
        Caption = 'Outil :'
      end
      object StaticPosition: TStaticText
        Left = 64
        Top = 8
        Width = 160
        Height = 17
        AutoSize = False
        BorderStyle = sbsSunken
        TabOrder = 0
      end
      object StaticField: TStaticText
        Left = 64
        Top = 32
        Width = 160
        Height = 17
        AutoSize = False
        BorderStyle = sbsSunken
        TabOrder = 1
      end
      object StaticEffect: TStaticText
        Left = 64
        Top = 48
        Width = 160
        Height = 17
        AutoSize = False
        BorderStyle = sbsSunken
        TabOrder = 2
      end
      object StaticTool: TStaticText
        Left = 64
        Top = 64
        Width = 160
        Height = 17
        AutoSize = False
        BorderStyle = sbsSunken
        TabOrder = 4
      end
      object StaticObstacle: TStaticText
        Left = 64
        Top = 80
        Width = 160
        Height = 17
        AutoSize = False
        BorderStyle = sbsSunken
        TabOrder = 3
      end
      object EditFloor: TSpinEdit
        Left = 296
        Top = 8
        Width = 49
        Height = 22
        Anchors = [akTop, akRight]
        EditorEnabled = False
        Enabled = False
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnChange = EditFloorChange
      end
    end
    object ScrollBoxMap: TScrollBox
      Left = 0
      Top = 21
      Width = 360
      Height = 290
      HorzScrollBar.Increment = 30
      VertScrollBar.Increment = 30
      Align = alClient
      BorderStyle = bsNone
      Constraints.MinHeight = 280
      Constraints.MinWidth = 360
      TabOrder = 2
      object PaintBoxMap: TPaintBox
        Left = 0
        Top = 0
        Width = 100
        Height = 100
        OnMouseDown = PaintBoxMapMouseDown
        OnMouseMove = PaintBoxMapMouseMove
        OnPaint = PaintBoxMapPaint
      end
    end
  end
  object ScrewsImages: TImageList
    Height = 30
    Width = 30
    Left = 248
    Top = 248
  end
end
