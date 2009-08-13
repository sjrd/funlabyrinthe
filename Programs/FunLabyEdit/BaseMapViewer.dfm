object FrameBaseMapViewer: TFrameBaseMapViewer
  Left = 0
  Top = 0
  Width = 364
  Height = 421
  Constraints.MinHeight = 407
  Constraints.MinWidth = 360
  TabOrder = 0
  TabStop = True
  object MapTabSet: TTabSet
    Left = 0
    Top = 0
    Width = 364
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
    Top = 315
    Width = 364
    Height = 106
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinWidth = 360
    TabOrder = 1
    DesignSize = (
      364
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
      Left = 260
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
      Left = 300
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
  inline MapView: TFrameMapImage
    Left = 0
    Top = 21
    Width = 364
    Height = 294
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 48
    ExplicitTop = 80
    ExplicitWidth = 257
    ExplicitHeight = 193
    inherited MapView: TImgView32
      Width = 364
      Height = 294
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 321
      ExplicitHeight = 241
    end
  end
end
