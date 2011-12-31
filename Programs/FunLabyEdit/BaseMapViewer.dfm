object FrameBaseMapViewer: TFrameBaseMapViewer
  Left = 0
  Top = 0
  Width = 455
  Height = 526
  Constraints.MinHeight = 509
  Constraints.MinWidth = 450
  TabOrder = 0
  TabStop = True
  object MapTabSet: TTabSet
    Left = 0
    Top = 0
    Width = 455
    Height = 26
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    Style = tsModernTabs
    TabPosition = tpTop
    OnChange = MapTabSetChange
  end
  object PanelMapInfos: TPanel
    Left = 0
    Top = 394
    Width = 455
    Height = 132
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinWidth = 450
    TabOrder = 1
    DesignSize = (
      455
      132)
    object LabelPosition: TLabel
      Left = 10
      Top = 10
      Width = 53
      Height = 16
      Caption = 'Position :'
    end
    object LabelField: TLabel
      Left = 10
      Top = 40
      Width = 51
      Height = 16
      Caption = 'Terrain :'
    end
    object LabelEffect: TLabel
      Left = 10
      Top = 60
      Width = 35
      Height = 16
      Caption = 'Effet :'
    end
    object LabelObstacle: TLabel
      Left = 10
      Top = 100
      Width = 58
      Height = 16
      Caption = 'Obstacle :'
    end
    object LabelFloor: TLabel
      Left = 325
      Top = 20
      Width = 41
      Height = 16
      Anchors = [akTop, akRight]
      Caption = #201'tage :'
    end
    object LabelTool: TLabel
      Left = 10
      Top = 80
      Width = 35
      Height = 16
      Caption = 'Outil :'
    end
    object LabelScale: TLabel
      Left = 296
      Top = 49
      Width = 41
      Height = 16
      Anchors = [akRight, akBottom]
      Caption = 'Zoom :'
    end
    object StaticPosition: TStaticText
      Left = 80
      Top = 10
      Width = 200
      Height = 21
      AutoSize = False
      BorderStyle = sbsSunken
      TabOrder = 0
    end
    object StaticField: TStaticText
      Left = 80
      Top = 40
      Width = 200
      Height = 21
      AutoSize = False
      BorderStyle = sbsSunken
      TabOrder = 1
    end
    object StaticEffect: TStaticText
      Left = 80
      Top = 60
      Width = 200
      Height = 21
      AutoSize = False
      BorderStyle = sbsSunken
      TabOrder = 2
    end
    object StaticTool: TStaticText
      Left = 80
      Top = 80
      Width = 200
      Height = 21
      AutoSize = False
      BorderStyle = sbsSunken
      TabOrder = 4
    end
    object StaticObstacle: TStaticText
      Left = 80
      Top = 100
      Width = 200
      Height = 21
      AutoSize = False
      BorderStyle = sbsSunken
      TabOrder = 3
    end
    object EditFloor: TSpinEdit
      Left = 375
      Top = 10
      Width = 61
      Height = 26
      Anchors = [akTop, akRight]
      EditorEnabled = False
      Enabled = False
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnChange = EditFloorChange
    end
    object TrackBarScale: TTrackBar
      Left = 288
      Top = 68
      Width = 153
      Height = 42
      Anchors = [akRight, akBottom]
      Min = 1
      Position = 10
      TabOrder = 6
      OnChange = TrackBarScaleChange
    end
  end
  inline MapView: TFrameMapImage
    Left = 0
    Top = 26
    Width = 455
    Height = 368
    Align = alClient
    TabOrder = 2
    ExplicitTop = 26
    ExplicitWidth = 455
    ExplicitHeight = 368
    inherited MapView: TImgView32
      Width = 455
      Height = 368
      ExplicitWidth = 455
      ExplicitHeight = 368
    end
  end
end
