object FrameMapImage: TFrameMapImage
  Left = 0
  Top = 0
  Width = 401
  Height = 301
  TabOrder = 0
  object MapView: TImgView32
    Left = 0
    Top = 0
    Width = 401
    Height = 301
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    SizeGrip = sgNone
    OverSize = 0
    TabOrder = 0
    OnMouseDown = MapViewMouseDown
    OnMouseMove = MapViewMouseMove
    OnMouseUp = MapViewMouseUp
  end
end
