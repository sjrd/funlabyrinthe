object FormMapViewer: TFormMapViewer
  Left = 0
  Top = 0
  Width = 372
  Height = 447
  BorderStyle = bsSizeToolWin
  Caption = 'Cartes'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inline MapViewer: TFrameBaseMapViewer
    Left = 0
    Top = 0
    Width = 364
    Height = 421
    Align = alClient
    Constraints.MinHeight = 407
    Constraints.MinWidth = 360
    TabOrder = 0
    TabStop = True
  end
end
