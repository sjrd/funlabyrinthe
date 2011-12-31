object FormMapViewer: TFormMapViewer
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Cartes'
  ClientHeight = 526
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 17
  inline MapViewer: TFrameBaseMapViewer
    Left = 0
    Top = 0
    Width = 455
    Height = 526
    Align = alClient
    Constraints.MinHeight = 509
    Constraints.MinWidth = 450
    TabOrder = 0
    TabStop = True
    inherited PanelMapInfos: TPanel
      inherited LabelPosition: TLabel
        Width = 56
        Height = 17
        ExplicitWidth = 56
        ExplicitHeight = 17
      end
      inherited LabelField: TLabel
        Height = 17
        ExplicitHeight = 17
      end
      inherited LabelEffect: TLabel
        Width = 37
        Height = 17
        ExplicitWidth = 37
        ExplicitHeight = 17
      end
      inherited LabelObstacle: TLabel
        Width = 61
        Height = 17
        ExplicitWidth = 61
        ExplicitHeight = 17
      end
      inherited LabelFloor: TLabel
        Width = 44
        Height = 17
        ExplicitWidth = 44
        ExplicitHeight = 17
      end
      inherited LabelTool: TLabel
        Width = 36
        Height = 17
        ExplicitWidth = 36
        ExplicitHeight = 17
      end
      inherited LabelScale: TLabel
        Width = 45
        Height = 17
        ExplicitWidth = 45
        ExplicitHeight = 17
      end
      inherited EditFloor: TSpinEdit
        Height = 27
        ExplicitHeight = 27
      end
    end
  end
end
