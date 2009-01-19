object FormCompilerMessages: TFormCompilerMessages
  Left = 0
  Top = 0
  Width = 502
  Height = 104
  BorderStyle = bsSizeToolWin
  Caption = 'Messages du compilateur'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBoxMessages: TListBox
    Left = 0
    Top = 0
    Width = 494
    Height = 78
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBoxMessagesDblClick
  end
end
