object FormCompilerMessages: TFormCompilerMessages
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Messages du compilateur'
  ClientHeight = 107
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 17
  object ListBoxMessages: TListBox
    Left = 0
    Top = 0
    Width = 666
    Height = 107
    Align = alClient
    ItemHeight = 17
    TabOrder = 0
    OnDblClick = ListBoxMessagesDblClick
    ExplicitWidth = 484
    ExplicitHeight = 64
  end
end
