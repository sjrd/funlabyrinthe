object FormPlugins: TFormPlugins
  Left = 250
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Plug-in du joueur'
  ClientHeight = 265
  ClientWidth = 345
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelAvailablePlugins: TLabel
    Left = 16
    Top = 16
    Width = 94
    Height = 13
    Caption = 'Plug-in disponibles :'
  end
  object ButtonAttachPlugin: TSpeedButton
    Left = 160
    Top = 32
    Width = 25
    Height = 25
    Caption = '>'
    OnClick = ButtonAttachPluginClick
  end
  object ButtonAttachAll: TSpeedButton
    Left = 160
    Top = 64
    Width = 25
    Height = 25
    Caption = '>>'
    OnClick = ButtonAttachAllClick
  end
  object ButtonDetachPlugin: TSpeedButton
    Left = 160
    Top = 96
    Width = 25
    Height = 25
    Caption = '<'
    Enabled = False
    OnClick = ButtonDetachPluginClick
  end
  object ButtonDetachAll: TSpeedButton
    Left = 160
    Top = 128
    Width = 25
    Height = 25
    Caption = '<<'
    Enabled = False
    OnClick = ButtonDetachAllClick
  end
  object LabelAttachedPlugins: TLabel
    Left = 192
    Top = 16
    Width = 133
    Height = 13
    Caption = 'Plug-in attach'#233's au joueur :'
  end
  object ButtonOK: TButton
    Left = 152
    Top = 224
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 248
    Top = 224
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
  object ListBoxAvailablePlugins: TListBox
    Left = 16
    Top = 32
    Width = 137
    Height = 177
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
  end
  object ListBoxAttachedPlugins: TListBox
    Left = 192
    Top = 32
    Width = 137
    Height = 177
    ItemHeight = 13
    MultiSelect = True
    Sorted = True
    TabOrder = 1
  end
end
