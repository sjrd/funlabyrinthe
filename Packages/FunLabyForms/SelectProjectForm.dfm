object FormSelectProjectFile: TFormSelectProjectFile
  Left = 0
  Top = 0
  Caption = 'S'#233'lectionner un labyrinthe'
  ClientHeight = 451
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListViewFiles: TListView
    Left = 0
    Top = 0
    Width = 579
    Height = 400
    Align = alClient
    Columns = <
      item
        Caption = 'Titre'
        Width = 200
      end
      item
        Caption = 'Genre'
        Width = 100
      end
      item
        Caption = 'Difficult'#233
        Width = 100
      end
      item
        Caption = 'Auteur'
        Width = 150
      end>
    Groups = <
      item
        Header = 'Nouveaux labyrinthes'
        GroupID = 0
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
        ExtendedImage = -1
      end
      item
        Header = 'Labyrinthes de la version 4.x'
        GroupID = 1
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
        ExtendedImage = -1
      end
      item
        Header = 'Tests et Co. qui ne sont pas pr'#233'vus pour '#234'tre jou'#233's'
        GroupID = 2
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
        ExtendedImage = -1
      end>
    GroupView = True
    ReadOnly = True
    RowSelect = True
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = ListViewFilesClick
    OnColumnClick = ListViewFilesColumnClick
    OnCompare = ListViewFilesCompare
    OnDblClick = ListViewFilesDblClick
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 400
    Width = 782
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      782
      51)
    object LabelFileName: TLabel
      Left = 16
      Top = 17
      Width = 75
      Height = 13
      Caption = 'Nom du fichier :'
    end
    object EditFileName: TComboBox
      Left = 136
      Top = 14
      Width = 305
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
    object ButtonOK: TButton
      Left = 496
      Top = 14
      Width = 121
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Ouvrir'
      Default = True
      TabOrder = 1
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 643
      Top = 14
      Width = 121
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Annuler'
      Default = True
      TabOrder = 2
      OnClick = ButtonCancelClick
    end
  end
  object PanelRight: TPanel
    Left = 579
    Top = 0
    Width = 203
    Height = 400
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object LabelDescription: TLabel
      Left = 16
      Top = 16
      Width = 60
      Height = 13
      Caption = 'Description :'
    end
    object EditDescription: TMemo
      Left = 16
      Top = 35
      Width = 169
      Height = 350
      ReadOnly = True
      TabOrder = 0
    end
  end
  object ChangeNotifier: TJvChangeNotify
    Notifications = <
      item
        Actions = [caChangeFileName, caChangeDirName, caChangeLastWrite]
      end>
    OnChangeNotify = ChangeNotifierChangeNotify
    Left = 112
    Top = 168
  end
end
