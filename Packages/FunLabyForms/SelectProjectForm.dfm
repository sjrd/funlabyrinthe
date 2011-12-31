object FormSelectProjectFile: TFormSelectProjectFile
  Left = 0
  Top = 0
  Caption = 'S'#233'lectionner un labyrinthe'
  ClientHeight = 590
  ClientWidth = 1023
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object ListViewFiles: TListView
    Left = 0
    Top = 0
    Width = 757
    Height = 523
    Align = alClient
    Columns = <
      item
        Caption = 'Titre'
        Width = 262
      end
      item
        Caption = 'Genre'
        Width = 131
      end
      item
        Caption = 'Difficult'#233
        Width = 131
      end
      item
        Caption = 'Auteur'
        Width = 196
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
      end
      item
        Header = 'Fichiers invalides'
        GroupID = 3
        State = [lgsNormal]
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
    OnColumnClick = ListViewFilesColumnClick
    OnCompare = ListViewFilesCompare
    OnDblClick = ListViewFilesDblClick
    OnSelectItem = ListViewFilesSelectItem
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 523
    Width = 1023
    Height = 67
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      1023
      67)
    object LabelFileName: TLabel
      Left = 21
      Top = 22
      Width = 97
      Height = 17
      Caption = 'Nom du fichier :'
    end
    object EditFileName: TComboBox
      Left = 178
      Top = 18
      Width = 399
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 17
      Sorted = True
      TabOrder = 0
    end
    object ButtonOK: TButton
      Left = 649
      Top = 18
      Width = 158
      Height = 33
      Anchors = [akTop, akRight]
      Caption = '&Ouvrir'
      Default = True
      TabOrder = 1
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 841
      Top = 18
      Width = 158
      Height = 33
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Annuler'
      Default = True
      TabOrder = 2
      OnClick = ButtonCancelClick
    end
  end
  object PanelRight: TPanel
    Left = 757
    Top = 0
    Width = 266
    Height = 523
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object LabelDescription: TLabel
      Left = 21
      Top = 21
      Width = 77
      Height = 17
      Caption = 'Description :'
    end
    object EditDescription: TMemo
      Left = 21
      Top = 46
      Width = 221
      Height = 457
      ReadOnly = True
      TabOrder = 0
    end
  end
  object ChangeNotifier: TJvChangeNotify
    Notifications = <
      item
        Actions = [caChangeFileName, caChangeDirName, caChangeLastWrite]
        IncludeSubTrees = True
      end>
    OnChangeNotify = ChangeNotifierChangeNotify
    Left = 112
    Top = 168
  end
end
