unit ObjectInspector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypInfo, JvExControls, JvInspector, ScDelphiLanguage, FunLabyUtils,
  FunLabyEditConsts, FunLabyEditTypes, SepiReflectionCore, SepiSystemUnit,
  FilesUtils, StrUtils, StdCtrls, Buttons, ToolWin, ComCtrls, ExtCtrls, ImgList;

type
  {$M+}

  {*
    Liste de PlayerData - utilis� uniquement dans l'inspecteur
    @author sjrd
    @version 5.0
  *}
  TPlayerDataList = class(TObject)
  private
    FInspectObject: TFunLabyComponent; /// Composant dont �diter les PlayerData
  public
    property InspectObject: TFunLabyComponent
      read FInspectObject write FInspectObject;
  end;

  {$M-}

  {*
    Item g�rant un TFunLabyPersistent dans un Inspector
    @author sjrd
    @version 5.0
  *}
  TJvInspectorFunLabyPersistentItem = class(TJvInspectorClassItem)
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  end;

  {*
    Item g�rant un TFunLabyCollection dans un Inspector
    @author sjrd
    @version 5.0
  *}
  TJvInspectorFunLabyCollectionItem = class(TJvInspectorClassItem)
  protected
    procedure Edit; override;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  end;

  {*
    Item g�rant un TPlayerDataList dans un Inspector
    @author sjrd
    @version 5.0
  *}
  TJvInspectorPlayerDataListItem = class(TJvInspectorClassItem)
  protected
    procedure CreateMembers; override;
    procedure DeleteMembers; override;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  end;

  {*
    Cadre inspecteur d'objet
    @author sjrd
    @version 5.0
  *}
  TFrameInspector = class(TFrame)
    Inspector: TJvInspector;
    PanelCollectionEditor: TPanel;
    PanelCollectionEditorTitle: TPanel;
    ToolBarCollectionEditor: TToolBar;
    ListBoxCollectionItems: TListBox;
    PanelInspectorTitle: TPanel;
    ButtonCollectionAdd: TToolButton;
    ButtonCollectionRemove: TToolButton;
    ButtonCollectionSep1: TToolButton;
    ButtonCollectionMoveUp: TToolButton;
    ButtonCollectionMoveDown: TToolButton;
    CollectionImages: TImageList;
    CollectionDisabledImages: TImageList;
    procedure InspectorDataValueChanged(Sender: TObject;
      Data: TJvCustomInspectorData);
    procedure ListBoxCollectionItemsClick(Sender: TObject);
    procedure ButtonCollectionAddClick(Sender: TObject);
    procedure ButtonCollectionRemoveClick(Sender: TObject);
    procedure ButtonCollectionMoveUpClick(Sender: TObject);
    procedure ButtonCollectionMoveDownClick(Sender: TObject);
  private
    MasterFile: TMasterFile; /// Fichier ma�tre
    Master: TMaster;         /// Ma�tre FunLabyrinthe

    /// Call-back marquant le fichier comme modif�
    FMarkModified: TMarkModifiedProc;

    FKnownAttributes: TStrings; /// Liste des attributs connus

    FInspectObject: TFunLabyPersistent; /// Composant actuellement inspect�
    FPlayerDataList: TPlayerDataList;   /// PlayerData list pour le composant

    FInspectCollection: TFunLabyCollection; /// Collection inspect�e

    procedure LoadKnownAttributes;
    procedure ClearInspector;
    procedure FillInspector;

    procedure UpdateCollectionEditor;

    procedure GetPlayerAttributeAsOrdinal(Sender: TJvInspectorEventData;
      var Value: Int64);
    procedure SetPlayerAttributeAsOrdinal(Sender: TJvInspectorEventData;
      var Value: Int64);

    procedure GetPlayerPluginAsOrdinal(Sender: TJvInspectorEventData;
      var Value: Int64);
    procedure SetPlayerPluginAsOrdinal(Sender: TJvInspectorEventData;
      var Value: Int64);

    procedure SetInspectObject(const Value: TFunLabyPersistent);
    procedure SetInspectCollection(const Value: TFunLabyCollection);

    property InspectCollection: TFunLabyCollection
      read FInspectCollection write SetInspectCollection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(AMasterFile: TMasterFile);
    procedure UnloadFile;

    property MarkModified: TMarkModifiedProc
      read FMarkModified write FMarkModified;

    property InspectObject: TFunLabyPersistent
      read FInspectObject write SetInspectObject;
  end;

implementation

{$R *.dfm}

type
  TPlayerDataAccess = class(TFunLabyComponent)
  public
    class function AccessPlayerDataClass(
      Component: TFunLabyComponent): TPlayerDataClass;
    class function AccessPlayerData(Component: TFunLabyComponent;
      Player: TPlayer): TPlayerData;
  end;

function HasAnyProperty(AClass: TClass): Boolean;
begin
  Result := (AClass.ClassInfo <> nil) and
    (GetPropList(AClass.ClassInfo, tkProperties, nil) > 0);
end;

{-------------------------}
{ TPlayerDataAccess class }
{-------------------------}

class function TPlayerDataAccess.AccessPlayerDataClass(
  Component: TFunLabyComponent): TPlayerDataClass;
begin
  Result := TPlayerDataAccess(Component).GetPlayerDataClass;
end;

class function TPlayerDataAccess.AccessPlayerData(Component: TFunLabyComponent;
  Player: TPlayer): TPlayerData;
begin
  Result := TPlayerDataAccess(Component).GetPlayerData(Player);
end;

{-----------------------------------------}
{ TJvInspectorFunLabyPersistentItem class }
{-----------------------------------------}

{*
  [@inheritDoc]
*}
constructor TJvInspectorFunLabyPersistentItem.Create(
  const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData);
begin
  Assert(GetTypeData(AData.TypeInfo).ClassType.InheritsFrom(
    TFunLabyPersistent));

  inherited;

  ItemClassFlags := [icfCreateMemberItems, icfShowClassName];
end;

{-----------------------------------------}
{ TJvInspectorFunLabyCollectionItem class }
{-----------------------------------------}

{*
  [@inheritDoc]
*}
constructor TJvInspectorFunLabyCollectionItem.Create(
  const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData);
begin
  Assert(GetTypeData(AData.TypeInfo).ClassType.InheritsFrom(
    TFunLabyCollection));

  inherited;

  Flags := Flags + [iifEditButton];
  ItemClassFlags := [icfCreateMemberItems, icfShowClassName];
end;

{*
  [@inheritDoc]
*}
procedure TJvInspectorFunLabyCollectionItem.Edit;
var
  Collection: TFunLabyCollection;
begin
  Collection := TFunLabyCollection(Data.AsOrdinal);

  if Collection <> nil then
    (Inspector.Owner as TFrameInspector).InspectObject := Collection;
end;

{--------------------------------------}
{ TJvInspectorPlayerDataListItem class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
constructor TJvInspectorPlayerDataListItem.Create(
  const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData);
begin
  Assert(GetTypeData(AData.TypeInfo).ClassType.InheritsFrom(TPlayerDataList));

  inherited;

  ItemClassFlags :=
    [icfCreateMemberItems, icfShowClassName, icfRenderAsCategory];
end;

procedure GetPlayerDataAsOrdinal(PlayerData: TPlayerData;
  Sender: TJvInspectorEventData; var Value: Int64);
begin
  Value := Cardinal(PlayerData);
end;

{*
  [@inheritDoc]
*}
procedure TJvInspectorPlayerDataListItem.CreateMembers;
var
  Component: TFunLabyComponent;
  Master: TMaster;
  I: Integer;
  Player: TPlayer;
  PlayerData: TPlayerData;
  SubData: TJvInspectorEventData;
begin
  if Data.IsInitialized and (Data.AsOrdinal <> 0) then
  begin
    Inspector.BeginUpdate;
    try
      inherited;

      Component := TPlayerDataList(Data.AsOrdinal).InspectObject;
      if Component = nil then
        Exit;
      Master := Component.Master;

      for I := 0 to Master.PlayerCount-1 do
      begin
        Player := Master.Players[I];
        PlayerData := TPlayerDataAccess.AccessPlayerData(Component, Player);

        SubData := TJvInspectorEventData.New(Self, Player.ID,
          PlayerData.ClassInfo).Data as TJvInspectorEventData;
        SubData.OnGetAsOrdinal := TJvInspAsInt64(MakeMethod(
          @GetPlayerDataAsOrdinal, PlayerData));
      end;
    finally
      Inspector.EndUpdate;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TJvInspectorPlayerDataListItem.DeleteMembers;
var
  I: Integer;
begin
  if Data.IsInitialized then
  begin
    Inspector.BeginUpdate;
    try
      for I := Pred(Count) downto 0 do
        Delete(I);

      inherited;
    finally
      Inspector.EndUpdate;
    end;
  end;
end;

{-----------------------}
{ TFrameInspector class }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TFrameInspector.Create(AOwner: TComponent);
begin
  inherited;

  FKnownAttributes := TStringList.Create;
  with TStringList(FKnownAttributes) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;

  FPlayerDataList := TPlayerDataList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TFrameInspector.Destroy;
begin
  FPlayerDataList.Free;
  FKnownAttributes.Free;

  inherited;
end;

{*
  Charge la liste des attributs connus
*}
procedure TFrameInspector.LoadKnownAttributes;
var
  I, J: Integer;
  AttrType: TSepiType;
  SepiUnit: TSepiUnit;
  Constant: TSepiConstant;
begin
  AttrType := TSepiSystemUnit(MasterFile.SepiRoot.SystemUnit).LongString;

  for I := 0 to MasterFile.SepiRoot.UnitCount-1 do
  begin
    SepiUnit := MasterFile.SepiRoot.Units[I];

    for J := 0 to SepiUnit.ChildCount-1 do
    begin
      if not (SepiUnit.Children[J] is TSepiConstant) then
        Continue;

      Constant := TSepiConstant(SepiUnit.Children[J]);

      if not AnsiStartsText('attr', Constant.Name) then
        Continue;
      if not Constant.ConstType.Equals(AttrType) then
        Continue;

      FKnownAttributes.Add(string(Constant.ValuePtr^));
    end;
  end;
end;

{*
  Vide l'inspecteur d'objets
*}
procedure TFrameInspector.ClearInspector;
begin
  Inspector.Clear;
  Inspector.InspectObject := nil;
  FPlayerDataList.InspectObject := nil;

  if (InspectCollection <> nil) and (InspectObject <> InspectCollection) and
    (InspectCollection.IndexOf(InspectObject) < 0) then
  begin
    InspectCollection := nil;
  end;
end;

{*
  Remplit l'inspecteur d'objet avec les propri�t� de InspectObject
*}
procedure TFrameInspector.FillInspector;
var
  Component: TFunLabyComponent;
  Category: TJvCustomInspectorItem;
  I: Integer;
  AttributeData, PluginData: TJvInspectorEventData;
begin
  Inspector.AddComponent(InspectObject, SComponentDataTitle);

  if InspectObject is TFunLabyCollection then
    InspectCollection := TFunLabyCollection(InspectObject);

  if InspectObject is TFunLabyComponent then
  begin
    Component := TFunLabyComponent(InspectObject);

    if Component.UsePlayerData and
      HasAnyProperty(TPlayerDataAccess.AccessPlayerDataClass(Component)) then
    begin
      FPlayerDataList.InspectObject := Component;

      TJvInspectorVarData.New(Inspector.Root, SPlayerDataTitle,
        TypeInfo(TPlayerDataList), @FPlayerDataList);
    end;

    if InspectObject is TPlayer then
    begin
      // Attributs du joueur

      Category := TJvInspectorCustomCategoryItem.Create(Inspector.Root, nil);
      Category.DisplayName := SPlayerAttributesTitle;

      for I := 0 to FKnownAttributes.Count-1 do
      begin
        AttributeData := TJvInspectorEventData.New(Category,
          FKnownAttributes[I], TypeInfo(Integer)).Data as TJvInspectorEventData;
        AttributeData.OnGetAsOrdinal := GetPlayerAttributeAsOrdinal;
        AttributeData.OnSetAsOrdinal := SetPlayerAttributeAsOrdinal;
      end;

      // Plug-in attach�s au joueur

      Category := TJvInspectorCustomCategoryItem.Create(Inspector.Root, nil);
      Category.DisplayName := SPlayerPluginsTitle;

      for I := 0 to Master.PluginCount-1 do
      begin
        PluginData := TJvInspectorEventData.New(Category,
          Master.Plugins[I].ID,
          TypeInfo(Boolean)).Data as TJvInspectorEventData;
        PluginData.OnGetAsOrdinal := GetPlayerPluginAsOrdinal;
        PluginData.OnSetAsOrdinal := SetPlayerPluginAsOrdinal;
      end;
    end;
  end;

  Inspector.Root.ExpandItems(True);
end;

{*
  Met � jour l'�diteur de collection
*}
procedure TFrameInspector.UpdateCollectionEditor;
var
  I: Integer;
begin
  ListBoxCollectionItems.Clear;

  if InspectCollection = nil then
    Exit;

  for I := 0 to InspectCollection.Count-1 do
    ListBoxCollectionItems.Items.Add(Format(SCollectionItemTitle,
      [I, InspectCollection[I].ClassName]));

  ListBoxCollectionItems.ItemIndex :=
    InspectCollection.IndexOf(InspectObject);
end;

{*
  Gestionnaire d'�v�nement OnGetAsOrdinal des items d'attribut du joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Value    En sortie : valeur ordinale de l'attribut
*}
procedure TFrameInspector.GetPlayerAttributeAsOrdinal(
  Sender: TJvInspectorEventData; var Value: Int64);
begin
  if InspectObject is TPlayer then
    Value := TPlayer(InspectObject).Attribute[Sender.Name]
  else
    Value := 0;
end;

{*
  Gestionnaire d'�v�nement OnSetAsOrdinal des items d'attribut du joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Value    En entr�e : valeur ordinale de l'attribut
*}
procedure TFrameInspector.SetPlayerAttributeAsOrdinal(
  Sender: TJvInspectorEventData; var Value: Int64);
begin
  if InspectObject is TPlayer then
    TPlayer(InspectObject).Attribute[Sender.Name] := Value;
end;

{*
  Gestionnaire d'�v�nement OnGetAsOrdinal des items de plug-in du joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Value    En sortie : valeur ordinale du plug-in
*}
procedure TFrameInspector.GetPlayerPluginAsOrdinal(
  Sender: TJvInspectorEventData; var Value: Int64);
begin
  if InspectObject is TPlayer then
    Value := Byte(TPlayer(InspectObject).HasPlugin(Master.Plugin[Sender.Name]))
  else
    Value := 0;
end;

{*
  Gestionnaire d'�v�nement OnSetAsOrdinal des items de plug-in du joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Value    En entr�e : valeur ordinale du plug-in
*}
procedure TFrameInspector.SetPlayerPluginAsOrdinal(
  Sender: TJvInspectorEventData; var Value: Int64);
var
  Plugin: TPlugin;
begin
  if InspectObject is TPlayer then
  begin
    Plugin := Master.Plugin[Sender.Name];

    if Value <> 0 then
      TPlayer(InspectObject).AddPlugin(Plugin)
    else
      TPlayer(InspectObject).RemovePlugin(Plugin);
  end;
end;

{*
  Modifie l'objet inspect�
  @param Value   Nouvel objet � inspecter
*}
procedure TFrameInspector.SetInspectObject(const Value: TFunLabyPersistent);
begin
  if Value <> FInspectObject then
  begin
    FInspectObject := Value;

    ClearInspector;

    if Value <> nil then
      FillInspector;
  end;
end;

{*
  Modifie la collection inspect�e
  @param Value   Nouvelle collection � inspecter
*}
procedure TFrameInspector.SetInspectCollection(const Value: TFunLabyCollection);
begin
  FInspectCollection := Value;

  PanelCollectionEditor.Visible := Value <> nil;

  if PanelCollectionEditor.Visible then
    UpdateCollectionEditor;
end;

{*
  Charge un fichier
  @param AMasterFile   Fichier ma�tre
*}
procedure TFrameInspector.LoadFile(AMasterFile: TMasterFile);
begin
  MasterFile := AMasterFile;
  Master := MasterFile.Master;

  Inspector.Root.SortKind := iskNone;

  LoadKnownAttributes;
end;

{*
  D�charge le fichier courant
*}
procedure TFrameInspector.UnloadFile;
begin
  InspectObject := nil;

  FKnownAttributes.Clear;

  Master := nil;
  MasterFile := nil;
end;

{*
  Gestionnaire d'�v�nement OnDataValueChanged de l'inspecteur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Data     Donn�es modifi�es
*}
procedure TFrameInspector.InspectorDataValueChanged(Sender: TObject;
  Data: TJvCustomInspectorData);
begin
  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick de la liste des �l�ments de collection
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameInspector.ListBoxCollectionItemsClick(Sender: TObject);
begin
  Assert(InspectCollection <> nil);

  if ListBoxCollectionItems.ItemIndex >= 0 then
    InspectObject := InspectCollection[ListBoxCollectionItems.ItemIndex];
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Ajouter un �l�ment de collection
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameInspector.ButtonCollectionAddClick(Sender: TObject);
begin
  Assert(InspectCollection <> nil);

  InspectObject := InspectCollection.AddDefault;
  UpdateCollectionEditor;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Supprimer un �l�ment de collection
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameInspector.ButtonCollectionRemoveClick(Sender: TObject);
var
  Index: Integer;
begin
  Assert(InspectCollection <> nil);

  Index := ListBoxCollectionItems.ItemIndex;

  if Index >= 0 then
  begin
    InspectObject := InspectCollection;
    InspectCollection.Delete(Index);
    UpdateCollectionEditor;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton D�placer un �l�ment vers le haut
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameInspector.ButtonCollectionMoveUpClick(Sender: TObject);
var
  Index: Integer;
begin
  Assert(InspectCollection <> nil);

  Index := ListBoxCollectionItems.ItemIndex;

  if Index > 0 then
  begin
    InspectCollection.Exchange(Index, Index-1);
    UpdateCollectionEditor;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton D�placer un �l�ment vers le bas
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameInspector.ButtonCollectionMoveDownClick(Sender: TObject);
var
  Index: Integer;
begin
  Assert(InspectCollection <> nil);

  Index := ListBoxCollectionItems.ItemIndex;

  if Index < InspectCollection.Count-1 then
  begin
    InspectCollection.Exchange(Index, Index+1);
    UpdateCollectionEditor;
  end;
end;

initialization
  with TJvCustomInspectorData.ItemRegister do
  begin
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorFunLabyPersistentItem,
      TypeInfo(TFunLabyPersistent)));

    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorFunLabyCollectionItem,
      TypeInfo(TFunLabyCollection)));

    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorPlayerDataListItem,
      TypeInfo(TPlayerDataList)));
  end;
end.

