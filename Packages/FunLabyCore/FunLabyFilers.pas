{*
  Gestion des filers FunLabyrinthe
  L'unité FunLabyFilers définit les classes de filers utilisés pour charger et
  enregistrer les objets persistents FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FunLabyFilers;

interface

uses
  SysUtils, Classes, TypInfo, Variants, FunLabyUtils, msxml;

type
  {*
    Lecteur FunLabyrinthe depuis un document XML
    @author sjrd
    @version 1.0
  *}
  TFunLabyXMLReader = class(TFunLabyFiler)
  private
    FNode: IXMLDOMElement; /// Noeud correspondant à cette instance
  protected
    procedure ReadProperty(PropInfo: PPropInfo; const PropNode: IXMLDOMElement);
    procedure ReadProperties(SubInstance: TFunLabyPersistent;
      const InstanceNode: IXMLDOMElement);

    procedure HandleProperty(PropInfo: PPropInfo; HasData: Boolean); override;

    procedure HandlePersistent(const Name: string;
      SubInstance: TFunLabyPersistent); override;

    procedure HandleCollection(const Name: string;
      Collection: TFunLabyCollection); override;

    procedure HandleComponent(const Name: string;
      Component: TFunLabyComponent); override;

    procedure HandleStrings(const Name: string; Strings: TStrings;
      ObjectKind: TStringsObjectKind); override;
  public
    procedure ReadNode(const ANode: IXMLDOMElement);

    class procedure ReadMaster(Master: TMaster; const Node: IXMLDOMElement);

    property Node: IXMLDOMElement read FNode;
  end;

  {*
    Écrivain FunLabyrinthe vers un document XML
    @author sjrd
    @version 1.0
  *}
  TFunLabyXMLWriter = class(TFunLabyFiler)
  private
    FNode: IXMLDOMElement; /// Noeud correspondant à cette instance
  protected
    procedure WriteProperty(PropInfo: PPropInfo;
      const PropNode: IXMLDOMElement);
    procedure WriteProperties(SubInstance: TFunLabyPersistent;
      const InstanceNode: IXMLDOMElement);

    procedure HandleProperty(PropInfo: PPropInfo; HasData: Boolean); override;

    procedure HandlePersistent(const Name: string;
      SubInstance: TFunLabyPersistent); override;

    procedure HandleCollection(const Name: string;
      Collection: TFunLabyCollection); override;

    procedure HandleComponent(const Name: string;
      Component: TFunLabyComponent); override;

    procedure HandleStrings(const Name: string; Strings: TStrings;
      ObjectKind: TStringsObjectKind); override;
  public
    procedure WriteNode(const ANode: IXMLDOMElement);

    class procedure WriteMaster(Master: TMaster; const Node: IXMLDOMElement);

    property Node: IXMLDOMElement read FNode;
  end;

implementation

{-------------------------}
{ TFunLabyXMLReader class }
{-------------------------}

{*
  Lit une propriété depuis le noeud correspondant
  @param PropInfo   PropInfo de la propriété
  @param PropNode   Noeud correspondant
*}
procedure TFunLabyXMLReader.ReadProperty(PropInfo: PPropInfo;
  const PropNode: IXMLDOMElement);
var
  Text: string;
  PropType: PTypeInfo;
  PropClass: TClass;
begin
  Text := PropNode.text;
  PropType := PropInfo.PropType^;

  case PropInfo.PropType^.Kind of
    tkInteger:
      SetOrdProp(Instance, PropInfo, StrToInt(Text));
    tkChar:
      SetOrdProp(Instance, PropInfo, Ord(Text[1]));
    tkEnumeration:
      SetOrdProp(Instance, PropInfo, GetEnumValue(PropType, Text));
    tkFloat:
      SetFloatProp(Instance, PropInfo, StrToFloat(Text));
    tkString, tkLString, tkWString:
      SetStrProp(Instance, PropInfo, Text);
    tkSet:
      SetSetProp(Instance, PropInfo, Text);
    tkInt64:
      SetInt64Prop(Instance, PropInfo, StrToInt64(Text));
    tkClass:
    begin
      PropClass := GetTypeData(PropType).ClassType;

      if PropClass.InheritsFrom(TFunLabyComponent) then
        SetOrdProp(Instance, PropInfo, Integer(
          Master.Component[Text] as PropClass));
    end;
  end;
end;

{*
  Lit les propriétés d'un objet persistent
  @param SubInstance    Objet persistent
  @param InstanceNode   Noeud contenant les propriétés de l'objet persistent
*}
procedure TFunLabyXMLReader.ReadProperties(SubInstance: TFunLabyPersistent;
  const InstanceNode: IXMLDOMElement);
var
  SubReader: TFunLabyXMLReader;
begin
  SubReader := TFunLabyXMLReader.Create(Master, SubInstance);
  try
    SubReader.ReadNode(InstanceNode);
  finally
    SubReader.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLReader.HandleProperty(PropInfo: PPropInfo;
  HasData: Boolean);
var
  TypeKindName: string;
  PropNode: IXMLDOMElement;
begin
  TypeKindName := GetEnumName(TypeInfo(TTypeKind),
    Byte(PropInfo.PropType^.Kind));

  PropNode := Node.selectSingleNode(
    Format('property[@name="%s"][@type="%s"]', [PropInfo.Name, TypeKindName])
    ) as IXMLDOMElement;

  if PropNode <> nil then
    ReadProperty(PropInfo, PropNode);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLReader.HandlePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
var
  InstanceNode: IXMLDOMElement;
begin
  InstanceNode := Node.selectSingleNode(
    Format('object[@name="%s"]', [Name])) as IXMLDOMElement;

  if InstanceNode <> nil then
    ReadProperties(SubInstance, InstanceNode);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLReader.HandleCollection(const Name: string;
  Collection: TFunLabyCollection);
var
  CollectionNode, ItemNode: IXMLDOMElement;
  ItemNodeList: IXMLDOMNodeList;
  I: Integer;
  ItemClass: TFunLabyPersistentClass;
  Item: TFunLabyPersistent;
begin
  CollectionNode := Node.selectSingleNode(
    Format('collection[@name="%s"]', [Name])) as IXMLDOMElement;

  if CollectionNode = nil then
    Exit;

  Collection.Clear;

  ReadProperties(Collection, CollectionNode);

  ItemNodeList := CollectionNode.selectNodes('items/item');
  for I := 0 to ItemNodeList.length-1 do
  begin
    ItemNode := ItemNodeList.item[I] as IXMLDOMElement;

    ItemClass := FunLabyFindClass(ItemNode.getAttribute('class'));
    Item := Collection.Add(ItemClass);

    ReadProperties(Item, ItemNode);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLReader.HandleComponent(const Name: string;
  Component: TFunLabyComponent);
var
  ComponentNode, PlayerDataNode: IXMLDOMElement;
  I: Integer;
  Player: TPlayer;
  PlayerData: TPlayerData;
begin
  ComponentNode := Node.selectSingleNode(
    Format('component[@name="%s"]', [Name])) as IXMLDOMElement;

  if ComponentNode = nil then
    Exit;

  ReadProperties(Component, ComponentNode);

  for I := 0 to Master.PlayerCount-1 do
  begin
    Player := Master.Players[I];
    PlayerDataNode := ComponentNode.selectSingleNode(
      Format('playerdata[@player="%s"]', [Player.ID])) as IXMLDOMElement;

    if PlayerDataNode <> nil then
    begin
      PlayerData := GetPlayerData(Component, Player);
      ReadProperties(PlayerData, PlayerDataNode);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLReader.HandleStrings(const Name: string; Strings: TStrings;
  ObjectKind: TStringsObjectKind);

  function VarAsInteger(const Value: Variant): Integer;
  begin
    if VarIsNull(Value) then
      Result := 0
    else
      Result := Value;
  end;

var
  StringsNode, ItemNode: IXMLDOMElement;
  ItemNodeList: IXMLDOMNodeList;
  I: Integer;
  ObjAttrValue: Variant;
  Obj: TObject;
begin
  StringsNode := Node.selectSingleNode(
    Format('collection[@name="%s"]', [Name])) as IXMLDOMElement;

  if StringsNode = nil then
    Exit;

  Strings.BeginUpdate;
  try
    Strings.Clear;
    ObjAttrValue := Null;

    ItemNodeList := StringsNode.selectNodes('items/item[@class="string"]');
    for I := 0 to ItemNodeList.length-1 do
    begin
      ItemNode := ItemNodeList.item[I] as IXMLDOMElement;

      case ObjectKind of
        sokInteger:
          ObjAttrValue := ItemNode.getAttribute('value');
        sokComponent:
          ObjAttrValue := ItemNode.getAttribute('component');
      end;

      if VarIsNull(ObjAttrValue) then
        Obj := nil
      else
      begin
        case ObjectKind of
          sokInteger:
            Obj := TObject(StrToInt(ObjAttrValue));
          sokComponent:
            Obj := Master.Component[ObjAttrValue];
        else
          Obj := nil;
        end;
      end;

      Strings.AddObject(ItemNode.text, Obj);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{*
  Lit un noeud
  @param ANode   Noeud à lire
*}
procedure TFunLabyXMLReader.ReadNode(const ANode: IXMLDOMElement);
begin
  FNode := ANode;
  EnumProperties;
end;

{*
  Lit les données persistentes d'un maître FunLabyrinthe depuis un noeud XML
  @param Master   Maître FunLabyrinthe
  @param Node     Noeud à lire
*}
class procedure TFunLabyXMLReader.ReadMaster(Master: TMaster;
  const Node: IXMLDOMElement);
var
  ANode: IXMLDOMElement absolute Node;
begin
  with Create(Master, Master) do
  try
    ReadNode(ANode);
  finally
    Free;
  end;
end;

{-------------------------}
{ TFunLabyXMLWriter class }
{-------------------------}

{*
  Écrit une propriété depuis le noeud correspondant
  @param PropInfo   PropInfo de la propriété
  @param PropNode   Noeud correspondant
*}
procedure TFunLabyXMLWriter.WriteProperty(PropInfo: PPropInfo;
  const PropNode: IXMLDOMElement);
var
  Text: string;
  PropType: PTypeInfo;
  PropClass: TClass;
begin
  PropType := PropInfo.PropType^;

  case PropInfo.PropType^.Kind of
    tkInteger:
      Text := IntToStr(GetOrdProp(Instance, PropInfo));
    tkChar:
      Text := Chr(GetOrdProp(Instance, PropInfo));
    tkEnumeration:
      Text := GetEnumName(PropType, GetOrdProp(Instance, PropInfo));
    tkFloat:
      Text := FloatToStr(GetFloatProp(Instance, PropInfo));
    tkString, tkLString, tkWString:
      Text := GetStrProp(Instance, PropInfo);
    tkSet:
      Text := GetSetProp(Instance, PropInfo, True);
    tkInt64:
      Text := IntToStr(GetInt64Prop(Instance, PropInfo));
    tkClass:
    begin
      PropClass := GetTypeData(PropType).ClassType;

      if PropClass.InheritsFrom(TFunLabyComponent) then
        Text := TFunLabyComponent(GetOrdProp(Instance, PropInfo)).ID;
    end;
  end;

  PropNode.text := Text;
end;

{*
  Écrit les propriétés d'un objet persistent
  @param SubInstance    Objet persistent
  @param InstanceNode   Noeud contenant les propriétés de l'objet persistent
*}
procedure TFunLabyXMLWriter.WriteProperties(SubInstance: TFunLabyPersistent;
  const InstanceNode: IXMLDOMElement);
var
  SubWriter: TFunLabyXMLWriter;
begin
  SubWriter := TFunLabyXMLWriter.Create(Master, SubInstance);
  try
    SubWriter.WriteNode(InstanceNode);
  finally
    SubWriter.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLWriter.HandleProperty(PropInfo: PPropInfo;
  HasData: Boolean);
var
  TypeKindName: string;
  PropNode: IXMLDOMElement;
begin
  if not HasData then
    Exit;

  TypeKindName := GetEnumName(TypeInfo(TTypeKind),
    Byte(PropInfo.PropType^.Kind));

  PropNode := Node.ownerDocument.createElement('property');
  PropNode.setAttribute('name', PropInfo.Name);
  PropNode.setAttribute('type', TypeKindName);

  WriteProperty(PropInfo, PropNode);

  Node.appendChild(PropNode);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLWriter.HandlePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
var
  InstanceNode: IXMLDOMElement;
begin
  InstanceNode := Node.ownerDocument.createElement('object');
  InstanceNode.setAttribute('name', Name);

  WriteProperties(SubInstance, InstanceNode);

  if InstanceNode.hasChildNodes then
    Node.appendChild(InstanceNode);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLWriter.HandleCollection(const Name: string;
  Collection: TFunLabyCollection);
var
  CollectionNode, ItemsNode, ItemNode: IXMLDOMElement;
  I: Integer;
begin
  CollectionNode := Node.ownerDocument.createElement('collection');
  CollectionNode.setAttribute('name', Name);

  WriteProperties(Collection, CollectionNode);

  if Collection.Count > 0 then
  begin
    ItemsNode := Node.ownerDocument.createElement('items');

    for I := 0 to Collection.Count-1 do
    begin
      ItemNode := Node.ownerDocument.createElement('item');
      ItemNode.setAttribute('class', Collection.Items[I].ClassName);

      WriteProperties(Collection.Items[I], ItemNode);

      ItemsNode.appendChild(ItemNode);
    end;

    CollectionNode.appendChild(ItemsNode);
  end;

  if CollectionNode.hasChildNodes then
    Node.appendChild(CollectionNode);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLWriter.HandleComponent(const Name: string;
  Component: TFunLabyComponent);
var
  ComponentNode, PlayerDataNode: IXMLDOMElement;
  I: Integer;
  Player: TPlayer;
begin
  ComponentNode := Node.ownerDocument.createElement('component');
  ComponentNode.setAttribute('name', Name);

  WriteProperties(Component, ComponentNode);

  for I := 0 to Master.PlayerCount-1 do
  begin
    Player := Master.Players[I];
    if not HasPlayerData(Component, Player) then
      Continue;

    PlayerDataNode := Node.ownerDocument.createElement('playerdata');
    PlayerDataNode.setAttribute('player', Player.ID);

    WriteProperties(GetPlayerData(Component, Player), PlayerDataNode);

    if PlayerDataNode.hasChildNodes then
      ComponentNode.appendChild(PlayerDataNode);
  end;

  if ComponentNode.hasChildNodes then
    Node.appendChild(ComponentNode);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLWriter.HandleStrings(const Name: string; Strings: TStrings;
  ObjectKind: TStringsObjectKind);
var
  StringsNode, ItemsNode, ItemNode: IXMLDOMElement;
  I: Integer;
begin
  StringsNode := Node.ownerDocument.createElement('collection');
  StringsNode.setAttribute('name', Name);

  if Strings.Count > 0 then
  begin
    ItemsNode := Node.ownerDocument.createElement('items');

    for I := 0 to Strings.Count-1 do
    begin
      ItemNode := Node.ownerDocument.createElement('item');
      ItemNode.setAttribute('class', 'string'); // for symetry

      case ObjectKind of
        sokInteger:
          ItemNode.setAttribute('value', IntToStr(Integer(Strings.Objects[I])));
        sokComponent:
          ItemNode.setAttribute('component',
            TFunLabyComponent(Strings.Objects[I]).ID);
      end;

      ItemNode.text := Strings[I];

      ItemsNode.appendChild(ItemNode);
    end;

    StringsNode.appendChild(ItemsNode);
  end;

  if StringsNode.hasChildNodes then
    Node.appendChild(StringsNode);
end;

{*
  Écrit un noeud
  @param ANode   Noeud à lire
*}
procedure TFunLabyXMLWriter.WriteNode(const ANode: IXMLDOMElement);
begin
  FNode := ANode;
  EnumProperties;
end;

{*
  Écrit les données persistentes d'un maître FunLabyrinthe depuis un noeud XML
  @param Master   Maître FunLabyrinthe
  @param Node     Noeud à lire
*}
class procedure TFunLabyXMLWriter.WriteMaster(Master: TMaster;
  const Node: IXMLDOMElement);
var
  ANode: IXMLDOMElement absolute Node;
begin
  with Create(Master, Master) do
  try
    WriteNode(ANode);
  finally
    Free;
  end;
end;

end.

