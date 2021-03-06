{*
  Gestion des filers FunLabyrinthe
  L'unit� FunLabyFilers d�finit les classes de filers utilis�s pour charger et
  enregistrer les objets persistents FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FunLabyFilers;

interface

uses
  SysUtils, Classes, TypInfo, Variants, msxml, ScTypInfo, ScXML, FunLabyUtils;

type
  {*
    Lecteur FunLabyrinthe depuis un document XML
    @author sjrd
    @version 1.0
  *}
  TFunLabyXMLReader = class(TFunLabyReader)
  private
    FNode: IXMLDOMElement; /// Noeud correspondant � cette instance
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
      ObjectType: PTypeInfo; HasData: Boolean); override;

    procedure HandleBinaryProperty(const Name: string;
      ReadProc, WriteProc: TStreamProc; HasData: Boolean); override;
  public
    procedure ReadNode(const ANode: IXMLDOMElement);

    class procedure ReadPersistent(AInstance: TFunLabyPersistent;
      const ANode: IXMLDOMElement);

    property Node: IXMLDOMElement read FNode;
  end;

  {*
    �crivain FunLabyrinthe vers un document XML
    @author sjrd
    @version 1.0
  *}
  TFunLabyXMLWriter = class(TFunLabyWriter)
  private
    FNode: IXMLDOMElement; /// Noeud correspondant � cette instance
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
      ObjectType: PTypeInfo; HasData: Boolean); override;

    procedure HandleBinaryProperty(const Name: string;
      ReadProc, WriteProc: TStreamProc; HasData: Boolean); override;
  public
    procedure WriteNode(const ANode: IXMLDOMElement);

    class procedure WritePersistent(AInstance: TFunLabyPersistent;
      const ANode: IXMLDOMElement);

    property Node: IXMLDOMElement read FNode;
  end;

implementation

const
  FloatFormatSettings: TFormatSettings = (
    DecimalSeparator: '.';
  );

{--------------------}
{ TStringsItem class }
{--------------------}

type
  TStringsItem = class(TFunLabyPersistent)
  private
    FStr: string;
    FObj: TObject;

    FObjectType: PTypeInfo;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
  public
    constructor Create(AObjectType: PTypeInfo);

    property Str: string read FStr write FStr;
    property Obj: TObject read FObj write FObj;
  end;

constructor TStringsItem.Create(AObjectType: PTypeInfo);
begin
  inherited Create;

  FObjectType := AObjectType;
end;

procedure TStringsItem.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  Filer.DefineFieldProperty('String', TypeInfo(string), @FStr);
  Filer.DefineFieldProperty('Object', FObjectType, @FObj);
end;

{-------------------------}
{ TFunLabyXMLReader class }
{-------------------------}

{*
  Lit une propri�t� depuis le noeud correspondant
  @param PropInfo   PropInfo de la propri�t�
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
      SetFloatProp(Instance, PropInfo, StrToFloat(Text, FloatFormatSettings));
    tkString, tkLString, tkWString, tkUString:
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
  Lit les propri�t�s d'un objet persistent
  @param SubInstance    Objet persistent
  @param InstanceNode   Noeud contenant les propri�t�s de l'objet persistent
*}
procedure TFunLabyXMLReader.ReadProperties(SubInstance: TFunLabyPersistent;
  const InstanceNode: IXMLDOMElement);
var
  SubReader: TFunLabyXMLReader;
begin
  SubReader := TFunLabyXMLReader.Create(SubInstance, Self);
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
const
  TypeIsAnyStringPredicate =
    '@type="tkString" or @type="tkLString" or @type="tkWString" or '+
    '@type="tkUString"';
var
  TypeKind: TTypeKInd;
  TypeKindName, TypeKindPredicate: string;
  PropNode: IXMLDOMElement;
begin
  TypeKind := PropInfo.PropType^.Kind;
  TypeKindName := GetEnumName(TypeInfo(TTypeKind), Byte(TypeKind));

  if TypeKind in [tkString, tkLString, tkWString, tkUString] then
    TypeKindPredicate := TypeIsAnyStringPredicate
  else
    TypeKindPredicate := Format('@type="%s"', [TypeKindName]);

  PropNode := Node.selectSingleNode(
    Format('property[@name="%s"][%s]', [PropInfo.Name, TypeKindPredicate])
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
  ObjectType: PTypeInfo; HasData: Boolean);
var
  Item: TStringsItem;
  ItemClassName: string;
  StringsNode, ItemNode: IXMLDOMElement;
  ItemNodeList: IXMLDOMNodeList;
  I: Integer;
begin
  StringsNode := Node.selectSingleNode(
    Format('collection[@name="%s"]', [Name])) as IXMLDOMElement;

  if StringsNode = nil then
    Exit;

  if ObjectType = nil then
    ItemClassName := 'string'
  else
    ItemClassName := 'string+'+
      GetEnumName(TypeInfo(TTypeKind), Byte(ObjectType.Kind));

  Item := nil;
  Strings.BeginUpdate;
  try
    if ObjectType <> nil then
      Item := TStringsItem.Create(ObjectType);

    Strings.Clear;

    ItemNodeList := StringsNode.selectNodes(
      Format('items/item[@class="%s"]', [ItemClassName]));
    for I := 0 to ItemNodeList.length-1 do
    begin
      ItemNode := ItemNodeList.item[I] as IXMLDOMElement;

      if ObjectType = nil then
        Strings.Add(ItemNode.text)
      else
      begin
        ReadProperties(Item, ItemNode);
        Strings.AddObject(Item.Str, Item.Obj);
      end;
    end;
  finally
    Item.Free;
    Strings.EndUpdate;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLReader.HandleBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean);
var
  PropNode: IXMLDOMElement;
  EncodedStream: TStringStream;
  Stream: TMemoryStream;
begin
  PropNode := Node.selectSingleNode(
    Format('property[@name="%s"][@type="tkBinary"]', [Name])) as IXMLDOMElement;

  if PropNode = nil then
    Exit;

  EncodedStream := nil;
  Stream := nil;
  try
    EncodedStream := TStringStream.Create(PropNode.text);
    Stream := TMemoryStream.Create;

    Base64Decode(EncodedStream, Stream);

    Stream.Seek(0, soFromBeginning);
    ReadProc(Stream);
  finally
    Stream.Free;
    EncodedStream.Free;
  end;
end;

{*
  Lit un noeud
  @param ANode   Noeud � lire
*}
procedure TFunLabyXMLReader.ReadNode(const ANode: IXMLDOMElement);
begin
  FNode := ANode;
  EnumProperties;
end;

{*
  Lit un objet persistant depuis un noeud XML
  @param AInstance   Instance � lire
  @param Node        Noeud � lire
*}
class procedure TFunLabyXMLReader.ReadPersistent(AInstance: TFunLabyPersistent;
  const ANode: IXMLDOMElement);
begin
  with Create(AInstance) do
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
  �crit une propri�t� depuis le noeud correspondant
  @param PropInfo   PropInfo de la propri�t�
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
      Text := FloatToStr(GetFloatProp(Instance, PropInfo), FloatFormatSettings);
    tkString, tkLString, tkWString, tkUString:
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
  �crit les propri�t�s d'un objet persistent
  @param SubInstance    Objet persistent
  @param InstanceNode   Noeud contenant les propri�t�s de l'objet persistent
*}
procedure TFunLabyXMLWriter.WriteProperties(SubInstance: TFunLabyPersistent;
  const InstanceNode: IXMLDOMElement);
var
  SubWriter: TFunLabyXMLWriter;
begin
  SubWriter := TFunLabyXMLWriter.Create(SubInstance, Self);
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
  ObjectType: PTypeInfo; HasData: Boolean);
var
  Item: TStringsItem;
  ItemClassName: string;
  StringsNode, ItemsNode, ItemNode: IXMLDOMElement;
  I: Integer;
begin
  if not HasData then
    Exit;

  StringsNode := Node.ownerDocument.createElement('collection');
  StringsNode.setAttribute('name', Name);

  if ObjectType = nil then
    ItemClassName := 'string'
  else
    ItemClassName := 'string+'+
      GetEnumName(TypeInfo(TTypeKind), Byte(ObjectType.Kind));

  Item := nil;
  try
    if ObjectType <> nil then
      Item := TStringsItem.Create(ObjectType);

    ItemsNode := Node.ownerDocument.createElement('items');

    for I := 0 to Strings.Count-1 do
    begin
      ItemNode := Node.ownerDocument.createElement('item');
      ItemNode.setAttribute('class', ItemClassName);

      if ObjectType = nil then
        ItemNode.text := Strings[I]
      else
      begin
        Item.Str := Strings[I];
        Item.Obj := Strings.Objects[I];
        WriteProperties(Item, ItemNode);
      end;

      ItemsNode.appendChild(ItemNode);
    end;

    StringsNode.appendChild(ItemsNode);
  finally
    Item.Free;
  end;

  if StringsNode.hasChildNodes then
    Node.appendChild(StringsNode);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyXMLWriter.HandleBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean);
var
  PropNode: IXMLDOMElement;
  EncodedStream: TStringStream;
  Stream: TMemoryStream;
begin
  if not HasData then
    Exit;

  PropNode := Node.ownerDocument.createElement('property');
  PropNode.setAttribute('name', Name);
  PropNode.setAttribute('type', 'tkBinary');

  EncodedStream := nil;
  Stream := nil;
  try
    EncodedStream := TStringStream.Create('');
    Stream := TMemoryStream.Create;

    WriteProc(Stream);

    Stream.Seek(0, soFromBeginning);
    Base64Encode(Stream, EncodedStream);

    PropNode.text := EncodedStream.DataString;
  finally
    Stream.Free;
    EncodedStream.Free;
  end;

  Node.appendChild(PropNode);
end;

{*
  �crit un noeud
  @param ANode   Noeud � lire
*}
procedure TFunLabyXMLWriter.WriteNode(const ANode: IXMLDOMElement);
begin
  FNode := ANode;
  EnumProperties;
end;

{*
  �crit un objet persistant dans un noeud XML
  @param AInstance   Instance � �crire
  @param Node        Noeud � �crire
*}
class procedure TFunLabyXMLWriter.WritePersistent(AInstance: TFunLabyPersistent;
  const ANode: IXMLDOMElement);
begin
  with Create(AInstance) do
  try
    WriteNode(ANode);
  finally
    Free;
  end;
end;

end.

