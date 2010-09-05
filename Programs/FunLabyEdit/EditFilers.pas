unit EditFilers;

interface

uses
  Types, SysUtils, Classes, TypInfo, ScCoroutines, ScTypInfo,
  FunLabyUtils, FunLabyFilers;

type
  {*
    Pseudo-filer servant à purger les références à un composant donné
    @author sjrd
    @version 5.1
  *}
  TFunLabyPurgeRefFiler = class(TFunLabyReader)
  private
    FReference: TFunLabyComponent; /// Référence à supprimer

    procedure HandleSubInstance(SubInstance: TFunLabyPersistent);
  protected
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
    constructor Create(AInstance: TFunLabyPersistent;
      AReference: TFunLabyComponent; AOwner: TFunLabyPurgeRefFiler = nil);

    class procedure PurgeReferences(AInstance: TFunLabyPersistent;
      AReference: TFunLabyComponent);

    property Reference: TFunLabyComponent read FReference;
  end;

  /// Type de propriété
  TPropKind = (pkProperty, pkPersistent, pkCollection, pkStrings, pkBinary);

  {*
    Pseudo-filer source d'une assignation en profondeur
    @author sjrd
    @version 5.1
  *}
  TFunLabyAssignSourceFiler = class(TFunLabyWriter)
  private
    FCoroutine: TCoroutine; /// Coroutine

    FPropName: string;    /// Nom de la propriété en cours
    FPropKind: TPropKind; /// Type de propriété en cours

    FPropInfo: PPropInfo; /// PropInfo de la propriété en cours (pkProperty)

    FSubInstance: TFunLabyPersistent; /// Sous-instance en cours (pkPersistent)

    FCollection: TFunLabyCollection; /// Collection en cours (pkCollection)

    FStrings: TStrings;     /// TStrings en cours (pkStrings)
    FObjectType: PTypeInfo; /// Type d'objet de TStrings en cours (pkStrings)

    FReadProc: TStreamProc; /// Méthode de lecture (pkBinary)
  protected
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

    procedure Yield(const PropName: string; PropKind: TPropKind);

    property Coroutine: TCoroutine read FCoroutine;
  public
    constructor Create(AInstance: TFunLabyPersistent; ACoroutine: TCoroutine);

    property PropName: string read FPropName;
    property PropKind: TPropKind read FPropKind;

    property PropInfo: PPropInfo read FPropInfo;
    property SubInstance: TFunLabyPersistent read FSubInstance;
    property Collection: TFunLabyCollection read FCollection;
    property Strings: TStrings read FStrings;
    property ObjectType: PTypeInfo read FObjectType;
    property ReadProc: TStreamProc read FReadProc;
  end;

  {*
    Pseudo-filer qui effectue une assignation en profondeur
    @author sjrd
    @version 5.1
  *}
  TFunLabyAssignmentFiler = class(TFunLabyReader)
  private
    FSource: TFunLabyPersistent; /// Instance source

    FCoroutine: TCoroutine;                  /// Coroutine
    FSourceFiler: TFunLabyAssignSourceFiler; /// Filer source

    FInvokeDone: Boolean; /// Utilisé par AskSource

    procedure CoroutineProc(Coroutine: TCoroutine);

    procedure HandleSubInstance(SubInstance, SubSource: TFunLabyPersistent);
  protected
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

    function AskSource(const PropName: string; PropKind: TPropKind): Boolean;

    property Coroutine: TCoroutine read FCoroutine;

    property SourceFiler: TFunLabyAssignSourceFiler read FSourceFiler;
  public
    constructor Create(AInstance, ASource: TFunLabyPersistent;
      AOwner: TFunLabyAssignmentFiler = nil);
    destructor Destroy; override;

    class procedure Assign(Dest, Source: TFunLabyPersistent);

    property Source: TFunLabyPersistent read FSource;
  end;

implementation

{-----------------------------}
{ TFunLabyPurgeRefFiler class }
{-----------------------------}

{*
  Crée le pseudo-filer
  @param AInstance    Instance à traiter
  @param AReference   Référence à supprimer
  @param AOwner       Filer propriétaire
*}
constructor TFunLabyPurgeRefFiler.Create(AInstance: TFunLabyPersistent;
  AReference: TFunLabyComponent; AOwner: TFunLabyPurgeRefFiler = nil);
begin
  inherited Create(AInstance, AOwner);

  FReference := AReference;
end;

{*
  Traite une sous-instance
  @param SubInstance   Sous-instance à traiter
*}
procedure TFunLabyPurgeRefFiler.HandleSubInstance(
  SubInstance: TFunLabyPersistent);
var
  SubFiler: TFunLabyPurgeRefFiler;
begin
  SubFiler := TFunLabyPurgeRefFiler.Create(SubInstance, Reference, Self);
  try
    SubFiler.EnumProperties;
  finally
    SubFiler.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleProperty(PropInfo: PPropInfo;
  HasData: Boolean);
var
  PropType: PTypeInfo;
  PropValue: TFunLabyComponent;
begin
  PropType := PropInfo.PropType^;

  if (PropType.Kind = tkClass) and
    GetTypeData(PropType).ClassType.InheritsFrom(TFunLabyComponent) then
  begin
    PropValue := TFunLabyComponent(GetOrdProp(Instance, PropInfo));

    if PropValue = Reference then
      SetOrdProp(Instance, PropInfo, 0);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandlePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
begin
  HandleSubInstance(SubInstance);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleCollection(const Name: string;
  Collection: TFunLabyCollection);
begin
  HandleSubInstance(Collection);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleComponent(const Name: string;
  Component: TFunLabyComponent);
begin
  HandleSubInstance(Component);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleStrings(const Name: string;
  Strings: TStrings; ObjectType: PTypeInfo; HasData: Boolean);
begin
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean);
begin
end;

{*
  Élimine les références à un composant dans un objet persistant
  @param AInstance    Objet persistant à traiter
  @param AReference   Référence à éliminer
*}
class procedure TFunLabyPurgeRefFiler.PurgeReferences(
  AInstance: TFunLabyPersistent; AReference: TFunLabyComponent);
begin
  with Create(AInstance, AReference) do
  try
    EnumProperties;
  finally
    Free;
  end;
end;

{---------------------------------}
{ TFunLabyAssignSourceFiler class }
{---------------------------------}

{*
  Crée une instance de TFunLabyAssignSourceFiler
  @param AInstance    Instance source
  @param ACoroutine   Coroutine
*}
constructor TFunLabyAssignSourceFiler.Create(AInstance: TFunLabyPersistent;
  ACoroutine: TCoroutine);
begin
  inherited Create(AInstance);

  FCoroutine := ACoroutine;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignSourceFiler.HandleProperty(PropInfo: PPropInfo;
  HasData: Boolean);
begin
  FPropInfo := PropInfo;
  Yield(TypeInfoDecode(PropInfo.Name), pkProperty);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignSourceFiler.HandlePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
begin
  FSubInstance := SubInstance;
  Yield(Name, pkPersistent);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignSourceFiler.HandleCollection(const Name: string;
  Collection: TFunLabyCollection);
begin
  FCollection := Collection;
  Yield(Name, pkCollection);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignSourceFiler.HandleComponent(const Name: string;
  Component: TFunLabyComponent);
begin
  Assert(False);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignSourceFiler.HandleStrings(const Name: string;
  Strings: TStrings; ObjectType: PTypeInfo; HasData: Boolean);
begin
  FStrings := Strings;
  FObjectType := ObjectType;
  Yield(Name, pkStrings);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignSourceFiler.HandleBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean);
begin
  FReadProc := ReadProc;
  Yield(Name, pkBinary);
end;

{*
  Renvoie une propriété
  @param PropName   Nom de la propriété
  @param PropKind   Type de propriété
*}
procedure TFunLabyAssignSourceFiler.Yield(const PropName: string;
  PropKind: TPropKind);
begin
  FPropName := PropName;
  FPropKind := PropKind;

  Coroutine.Yield;
end;

{-------------------------------}
{ TFunLabyAssignmentFiler class }
{-------------------------------}

{*
  Crée une instance de TFunLabyAssignmentFiler
  @param AInstance   Instance à modifier
  @param ASource     Instance source
  @param AOwner      Filer propriétaire
*}
constructor TFunLabyAssignmentFiler.Create(
  AInstance, ASource: TFunLabyPersistent;
  AOwner: TFunLabyAssignmentFiler = nil);
begin
  inherited Create(AInstance, AOwner);

  FSource := ASource;

  FCoroutine := TCoroutine.Create(CoroutineProc);
  FSourceFiler := TFunLabyAssignSourceFiler.Create(FSource, FCoroutine);
end;

{*
  [@inheritDoc]
*}
destructor TFunLabyAssignmentFiler.Destroy;
begin
  FSourceFiler.Free;
  FCoroutine.Free;

  inherited;
end;

{*
  Méthode de la coroutine
  @param Coroutine   Coroutine
*}
procedure TFunLabyAssignmentFiler.CoroutineProc(Coroutine: TCoroutine);
begin
  SourceFiler.EnumProperties;
  SourceFiler.FPropName := '';
end;

{*
  Gère une sous-instance
  @param SubInstance   Sous-instance destination
  @param SubSource     Sous-instance source
*}
procedure TFunLabyAssignmentFiler.HandleSubInstance(
  SubInstance, SubSource: TFunLabyPersistent);
begin
  Assign(SubInstance, SubSource);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignmentFiler.HandleProperty(PropInfo: PPropInfo;
  HasData: Boolean);
var
  SrcPropInfo: PPropInfo;
  PropType: PTypeInfo;
begin
  if not AskSource(TypeInfoDecode(PropInfo.Name), pkProperty) then
    Exit;

  SrcPropInfo := SourceFiler.PropInfo;
  PropType := PropInfo.PropType^;

  Assert(SrcPropInfo.PropType^ = PropInfo.PropType^);

  case PropType.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkClass, tkWChar:
      SetOrdProp(Instance, PropInfo, GetOrdProp(Source, SrcPropInfo));

    tkString, tkLString, tkWString, tkUString:
      SetStrProp(Instance, PropInfo, GetStrProp(Source, SrcPropInfo));

    tkFloat:
      SetFloatProp(Instance, PropInfo, GetFloatProp(Source, SrcPropInfo));

    tkVariant:
      SetVariantProp(Instance, PropInfo, GetVariantProp(Source, SrcPropInfo));

    tkInt64:
      SetInt64Prop(Instance, PropInfo, GetInt64Prop(Source, SrcPropInfo));
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignmentFiler.HandlePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
begin
  if not AskSource(Name, pkPersistent) then
    Exit;

  HandleSubInstance(SubInstance, SourceFiler.SubInstance);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignmentFiler.HandleCollection(const Name: string;
  Collection: TFunLabyCollection);
var
  SrcCollection: TFunLabyCollection;
  I: Integer;
  Item, SrcItem: TFunLabyPersistent;
begin
  if not AskSource(Name, pkPersistent) then
    Exit;

  SrcCollection := SourceFiler.Collection;

  Collection.Clear;

  HandleSubInstance(Collection, SrcCollection);

  for I := 0 to SrcCollection.Count-1 do
  begin
    SrcItem := SrcCollection.Items[I];
    Item := Collection.Add(TFunLabyPersistentClass(SrcItem.ClassType));

    HandleSubInstance(Item, SrcItem);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignmentFiler.HandleComponent(const Name: string;
  Component: TFunLabyComponent);
begin
  Assert(False);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignmentFiler.HandleStrings(const Name: string;
  Strings: TStrings; ObjectType: PTypeInfo; HasData: Boolean);
begin
  if not AskSource(Name, pkStrings) then
    Exit;

  Assert(SourceFiler.ObjectType = ObjectType);

  Strings.Assign(SourceFiler.Strings);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyAssignmentFiler.HandleBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean);
var
  Stream: TMemoryStream;
begin
  if not AskSource(Name, pkBinary) then
    Exit;

  Stream := TMemoryStream.Create;
  try
    SourceFiler.ReadProc(Stream);
    Stream.Seek(0, soBeginning);
    WriteProc(Stream);
  finally
    Stream.Free;
  end;
end;

{*
  Demande une propriété à la source
  @param PropName   Nom de la propriété
  @param PropKind   Type de propriété
  @return True si la propriété a été trouvée, False sinon
*}
function TFunLabyAssignmentFiler.AskSource(const PropName: string;
  PropKind: TPropKind): Boolean;
begin
  if not FInvokeDone then
    Coroutine.Invoke;

  if SourceFiler.PropName = PropName then
  begin
    Result := PropName <> 'ID'; // ID is *really* special
    FInvokeDone := False;
  end else
  begin
    Result := False;
    FInvokeDone := True;
  end;

  Assert((not Result) or (SourceFiler.PropKind = PropKind));
end;

{*
  Assigne un objet persistent à un autre
  @param Dest     Objet destination
  @param Source   Objet source
*}
class procedure TFunLabyAssignmentFiler.Assign(
  Dest, Source: TFunLabyPersistent);
begin
  Assert(Dest.ClassType = Source.ClassType);

  with Create(Dest, Source) do
  try
    EnumProperties;
  finally
    Free;
  end;
end;

end.

