{*
  Système de base des éditeurs d'unité de FunLabyEdit
  @author sjrd
  @version 5.0
*}
unit SourceEditors;

interface

uses
  SysUtils, Classes, Controls, ScUtils, ScLists, ScStrUtils, FilesUtils,
  UnitFiles, SepiReflectionCore, SepiCompilerErrors, FunLabyCoreConsts;

resourcestring
  SAllSourceTypes = 'Tous les types de source';

type
  ISourceEditor50 = interface;

  {*
    Événement de notification dont l'envoyeur est un éditeur de source
    @param Sender   Éditeur de source qui a déclenché l'événement
  *}
  TSourceEditorNotifyEvent = procedure(const Sender: ISourceEditor50) of object;

  {*
    Interface d'un éditeur de fichier source de FunLabyEdit
    Les classes implémentant cette interface doivent outrepasser le comptage de
    références, mais se libérer dans la méthode Release.
    @author sjrd
    @version 5.0
  *}
  ISourceEditor50 = interface
    ['{301FC63B-E81A-4861-A1EA-90AFD1FA038F}']

    {*
      Fichier source ouvert dans l'éditeur
      @return Fichier source ouvert
    *}
    function GetSourceFile: TSourceFile;

    {*
      Contrôle d'édition à placer dans la fiche principale de FunLabyEdit
      @return Contrôle d'édition
    *}
    function GetControl: TControl;

    {*
      Indique si le fichier source a été modifié
      @return True si le fichier source a été modifié, False sinon
    *}
    function GetModified: Boolean;

    {*
      Événement déclenché lorsque l'état de l'éditeur change
      @return L'événement associé
    *}
    function GetOnStateChange: TSourceEditorNotifyEvent;

    {*
      Modifie l'événement OnStateChange
      @param Value   Nouvel événement OnStateChange
    *}
    procedure SetOnStateChange(Value: TSourceEditorNotifyEvent);

    {*
      Enregistre le fichier source
      @return True si le fichier a bien été enregistré, False sinon
    *}
    function SaveFile: Boolean;

    {*
      Teste si l'éditeur peut être fermé
      @return True s'il peut être fermé, False pour empêcher la fermeture
    *}
    function CanClose: Boolean;

    {*
      Libère l'éditeur
    *}
    procedure Release;

    property SourceFile: TSourceFile read GetSourceFile;
    property Control: TControl read GetControl;
    property Modified: Boolean read GetModified;

    property OnStateChange: TSourceEditorNotifyEvent
      read GetOnStateChange write SetOnStateChange;
  end;

  {*
    Interface d'un éditeur de fichier source qui peut charger une unité
    @author sjrd
    @version 5.0
  *}
  IUnitEditor50 = interface(ISourceEditor50)
    ['{4F0CC4F1-D7F1-4D3A-B4A8-1DB7E713CEE4}']

    {*
      Nom de l'unité éditée
      @return Nom de l'unité éditée
    *}
    function GetUnitName: string;

    {*
      Charge l'unité éditée
      @param SepiRoot   Racine Sepi
      @return Unité Sepi chargée
    *}
    function LoadUnit(SepiRoot: TSepiRoot): TSepiUnit;

    property UnitName: string read GetUnitName;
  end;

  {*
    Interface d'un éditeur de fichier source qui doit compiler son source
    @author sjrd
    @version 5.0
  *}
  ISourceCompiler50 = interface(ISourceEditor50)
    ['{8B6345C1-7BCD-4C2D-8E1D-FE3549823006}']

    {*
      Nom de l'unité éditée
      @return Nom de l'unité éditée
    *}
    function GetUnitName: string;

    {*
      Compile le fichier source
      @param SepiRoot   Racine Sepi
      @param Errors     Erreurs de compilation
      @return Unité Sepi compilée
      @throws ESepiCompilerFatalError Erreur fatale de compilation
    *}
    function CompileFile(SepiRoot: TSepiRoot;
      Errors: TSepiCompilerErrorList): TSepiUnit;

    {*
      Montre une erreur dans le code source
      @param Error   Erreur à montrer
    *}
    procedure ShowError(Error: TSepiCompilerError);

    property UnitName: string read GetUnitName;
  end;

  {*
    Type de routine de call-back qui crée un éditeur de fichier source
    @param SourceFile   Fichier source à éditer
    @return Interface vers l'éditeur créé
  *}
  TCreateSourceEditorProc = function(SourceFile: TSourceFile): ISourceEditor50;

  {*
    Classe de base pour des tables associatives avec une clef de type string
    @author sjrd
    @version 5.0
  *}
  TCustomStringKeyValueBucketList = class(TCustomValueBucketList)
  protected
    function BucketFor(const Key): Cardinal; override;
    function KeyEquals(const Key1, Key2): Boolean; override;
  end;

  {*
    Collection d'éditeurs de fichiers source
    @author sjrd
    @version 5.0
  *}
  TSourceEditorList = class(TCustomStringKeyValueBucketList)
  private
    FFilters: TStrings; /// Filtres d'ouverture de fichier source

    function GetFilterCount: Integer;
    function GetFilters(Index: Integer): string;
    function GetFiltersAsText: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const Extension: string; CreateProc: TCreateSourceEditorProc);
    procedure Remove(const Extension: string);
    function Exists(const Extension: string): Boolean;
    function Find(const Extension: string): TCreateSourceEditorProc;

    procedure AddFilter(const Filter: string);
    procedure RemoveFilter(const Filter: string);

    class function SourceFileKey(SourceFile: TSourceFile): string;

    function ExistsEditor(SourceFile: TSourceFile): Boolean;
    function CreateEditor(SourceFile: TSourceFile): ISourceEditor50;

    property FilterCount: Integer read GetFilterCount;
    property Filters[Index: Integer]: string read GetFilters;
    property FiltersAsText: string read GetFiltersAsText;
  end;

  {*
    Type de routine de call-back qui crée un nouveau fichier source
    Si l'enregistrement du créateur a spécifié qu'il ne fallait pas demander
    automatiquement un nom de fichier, le paramètre FileName est vide lors de
    l'appel.
    Dans tous les cas, FileName peut être modifié, et doit indiquer en sortie -
    si la fonction renvoie True - le nom de fichier réel, qui doit exister.
    Si la fonction renvoie False, la valeur de FileName est indéfinie en sortie.
    @param FileName   Nom du fichier unité à créer
    @return True si le fichier a bien été créé, False sinon
  *}
  TCreateNewSourceFileProc = function(var FileName: TFileName): Boolean;

  /// Pointeur vers TSourceFileCreatorInfo
  PSourceFileCreatorInfo = ^TSourceFileCreatorInfo;

  {*
    Informations sur un créateur de fichier source
    @author sjrd
    @version 5.0
  *}
  TSourceFileCreatorInfo = record
    Title: string;           /// Titre du créateur
    Description: string;     /// Description longue
    AskForFileName: Boolean; /// Demander un nom de fichier
    Extension: string;       /// Extension des fichiers créés
  end;

  {*
    Collection de créateurs d'unité
    @author sjrd
    @version 5.0
  *}
  TSourceFileCreatorList = class(TCustomValueBucketList)
  public
    constructor Create;

    procedure Add(CreateProc: TCreateNewSourceFileProc;
      const Info: TSourceFileCreatorInfo);
    procedure Remove(CreateProc: TCreateNewSourceFileProc);
  end;

  {*
    Collection de filtres d'unité
    @author sjrd
    @version 5.0
  *}
  TUnitFilterList = class(TCustomStringKeyValueBucketList)
  private
    function GetExtension(const Filter: string): string;
  public
    constructor Create;

    procedure Add(const Filter: string; const Extension: string);
    procedure Remove(const Filter: string);

    property Extension[const Filter: string]: string read GetExtension;
  end;

var
  /// Éditeurs de source
  SourceFileEditors: TSourceEditorList = nil;

  /// Créateurs de source
  SourceFileCreators: TSourceFileCreatorList = nil;

  /// Filtres d'unité
  UnitFilters: TUnitFilterList = nil;

resourcestring
  /// Filtre BPL
  BPLFilter = 'Paquets Borland (*.bpl)|*.bpl';

  /// Filtre SCU
  SepiFilter = 'Unité Sepi compilée (*.scu)|*.scu';

implementation

{----------------------------------------}
{ Classe TCustomStringKeyValueBucketList }
{----------------------------------------}

{*
  [@inheritDoc]
*}
function TCustomStringKeyValueBucketList.BucketFor(const Key): Cardinal;
begin
  Result := HashOfStr(string(Key)) mod Cardinal(BucketCount);
end;

{*
  [@inheritDoc]
*}
function TCustomStringKeyValueBucketList.KeyEquals(const Key1, Key2): Boolean;
begin
  Result := string(Key1) = string(Key2);
end;

{--------------------------}
{ Classe TSourceEditorList }
{--------------------------}

{*
  Crée une nouvelle instance de TSourceEditorList
*}
constructor TSourceEditorList.Create;
begin
  inherited Create(SizeOf(string), SizeOf(TCreateSourceEditorProc));

  FFilters := TStringList.Create;
  FFilters.Delimiter := '|';
  TStringList(FFilters).Sorted := True;
end;

{*
  [@inheritDoc]
*}
destructor TSourceEditorList.Destroy;
begin
  FFilters.Free;
  inherited;
end;

{*
  Nombre de filtres enregistrés
  @return Nombre de filtres enregistrés
*}
function TSourceEditorList.GetFilterCount: Integer;
begin
  Result := FFilters.Count;
end;

{*
  Tableau zero-based des filtres enregistrés
  @param Index   Index d'un filtre
  @return Filtre à l'index donné
*}
function TSourceEditorList.GetFilters(Index: Integer): string;
begin
  Result := FFilters[Index];
end;

{*
  Filtres dans une chaîne unique
  Cette chaîne peut être utilisée par les propriétés Filter des composants
  boîte de dialogue.
  @return Filtres dans une chaîne unique
*}
function TSourceEditorList.GetFiltersAsText: string;
var
  I: Integer;
  AllExtensions: string;
begin
  Result := '';

  if FFilters.Count = 0 then
    Exit;

  AllExtensions := '';

  for I := 0 to FFilters.Count-1 do
  begin
    Result := Result + '|' + FFilters[I];
    AllExtensions := AllExtensions + ';' + GetLastToken(FFilters[I], '|');
  end;

  AllExtensions[1] := '|';
  Result := SAllSourceTypes + AllExtensions + Result;
end;

{*
  Ajoute un éditeur
  @param Extension    Extension du type de fichiers géré
  @param CreateProc   Routine de call-back créant l'éditeur
*}
procedure TSourceEditorList.Add(const Extension: string;
  CreateProc: TCreateSourceEditorProc);
begin
  AddData(Extension, CreateProc);
end;

{*
  Supprime un éditeur
  @param Extension   Extension du type de fichiers géré
*}
procedure TSourceEditorList.Remove(const Extension: string);
begin
  RemoveData(Extension);
end;

{*
  Teste s'il existe un éditeur enregistré pour un type de fichiers donné
  @param Extension   Extension du type de fichiers
  @return True s'il existe un éditeur approprié, False sinon
*}
function TSourceEditorList.Exists(const Extension: string): Boolean;
begin
  Result := (inherited Exists(Extension));
end;

{*
  Trouve la routine de call-back correspondant à un type de fichiers
  @param Extension   Extension du type de fichiers
  @return Routine de call-back de création de l'éditeur
  @throws EFileError Type de fichier inconnu
*}
function TSourceEditorList.Find(
  const Extension: string): TCreateSourceEditorProc;
begin
  if not (inherited Find(Extension, Result)) then
    raise EInOutError.CreateFmt(SUnknownUnitType, [Extension]);
end;

{*
  Enregistre un filtre
  @param Filter   Filtre à enregistrer
*}
procedure TSourceEditorList.AddFilter(const Filter: string);
var
  Index: Integer;
begin
  Index := FFilters.IndexOf(Filter);
  if Index < 0 then
    FFilters.Add(Filter)
  else
    FFilters.Objects[Index] := TObject(Integer(FFilters.Objects[Index])+1);
end;

{*
  Supprime un filtre enregistré
  @return Filter   Filtre à supprimer
*}
procedure TSourceEditorList.RemoveFilter(const Filter: string);
var
  Index, Count: Integer;
begin
  Index := FFilters.IndexOf(Filter);
  if Index < 0 then
    Exit;

  Count := Integer(FFilters.Objects[Index]);
  if Count = 0 then
    FFilters.Delete(Index)
  else
    FFilters.Objects[Index] := TObject(Count-1);
end;

{*
  Obtient la clef d'indexation pour un fichier source donné
  Actuellement, il s'agit de l'extension du nom fichier, sans le point.
  @param SourceFile   Fichier source
  @return Clef d'indexation pour ce fichier source
*}
class function TSourceEditorList.SourceFileKey(SourceFile: TSourceFile): string;
begin
  Result := Copy(ExtractFileExt(SourceFile.FileName), 2, MaxInt);
end;

{*
  Teste s'il existe un éditeur pour un fichier source donné
  @param SourceFile   Fichier source
  @return True s'il existe un éditeur pour ce fichier source, False sinon
*}
function TSourceEditorList.ExistsEditor(SourceFile: TSourceFile): Boolean;
begin
  Result := Exists(SourceFileKey(SourceFile));
end;

{*
  Crée un éditeur pour un fichier source donné
  @param SourceFile   Fichier source
  @return Un nouvel éditeur pour le fichier SourceFile
  @throws EFileError Type de fichier inconnu
*}
function TSourceEditorList.CreateEditor(
  SourceFile: TSourceFile): ISourceEditor50;
var
  CreateProc: TCreateSourceEditorProc;
begin
  CreateProc := Find(SourceFileKey(SourceFile));
  Result := CreateProc(SourceFile);
end;

{-------------------------------}
{ Classe TSourceFileCreatorList }
{-------------------------------}

{*
  Crée une nouvelle instance de TSourceFileCreatorList
*}
constructor TSourceFileCreatorList.Create;
begin
  inherited Create(SizeOf(TCreateNewSourceFileProc),
    TypeInfo(TSourceFileCreatorInfo));
end;

{*
  Ajoute un créateur de fichier source
  @param CreateProc   Routine de call-back de création de fichier source
  @param Info         Informations sur ce créateur de fichier source
*}
procedure TSourceFileCreatorList.Add(CreateProc: TCreateNewSourceFileProc;
  const Info: TSourceFileCreatorInfo);
begin
  AddData(CreateProc, Info);
end;

{*
  Supprime un créateur de fichier source
  @param CreateProc   Routine de call-back de création de fichier source
*}
procedure TSourceFileCreatorList.Remove(CreateProc: TCreateNewSourceFileProc);
begin
  RemoveData(CreateProc);
end;

{------------------------}
{ Classe TUnitFilterList }
{------------------------}

{*
  Crée une nouvelle instance de TUnitFilterList
*}
constructor TUnitFilterList.Create;
begin
  inherited Create(TypeInfo(string), TypeInfo(string));
end;

{*
  Table de correspondance Filtre -> GUID
  @param Filter   Filtre de fichiers
  @result Extension de ce type de fichiers
*}
function TUnitFilterList.GetExtension(const Filter: string): string;
begin
  GetData(Filter, Result);
end;

{*
  Ajoute un filtre d'unité
  @param Filter      Filtre d'unité (tel qu'utilisé par TOpenDialog/TSaveDialog)
  @param Extension   Extension de type des fichiers correspondant à Filter
*}
procedure TUnitFilterList.Add(const Filter: string; const Extension: string);
begin
  AddData(Filter, Extension);
end;

{*
  Supprime un filtre d'unité
  @param Filter   Filtre d'unité
*}
procedure TUnitFilterList.Remove(const Filter: string);
begin
  RemoveData(Filter);
end;

initialization
  SourceFileEditors := TSourceEditorList.Create;
  SourceFileCreators := TSourceFileCreatorList.Create;
  UnitFilters := TUnitFilterList.Create;

  UnitFilters.Add(BPLFilter, BPLUnitExtension);
  UnitFilters.Add(SepiFilter, SepiUnitExtension);
finalization
  SourceFileEditors.Free;
  SourceFileEditors := nil;
  SourceFileCreators.Free;
  SourceFileCreators := nil;
  UnitFilters.Free;
  UnitFilters := nil;
end.

