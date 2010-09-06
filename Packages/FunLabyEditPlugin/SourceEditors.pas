{*
  Syst�me de base des �diteurs d'unit� de FunLabyEdit
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
    �v�nement de notification dont l'envoyeur est un �diteur de source
    @param Sender   �diteur de source qui a d�clench� l'�v�nement
  *}
  TSourceEditorNotifyEvent = procedure(const Sender: ISourceEditor50) of object;

  {*
    Interface d'un �diteur de fichier source de FunLabyEdit
    Les classes impl�mentant cette interface doivent outrepasser le comptage de
    r�f�rences, mais se lib�rer dans la m�thode Release.
    @author sjrd
    @version 5.0
  *}
  ISourceEditor50 = interface
    ['{301FC63B-E81A-4861-A1EA-90AFD1FA038F}']

    {*
      Fichier source ouvert dans l'�diteur
      @return Fichier source ouvert
    *}
    function GetSourceFile: TSourceFile;

    {*
      Contr�le d'�dition � placer dans la fiche principale de FunLabyEdit
      @return Contr�le d'�dition
    *}
    function GetControl: TControl;

    {*
      Indique si le fichier source a �t� modifi�
      @return True si le fichier source a �t� modifi�, False sinon
    *}
    function GetModified: Boolean;

    {*
      �v�nement d�clench� lorsque l'�tat de l'�diteur change
      @return L'�v�nement associ�
    *}
    function GetOnStateChange: TSourceEditorNotifyEvent;

    {*
      Modifie l'�v�nement OnStateChange
      @param Value   Nouvel �v�nement OnStateChange
    *}
    procedure SetOnStateChange(Value: TSourceEditorNotifyEvent);

    {*
      Enregistre le fichier source
      @return True si le fichier a bien �t� enregistr�, False sinon
    *}
    function SaveFile: Boolean;

    {*
      Teste si l'�diteur peut �tre ferm�
      @return True s'il peut �tre ferm�, False pour emp�cher la fermeture
    *}
    function CanClose: Boolean;

    {*
      Lib�re l'�diteur
    *}
    procedure Release;

    property SourceFile: TSourceFile read GetSourceFile;
    property Control: TControl read GetControl;
    property Modified: Boolean read GetModified;

    property OnStateChange: TSourceEditorNotifyEvent
      read GetOnStateChange write SetOnStateChange;
  end;

  {*
    Interface d'un �diteur de fichier source qui peut charger une unit�
    @author sjrd
    @version 5.0
  *}
  IUnitEditor50 = interface(ISourceEditor50)
    ['{4F0CC4F1-D7F1-4D3A-B4A8-1DB7E713CEE4}']

    {*
      Nom de l'unit� �dit�e
      @return Nom de l'unit� �dit�e
    *}
    function GetUnitName: string;

    {*
      Charge l'unit� �dit�e
      @param SepiRoot   Racine Sepi
      @return Unit� Sepi charg�e
    *}
    function LoadUnit(SepiRoot: TSepiRoot): TSepiUnit;

    property UnitName: string read GetUnitName;
  end;

  {*
    Interface d'un �diteur de fichier source qui doit compiler son source
    @author sjrd
    @version 5.0
  *}
  ISourceCompiler50 = interface(ISourceEditor50)
    ['{8B6345C1-7BCD-4C2D-8E1D-FE3549823006}']

    {*
      Nom de l'unit� �dit�e
      @return Nom de l'unit� �dit�e
    *}
    function GetUnitName: string;

    {*
      Compile le fichier source
      @param SepiRoot   Racine Sepi
      @param Errors     Erreurs de compilation
      @return Unit� Sepi compil�e
      @throws ESepiCompilerFatalError Erreur fatale de compilation
    *}
    function CompileFile(SepiRoot: TSepiRoot;
      Errors: TSepiCompilerErrorList): TSepiUnit;

    {*
      Montre une erreur dans le code source
      @param Error   Erreur � montrer
    *}
    procedure ShowError(Error: TSepiCompilerError);

    property UnitName: string read GetUnitName;
  end;

  {*
    Type de routine de call-back qui cr�e un �diteur de fichier source
    @param SourceFile   Fichier source � �diter
    @return Interface vers l'�diteur cr��
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
    Collection d'�diteurs de fichiers source
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
    Type de routine de call-back qui cr�e un nouveau fichier source
    Si l'enregistrement du cr�ateur a sp�cifi� qu'il ne fallait pas demander
    automatiquement un nom de fichier, le param�tre FileName est vide lors de
    l'appel.
    Dans tous les cas, FileName peut �tre modifi�, et doit indiquer en sortie -
    si la fonction renvoie True - le nom de fichier r�el, qui doit exister.
    Si la fonction renvoie False, la valeur de FileName est ind�finie en sortie.
    @param FileName   Nom du fichier unit� � cr�er
    @return True si le fichier a bien �t� cr��, False sinon
  *}
  TCreateNewSourceFileProc = function(var FileName: TFileName): Boolean;

  /// Pointeur vers TSourceFileCreatorInfo
  PSourceFileCreatorInfo = ^TSourceFileCreatorInfo;

  {*
    Informations sur un cr�ateur de fichier source
    @author sjrd
    @version 5.0
  *}
  TSourceFileCreatorInfo = record
    Title: string;           /// Titre du cr�ateur
    Description: string;     /// Description longue
    AskForFileName: Boolean; /// Demander un nom de fichier
    Extension: string;       /// Extension des fichiers cr��s
  end;

  {*
    Collection de cr�ateurs d'unit�
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
    Collection de filtres d'unit�
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
  /// �diteurs de source
  SourceFileEditors: TSourceEditorList = nil;

  /// Cr�ateurs de source
  SourceFileCreators: TSourceFileCreatorList = nil;

  /// Filtres d'unit�
  UnitFilters: TUnitFilterList = nil;

resourcestring
  /// Filtre BPL
  BPLFilter = 'Paquets Borland (*.bpl)|*.bpl';

  /// Filtre SCU
  SepiFilter = 'Unit� Sepi compil�e (*.scu)|*.scu';

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
  Cr�e une nouvelle instance de TSourceEditorList
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
  Nombre de filtres enregistr�s
  @return Nombre de filtres enregistr�s
*}
function TSourceEditorList.GetFilterCount: Integer;
begin
  Result := FFilters.Count;
end;

{*
  Tableau zero-based des filtres enregistr�s
  @param Index   Index d'un filtre
  @return Filtre � l'index donn�
*}
function TSourceEditorList.GetFilters(Index: Integer): string;
begin
  Result := FFilters[Index];
end;

{*
  Filtres dans une cha�ne unique
  Cette cha�ne peut �tre utilis�e par les propri�t�s Filter des composants
  bo�te de dialogue.
  @return Filtres dans une cha�ne unique
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
  Ajoute un �diteur
  @param Extension    Extension du type de fichiers g�r�
  @param CreateProc   Routine de call-back cr�ant l'�diteur
*}
procedure TSourceEditorList.Add(const Extension: string;
  CreateProc: TCreateSourceEditorProc);
begin
  AddData(Extension, CreateProc);
end;

{*
  Supprime un �diteur
  @param Extension   Extension du type de fichiers g�r�
*}
procedure TSourceEditorList.Remove(const Extension: string);
begin
  RemoveData(Extension);
end;

{*
  Teste s'il existe un �diteur enregistr� pour un type de fichiers donn�
  @param Extension   Extension du type de fichiers
  @return True s'il existe un �diteur appropri�, False sinon
*}
function TSourceEditorList.Exists(const Extension: string): Boolean;
begin
  Result := (inherited Exists(Extension));
end;

{*
  Trouve la routine de call-back correspondant � un type de fichiers
  @param Extension   Extension du type de fichiers
  @return Routine de call-back de cr�ation de l'�diteur
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
  @param Filter   Filtre � enregistrer
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
  Supprime un filtre enregistr�
  @return Filter   Filtre � supprimer
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
  Obtient la clef d'indexation pour un fichier source donn�
  Actuellement, il s'agit de l'extension du nom fichier, sans le point.
  @param SourceFile   Fichier source
  @return Clef d'indexation pour ce fichier source
*}
class function TSourceEditorList.SourceFileKey(SourceFile: TSourceFile): string;
begin
  Result := Copy(ExtractFileExt(SourceFile.FileName), 2, MaxInt);
end;

{*
  Teste s'il existe un �diteur pour un fichier source donn�
  @param SourceFile   Fichier source
  @return True s'il existe un �diteur pour ce fichier source, False sinon
*}
function TSourceEditorList.ExistsEditor(SourceFile: TSourceFile): Boolean;
begin
  Result := Exists(SourceFileKey(SourceFile));
end;

{*
  Cr�e un �diteur pour un fichier source donn�
  @param SourceFile   Fichier source
  @return Un nouvel �diteur pour le fichier SourceFile
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
  Cr�e une nouvelle instance de TSourceFileCreatorList
*}
constructor TSourceFileCreatorList.Create;
begin
  inherited Create(SizeOf(TCreateNewSourceFileProc),
    TypeInfo(TSourceFileCreatorInfo));
end;

{*
  Ajoute un cr�ateur de fichier source
  @param CreateProc   Routine de call-back de cr�ation de fichier source
  @param Info         Informations sur ce cr�ateur de fichier source
*}
procedure TSourceFileCreatorList.Add(CreateProc: TCreateNewSourceFileProc;
  const Info: TSourceFileCreatorInfo);
begin
  AddData(CreateProc, Info);
end;

{*
  Supprime un cr�ateur de fichier source
  @param CreateProc   Routine de call-back de cr�ation de fichier source
*}
procedure TSourceFileCreatorList.Remove(CreateProc: TCreateNewSourceFileProc);
begin
  RemoveData(CreateProc);
end;

{------------------------}
{ Classe TUnitFilterList }
{------------------------}

{*
  Cr�e une nouvelle instance de TUnitFilterList
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
  Ajoute un filtre d'unit�
  @param Filter      Filtre d'unit� (tel qu'utilis� par TOpenDialog/TSaveDialog)
  @param Extension   Extension de type des fichiers correspondant � Filter
*}
procedure TUnitFilterList.Add(const Filter: string; const Extension: string);
begin
  AddData(Filter, Extension);
end;

{*
  Supprime un filtre d'unit�
  @param Filter   Filtre d'unit�
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

