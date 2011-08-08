unit ProjectDatabase;

interface

uses
  SysUtils, Classes, ScXML, FunLabyUtils, FilesUtils, FunLabyFilers;

type
  /// Actions qui peuvent être entreprises sur un projet
  TProjectAction = (paNone, paInstall);

  {*
    Classe de base pour TLocalProject et TRemoteProject
    @author sjrd
    @version 5.2
  *}
  TAbstractProject = class(TFunLabyPersistent)
  private
    FTouched: Boolean; /// Pour élimination des projets supprimés

    FTitle: string;       /// Titre du labyrinthe
    FDescription: string; /// Description
    FKind: string;        /// Genre
    FDifficulty: string;  /// Difficulté
    FAuthor: string;      /// Nom de l'auteur
    FVersion: string;     /// Version du projet
  protected
    function GetIsDefined: Boolean; virtual; abstract;
  public
    procedure Undefine; virtual;

    procedure Touch;

    property IsDefined: Boolean read GetIsDefined;
  published
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Kind: string read FKind write FKind;
    property Difficulty: string read FDifficulty write FDifficulty;
    property Author: string read FAuthor write FAuthor;
    property Version: string read FVersion write FVersion;
  end;

  {*
    Informations sur un projet local
    @author sjrd
    @version 5.2
  *}
  TLocalProject = class(TAbstractProject)
  private
    FFileName: TFileName;   /// Nom du fichier

    FOwnProject: Boolean; /// True ssi considéré comme projet propre
  protected
    function GetIsDefined: Boolean; override;
  public
    procedure Undefine; override;
  published
    property FileName: TFileName read FFileName write FFileName;

    property OwnProject: Boolean read FOwnProject write FOwnProject
      default False;
  end;

  {*
    Informations sur un projet disponible sur Internet
    @author sjrd
    @version 5.2
  *}
  TRemoteProject = class(TAbstractProject)
  private
    FID: Integer;       /// ID du projet
    FAuthorID: Integer; /// ID de l'auteur

    FURL: string;     /// URL de la page d'informations
    FArchive: string; /// URL de l'archive du projet

    FRatingCount: Integer; /// Nombre de votes effectués sur ce projet
    FRatingAvg: Single;    /// Moyenne des votes effectués sur ce projet
  protected
    function GetIsDefined: Boolean; override;
  public
    procedure Undefine; override;
  published
    property ID: Integer read FID write FID;
    property AuthorID: Integer read FAuthorID write FAuthorID;

    property URL: string read FURL write FURL;
    property Archive: string read FArchive write FArchive;

    property RatingCount: Integer read FRatingCount write FRatingCount
      default 0;
    property RatingAvg: Single read FRatingAvg write FRatingAvg;
  end;

  {*
    Informations locales sur un projet
    @author sjrd
    @version 5.2
  *}
  TProject = class(TFunLabyPersistent)
  private
    FPath: TFileName;        /// Dossier du projet
    FLocal: TLocalProject;   /// Projet local
    FRemote: TRemoteProject; /// Projet disponible sur Internet

    FAction: TProjectAction; /// Action à entreprendre pour ce projet

    function GetIsLocalDefined: Boolean;
    function GetIsRemoteDefined: Boolean;

    function GetTitle: string;
    function GetDescription: string;
    function GetKind: string;
    function GetDifficulty: string;
    function GetAuthor: string;

    function GetLocalVersion: string;
    function GetRemoteVersion: string;

    function GetOwnProject: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property IsLocalDefined: Boolean read GetIsLocalDefined;
    property IsRemoteDefined: Boolean read GetIsRemoteDefined;

    property Title: string read GetTitle;
    property Description: string read GetDescription;
    property Kind: string read GetKind;
    property Difficulty: string read GetDifficulty;
    property Author: string read GetAuthor;

    property LocalVersion: string read GetLocalVersion;
    property RemoteVersion: string read GetRemoteVersion;

    property OwnProject: Boolean read GetOwnProject;

    property Action: TProjectAction read FAction write FAction;
  published
    property Path: TFileName read FPath write FPath;
    property Local: TLocalProject read FLocal stored GetIsLocalDefined;
    property Remote: TRemoteProject read FRemote stored GetIsRemoteDefined;
  end;

  TProjectPredicate = reference to function(Project: TProject): Boolean;

  {*
    Liste de projets
    @author sjrd
    @version 5.2
  *}
  TProjectList = class(TFunLabyCollection)
  private
    function InternalGetProject(const Predicate: TProjectPredicate): TProject;

    function GetItems(Index: Integer): TProject;
  protected
    function CreateItem(ItemClass: TFunLabyPersistentClass):
      TFunLabyPersistent; override;

    function GetDefaultItemClass: TFunLabyPersistentClass; override;
  public
    procedure BeginUpdate;
    procedure EndUpdate;

    function AddProject: TProject;

    function GetProject(const Path: TFileName): TProject;

    property Items[Index: Integer]: TProject read GetItems; default;
  end;

  {*
    Base de données des projets
    @author sjrd
    @version 5.2
  *}
  TProjectDatabase = class(TFunLabyPersistent)
  private
    FProjects: TProjectList; /// Liste des projets
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: TFileName);
    procedure SaveToFile(const FileName: TFileName);
  published
    property Projects: TProjectList read FProjects;
  end;

implementation

uses
  msxml;

{------------------------}
{ TAbstractProject class }
{------------------------}

{*
  Supprime toutes les informations sur ce projet
*}
procedure TAbstractProject.Undefine;
begin
  FTitle := '';
  FDescription := '';
  FKind := '';
  FDifficulty := '';
  FAuthor := '';
  FVersion := '';
end;

{*
  Marque ce projet
*}
procedure TAbstractProject.Touch;
begin
  FTouched := True;
end;

{---------------------}
{ TLocalProject class }
{---------------------}

function TLocalProject.GetIsDefined: Boolean;
begin
  Result := FileName <> '';
end;

{*
  [@inheritDoc]
*}
procedure TLocalProject.Undefine;
begin
  inherited;

  FFileName := '';
  FOwnProject := False;
end;

{----------------------}
{ TRemoteProject class }
{----------------------}

function TRemoteProject.GetIsDefined: Boolean;
begin
  Result := ID <> 0;
end;

{*
  [@inheritDoc]
*}
procedure TRemoteProject.Undefine;
begin
  inherited;

  FID := 0;
  FAuthorID := 0;
  FURL := '';
  FArchive := '';
  FRatingCount := 0;
  FRatingAvg := 0.0;
end;

{----------------}
{ TProject class }
{----------------}

{*
  Crée une instance de TProject
*}
constructor TProject.Create;
begin
  inherited Create;

  FLocal := TLocalProject.Create;
  FRemote := TRemoteProject.Create;
end;

{*
  [@inheritDoc]
*}
destructor TProject.Destroy;
begin
  FRemote.Free;
  FLocal.Free;

  inherited;
end;

function TProject.GetIsLocalDefined: Boolean;
begin
  Result := Local.IsDefined;
end;

function TProject.GetIsRemoteDefined: Boolean;
begin
  Result := Remote.IsDefined;
end;

function TProject.GetTitle: string;
begin
  if IsLocalDefined then
    Result := Local.Title
  else
    Result := Remote.Title;
end;

function TProject.GetDescription: string;
begin
  if IsLocalDefined then
    Result := Local.Description
  else
    Result := Remote.Description;
end;

function TProject.GetKind: string;
begin
  if IsLocalDefined then
    Result := Local.Kind
  else
    Result := Remote.Kind;
end;

function TProject.GetDifficulty: string;
begin
  if IsLocalDefined then
    Result := Local.Difficulty
  else
    Result := Remote.Difficulty;
end;

function TProject.GetAuthor: string;
begin
  if IsLocalDefined then
    Result := Local.Author
  else
    Result := Remote.Author;
end;

function TProject.GetLocalVersion: string;
begin
  Result := Local.Version;
end;

function TProject.GetRemoteVersion: string;
begin
  Result := Remote.Version;
end;

function TProject.GetOwnProject: Boolean;
begin
  Result := Local.OwnProject;
end;

{-------------------}
{ TProjetList class }
{-------------------}

{*
  Trouve et renvoie un projet recherché par un prédicat quelconque
  @param ID   ID distant
  @return Projet correspondant, ou un nouveau projet sinon
*}
function TProjectList.InternalGetProject(
  const Predicate: TProjectPredicate): TProject;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := Items[I];
    if Predicate(Result) then
      Exit;
  end;

  Result := AddProject;
end;

{*
  Tableau zero-based des projets
  @param Index   Index compris entre 0 inclus et Count exclu
  @return Projet à l'index spécifié
*}
function TProjectList.GetItems(Index: Integer): TProject;
begin
  Result := (inherited Items[Index]) as TProject;
end;

{*
  [@inheritDoc]
*}
function TProjectList.CreateItem(
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Assert(ItemClass = TProject);
  Result := TProject.Create;
end;

{*
  [@inheritDoc]
*}
function TProjectList.GetDefaultItemClass: TFunLabyPersistentClass;
begin
  Result := TProject;
end;

procedure TProjectList.BeginUpdate;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  begin
    Items[I].Local.FTouched := False;
    Items[I].Remote.FTouched := False;
  end;
end;

procedure TProjectList.EndUpdate;
var
  I: Integer;
  Project: TProject;
begin
  for I := Count-1 downto 0 do
  begin
    Project := Items[I];

    if not Project.Local.FTouched then
      Project.Local.Undefine;
    if not Project.Remote.FTouched then
      Project.Remote.Undefine;

    if not (Project.Local.IsDefined or Project.Remote.IsDefined) then
      Delete(I);
  end;
end;

{*
  Ajoute un nouveau projet
  @return Projet ajouté
*}
function TProjectList.AddProject: TProject;
begin
  Result := AddDefault as TProject;
end;

{*
  Trouve et renvoie un projet recherché par son nom de dossier
  @param Path   Dossier du projet
  @return Projet correspondant, ou un nouveau projet sinon
*}
function TProjectList.GetProject(const Path: TFileName): TProject;
begin
  Result := InternalGetProject(
    function(Project: TProject): Boolean
    begin
      Result := AnsiSameText(Project.Path, Path);
    end);

  Result.Path := Path;
end;

{------------------------}
{ TProjectDatabase class }
{------------------------}

{*
  Crée une instance de TProjectDatabase
*}
constructor TProjectDatabase.Create;
begin
  inherited Create;

  FProjects := TProjectList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TProjectDatabase.Destroy;
begin
  FProjects.Free;

  inherited;
end;

{*
  Charge la base de données depuis un fichier
  @param FileName   Nom du fichier
*}
procedure TProjectDatabase.LoadFromFile(const FileName: TFileName);
var
  Document: IXMLDOMDocument;
begin
  Document := LoadXMLDocumentFromFile(FileName);
  TFunLabyXMLReader.ReadPersistent(Self, Document.documentElement);
end;

{*
  Enregistre la base de données dans un fichier
  @param FileName   Nom du fichier
*}
procedure TProjectDatabase.SaveToFile(const FileName: TFileName);
var
  Document: IXMLDOMDocument;
  Element: IXMLDOMElement;
begin
  Document := CoDOMDocument.Create;
  Document.async := False;

  Element := Document.createElement('projects');
  TFunLabyXMLWriter.WritePersistent(Self, Element);
  Document.appendChild(Element);

  SaveXMLDocumentToFile(Document, FileName);
end;

initialization
  FunLabyRegisterClass(TProject);
finalization
  FunLabyUnregisterClass(TProject);
end.

