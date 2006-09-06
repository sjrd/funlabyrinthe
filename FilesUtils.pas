{*
  Gestion des fichiers FunLabyrinthe
  L'unit� FilesUtils contient des routines et classes se chargeant du
  traitement (cr�ation, ouverture, enregistrement) des fichiers et
  l'interfa�age de leurs donn�es avec les classes m�tier.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FilesUtils;

interface

uses
  SysUtils, Classes, Contnrs, ScUtils, ScLists, ScStrUtils, MSXML, FunLabyUtils,
  Screws;

resourcestring
  sInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  sVersionTooHigh = 'Le fichier a �t� enregistr� avec une version ult�rieure '+
    'de FunLabyrinthe (v%s). Il ne peut �tre ouvert.';
  sFileNotFound = 'Le fichier sp�cifi� "%s" n''existe pas';
  sUnknownUnitType = 'Type d''unit� ''%s'' inconnu';
  sUnknownMapType = 'Type de carte ''%s'' inconnu';
  sThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  sEditingNotAllowed = 'L''�dition de ce fichier n''est pas permise';
  sCantEditSaveguard = 'L''�dition d''une sauvegarde est impossible';

type
  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmEditActions, fmPlay);

  /// G�n�r�e si un fichier ne respecte pas le format attendu
  EFileError = class(Exception);

  TMasterFile = class;

  {*
    Repr�sente un fichier d�pendant d'un fichier ma�tre FunLabyrinthe
    TDependantFile est la classe de base pour les classes chargeant et
    enregistrant des fichiers d�pendant d'un fichier ma�tre FunLabyrinthe.
  *}
  TDependantFile = class
  private
    FMasterFile : TMasterFile; /// Fichier ma�tre
    FFileName : TFileName;     /// Nom du fichier
    FMIMEType : string;        /// Type MIME du fichier
    FMaster : TMaster;         /// Ma�tre FunLabyrinthe
  protected
    {*
      Enregistre le fichier
      Les descendants de TDependantFile doivent impl�menter SaveFile pour
      pouvoir enregistrer le fichier
    *}
    procedure SaveFile; virtual; abstract;
  public
    constructor Create(AMasterFile : TMasterFile; const AFileName : TFileName;
      const AMIMEType : string);

    procedure Save(const AFileName : TFileName = '');

    property MasterFile : TMasterFile read FMasterFile;
    property FileName : TFileName read FFileName;
    property MIMEType : string read FMIMEType;
    property Master : TMaster read FMaster;
  end;

  {*
    Repr�sente un fichier unit�
    TUnitFile est la classe de base pour les fichiers d'unit� FunLabyrinthe.
  *}
  TUnitFile = class(TDependantFile)
  public
    constructor Create(AMasterFile : TMasterFile; const AFileName : TFileName;
      const AMIMEType : string); virtual;
    procedure AfterConstruction; override;
  end;
  TUnitFileClass = class of TUnitFile;

  {*
    Repr�sente un fichier carte
    TMapFile est la classe de base pour les fichiers de carte FunLabyrinthe.
    Elle fournit des propri�t�s pour identifier la carte li�e.
  *}
  TMapFile = class(TDependantFile)
  private
    FMapID : TComponentID; /// ID de la carte li�e
    FMap : TMap;           /// Carte li�e
  protected
    constructor Create(AMasterFile : TMasterFile; const AFileName : TFileName;
      const AMIMEType : string; const AMapID : TComponentID;
      ADimensions : T3DPoint); overload;
  public
    constructor Create(AMasterFile : TMasterFile; const AFileName : TFileName;
      const AMIMEType : string;
      const AMapID : TComponentID); overload; virtual; abstract;
    procedure AfterConstruction; override;

    property MapID : TComponentID read FMapID;
    property Map : TMap read FMap;
  end;
  TMapFileClass = class of TMapFile;

  {*
    Repr�sente un fichier ma�tre FunLabyrinthe
    TMasterFile repr�sente un fichier ma�tre FunLabyrinthe (extension .flg).
    Elle est capable de charger tous les fichiers annexes au moyen des autres
    classes de l'unit�.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
  *}
  TMasterFile = class
  private
    FFileName : TFileName;    /// Nom du fichier
    FMode : TFileMode;        /// Mode d'ouverture du fichier
    FVersion : string;        /// Version lors de l'enregistrement

    FTitle : string;          /// Titre du labyrinthe
    FDescription : string;    /// Description
    FDifficulty : string;     /// Difficult�
    FAuthorID : integer;      /// ID Web de l'auteur, ou 0 si non renseign�
    FAuthor : string;         /// Nom de l'auteur

    FAllowEdit : boolean;     /// Indique si le fichier peut �tre �dit�
    FIsSaveguard : boolean;   /// Indique si le fichier �tait une sauvegarde

    FMaster : TMaster;        /// Ma�tre FunLabyrinthe

    FUnitFiles : TObjectList; /// Liste des fichiers unit�
    FMapFiles : TObjectList;  /// Liste des fichiers carte

    procedure InvalidFormat;

    procedure TitleFromFileName;
    function ResolveHRef(const HRef, DefaultDir : string) : TFileName;
    procedure Load(Document : IXMLDOMDocument);
    procedure TestOpeningValidity;

    function GetUnitFileCount : integer;
    function GetUnitFiles(Index : integer) : TUnitFile;
    function GetMapFileCount : integer;
    function GetMapFiles(Index : integer) : TMapFile;
  public
    constructor Create(const AFileName : TFileName; AMode : TFileMode);
    constructor CreateNew(FileContents : TStrings = nil);
    destructor Destroy; override;

    class procedure RegisterUnitFileClass(const MIMEType : string;
      UnitFileClass : TUnitFileClass);
    class function FindUnitFileClass(const MIMEType : string) : TUnitFileClass;

    class procedure RegisterMapFileClass(const MIMEType : string;
      MapFileClass : TMapFileClass);
    class function FindMapFileClass(const MIMEType : string) : TMapFileClass;

    procedure Save(const AFileName : TFileName = '');

    property FileName : TFileName read FFileName;
    property Mode : TFileMode read FMode;
    property Version : string read FVersion;

    property Title : string read FTitle write FTitle;
    property Description : string read FDescription write FDescription;
    property Difficulty : string read FDifficulty write FDifficulty;
    property AuthorID : integer read FAuthorID write FAuthorID;
    property Author : string read FAuthor write FAuthor;

    property AllowEdit : boolean read FAllowEdit;
    property IsSaveguard : boolean read FIsSaveguard;

    property Master : TMaster read FMaster;

    property UnitFileCount : integer read GetUnitFileCount;
    property UnitFiles[index : integer] : TUnitFile read GetUnitFiles;
    property MapFileCount : integer read GetMapFileCount;
    property MapFiles[index : integer] : TMapFile read GetMapFiles;
  end;

implementation

var
  UnitFileClasses : TStrings; /// Table de hashage Type MIME -> Classe unit�
  MapFileClasses : TStrings;  /// Table de hashage Type MIME -> Classe carte

function CompareVersions(Version1, Version2 : string) : integer;
var SubVer1, SubVer2 : string;
begin
  repeat
    SubVer1 := GetFirstToken(Version1, '.');
    Delete(Version1, 1, Length(SubVer1)+1);
    SubVer2 := GetFirstToken(Version2, '.');
    Delete(Version2, 1, Length(SubVer2)+1);
    Result := StrToIntDef(SubVer1, 0) - StrToIntDef(SubVer2, 0);
  until (Result <> 0) or ((Version1 = '') and (Version2 = ''));
end;

/////////////////////////////
/// Classe TDependantFile ///
/////////////////////////////

{*
  Cr�e une instance de TDependantFile
  @param AMasterFile   Fichier ma�tre
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
*}
constructor TDependantFile.Create(AMasterFile : TMasterFile;
  const AFileName : TFileName; const AMIMEType : string);
begin
  inherited Create;
  FMasterFile := AMasterFile;
  FFileName := AFileName;
  FMIMEType := AMIMEType;
  FMaster := FMasterFile.Master;
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier dans lequel enregistrer
*}
procedure TDependantFile.Save(const AFileName : TFileName = '');
begin
  if AFileName <> '' then
    FFileName := AFileName;
  SaveFile;
end;

////////////////////////
/// Classe TUnitFile ///
////////////////////////

{*
  Cr�e une instance de TUnitFile
  @param AMasterFile   Fichier ma�tre
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
*}
constructor TUnitFile.Create(AMasterFile : TMasterFile;
  const AFileName : TFileName; const AMIMEType : string);
begin
  inherited Create(AMasterFile, AFileName, AMIMEType);
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TUnitFile.AfterConstruction;
begin
  inherited;
  MasterFile.FUnitFiles.Add(Self);
end;

///////////////////////
/// Classe TMapFile ///
///////////////////////

{*
  Cr�e une instance de TMapFile
  @param AMasterFile   Fichier ma�tre
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param AMapID        ID de la carte
  @param ADimensions   Dimensions de la carte
*}
constructor TMapFile.Create(AMasterFile : TMasterFile; const AFileName : TFileName;
  const AMIMEType : string; const AMapID : TComponentID;
  ADimensions : T3DPoint);
begin
  inherited Create(AMasterFile, AFileName, AMIMEType);
  FMapID := AMapID;
  FMap := TMap.Create(Master, AMapID, ADimensions);
  Master.AddComponent(FMap);
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TMapFile.AfterConstruction;
begin
  inherited;
  MasterFile.FMapFiles.Add(Self);
end;

//////////////////////////
/// Classe TMasterFile ///
//////////////////////////

{*
  Ouvre un fichier FunLabyrinthe
  @param AFileName   Nom du fichier � ouvrir
  @param AMode       Mode sous lequel ouvrir le fichier
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TMasterFile.Create(const AFileName : TFileName; AMode : TFileMode);
var Document : IXMLDOMDocument;
begin
  inherited Create;
  FFileName := AFileName;
  FMode := AMode;
  FVersion := CurrentVersion;

  FTitle := '';
  FDescription := '';
  FDifficulty := '';
  FAuthorID := 0;
  FAuthor := '';

  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create;

  FUnitFiles := TObjectList.Create;
  FMapFiles := TObjectList.Create;

  try
    Document := CoDOMDocument.Create;
    Document.async := False;
    if not Document.load(FFileName) then
      InvalidFormat;

    Load(Document);
    TestOpeningValidity;
  except
    FMapFiles.Free;
    FUnitFiles.Free;
    FMaster.Free;
    raise;
  end;
end;

{*
  Cr�e un nouveau fichier FunLabyrinthe en mode �dition
  @param Dimensions     Dimensions de la carte
  @param FileContents   Contenu pr�-cr�� du fichier (ou nil pour cr�er un vide)
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TMasterFile.CreateNew(FileContents : TStrings = nil);
var Document : IXMLDOMDocument;
begin
  inherited Create;
  FFileName := '';
  FMode := fmEdit;
  FVersion := CurrentVersion;

  FTitle := '';
  FDescription := '';
  FDifficulty := '';
  FAuthorID := 0;
  FAuthor := '';

  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create;

  FUnitFiles := TObjectList.Create;
  FMapFiles := TObjectList.Create;

  if Assigned(FileContents) then
  try
    Document := CoDOMDocument.Create;
    Document.async := False;
    if not Document.loadXML(FileContents.Text) then
      InvalidFormat;

    Load(Document);
    TestOpeningValidity;
  except
    FMapFiles.Free;
    FUnitFiles.Free;
    FMaster.Free;
    raise;
  end;
end;

{*
  D�truit l'instance
*}
destructor TMasterFile.Destroy;
begin
  FMapFiles.Free;
  FUnitFiles.Free;
  FMaster.Free;
  inherited;
end;

{*
  G�n�re une erreur indiquant que le fichier ne respecte pas le format attendu
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
procedure TMasterFile.InvalidFormat;
begin
  raise EFileError.Create(sInvalidFileFormat);
end;

{*
  Donne au labyrinthe un titre par d�faut � partir du nom du fichier
*}
procedure TMasterFile.TitleFromFileName;
var I : integer;
begin
  FTitle := ExtractFileName(FFileName);
  I := Length(FTitle);
  while (I > 0) and (FTitle[I] <> '.') do dec(I);
  if I > 0 then
    SetLength(FTitle, I);
end;

{*
  R�soud l'adresse HRef en cherchant dans les dossiers correspondants
  @param HRef         Adresse HRef du fichier
  @param DefaultDir   Dossier par d�faut du type de fichier attendu
  @return Nom du fichier qualifi� de son chemin d'acc�s
  @throws EFileError : Le fichier n'existe pas
*}
function TMasterFile.ResolveHRef(const HRef, DefaultDir : string) : TFileName;
begin
  if FileExists(DefaultDir+Href) then Result := DefaultDir+HRef else
  if FileExists(ExtractFileDir(FFileName)+HRef) then
    Result := ExtractFileDir(FFileName)+HRef else
  if FileExists(HRef) then Result := HRef else
    raise EFileError.CreateFmt(sFileNotFound, [HRef]);
end;

{*
  Charge le contenu du document
  @param Document   Document XML DOM contenu du fichier
  @throws EFileError : Un fichier � charger n'existe pas ou n'est pas valide
*}
procedure TMasterFile.Load(Document : IXMLDOMDocument);
  function ExtractContents(Node : IXMLDOMNode) : string;
  var I : integer;
  begin
    Result := '';
    for I := 0 to Node.childNodes.length-1 do
      Result := Result+Node.childNodes.item[I].xml;
  end;
var Element : IXMLDOMElement;
    NodeList : IXMLDOMNodeList;
    I : integer;
    ID, FileType : string;
    FileName : TFileName;
begin
  { Don't localize strings in this method }

  Element := Document.documentElement.firstChild as IXMLDOMElement;
  if Element.nodeName <> 'funlabyrinthe' then InvalidFormat;

  // Test de version
  FVersion := Element.getAttribute('version');
  if FVersion = '' then InvalidFormat;
  if CompareVersions(FVersion, CurrentVersion) > 0 then
    raise EFileError.CreateFmt(sVersionTooHigh, [FVersion]);

  // Attributs du fichier
  FAllowEdit := Element.getAttribute('allowedit') <> 'no';
  FIsSaveguard := Element.getAttribute('issaveguard') = 'yes';

  // Infos standart sur le labyrinthe
  FTitle := Element.selectSingleNode('./title').text;
  if FTitle = '' then TitleFromFileName;
  FDescription := ExtractContents(Element.selectSingleNode('./description'));
  FDifficulty := Element.selectSingleNode('./difficulty').text;
  with Element.selectSingleNode('./author') as IXMLDOMElement do
  begin
    FAuthorID := StrToIntDef(getAttribute('id'), 0);
    FAuthor := text;
  end;

  // Unit�s utilis�es
  NodeList := Element.selectNodes('./units/unit');
  for I := 0 to NodeList.length-1 do with NodeList.item[I] as IXMLDOMElement do
  begin
    FileType := getAttribute('type');
    FileName := ResolveHRef(getAttribute('href'), fUnitsDir);

    FindUnitFileClass(FileType).Create(Self, FileName, FileType);
  end;

  // Cartes
  NodeList := Element.selectNodes('./maps/map');
  for I := 0 to NodeList.length-1 do with NodeList.item[I] as IXMLDOMElement do
  begin
    ID := getAttribute('id');
    FileType := getAttribute('type');
    FileName := ResolveHRef(getAttribute('href'), fMapsDir);

    FindMapFileClass(FileType).Create(Self, FileName, FileType, ID);
  end;

  // Joueurs
  NodeList := Element.selectNodes('./players/player');
  if NodeList.length <> 1 then
    raise EFileError.Create(sThereMustBeOnePlayer);
  for I := 0 to NodeList.length-1 do with NodeList.item[I] as IXMLDOMElement do
  begin
    ID := getAttribute('id');
  end;
end;

{*
  Teste la validit� de l'ouverture d'un fichier
  TestOpeningValidity v�rifie que le fichier ouvert ne l'a pas �t�
  � ill�galement �. Deux cas d'ill�galit� sont � tester :
  - Le fichier est une sauvegarde et est ouvert autrement que pour y jouer ;
  - Le fichier a �t� interdit d'�dition, et ouvert dans ce mode.
  @throws EFileError : Le fichier a �t� ouvert ill�galement
*}
procedure TMasterFile.TestOpeningValidity;
begin
  if IsSaveguard and (Mode <> fmPlay) then
    raise EFileError.Create(sCantEditSaveguard);
  if (not AllowEdit) and (Mode = fmEdit) then
    raise EFileError.Create(sEditingNotAllowed);
end;

{*
  Nombre de fichiers unit�
  @return Nombre de fichiers unit�
*}
function TMasterFile.GetUnitFileCount : integer;
begin
  Result := FUnitFiles.Count;
end;

{*
  Tableau zero-based des fichiers unit�
  @param Index   Index du fichier unit�
  @return Le fichier unit� dont l'index a �t� sp�cifi�
*}
function TMasterFile.GetUnitFiles(Index : integer) : TUnitFile;
begin
  Result := TUnitFile(FUnitFiles[Index]);
end;

{*
  Nombre de fichiers carte
  @return Nombre de fichiers carte
*}
function TMasterFile.GetMapFileCount : integer;
begin
  Result := FMapFiles.Count;
end;

{*
  Tableau zero-based des fichiers carte
  @param Index   Index du fichier carte
  @return Le fichier carte dont l'index a �t� sp�cifi�
*}
function TMasterFile.GetMapFiles(Index : integer) : TMapFile;
begin
  Result := TMapFile(FMapFiles[Index]);
end;

{*
  Enregistre un gestionnaire d'unit�
  @param MIMEType       Type MIME g�r� par la classe
  @param MapFileClass   Classe gestionnaire du type MIME
*}
class procedure TMasterFile.RegisterUnitFileClass(const MIMEType : string;
  UnitFileClass : TUnitFileClass);
var Index : integer;
begin
  Index := UnitFileClasses.IndexOf(MIMEType);
  if Index < 0 then
    UnitFileClasses.AddObject(MIMEType, TObject(UnitFileClass))
  else
    UnitFileClasses.Objects[Index] := TObject(UnitFileClass);
end;

{*
  Trouve la classe de fichier unit� g�rant le type MIME donn�
  @param MIMEType   Type MIME dont il faut trouver le gestionnaire
  @return Classe de fichier unit� g�rant le type MIME sp�cifi�
  @throws EFileError : Type d'unit� inconnu
*}
class function TMasterFile.FindUnitFileClass(
  const MIMEType : string) : TUnitFileClass;
var Index : integer;
begin
  Index := UnitFileClasses.IndexOf(MIMEType);
  if Index < 0 then
    raise EFileError.CreateFmt(sUnknownUnitType, [MIMEType])
  else
    Result := TUnitFileClass(UnitFileClasses.Objects[Index]);
end;

{*
  Enregistre un gestionnaire de carte
  @param MIMEType       Type MIME g�r� par la classe
  @param MapFileClass   Classe gestionnaire du type MIME
*}
class procedure TMasterFile.RegisterMapFileClass(const MIMEType : string;
  MapFileClass : TMapFileClass);
var Index : integer;
begin
  Index := MapFileClasses.IndexOf(MIMEType);
  if Index < 0 then
    MapFileClasses.AddObject(MIMEType, TObject(MapFileClass))
  else
    MapFileClasses.Objects[Index] := TObject(MapFileClass);
end;

{*
  Trouve la classe de fichier carte g�rant le type MIME donn�
  @param MIMEType   Type MIME dont il faut trouver le gestionnaire
  @return Classe de fichier carte g�rant le type MIME sp�cifi�
  @throws EFileError : Type de carte inconnu
*}
class function TMasterFile.FindMapFileClass(
  const MIMEType : string) : TMapFileClass;
var Index : integer;
begin
  Index := MapFileClasses.IndexOf(MIMEType);
  if Index < 0 then
    raise EFileError.CreateFmt(sUnknownMapType, [MIMEType])
  else
    Result := TMapFileClass(MapFileClasses.Objects[Index]);
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier � enregistrer (si vide, conserve l'existant)
*}
procedure TMasterFile.Save(const AFileName : TFileName = '');
begin
  { TODO 2 : Enregistrer le fichier }
end;

initialization
  UnitFileClasses := TStringList.Create;
  MapFileClasses := TStringList.Create;
finalization
  MapFileClasses.Free;
  UnitFileClasses.Free;
end.

