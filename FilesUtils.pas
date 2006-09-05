{*
  Gestion des fichiers FunLabyrinthe
  L'unité FilesUtils contient des routines et classes se chargeant du
  traitement (création, ouverture, enregistrement) des fichiers et
  l'interfaçage de leurs données avec les classes métier.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FilesUtils;

interface

uses
  SysUtils, Classes, ScUtils, ScLists, ScStrUtils, MSXML, FunLabyUtils, Screws;

resourcestring
  sInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  sVersionTooHigh = 'Le fichier a été enregistré avec une version ultérieure '+
    'de FunLabyrinthe (v%s). Il ne peut être ouvert.';
  sThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  sEditingNotAllowed = 'L''édition de ce fichier n''est pas permise';
  sCantEditSaveguard = 'L''édition d''une sauvegarde est impossible';

type
  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmEditActions, fmPlay);

  /// Générée si un fichier ne respecte pas le format attendu
  EFileError = class(Exception);

  {*
    Représente un fichier maître FunLabyrinthe
    TFunLabyFile représente un fichier maître FunLabyrinthe (extension .flg).
    Elle est capable de charger tous les fichiers annexes au moyen des autres
    classes de l'unité.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
  *}
  TFunLabyFile = class
  private
    FFileName : TFileName;  /// Nom du fichier
    FMode : TFileMode;      /// Mode d'ouverture du fichier
    FVersion : string;      /// Version lors de l'enregistrement

    FTitle : string;        /// Titre du labyrinthe
    FDescription : string;  /// Description
    FDifficulty : string;   /// Difficulté
    FAuthorID : integer;    /// ID Web de l'auteur, ou 0 si non renseigné
    FAuthor : string;       /// Nom de l'auteur

    FAllowEdit : boolean;   /// Indique si le fichier peut être édité
    FIsSaveguard : boolean; /// Indique si le fichier était une sauvegarde

    FMaster : TMaster;      /// Maître FunLabyrinthe

    procedure InvalidFormat;

    procedure TitleFromFileName;
    procedure Load(Document : IXMLDOMDocument);
    procedure TestOpeningValidity;
  public
    constructor Create(const AFileName : TFileName; AMode : TFileMode);
    constructor CreateNew(FileContents : TStrings = nil);
    destructor Destroy; override;

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
  end;

implementation

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

///////////////////////////
/// Classe TFunLabyFile ///
///////////////////////////

{*
  Ouvre un fichier FunLabyrinthe
  @param AFileName   Nom du fichier à ouvrir
  @param AMode       Mode sous lequel ouvrir le fichier
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TFunLabyFile.Create(const AFileName : TFileName; AMode : TFileMode);
var Document : IXMLDOMDocument;
begin
  inherited Create;
  FFileName := AFileName;
  FMode := AMode;
  FVersion := CurrentVersion;

  TitleFromFileName;
  FDescription := '';
  FDifficulty := '';
  FAuthorID := 0;
  FAuthor := '';

  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create;

  try
    Document := CoDOMDocument.Create;
    Document.async := False;
    if not Document.load(FFileName) then
      InvalidFormat;

    Load(Document);
    TestOpeningValidity;
  except
    FMaster.Free;
    raise;
  end;
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param Dimensions     Dimensions de la carte
  @param FileContents   Contenu pré-créé du fichier (ou nil pour créer un vide)
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TFunLabyFile.CreateNew(FileContents : TStrings = nil);
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

  if Assigned(FileContents) then
  try
    Document := CoDOMDocument.Create;
    Document.async := False;
    if not Document.loadXML(FileContents.Text) then
      InvalidFormat;

    Load(Document);
    TestOpeningValidity;
  except
    FMaster.Free;
    raise;
  end;
end;

{*
  Détruit l'instance
*}
destructor TFunLabyFile.Destroy;
begin
  FMaster.Free;
  inherited;
end;

{*
  Génère une erreur indiquant que le fichier ne respecte pas le format attendu
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
procedure TFunLabyFile.InvalidFormat;
begin
  raise EFileError.Create(sInvalidFileFormat);
end;

{*
  Donne au labyrinthe un nom par défaut à partir du nom du fichier
*}
procedure TFunLabyFile.TitleFromFileName;
var I : integer;
begin
  FTitle := ExtractFileName(FFileName);
  I := Length(FTitle);
  while (I > 0) and (FTitle[I] <> '.') do dec(I);
  if I > 0 then
    SetLength(FTitle, I);
end;

{*
  Charge le contenu du document
  @param Document   Document XML DOM contenu du fichier
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
procedure TFunLabyFile.Load(Document : IXMLDOMDocument);
var Element : IXMLDOMElement;
    NodeList : IXMLDOMNodeList;
    I : integer;
    ID, FileType, HRef : string;
begin
  { TODO 2 : Charger le fichier }
  Element := Document.documentElement.firstChild as IXMLDOMElement;
  if Element.nodeName <> 'funlabyrinthe' then InvalidFormat;

  FVersion := Element.getAttribute('version');
  if FVersion = '' then InvalidFormat;
  if CompareVersions(FVersion, CurrentVersion) > 0 then
    raise EFileError.CreateFmt(sVersionTooHigh, [FVersion]);

  FAllowEdit := Element.getAttribute('allowedit') <> 'no';

  FTitle := Element.selectSingleNode('./title').text;
  FDescription := Element.selectSingleNode('./description').xml;
  FDifficulty := Element.selectSingleNode('./difficulty').text;
  with Element.selectSingleNode('./author') as IXMLDOMElement do
  begin
    FAuthorID := StrToIntDef(getAttribute('id'), 0);
    FAuthor := text;
  end;

  NodeList := Element.selectNodes('./uses/use');
  for I := 0 to NodeList.length-1 do with NodeList.item[I] as IXMLDOMElement do
  begin
    FileType := getAttribute('type');
    HRef := getAttribute('href');
  end;

  NodeList := Element.selectNodes('./maps/map');
  for I := 0 to NodeList.length-1 do with NodeList.item[I] as IXMLDOMElement do
  begin
    ID := getAttribute('id');
    FileType := getAttribute('type');
    HRef := getAttribute('href');
  end;

  NodeList := Element.selectNodes('./players/player');
  if NodeList.length <> 1 then
    raise EFileError.Create(sThereMustBeOnePlayer);
  for I := 0 to NodeList.length-1 do with NodeList.item[I] as IXMLDOMElement do
  begin
    ID := getAttribute('id');
  end;
end;

{*
  Teste la validité de l'ouverture d'un fichier
  TestOpeningValidity vérifie que le fichier ouvert ne l'a pas été
  « illégalement ». Deux cas d'illégalité sont à tester :
  - Le fichier est une sauvegarde et est ouvert autrement que pour y jouer ;
  - Le fichier a été interdit d'édition, et ouvert dans ce mode.
  @throws EInvalidFileOpening : Le fichier a été ouvert illégalement
*}
procedure TFunLabyFile.TestOpeningValidity;
begin
  if IsSaveguard and (Mode <> fmPlay) then
    raise EFileError.Create(sCantEditSaveguard);
  if (not AllowEdit) and (Mode = fmEdit) then
    raise EFileError.Create(sEditingNotAllowed);
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier à enregistrer (si vide, conserve l'existant)
*}
procedure TFunLabyFile.Save(const AFileName : TFileName = '');
begin
  { TODO 2 : Enregistrer le fichier }
end;

end.

