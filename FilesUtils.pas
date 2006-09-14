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
  Variants, IniFiles;

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

  sNoFileName = 'Aucun nom de fichier sp�cifi�';

type
  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmPlay);

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
    FHRef : string;            /// HRef
    FFileName : TFileName;     /// Nom du fichier
    FMIMEType : string;        /// Type MIME du fichier
    FMaster : TMaster;         /// Ma�tre FunLabyrinthe
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string);

    property MasterFile : TMasterFile read FMasterFile;
    property HRef : string read FHRef;
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
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      Params : TStrings); virtual;
    procedure AfterConstruction; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc); virtual;

    procedure GetParams(Params : TStrings); virtual;
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
    {*
      Enregistre le fichier
      Les descendants de TMapFile doivent impl�menter SaveFile pour pouvoir
      enregistrer la carte
    *}
    procedure SaveFile; virtual; abstract;
  protected
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      const AMapID : TComponentID;
      ADimensions : T3DPoint; AZoneWidth, AZoneHeight : integer); overload;
  public
    {*
      Cr�e une instance de TMapFile
      Les descendants de TMapFile doivent impl�menter ce constructeur, lire
      les dimensions, avant d'appeler le constructeur prot�g� qui va cr�er la
      carte.
      @param AMasterFile   Fichier ma�tre
      @param AFileName     Nom du fichier
      @param AMIMEType     Type MIME du fichier
      @param AMapID        ID de la carte
    *}
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      const AMapID : TComponentID); overload; virtual; abstract;
    procedure AfterConstruction; override;

    procedure Save(const AHRef : string = ''; const AFileName : TFileName = '');

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

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc);

    procedure Save(AFileName : TFileName = '');

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

uses
  UnitFiles, MapFiles;

var
  UnitFileClasses : TStrings = nil; /// Type MIME -> Classe unit�
  MapFileClasses : TStrings = nil;  /// Type MIME -> Classe carte

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

procedure EnsureClassListCreated;
begin
  if UnitFileClasses = nil then
    UnitFileClasses := TStringList.Create;
  if MapFileClasses = nil then
    MapFileClasses := TStringList.Create;
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
  const AHRef : string; const AFileName : TFileName; const AMIMEType : string);
begin
  inherited Create;
  FMasterFile := AMasterFile;
  FHRef := AHRef;
  FFileName := AFileName;
  FMIMEType := AMIMEType;
  FMaster := FMasterFile.Master;
end;

////////////////////////
/// Classe TUnitFile ///
////////////////////////

{*
  Cr�e une instance de TUnitFile
  @param AMasterFile   Fichier ma�tre
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Param�tres envoy�s � l'unit�
*}
constructor TUnitFile.Create(AMasterFile : TMasterFile; const AHRef : string;
  const AFileName : TFileName; const AMIMEType : string; Params : TStrings);
begin
  inherited Create(AMasterFile, AHRef, AFileName, AMIMEType);
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

{*
  Enregistre les diff�rents composants � placer dans la palette d'�dition
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure TUnitFile.RegisterComponents(
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);
begin
end;

{*
  Dresse la liste des param�tres � enregistrer
  Les descendants de TUnitFile peuvent surcharger cette m�thode pour indiquer
  au fichier ma�tre les param�tres qu'il doit enregistrer.
  @param Params   Liste des param�tres
*}
procedure TUnitFile.GetParams(Params : TStrings);
begin
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
  @param AZoneSize     Taille d'une zone de la carte
*}
constructor TMapFile.Create(AMasterFile : TMasterFile; const AHRef : string;
  const AFileName : TFileName; const AMIMEType : string;
  const AMapID : TComponentID; ADimensions : T3DPoint;
  AZoneWidth, AZoneHeight : integer);
begin
  inherited Create(AMasterFile, AHRef, AFileName, AMIMEType);
  FMapID := AMapID;
  FMap := TMap.Create(Master, AMapID, ADimensions, AZoneWidth, AZoneHeight);
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

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier dans lequel enregistrer
*}
procedure TMapFile.Save(const AHRef : string = '';
  const AFileName : TFileName = '');
begin
  if AHRef <> '' then
  begin
    FHRef := AHRef;
    FFileName := AFileName;
  end;
  SaveFile;
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

  FMaster := TMaster.Create(Mode = fmEdit);

  FUnitFiles := TObjectList.Create;
  FMapFiles := TObjectList.Create;

  Document := CoDOMDocument.Create;
  Document.async := False;
  if not Document.load(FFileName) then
    InvalidFormat;

  Load(Document);
  TestOpeningValidity;
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

  FMaster := TMaster.Create(True);

  FUnitFiles := TObjectList.Create;
  FMapFiles := TObjectList.Create;

  if Assigned(FileContents) then
  begin
    Document := CoDOMDocument.Create;
    Document.async := False;
    if not Document.loadXML(FileContents.Text) then
      InvalidFormat;

    Load(Document);
    TestOpeningValidity;
  end;
end;

{*
  D�truit l'instance
*}
destructor TMasterFile.Destroy;
begin
  { Ici on d�truit le ma�tre d'abord, afin de permettre aux unit�s de
    d�charger leurs infos sans contraintes. �videmment il ne faut donc pas que
    les gestionnaires d'unit�s et de cartes n'acc�dent encore au ma�tre dans
    le destructeur. }
  FMaster.Free;
  FMapFiles.Free;

  { Dans la mesure o� des unit�s pourraient �tre d�pendantes d'autres, il faut
    absolument les lib�rer dans l'ordre inverse de chargement. }
  while FUnitFiles.Count > 0 do
    FUnitFiles.Delete(FUnitFiles.Count-1);
  FUnitFiles.Free;

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
var Params : TStrings;
    I, J, MaxViewSize : integer;
    ID, FileType, HRef, Name, MapID : string;
    Position : T3DPoint;
    Player : TPlayer;
    FileName : TFileName;
    MapFile : TMapFile;
begin
  { Don't localize strings in this method }

  with Document.documentElement as IXMLDOMElement do
  begin
    if nodeName <> 'funlabyrinthe' then InvalidFormat;

    // Test de version
    FVersion := getAttribute('version');
    if FVersion = '' then InvalidFormat;
    if CompareVersions(FVersion, CurrentVersion) > 0 then
      raise EFileError.CreateFmt(sVersionTooHigh, [FVersion]);

    // Attributs du fichier
    FAllowEdit := getAttribute('allowedit') <> 'no';
    FIsSaveguard := getAttribute('issaveguard') = 'yes';

    // Infos standart sur le labyrinthe
    FTitle := selectSingleNode('./title').text;
    if FTitle = '' then TitleFromFileName;
    FDescription := ExtractContents(selectSingleNode('./description'));
    FDifficulty := selectSingleNode('./difficulty').text;
    with selectSingleNode('./author') as IXMLDOMElement do
    begin
      FAuthorID := StrToIntDef(getAttribute('id'), 0);
      FAuthor := text;
    end;

    // Unit�s utilis�es
    Params := THashedStringList.Create;
    try
      with selectNodes('./units/unit') do
      begin
        for I := 0 to length-1 do with item[I] as IXMLDOMElement do
        begin
          FileType := getAttribute('type');
          HRef := getAttribute('href');
          FileName := ResolveHRef(HRef, fUnitsDir);

          Params.Clear;
          with selectNodes('./param') do
          begin
            for J := 0 to length-1 do with item[J] as IXMLDOMElement do
              Params.Values[getAttribute('name')] := getAttribute('value');
          end;

          FindUnitFileClass(FileType).Create(
            Self, HRef, FileName, FileType, Params);
        end;
      end;
    finally
      Params.Free;
    end;

    // Cartes
    with selectNodes('./maps/map') do
    begin
      for I := 0 to length-1 do with item[I] as IXMLDOMElement do
      begin
        ID := getAttribute('id');
        FileType := getAttribute('type');
        HRef := getAttribute('href');
        FileName := ResolveHRef(HRef, fMapsDir);

        if VarIsNull(getAttribute('maxviewsize')) then
          MaxViewSize := MinViewSize
        else
          MaxViewSize := getAttribute('maxviewsize');

        MapFile := FindMapFileClass(FileType).Create(
          Self, HRef, FileName, FileType, ID);
        MapFile.Map.MaxViewSize := MaxViewSize;
      end;
    end;

    // Joueurs
    with selectNodes('./players/player') do
    begin
      if length <> 1 then
        raise EFileError.Create(sThereMustBeOnePlayer);
      for I := 0 to length-1 do with item[I] as IXMLDOMElement do
      begin
        ID := getAttribute('id');
        if VarIsNull(getAttribute('name')) then Name := ID else
          Name := getAttribute('name');

        with selectSingleNode('./position') as IXMLDOMElement do
        begin
          MapID := getAttribute('map');
          Position.X := getAttribute('posx');
          Position.Y := getAttribute('posy');
          Position.Z := getAttribute('posz');
        end;

        Player := TPlayer.Create(Master, ID, Name, Master.Map[MapID], Position);

        if (not VarIsNull(getAttribute('lost'))) and
           (getAttribute('lost') = 'yes') then
          Player.Lose;

        with selectNodes('./attributes/attribute') do
        begin
          for J := 0 to length-1 do with item[J] as IXMLDOMElement do
            Player.Attribute[getAttribute('name')] := getAttribute('value');
        end;

        with selectNodes('./plugins/plugin') do
        begin
          for J := 0 to length-1 do with item[J] as IXMLDOMElement do
            Player.AddPlugin(Master.Plugin[getAttribute('id')]);
        end;

        { TODO 1 : Ajouter le support du dessin du joueur }
      end;
    end;
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
  EnsureClassListCreated;
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
  EnsureClassListCreated;
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
  EnsureClassListCreated;
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
  EnsureClassListCreated;
  Index := MapFileClasses.IndexOf(MIMEType);
  if Index < 0 then
    raise EFileError.CreateFmt(sUnknownMapType, [MIMEType])
  else
    Result := TMapFileClass(MapFileClasses.Objects[Index]);
end;

{*
  Enregistre les diff�rents composants � placer dans la palette d'�dition
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure TMasterFile.RegisterComponents(
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);
var I : integer;
begin
  for I := 0 to UnitFileCount-1 do
  begin
    UnitFiles[I].RegisterComponents(
      RegisterSingleComponentProc, RegisterComponentSetProc);
  end;
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier � enregistrer (si vide, conserve l'existant)
*}
procedure TMasterFile.Save(AFileName : TFileName = '');
var Document : IXMLDOMDocument;
    FunLabyrinthe, Units, Maps, Players, Player : IXMLDOMElement;
    Element, Param : IXMLDOMElement;
    Params : TStrings;
    MapHRef : string;
    MapFileName : TFileName;
    I, J : integer;
begin
  { Don't localize strings in this method }

  if (AFileName = '') and (Mode = fmPlay) and (not IsSaveguard) then
    raise EFileError.Create(sNoFileName);
  if AFileName = '' then
    AFileName := FFileName;

  if Mode = fmPlay then
  begin
    MapHRef := ExtractFileName(AFileName);
    I := LastDelimiter('.', MapHRef);
    if I > 0 then
    begin
      SetLength(MapHRef, I);
      MapHRef[I] := PathDelim;
    end else MapHRef := MapHRef + '-files' + PathDelim;

    MapFileName := ExtractFilePath(AFileName) + MapHRef;

    ForceDirectories(MapFileName);
  end else
  begin
    MapHRef := '';
    MapFileName := '';
  end;

  Document := CoDOMDocument.Create;
  Document.async := False;

  with Document do
  begin
    FunLabyrinthe := Document.createElement('funlabyrinthe');

    FunLabyrinthe.setAttribute('version', CurrentVersion);
    if not AllowEdit then
      FunLabyrinthe.setAttribute('allowedit', 'no');
    if Mode = fmPlay then
      FunLabyrinthe.setAttribute('issaveguard', 'yes');

    with FunLabyrinthe do
    begin
      Element := Document.createElement('title');
      Element.text := Title;
      appendChild(Element);

      Element := Document.createElement('description');
      Element.text := Description;
      appendChild(Element);

      Element := Document.createElement('difficulty');
      Element.text := Difficulty;
      appendChild(Element);

      Element := Document.createElement('author');
      Element.text := Author;
      if AuthorID > 0 then
        Element.setAttribute('id', AuthorID);
      appendChild(Element);

      // Unit�s
      Units := Document.createElement('units');
      with Units do
      begin
        Params := TStringList.Create;
        try
          for I := 0 to UnitFileCount-1 do with UnitFiles[I] do
          begin
            Params.Clear;
            GetParams(Params);

            Element := Document.createElement('unit');
            Element.setAttribute('type', MIMEType);
            Element.setAttribute('href', HRef);

            with Element do for J := 0 to Params.Count-1 do
            begin
              Param := Document.createElement('param');
              Param.setAttribute('name', Params.Names[J]);
              Param.setAttribute('value', Params.ValueFromIndex[J]);
              appendChild(Param);
            end;

            appendChild(Element); // unit
          end;
        finally
          Params.Free;
        end;
      end;
      appendChild(Units);

      // Cartes
      Maps := Document.createElement('maps');
      with Maps do
      begin
        for I := 0 to MapFileCount-1 do with MapFiles[I] do
        begin
          if Mode <> fmPlay then Save else
            Save(MapHRef+ExtractFileName(FileName),
              MapFileName+ExtractFileName(FileName));

          Element := Document.createElement('map');
          Element.setAttribute('id', Map.ID);
          Element.setAttribute('type', MIMEType);
          Element.setAttribute('href', HRef);
          if Map.MaxViewSize > 1 then
            Element.setAttribute('maxviewsize', Map.MaxViewSize);
          appendChild(Element);
        end;
      end;
      appendChild(Maps);

      // Joueurs
      Players := Document.createElement('players');
      with Players do
      begin
        Params := TStringList.Create;
        try
          for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
          begin
            Player := Document.createElement('player');
            Player.setAttribute('id', ID);
            Player.setAttribute('name', Name);
            if PlayState = psLost then
              Player.setAttribute('lost', 'yes');

            with Player do
            begin
              Element := Document.createElement('position');
              Element.setAttribute('map', Map.ID);
              Element.setAttribute('posx', Position.X);
              Element.setAttribute('posy', Position.Y);
              Element.setAttribute('posz', Position.Z);
              appendChild(Element);

              Element := Document.createElement('attributes');
              with Element do
              begin
                GetAttributes(Params);
                for J := 0 to Params.Count-1 do
                begin
                  Param := Document.createElement('attribute');
                  Param.setAttribute('name', Params[J]);
                  Param.setAttribute('value', integer(Params.Objects[J]));
                  appendChild(Param);
                end;
              end;
              appendChild(Element); // attributes

              Element := Document.createElement('plugins');
              with Element do
              begin
                GetPluginIDs(Params);
                for J := 0 to Params.Count-1 do
                begin
                  Param := Document.createElement('plugin');
                  Param.setAttribute('id', Params[J]);
                  appendChild(Param);
                end;
              end;
              appendChild(Element); // plugins
            end;

            appendChild(Player);
          end;
        finally
          Params.Free;
        end;
      end;
      appendChild(Players);
    end;

    appendChild(FunLabyrinthe);
  end;

  Document.save(AFileName);
  FFileName := AFileName;
  if Mode = fmPlay then
    FIsSaveguard := True;
end;

initialization
  EnsureClassListCreated;
finalization
  MapFileClasses.Free;
  UnitFileClasses.Free;
end.

