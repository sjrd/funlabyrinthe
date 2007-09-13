{*
  Système de base des éditeurs d'unité de FunLabyEdit
  @author sjrd
  @version 5.0
*}
unit UnitEditorIntf;

interface

uses
  SysUtils, Classes, Controls, ScUtils, ScLists, ScStrUtils, FilesUtils;

type
  {*
    Interface d'un éditeur d'unité de FunLabyEdit
    Les classes implémentant cette interface doivent outrepasser la comptage de
    références, mais se libérer dans la méthode Release.
    @author sjrd
    @version 5.0
  *}
  IUnitEditor50 = interface
    ['{5AA72B3E-FFCB-4185-A846-A87353165647}']

    {*
      Unité ouverte dans l'éditeur
      @return Unité ouverte
    *}
    function GetUnitFile: TUnitFile;

    {*
      Contrôle d'édition à placer dans la fiche principale de FunLabyEdit
      @return Contrôle d'édition
    *}
    function GetControl: TControl;

    {*
      Teste si l'éditeur peut être fermé
      @return True s'il peut être fermé, False pour empêcher la fermeture
    *}
    function CanClose: Boolean;

    {*
      Libère l'éditeur
    *}
    procedure Release;

    property UnitFile: TUnitFile read GetUnitFile;
    property Control: TControl read GetControl;
  end;

  {*
    Type de routine de call-back qui crée un éditeur d'unité
    @param UnitFile   Fichier unité à éditer
    @return Interface vers l'éditeur créé
  *}
  TCreateUnitEditorProc = function(UnitFile: TUnitFile): IUnitEditor50;

  {*
    Collection d'éditeurs d'unité
    @author sjrd
    @version 5.0
  *}
  TUnitEditorList = class(TCustomValueBucketList)
  public
    constructor Create;

    procedure Add(const GUID: TGUID; CreateProc: TCreateUnitEditorProc);
    procedure Remove(const GUID: TGUID);
    function Exists(const GUID: TGUID): Boolean;
    function Find(const GUID: TGUID): TCreateUnitEditorProc;

    function CreateEditor(UnitFile: TUnitFile): IUnitEditor50;
  end;

  {*
    Type de routine de call-back qui crée une nouvelle unité
    Si l'enregistrement du créateur a spécifié qu'il ne fallait pas demander
    automatiquement un nom de fichier, le paramètre FileName est vide lors de
    l'appel.
    Dans tous les cas, FileName peut être modifié, et doit indiquer en sortie -
    si la fonction renvoie True - le nom de fichier réel, qui doit exister.
    Si la fonction renvoie False, la valeur de FileName est indéfinie en sortie.
    @param FileName   Nom du fichier unité à créer
    @param GUID       GUID du type du fichier créé
    @return True si le fichier a bien été créé, False sinon
  *}
  TCreateNewUnitProc = function(var FileName: TFileName;
    out GUID: TGUID): Boolean;

  /// Pointeur vers TUnitCreatorInfo
  PUnitCreatorInfo = ^TUnitCreatorInfo;

  {*
    Informations sur un créateur de fichier unité
    @author sjrd
    @version 5.0
  *}
  TUnitCreatorInfo = record
    Title: string;           /// Titre du créateur
    Description: string;     /// Description longue
    AskForFileName: Boolean; /// Demander un nom de fichier
    Filter: string;          /// Filtre de nom de fichier
  end;

  {*
    Collection de créateurs d'unité
    @author sjrd
    @version 5.0
  *}
  TUnitCreatorList = class(TCustomValueBucketList)
  public
    constructor Create;

    procedure Add(CreateProc: TCreateNewUnitProc;
      const Info: TUnitCreatorInfo);
    procedure Remove(CreateProc: TCreateNewUnitProc);
  end;

  {*
    Collection de filtres d'unité
    @author sjrd
    @version 5.0
  *}
  TUnitFilterList = class(TCustomValueBucketList)
  private
    function GetGUID(const Filter: string): TGUID;
  protected
    function BucketFor(const Key): Cardinal; override;
    function KeyEquals(const Key1, Key2): Boolean; override;
  public
    constructor Create;

    procedure Add(const Filter: string; const GUID: TGUID);
    procedure Remove(const Filter: string);

    property GUID[const Filter: string]: TGUID read GetGUID;
  end;

var
  /// Éditeurs d'unité
  UnitEditors: TUnitEditorList = nil;

  /// Créateurs d'unité
  UnitCreators: TUnitCreatorList = nil;

  /// Filtres d'unité
  UnitFilters: TUnitFilterList = nil;

{procedure RegisterUnitEditor(const GUID : TGUID;
  CreateProc : TCreateUnitEditorProc);
procedure UnregisterUnitEditor(const GUID : TGUID);
function FindUnitEditorCreateProc(const GUID : TGUID) : TCreateUnitEditorProc;
function CreateUnitEditor(UnitFile : TUnitFile) : IUnitEditor50;
function UnitEditorExists(const GUID : TGUID) : boolean;

procedure RegisterUnitCreator(const Title, Description : string;
  CreateProc : TCreateNewUnitProc; AskForFileName : boolean = True;
  Filter : string = '');
procedure UnregisterUnitCreator(CreateProc : TCreateNewUnitProc);
procedure GetUnitCreators(out UnitCreatorArray : TUnitCreatorArray);

procedure RegisterUnitFilter(const GUID : TGUID; const Filter : string);
procedure UnregisterUnitFilter(const GUID : TGUID);
procedure GetUnitFilters(out UnitFilterArray : TUnitFilterArray);}

implementation

{------------------------}
{ Classe TUnitEditorList }
{------------------------}

{*
  Crée une nouvelle instance de TUnitEditorList
*}
constructor TUnitEditorList.Create;
begin
  inherited Create(SizeOf(TGUID), SizeOf(TCreateUnitEditorProc));
end;

{*
  Ajoute un éditeur
  @param GUID         GUID du type de fichiers géré
  @param CreateProc   Routine de call-back créant l'éditeur
*}
procedure TUnitEditorList.Add(const GUID: TGUID;
  CreateProc: TCreateUnitEditorProc);
begin
  AddData(GUID, CreateProc);
end;

{*
  Supprime un éditeur
  @param GUID   GUID du type de fichiers géré
*}
procedure TUnitEditorList.Remove(const GUID: TGUID);
begin
  RemoveData(GUID);
end;

{*
  Teste s'il existe un éditeur enregistré pour un type de fichiers donné
  @param GUID   GUID du type de fichiers
  @return True s'il existe un éditeur approprié, False sinon
*}
function TUnitEditorList.Exists(const GUID: TGUID): Boolean;
begin
  Result := (inherited Exists(GUID));
end;

{*
  Trouve la routine de call-back correspondant à un type de fichiers
  @param GUID   GUID du type de fichiers
  @return Routine de call-back de création de l'éditeur
  @throws EFileError Type de fichier inconnu
*}
function TUnitEditorList.Find(const GUID: TGUID): TCreateUnitEditorProc;
begin
  if not (inherited Find(GUID, Result)) then
    raise EFileError.CreateFmt(sUnknownUnitType, [GUIDToString(GUID)]);
end;

{*
  Crée un éditeur pour un fichier unité donné
  @param UnitFile   Fichier unité
  @return Un nouvel éditeur pour le fichier UnitFile
  @throws EFileError Type de fichier inconnu
*}
function TUnitEditorList.CreateEditor(UnitFile: TUnitFile): IUnitEditor50;
var
  CreateProc: TCreateUnitEditorProc;
begin
  CreateProc := Find(UnitFile.HandlerGUID);
  Result := CreateProc(UnitFile);
end;

{-------------------------}
{ Classe TUnitCreatorList }
{-------------------------}

{*
  Crée une nouvelle instance de TUnitCreatorList
*}
constructor TUnitCreatorList.Create;
begin
  inherited Create(SizeOf(TCreateNewUnitProc), TypeInfo(TUnitCreatorInfo));
end;

{*
  Ajoute un créateur d'unité
  @param CreateProc   Routine de call-back de création d'unité
  @param Info         Informations sur ce créateur d'unité
*}
procedure TUnitCreatorList.Add(CreateProc: TCreateNewUnitProc;
  const Info: TUnitCreatorInfo);
begin
  AddData(CreateProc, Info);
end;

{*
  Supprime un créateur d'unité
  @param CreateProc   Routine de call-back de création d'unité
*}
procedure TUnitCreatorList.Remove(CreateProc: TCreateNewUnitProc);
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
  inherited Create(TypeInfo(string), SizeOf(TGUID));
end;

{*
  Table de correspondance Filtre -> GUID
  @param Filter   Filtre de fichiers
  @result GUID de ce type de fichiers
*}
function TUnitFilterList.GetGUID(const Filter: string): TGUID;
begin
  GetData(Filter, Result);
end;

{*
  [@inheritDoc]
*}
function TUnitFilterList.BucketFor(const Key): Cardinal;
begin
  Result := HashOfStr(string(Key)) mod BucketCount;
end;

{*
  [@inheritDoc]
*}
function TUnitFilterList.KeyEquals(const Key1, Key2): Boolean;
begin
  Result := string(Key1) = string(Key2);
end;

{*
  Ajoute un filtre d'unité
  @param Filter   Filtre d'unité (tel qu'utilisé par TOpenDialog/TSaveDialog)
  @param GUID     GUID de type des fichiers correspondant à Filter
*}
procedure TUnitFilterList.Add(const Filter: string; const GUID: TGUID);
begin
  AddData(Filter, GUID);
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
  UnitEditors := TUnitEditorList.Create;
  UnitCreators := TUnitCreatorList.Create;
  UnitFilters := TUnitFilterList.Create;
finalization
  UnitEditors.Free;
  UnitEditors := nil;
  UnitCreators.Free;
  UnitCreators := nil;
  UnitFilters.Free;
  UnitFilters := nil;
end.

