{*
  Syst�me de base des �diteurs d'unit� de FunLabyEdit
  @author sjrd
  @version 5.0
*}
unit UnitEditorIntf;

interface

uses
  SysUtils, Classes, Controls, ScUtils, ScLists, ScStrUtils, FilesUtils;

type
  {*
    Interface d'un �diteur d'unit� de FunLabyEdit
    Les classes impl�mentant cette interface doivent outrepasser la comptage de
    r�f�rences, mais se lib�rer dans la m�thode Release.
    @author sjrd
    @version 5.0
  *}
  IUnitEditor50 = interface
    ['{5AA72B3E-FFCB-4185-A846-A87353165647}']

    {*
      Unit� ouverte dans l'�diteur
      @return Unit� ouverte
    *}
    function GetUnitFile: TUnitFile;

    {*
      Contr�le d'�dition � placer dans la fiche principale de FunLabyEdit
      @return Contr�le d'�dition
    *}
    function GetControl: TControl;

    {*
      Teste si l'�diteur peut �tre ferm�
      @return True s'il peut �tre ferm�, False pour emp�cher la fermeture
    *}
    function CanClose: Boolean;

    {*
      Lib�re l'�diteur
    *}
    procedure Release;

    property UnitFile: TUnitFile read GetUnitFile;
    property Control: TControl read GetControl;
  end;

  {*
    Type de routine de call-back qui cr�e un �diteur d'unit�
    @param UnitFile   Fichier unit� � �diter
    @return Interface vers l'�diteur cr��
  *}
  TCreateUnitEditorProc = function(UnitFile: TUnitFile): IUnitEditor50;

  {*
    Collection d'�diteurs d'unit�
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
    Type de routine de call-back qui cr�e une nouvelle unit�
    Si l'enregistrement du cr�ateur a sp�cifi� qu'il ne fallait pas demander
    automatiquement un nom de fichier, le param�tre FileName est vide lors de
    l'appel.
    Dans tous les cas, FileName peut �tre modifi�, et doit indiquer en sortie -
    si la fonction renvoie True - le nom de fichier r�el, qui doit exister.
    Si la fonction renvoie False, la valeur de FileName est ind�finie en sortie.
    @param FileName   Nom du fichier unit� � cr�er
    @param GUID       GUID du type du fichier cr��
    @return True si le fichier a bien �t� cr��, False sinon
  *}
  TCreateNewUnitProc = function(var FileName: TFileName;
    out GUID: TGUID): Boolean;

  /// Pointeur vers TUnitCreatorInfo
  PUnitCreatorInfo = ^TUnitCreatorInfo;

  {*
    Informations sur un cr�ateur de fichier unit�
    @author sjrd
    @version 5.0
  *}
  TUnitCreatorInfo = record
    Title: string;           /// Titre du cr�ateur
    Description: string;     /// Description longue
    AskForFileName: Boolean; /// Demander un nom de fichier
    Filter: string;          /// Filtre de nom de fichier
  end;

  {*
    Collection de cr�ateurs d'unit�
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
    Collection de filtres d'unit�
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
  /// �diteurs d'unit�
  UnitEditors: TUnitEditorList = nil;

  /// Cr�ateurs d'unit�
  UnitCreators: TUnitCreatorList = nil;

  /// Filtres d'unit�
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
  Cr�e une nouvelle instance de TUnitEditorList
*}
constructor TUnitEditorList.Create;
begin
  inherited Create(SizeOf(TGUID), SizeOf(TCreateUnitEditorProc));
end;

{*
  Ajoute un �diteur
  @param GUID         GUID du type de fichiers g�r�
  @param CreateProc   Routine de call-back cr�ant l'�diteur
*}
procedure TUnitEditorList.Add(const GUID: TGUID;
  CreateProc: TCreateUnitEditorProc);
begin
  AddData(GUID, CreateProc);
end;

{*
  Supprime un �diteur
  @param GUID   GUID du type de fichiers g�r�
*}
procedure TUnitEditorList.Remove(const GUID: TGUID);
begin
  RemoveData(GUID);
end;

{*
  Teste s'il existe un �diteur enregistr� pour un type de fichiers donn�
  @param GUID   GUID du type de fichiers
  @return True s'il existe un �diteur appropri�, False sinon
*}
function TUnitEditorList.Exists(const GUID: TGUID): Boolean;
begin
  Result := (inherited Exists(GUID));
end;

{*
  Trouve la routine de call-back correspondant � un type de fichiers
  @param GUID   GUID du type de fichiers
  @return Routine de call-back de cr�ation de l'�diteur
  @throws EFileError Type de fichier inconnu
*}
function TUnitEditorList.Find(const GUID: TGUID): TCreateUnitEditorProc;
begin
  if not (inherited Find(GUID, Result)) then
    raise EFileError.CreateFmt(sUnknownUnitType, [GUIDToString(GUID)]);
end;

{*
  Cr�e un �diteur pour un fichier unit� donn�
  @param UnitFile   Fichier unit�
  @return Un nouvel �diteur pour le fichier UnitFile
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
  Cr�e une nouvelle instance de TUnitCreatorList
*}
constructor TUnitCreatorList.Create;
begin
  inherited Create(SizeOf(TCreateNewUnitProc), TypeInfo(TUnitCreatorInfo));
end;

{*
  Ajoute un cr�ateur d'unit�
  @param CreateProc   Routine de call-back de cr�ation d'unit�
  @param Info         Informations sur ce cr�ateur d'unit�
*}
procedure TUnitCreatorList.Add(CreateProc: TCreateNewUnitProc;
  const Info: TUnitCreatorInfo);
begin
  AddData(CreateProc, Info);
end;

{*
  Supprime un cr�ateur d'unit�
  @param CreateProc   Routine de call-back de cr�ation d'unit�
*}
procedure TUnitCreatorList.Remove(CreateProc: TCreateNewUnitProc);
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
  Ajoute un filtre d'unit�
  @param Filter   Filtre d'unit� (tel qu'utilis� par TOpenDialog/TSaveDialog)
  @param GUID     GUID de type des fichiers correspondant � Filter
*}
procedure TUnitFilterList.Add(const Filter: string; const GUID: TGUID);
begin
  AddData(Filter, GUID);
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

