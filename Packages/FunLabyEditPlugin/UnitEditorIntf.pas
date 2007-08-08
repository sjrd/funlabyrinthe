{*
  Système de base des éditeurs d'unité de FunLabyEdit
  @author sjrd
  @version 5.0
*}
unit UnitEditorIntf;

interface

uses
  SysUtils, Classes, Controls, ScUtils, FilesUtils;

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
    function GetUnitFile : TUnitFile;

    {*
      Contrôle d'édition à placer dans la fiche principale de FunLabyEdit
      @return Contrôle d'édition
    *}
    function GetControl : TControl;

    {*
      Teste si l'éditeur peut être fermé
      @return True s'il peut être fermé, False pour empêcher la fermeture
    *}
    function CanClose : boolean;

    {*
      Libère l'éditeur
    *}
    procedure Release;

    property UnitFile : TUnitFile read GetUnitFile;
    property Control : TControl read GetControl;
  end;

  {*
    Type de routine de call-back qui crée un éditeur d'unité
    @param UnitFile   Fichier unité à éditer
    @return Interface vers l'éditeur créé
  *}
  TCreateUnitEditorProc = function(UnitFile : TUnitFile) : IUnitEditor50;

  {*
    Type de routine de call-back qui crée une nouvelle unité et son éditeur
    @param MasterFile   Fichier maître FunLabyrinthe
    @return Interface vers l'éditeur créé
  *}
  TCreateNewUnitProc = function(MasterFile : TMasterFile) : IUnitEditor50;

  PUnitCreator = ^TUnitCreator; /// Pointeur vers TUnitCreator

  {*
    Informations sur un créateur de fichier unité
    @author sjrd
    @version 5.0
  *}
  TUnitCreator = record
    Title : string;                  /// Titre du créateur
    Description : string;            /// Description longue
    CreateProc : TCreateNewUnitProc; /// Routine fabrique
  end;

  /// Tableau de TUnitCreator
  TUnitCreatorArray = array of TUnitCreator;

  PUnitFilter = ^TUnitFilter; /// Pointeur vers TUnitFilter

  {*
    Informations sur un filtre de fichier unité
    @author sjrd
    @version 5.0
  *}
  TUnitFilter = record
    GUID : TGUID;    /// GUID du handler
    Filter : string; /// Filtre de fichiers
  end;

  /// Tableau de TUnitFilter
  TUnitFilterArray = array of TUnitFilter;

procedure RegisterUnitEditor(const GUID : TGUID;
  CreateProc : TCreateUnitEditorProc);
procedure UnregisterUnitEditor(const GUID : TGUID);
function FindUnitEditorCreateProc(const GUID : TGUID) : TCreateUnitEditorProc;
function CreateUnitEditor(UnitFile : TUnitFile) : IUnitEditor50;
function UnitEditorExists(const GUID : TGUID) : boolean;

procedure RegisterUnitCreator(const Title, Description : string;
  CreateProc : TCreateNewUnitProc);
procedure UnregisterUnitCreator(CreateProc : TCreateNewUnitProc);
procedure GetUnitCreators(out UnitCreatorArray : TUnitCreatorArray);

procedure RegisterUnitFilter(const GUID : TGUID; const Filter : string);
procedure UnregisterUnitFilter(const GUID : TGUID);
procedure GetUnitFilters(out UnitFilterArray : TUnitFilterArray);

implementation

type
  PUnitEditor = ^TUnitEditor; /// Pointeur vers TUnitHandler

  {*
    Informations sur un éditeur de fichier unité
    @author sjrd
    @version 5.0
  *}
  TUnitEditor = record
    GUID : TGUID;                       /// GUID du handler
    CreateProc : TCreateUnitEditorProc; /// Routine fabrique
  end;

var
  UnitEditors : TList = nil;  /// Liste des éditeurs d'unités
  UnitCreators : TList = nil; /// Liste des créateurs d'unités
  UnitFilters : TList = nil;  /// Liste des filtres d'unités

{*
  S'assure qu'une liste TList est créée
  @param List   Liste à vérifier
*}
procedure EnsureListCreated(var List : TList);
begin
  if List = nil then
    List := TList.Create;
end;

{*
  Cherche un enregistrement dans une liste par un GUID
  @param GUID   GUID de l'éditeur recherché
  @return Index de l'éditeur dans la liste
*}
function IndexOfGUID(List : TList; const GUID : TGUID) : integer;
var PointerList : PPointerList;
begin
  PointerList := List.List;
  Result := 0;

  while Result < List.Count do
  begin
    if SameGUID(GUID, PGUID(PointerList[Result])^) then
      exit
    else
      inc(Result);
  end;

  Result := -1;
end;

{-----------------------------}
{ Liste des éditeurs d'unités }
{-----------------------------}

{*
  Enregistre un éditeur de fichier unité
  Les doublons ne sont pas supportés.
  @param GUID         GUID géré par l'éditeur
  @param CreateProc   Routine fabrique
*}
procedure RegisterUnitEditor(const GUID : TGUID;
  CreateProc : TCreateUnitEditorProc);
var Index : integer;
    Editor : PUnitEditor;
begin
  EnsureListCreated(UnitEditors);
  Index := IndexOfGUID(UnitEditors, GUID);

  if Index >= 0 then Editor := UnitEditors[Index] else
  begin
    New(Editor);
    Editor.GUID := GUID;
    UnitEditors.Add(Editor);
  end;

  Editor.CreateProc := CreateProc;
end;

{*
  Supprime un éditeur d'unité
  UnregisterUnitEditor est insensible à une tentative de suppression d'un
  éditeur inexistant.
  @param GUID   GUID géré par l'éditeur
*}
procedure UnregisterUnitEditor(const GUID : TGUID);
var Index : integer;
    Editor : PUnitEditor;
begin
  if not Assigned(UnitEditors) then exit;
  Index := IndexOfGUID(UnitEditors, GUID);
  if Index < 0 then exit;

  Editor := UnitEditors[Index];
  UnitEditors.Delete(Index);
  Dispose(Editor);
end;

{*
  Trouve la routine fabrique d'éditeur d'unité gérant le GUID donné
  @param GUID   GUID dont il faut trouver la routine fabrique
  @return Routine fabrique d'éditeur d'unité gérant le GUID spécifié
  @throws EFileError : Type d'unité inconnu
*}
function FindUnitEditorCreateProc(const GUID : TGUID) : TCreateUnitEditorProc;
var Index : integer;
begin
  if Assigned(UnitEditors) then
    Index := IndexOfGUID(UnitEditors, GUID)
  else
    Index := -1;

  if Index < 0 then
    raise EFileError.CreateFmt(sUnknownUnitType, [GUIDToString(GUID)])
  else
    Result := PUnitEditor(UnitEditors[Index]).CreateProc;
end;

{*
  Crée un éditeur d'unité pour l'unité spécifiée
  @param UnitFile   Fichier unité pour lequel créer un éditeur
  @return Interface vers l'éditeur créé
  @throws EFileError : Type d'unité inconnu
*}
function CreateUnitEditor(UnitFile : TUnitFile) : IUnitEditor50;
var CreateProc : TCreateUnitEditorProc;
begin
  CreateProc := FindUnitEditorCreateProc(UnitFile.HandlerGUID);
  Result := CreateProc(UnitFile);
end;

{*
  Détermine si un éditeur est disponible pour un type de fichier donné
  @param GUID   GUID du type de fichier à tester
  @return True si un éditeur est disponible, False sinon
*}
function UnitEditorExists(const GUID : TGUID) : boolean;
begin
  Result := Assigned(UnitEditors) and
    (IndexOfGUID(UnitEditors, GUID) >= 0);
end;

{------------------------------}
{ Liste des créateurs d'unités }
{------------------------------}

{*
  Cherche un enregistrement dans la liste des créateurs d'unités
  @param CreateProc   Routine fabrique
  @return Index de l'éditeur dans la liste
*}
function IndexOfUnitCreator(CreateProc : TCreateNewUnitProc) : integer;
var List : PPointerList;
begin
  List := UnitCreators.List;
  Result := 0;

  while Result < UnitCreators.Count do
  begin
    if @CreateProc = @PUnitCreator(List[Result]).CreateProc then
      exit
    else
      inc(Result);
  end;

  Result := -1;
end;

{*
  Enregistre un créateur de fichier unité
  Les doublons ne sont pas supportés.
  @param Title         Titre du créateur
  @param Description   Description du créateur
  @param CreateProc    Routine fabrique
*}
procedure RegisterUnitCreator(const Title, Description : string;
  CreateProc : TCreateNewUnitProc);
var Index : integer;
    Creator : PUnitCreator;
begin
  EnsureListCreated(UnitCreators);
  Index := IndexOfUnitCreator(CreateProc);

  if Index >= 0 then Creator := UnitCreators[Index] else
  begin
    New(Creator);
    Initialize(Creator^);
    Creator.CreateProc := CreateProc;
    UnitCreators.Add(Creator);
  end;

  Creator.Title := Title;
  Creator.Description := Description;
end;

{*
  Supprime un créateur d'unité
  UnregisterUnitCreator est insensible à une tentative de suppression d'un
  créateur inexistant.
  @param CreateProc   Routine fabrique
*}
procedure UnregisterUnitCreator(CreateProc : TCreateNewUnitProc);
var Index : integer;
    Creator : PUnitCreator;
begin
  if not Assigned(UnitCreators) then exit;
  Index := IndexOfUnitCreator(CreateProc);
  if Index < 0 then exit;

  Creator := UnitCreators[Index];
  UnitCreators.Delete(Index);
  Finalize(Creator^);
  Dispose(Creator);
end;

{*
  Obtient un tableau des créateurs d'unités disponibles
  @param UnitCreatorArray   Tableau dans lequel stocker le résultat
*}
procedure GetUnitCreators(out UnitCreatorArray : TUnitCreatorArray);
var I : integer;
begin
  SetLength(UnitCreatorArray, UnitCreators.Count);
  for I := 0 to UnitCreators.Count-1 do
    UnitCreatorArray[I] := PUnitCreator(UnitCreators[I])^;
end;

{----------------------------}
{ Liste des filtres d'unités }
{----------------------------}

{*
  Enregistre un filtre de fichier unité
  Les doublons ne sont pas supportés.
  @param GUID     GUID du type fichier
  @param Filter   Filtre de fichier
*}
procedure RegisterUnitFilter(const GUID : TGUID; const Filter : string);
var Index : integer;
    FilterInfo : PUnitFilter;
begin
  EnsureListCreated(UnitFilters);
  Index := IndexOfGUID(UnitFilters, GUID);

  if Index >= 0 then FilterInfo := UnitFilters[Index] else
  begin
    New(FilterInfo);
    FilterInfo.GUID := GUID;
    UnitFilters.Add(FilterInfo);
  end;

  FilterInfo.Filter := Filter;
end;

{*
  Supprime un filtre d'unité
  UnregisterUnitFilter est insensible à une tentative de suppression d'un
  filtre inexistant.
  @param GUID   GUID du type de fichier à supprimer
*}
procedure UnregisterUnitFilter(const GUID : TGUID);
var Index : integer;
    Filter : PUnitFilter;
begin
  if not Assigned(UnitFilters) then exit;
  Index := IndexOfGUID(UnitFilters, GUID);
  if Index < 0 then exit;

  Filter := UnitFilters[Index];
  UnitFilters.Delete(Index);
  Dispose(Filter);
end;

{*
  Obtient un tableau des filtres d'unités disponibles
  @param UnitFilterArray   Tableau dans lequel stocker le résultat
*}
procedure GetUnitFilters(out UnitFilterArray : TUnitFilterArray);
var I : integer;
begin
  SetLength(UnitFilterArray, UnitFilters.Count);
  for I := 0 to UnitFilters.Count-1 do
    UnitFilterArray[I] := PUnitFilter(UnitFilters[I])^;
end;

initialization
  EnsureListCreated(UnitEditors);
  EnsureListCreated(UnitCreators);
  EnsureListCreated(UnitFilters);
finalization
  UnitEditors.Free;
  UnitEditors := nil;
  UnitCreators.Free;
  UnitCreators := nil;
  UnitFilters.Free;
  UnitFilters := nil;
end.

