{*
  Syst�me de base des �diteurs d'unit� de FunLabyEdit
  @author sjrd
  @version 5.0
*}
unit UnitEditorIntf;

interface

uses
  SysUtils, Classes, Controls, ScUtils, FilesUtils;

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
    function GetUnitFile : TUnitFile;

    {*
      Contr�le d'�dition � placer dans la fiche principale de FunLabyEdit
      @return Contr�le d'�dition
    *}
    function GetControl : TControl;

    {*
      Teste si l'�diteur peut �tre ferm�
      @return True s'il peut �tre ferm�, False pour emp�cher la fermeture
    *}
    function CanClose : boolean;

    {*
      Lib�re l'�diteur
    *}
    procedure Release;

    property UnitFile : TUnitFile read GetUnitFile;
    property Control : TControl read GetControl;
  end;

  {*
    Type de routine de call-back qui cr�e un �diteur d'unit�
    @param UnitFile   Fichier unit� � �diter
    @return Interface vers l'�diteur cr��
  *}
  TCreateUnitEditorProc = function(UnitFile : TUnitFile) : IUnitEditor50;

  {*
    Type de routine de call-back qui cr�e une nouvelle unit� et son �diteur
    @param MasterFile   Fichier ma�tre FunLabyrinthe
    @return Interface vers l'�diteur cr��
  *}
  TCreateNewUnitProc = function(MasterFile : TMasterFile) : IUnitEditor50;

  PUnitCreator = ^TUnitCreator; /// Pointeur vers TUnitCreator

  {*
    Informations sur un cr�ateur de fichier unit�
    @author sjrd
    @version 5.0
  *}
  TUnitCreator = record
    Title : string;                  /// Titre du cr�ateur
    Description : string;            /// Description longue
    CreateProc : TCreateNewUnitProc; /// Routine fabrique
  end;

  /// Tableau de TUnitCreator
  TUnitCreatorArray = array of TUnitCreator;

  PUnitFilter = ^TUnitFilter; /// Pointeur vers TUnitFilter

  {*
    Informations sur un filtre de fichier unit�
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
    Informations sur un �diteur de fichier unit�
    @author sjrd
    @version 5.0
  *}
  TUnitEditor = record
    GUID : TGUID;                       /// GUID du handler
    CreateProc : TCreateUnitEditorProc; /// Routine fabrique
  end;

var
  UnitEditors : TList = nil;  /// Liste des �diteurs d'unit�s
  UnitCreators : TList = nil; /// Liste des cr�ateurs d'unit�s
  UnitFilters : TList = nil;  /// Liste des filtres d'unit�s

{*
  S'assure qu'une liste TList est cr��e
  @param List   Liste � v�rifier
*}
procedure EnsureListCreated(var List : TList);
begin
  if List = nil then
    List := TList.Create;
end;

{*
  Cherche un enregistrement dans une liste par un GUID
  @param GUID   GUID de l'�diteur recherch�
  @return Index de l'�diteur dans la liste
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
{ Liste des �diteurs d'unit�s }
{-----------------------------}

{*
  Enregistre un �diteur de fichier unit�
  Les doublons ne sont pas support�s.
  @param GUID         GUID g�r� par l'�diteur
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
  Supprime un �diteur d'unit�
  UnregisterUnitEditor est insensible � une tentative de suppression d'un
  �diteur inexistant.
  @param GUID   GUID g�r� par l'�diteur
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
  Trouve la routine fabrique d'�diteur d'unit� g�rant le GUID donn�
  @param GUID   GUID dont il faut trouver la routine fabrique
  @return Routine fabrique d'�diteur d'unit� g�rant le GUID sp�cifi�
  @throws EFileError : Type d'unit� inconnu
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
  Cr�e un �diteur d'unit� pour l'unit� sp�cifi�e
  @param UnitFile   Fichier unit� pour lequel cr�er un �diteur
  @return Interface vers l'�diteur cr��
  @throws EFileError : Type d'unit� inconnu
*}
function CreateUnitEditor(UnitFile : TUnitFile) : IUnitEditor50;
var CreateProc : TCreateUnitEditorProc;
begin
  CreateProc := FindUnitEditorCreateProc(UnitFile.HandlerGUID);
  Result := CreateProc(UnitFile);
end;

{*
  D�termine si un �diteur est disponible pour un type de fichier donn�
  @param GUID   GUID du type de fichier � tester
  @return True si un �diteur est disponible, False sinon
*}
function UnitEditorExists(const GUID : TGUID) : boolean;
begin
  Result := Assigned(UnitEditors) and
    (IndexOfGUID(UnitEditors, GUID) >= 0);
end;

{------------------------------}
{ Liste des cr�ateurs d'unit�s }
{------------------------------}

{*
  Cherche un enregistrement dans la liste des cr�ateurs d'unit�s
  @param CreateProc   Routine fabrique
  @return Index de l'�diteur dans la liste
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
  Enregistre un cr�ateur de fichier unit�
  Les doublons ne sont pas support�s.
  @param Title         Titre du cr�ateur
  @param Description   Description du cr�ateur
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
  Supprime un cr�ateur d'unit�
  UnregisterUnitCreator est insensible � une tentative de suppression d'un
  cr�ateur inexistant.
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
  Obtient un tableau des cr�ateurs d'unit�s disponibles
  @param UnitCreatorArray   Tableau dans lequel stocker le r�sultat
*}
procedure GetUnitCreators(out UnitCreatorArray : TUnitCreatorArray);
var I : integer;
begin
  SetLength(UnitCreatorArray, UnitCreators.Count);
  for I := 0 to UnitCreators.Count-1 do
    UnitCreatorArray[I] := PUnitCreator(UnitCreators[I])^;
end;

{----------------------------}
{ Liste des filtres d'unit�s }
{----------------------------}

{*
  Enregistre un filtre de fichier unit�
  Les doublons ne sont pas support�s.
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
  Supprime un filtre d'unit�
  UnregisterUnitFilter est insensible � une tentative de suppression d'un
  filtre inexistant.
  @param GUID   GUID du type de fichier � supprimer
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
  Obtient un tableau des filtres d'unit�s disponibles
  @param UnitFilterArray   Tableau dans lequel stocker le r�sultat
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

