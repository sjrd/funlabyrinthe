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

procedure RegisterUnitEditor(const GUID : TGUID;
  CreateProc : TCreateUnitEditorProc);
procedure UnregisterUnitEditor(const GUID : TGUID);
function FindUnitEditorCreateProc(const GUID : TGUID) : TCreateUnitEditorProc;
function CreateUnitEditor(UnitFile : TUnitFile) : IUnitEditor50;
function UnitEditorExists(const GUID : TGUID) : boolean;

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
  UnitEditors : TList = nil; /// Liste des éditeurs d'unités

{*
  S'assure que la liste des d'éditeurs d'unités est créée
*}
procedure EnsureUnitEditorListCreated;
begin
  if UnitEditors = nil then
    UnitEditors := TList.Create;
end;

{*
  Cherche un enregistrement dans la liste des éditeurs d'unités
  @param GUID   GUID de l'éditeur recherché
  @return Index de l'éditeur dans la liste
*}
function IndexOfUnitEditor(const GUID : TGUID) : integer;
var List : PPointerList;
begin
  List := UnitEditors.List;
  Result := 0;

  while Result < UnitEditors.Count do
  begin
    if SameGUID(GUID, PUnitEditor(List[Result]).GUID) then
      exit
    else
      inc(Result);
  end;

  Result := -1;
end;

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
  EnsureUnitEditorListCreated;
  Index := IndexOfUnitEditor(GUID);

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
  UnregisterUnitEditorClass est insensible à une tentative de suppression d'un
  éditeur inexistant.
  @param GUID   GUID géré par l'éditeur
*}
procedure UnregisterUnitEditor(const GUID : TGUID);
var Index : integer;
    Handler : PUnitEditor;
begin
  if not Assigned(UnitEditors) then exit;
  Index := IndexOfUnitEditor(GUID);
  if Index < 0 then exit;

  Handler := UnitEditors[Index];
  UnitEditors.Delete(Index);
  Dispose(Handler);
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
    Index := IndexOfUnitEditor(GUID)
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
    (IndexOfUnitEditor(GUID) >= 0);
end;

initialization
  EnsureUnitEditorListCreated;
finalization
  UnitEditors.Free;
  UnitEditors := nil;
end.

