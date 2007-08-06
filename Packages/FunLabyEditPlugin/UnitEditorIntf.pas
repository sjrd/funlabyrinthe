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
    Informations sur un �diteur de fichier unit�
    @author sjrd
    @version 5.0
  *}
  TUnitEditor = record
    GUID : TGUID;                       /// GUID du handler
    CreateProc : TCreateUnitEditorProc; /// Routine fabrique
  end;

var
  UnitEditors : TList = nil; /// Liste des �diteurs d'unit�s

{*
  S'assure que la liste des d'�diteurs d'unit�s est cr��e
*}
procedure EnsureUnitEditorListCreated;
begin
  if UnitEditors = nil then
    UnitEditors := TList.Create;
end;

{*
  Cherche un enregistrement dans la liste des �diteurs d'unit�s
  @param GUID   GUID de l'�diteur recherch�
  @return Index de l'�diteur dans la liste
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
  Supprime un �diteur d'unit�
  UnregisterUnitEditorClass est insensible � une tentative de suppression d'un
  �diteur inexistant.
  @param GUID   GUID g�r� par l'�diteur
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
  Trouve la routine fabrique d'�diteur d'unit� g�rant le GUID donn�
  @param GUID   GUID dont il faut trouver la routine fabrique
  @return Routine fabrique d'�diteur d'unit� g�rant le GUID sp�cifi�
  @throws EFileError : Type d'unit� inconnu
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
    (IndexOfUnitEditor(GUID) >= 0);
end;

initialization
  EnsureUnitEditorListCreated;
finalization
  UnitEditors.Free;
  UnitEditors := nil;
end.

