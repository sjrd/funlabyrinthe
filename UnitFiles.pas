 {*
  Gestion des fichiers unit� de base de FunLabyrinthe
  L'unit� UnitFiles d�crit les classes de gestion des fichiers unit� de base de
  FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit UnitFiles;

interface

uses
  Windows, SysUtils, Classes, FunLabyUtils, FilesUtils;

resourcestring
  sCantLoadPackage = 'Impossible de charger le paquet "%s"';

type
  {*
    Repr�sente un fichier unit� de type paquet Borland
    TBPLUnitFile repr�sente un fichier unit� qui charge un paquet Borland
    d�finissant les nouveaux �l�ments.
  *}
  TBPLUnitFile = class(TUnitFile)
  private
    FHandle : HMODULE;      /// Module du package charg�
    FAttributes : TStrings; /// Attributs libres pour le package
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      Params : TStrings); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc); override;

    procedure GetParams(Params : TStrings); override;

    property Attributes : TStrings read FAttributes;
  end;

implementation

{---------------------}
{ Classe TBPLUnitFile }
{---------------------}

const {don't localize}
  BPLMIMEType = 'application/bpl';
  LoadComponentsProc = 'LoadComponents';
  UnloadComponentsProc = 'UnloadComponents';
  RegisterComponentsProc = 'RegisterComponents';
  GetParamsProc = 'GetParams';

{*
  Cr�e une instance de TBPLUnitFile
  @param AMasterFile   Fichier ma�tre
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Param�tres envoy�s � l'unit�
*}
constructor TBPLUnitFile.Create(AMasterFile : TMasterFile; const AHRef : string;
  const AFileName : TFileName; const AMIMEType : string; Params : TStrings);
begin
  inherited;

  FAttributes := TStringList.Create;
  FAttributes.Assign(Params);

  FHandle := LoadPackage(FileName);
  if FHandle = 0 then
    raise EFileError.CreateFmt(sCantLoadPackage, [FileName]);
end;

{*
  D�truit l'instance
*}
destructor TBPLUnitFile.Destroy;
begin
  if FHandle <> 0 then
    UnloadPackage(FHandle);

  FAttributes.Free;

  inherited;
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TBPLUnitFile.AfterConstruction;
type
  TLoadComponentsProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster;
    Params : TStrings); stdcall;
var LoadComponents : TLoadComponentsProc;
    Params : TStrings;
begin
  inherited;

  LoadComponents := TLoadComponentsProc(
    GetProcAddress(FHandle, LoadComponentsProc));

  if Assigned(LoadComponents) then
  begin
    Params := TStringList.Create;
    try
      Params.Assign(FAttributes);
      FAttributes.Clear;

      LoadComponents(Self, Master, Params);
    finally
      Params.Free;
    end;
  end else FAttributes.Clear;
end;

{*
  Ex�cut� avant la destruction de l'objet
  BeforeDestruction est appel� avant l'ex�cution du premier destructeur.
  N'appelez pas directement BeforeDestruction.
*}
procedure TBPLUnitFile.BeforeDestruction;
type
  TUnloadComponentsProc = procedure(UnitFile : TBPLUnitFile;
    Master : TMaster); stdcall;
var UnloadComponents : TUnloadComponentsProc;
begin
  UnloadComponents := TUnloadComponentsProc(
    GetProcAddress(FHandle, UnloadComponentsProc));

  if Assigned(UnloadComponents) then
    UnloadComponents(Self, Master);

  inherited;
end;

{*
  Enregistre les diff�rents composants � placer dans la palette d'�dition
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure TBPLUnitFile.RegisterComponents(
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);
type
  TRegisterComponentsProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster;
    RegisterSingleComponentProc : TRegisterSingleComponentProc;
    RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;
var RegisterComponents : TRegisterComponentsProc;
begin
  RegisterComponents := TRegisterComponentsProc(
    GetProcAddress(FHandle, RegisterComponentsProc));

  if Assigned(RegisterComponents) then
    RegisterComponents(Self, Master,
      RegisterSingleComponentProc, RegisterComponentSetProc);
end;

{*
  Dresse la liste des param�tres � enregistrer
  @param Params   Liste des param�tres
*}
procedure TBPLUnitFile.GetParams(Params : TStrings);
type
  TGetParamsProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster;
    Params : TStrings); stdcall;
var GetParams : TGetParamsProc;
begin
  GetParams := TGetParamsProc(GetProcAddress(FHandle, GetParamsProc));

  if Assigned(GetParams) then
    GetParams(Self, Master, Params);
end;

initialization
  TMasterFile.RegisterUnitFileClass(BPLMIMEType, TBPLUnitFile);
end.

