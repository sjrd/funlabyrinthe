unit UnitFiles;

interface

uses
  Windows, SysUtils, Classes, FunLabyUtils, FilesUtils;

resourcestring
  sCantLoadPackage = 'Impossible de charger le paquet "%s"';

type
  TBPLUnitFile = class(TUnitFile)
  private
    FHandle : HMODULE; /// Module du package charg�
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      Params : TStrings); override;
    destructor Destroy; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc); override;

    procedure GetParams(Params : TStrings); override;
  end;

implementation

{---------------------}
{ Classe TBPLUnitFile }
{---------------------}

const {don't localize}
  BPLMIMEType = 'application/bpl';
  LoadComponentsProc = 'LoadComponents';
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
type
  TLoadComponentsProc = procedure(Master : TMaster; Params : TStrings); stdcall;
var LoadComponents : TLoadComponentsProc;
begin
  inherited;

  FHandle := LoadPackage(FileName);
  if FHandle = 0 then
    raise EFileError.CreateFmt(sCantLoadPackage, [FileName]);

  LoadComponents := TLoadComponentsProc(
    GetProcAddress(FHandle, LoadComponentsProc));
  if not Assigned(LoadComponents) then
    raise EFileError.CreateFmt(sCantLoadPackage, [FileName]);

  LoadComponents(Master, Params);
end;

{*
  D�truit l'instance
*}
destructor TBPLUnitFile.Destroy;
begin
  if FHandle <> 0 then
    UnloadPackage(FHandle);

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
  TRegisterComponentsProc = procedure(Master : TMaster;
    RegisterSingleComponentProc : TRegisterSingleComponentProc;
    RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;
var RegisterComponents : TRegisterComponentsProc;
begin
  RegisterComponents := TRegisterComponentsProc(
    GetProcAddress(FHandle, RegisterComponentsProc));

  if Assigned(RegisterComponents) then
    RegisterComponents(Master,
      RegisterSingleComponentProc, RegisterComponentSetProc);
end;

{*
  Dresse la liste des param�tres � enregistrer
  @param Params   Liste des param�tres
*}
procedure TBPLUnitFile.GetParams(Params : TStrings);
type
  TGetParamsProc = procedure(Master : TMaster; Params : TStrings); stdcall;
var GetParams : TGetParamsProc;
begin
  GetParams := TGetParamsProc(GetProcAddress(FHandle, GetParamsProc));

  if Assigned(GetParams) then
    GetParams(Master, Params);
end;

initialization
  TMasterFile.RegisterUnitFileClass(BPLMIMEType, TBPLUnitFile);
end.

