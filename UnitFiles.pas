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
  end;

implementation

///////////////////////////
/// Classe TBPLUnitFile ///
///////////////////////////

const {don't localize}
  BPLMIMEType = 'application/bpl';
  LoadComponentsProc = 'LoadComponents';

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

initialization
  TMasterFile.RegisterUnitFileClass(BPLMIMEType, TBPLUnitFile);
end.

