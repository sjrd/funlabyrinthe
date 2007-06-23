{*
  Importe l'unité UnitFiles dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsUnitFiles;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Classes, UnitFiles;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTBPLUnitFile = class(TBPLUnitFile)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{---------------------}
{ TBPLUnitFile import }
{---------------------}

class function TSepiImportsTBPLUnitFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TBPLUnitFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FHandle', System.TypeInfo(HMODULE));
    AddField('FAttributes', System.TypeInfo(TStrings));

    AddMethod('Notify', nil,
      'procedure(const ProcName : string)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTBPLUnitFile.Create,
      'constructor(AMasterFile : TMasterFile; const AHRef : string; const AFileName : TFileName ; const AMIMEType : string ; Params : TStrings )',
      mlkOverride);
    AddMethod('Destroy', @TSepiImportsTBPLUnitFile.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTBPLUnitFile.Loaded,
      'procedure',
      mlkOverride);
    AddMethod('Unloading', @TSepiImportsTBPLUnitFile.Unloading,
      'procedure',
      mlkOverride);
    AddMethod('GameStarted', @TSepiImportsTBPLUnitFile.GameStarted,
      'procedure',
      mlkOverride);
    AddMethod('GameEnded', @TSepiImportsTBPLUnitFile.GameEnded,
      'procedure',
      mlkOverride);
    AddMethod('RegisterComponents', @TSepiImportsTBPLUnitFile.RegisterComponents,
      'procedure( RegisterSingleComponentProc : TRegisterSingleComponentProc ; RegisterComponentSetProc : TRegisterComponentSetProc )',
      mlkOverride);
    AddMethod('GetParams', @TSepiImportsTBPLUnitFile.GetParams,
      'procedure(Params : TStrings)',
      mlkOverride);

    AddProperty('Attributes', 'property: TStrings',
      'FAttributes', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'UnitFiles',
    ['SysUtils', 'Classes', 'FunLabyUtils', 'FilesUtils']);

  // Constants
  TSepiConstant.Create(Result, 'sCantLoadPackage', sCantLoadPackage);

  // Types
  TSepiImportsTBPLUnitFile.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('UnitFiles', ImportUnit);
end.

