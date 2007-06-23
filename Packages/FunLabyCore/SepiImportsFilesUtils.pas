{*
  Importe l'unité FilesUtils dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsFilesUtils;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, SysUtils, Contnrs, FunLabyUtils, FilesUtils;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEFileError = class(EFileError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTDependantFile = class(TDependantFile)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTUnitFile = class(TUnitFile)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMapFile = class(TMapFile)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMasterFile = class(TMasterFile)
  private
    function GetUnitFileCount: integer;
    function GetUnitFiles(Index : integer) : TUnitFile;
    function GetMapFileCount: integer;
    function GetMapFiles(Index : integer) : TMapFile;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{-------------------}
{ EFileError import }
{-------------------}

class function TSepiImportsEFileError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EFileError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------}
{ TDependantFile import }
{-----------------------}

class function TSepiImportsTDependantFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDependantFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMasterFile', System.TypeInfo(TMasterFile));
    AddField('FHRef', System.TypeInfo(string));
    AddField('FFileName', System.TypeInfo(TFileName));
    AddField('FMaster', System.TypeInfo(TMaster));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDependantFile.Create,
      'constructor(AMasterFile : TMasterFile; const AHRef : string; const AFileName : TFileName )');

    AddProperty('MasterFile', 'property: TMasterFile',
      'FMasterFile', '');
    AddProperty('HRef', 'property: string',
      'FHRef', '');
    AddProperty('FileName', 'property: TFileName',
      'FFileName', '');
    AddProperty('Master', 'property: TMaster',
      'FMaster', '');

    Complete;
  end;
end;

{------------------}
{ TUnitFile import }
{------------------}

class function TSepiImportsTUnitFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TUnitFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMIMEType', System.TypeInfo(string));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTUnitFile.Create,
      'constructor(AMasterFile : TMasterFile; const AHRef : string; const AFileName : TFileName ; const AMIMEType : string ; Params : TStrings )',
      mlkVirtual);
    AddMethod('AfterConstruction', @TSepiImportsTUnitFile.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('Loaded', @TSepiImportsTUnitFile.Loaded,
      'procedure',
      mlkVirtual);
    AddMethod('Unloading', @TSepiImportsTUnitFile.Unloading,
      'procedure',
      mlkVirtual);
    AddMethod('GameStarted', @TSepiImportsTUnitFile.GameStarted,
      'procedure',
      mlkVirtual);
    AddMethod('GameEnded', @TSepiImportsTUnitFile.GameEnded,
      'procedure',
      mlkVirtual);
    AddMethod('RegisterComponents', @TSepiImportsTUnitFile.RegisterComponents,
      'procedure( RegisterSingleComponentProc : TRegisterSingleComponentProc ; RegisterComponentSetProc : TRegisterComponentSetProc )',
      mlkVirtual);
    AddMethod('GetParams', @TSepiImportsTUnitFile.GetParams,
      'procedure(Params : TStrings)',
      mlkVirtual);

    AddProperty('MIMEType', 'property: string',
      'FMIMEType', '');

    Complete;
  end;
end;

{-----------------}
{ TMapFile import }
{-----------------}

class function TSepiImportsTMapFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TMapFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMapID', System.TypeInfo(TComponentID));
    AddField('FMap', System.TypeInfo(TMap));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMapFile.Create,
      'constructor(AMasterFile : TMasterFile; const AHRef : string; const AFileName : TFileName ; const AMapID : TComponentID )');
    AddMethod('CreateNew', @TSepiImportsTMapFile.CreateNew,
      'constructor(AMasterFile : TMasterFile; const AMapID : TComponentID ; const ADimensions : T3DPoint ; AZoneWidth, AZoneHeight : integer )');
    AddMethod('AfterConstruction', @TSepiImportsTMapFile.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('Save', @TSepiImportsTMapFile.Save,
      'procedure(const AHRef : string = ''''; const AFileName : TFileName = '''')');

    AddProperty('MapID', 'property: TComponentID',
      'FMapID', '');
    AddProperty('Map', 'property: TMap',
      'FMap', '');

    Complete;
  end;
end;

{--------------------}
{ TMasterFile import }
{--------------------}

function TSepiImportsTMasterFile.GetUnitFileCount: integer;
begin
  Result := UnitFileCount;
end;

function TSepiImportsTMasterFile.GetUnitFiles(Index : integer) : TUnitFile;
begin
  Result := UnitFiles[Index];
end;

function TSepiImportsTMasterFile.GetMapFileCount: integer;
begin
  Result := MapFileCount;
end;

function TSepiImportsTMasterFile.GetMapFiles(Index : integer) : TMapFile;
begin
  Result := MapFiles[Index];
end;

class function TSepiImportsTMasterFile.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TMasterFile'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TMasterFile));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FFileName', System.TypeInfo(TFileName));
    AddField('FMode', System.TypeInfo(TFileMode));
    AddField('FVersion', System.TypeInfo(string));
    AddField('FTitle', System.TypeInfo(string));
    AddField('FDescription', System.TypeInfo(string));
    AddField('FDifficulty', System.TypeInfo(string));
    AddField('FAuthorID', System.TypeInfo(integer));
    AddField('FAuthor', System.TypeInfo(string));
    AddField('FAllowEdit', System.TypeInfo(boolean));
    AddField('FIsSaveguard', System.TypeInfo(boolean));
    AddField('FMaster', System.TypeInfo(TMaster));
    AddField('FUnitFiles', System.TypeInfo(TObjectList));
    AddField('FMapFiles', System.TypeInfo(TObjectList));

    AddMethod('InvalidFormat', nil,
      'procedure');
    AddMethod('Load', nil,
      'procedure(ADocument : IInterface)');
    AddMethod('TestOpeningValidity', nil,
      'procedure');
    AddMethod('GetUnitFileCount', @TSepiImportsTMasterFile.GetUnitFileCount,
      'function: integer');
    AddMethod('GetUnitFiles', @TSepiImportsTMasterFile.GetUnitFiles,
      'function(Index : integer) : TUnitFile');
    AddMethod('GetMapFileCount', @TSepiImportsTMasterFile.GetMapFileCount,
      'function: integer');
    AddMethod('GetMapFiles', @TSepiImportsTMasterFile.GetMapFiles,
      'function(Index : integer) : TMapFile');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMasterFile.Create,
      'constructor(const AFileName : TFileName; AMode : TFileMode)');
    AddMethod('CreateNew', @TSepiImportsTMasterFile.CreateNew,
      'constructor(FileContents : TStrings = nil)');
    AddMethod('Destroy', @TSepiImportsTMasterFile.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AfterConstruction', @TSepiImportsTMasterFile.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('BeforeDestruction', @TSepiImportsTMasterFile.BeforeDestruction,
      'procedure',
      mlkOverride);
    AddMethod('RegisterUnitFileClass', @TSepiImportsTMasterFile.RegisterUnitFileClass,
      'class procedure(const MIMEType : string; UnitFileClass : TUnitFileClass )');
    AddMethod('UnregisterUnitFileClass', @TSepiImportsTMasterFile.UnregisterUnitFileClass,
      'class procedure(const MIMEType : string; UnitFileClass : TUnitFileClass )');
    AddMethod('FindUnitFileClass', @TSepiImportsTMasterFile.FindUnitFileClass,
      'class function(const MIMEType : string) : TUnitFileClass');
    AddMethod('ResolveHRef', @TSepiImportsTMasterFile.ResolveHRef,
      'function(const HRef, DefaultDir : string) : TFileName');
    AddMethod('AddUnitFile', @TSepiImportsTMasterFile.AddUnitFile,
      'function(const MIMEType, HRef : string; Params : TStrings = nil ) : TUnitFile');
    AddMethod('AddMapFile', @TSepiImportsTMasterFile.AddMapFile,
      'function(const ID : TComponentID; const HRef : string; MaxViewSize : integer = 1 ) : TMapFile');
    AddMethod('AddNewMapFile', @TSepiImportsTMasterFile.AddNewMapFile,
      'function(const ID : TComponentID; const Dimensions : T3DPoint; ZoneWidth, ZoneHeight : integer ; MaxViewSize : integer = 1 ) : TMapFile');
    AddMethod('GameStarted', @TSepiImportsTMasterFile.GameStarted,
      'procedure');
    AddMethod('GameEnded', @TSepiImportsTMasterFile.GameEnded,
      'procedure');
    AddMethod('RegisterComponents', @TSepiImportsTMasterFile.RegisterComponents,
      'procedure( RegisterSingleComponentProc : TRegisterSingleComponentProc ; RegisterComponentSetProc : TRegisterComponentSetProc )');
    AddMethod('Save', @TSepiImportsTMasterFile.Save,
      'procedure(AFileName : TFileName = '''')');

    AddProperty('FileName', 'property: TFileName',
      'FFileName', '');
    AddProperty('Mode', 'property: TFileMode',
      'FMode', '');
    AddProperty('Version', 'property: string',
      'FVersion', '');
    AddProperty('Title', 'property: string',
      'FTitle', 'FTitle');
    AddProperty('Description', 'property: string',
      'FDescription', 'FDescription');
    AddProperty('Difficulty', 'property: string',
      'FDifficulty', 'FDifficulty');
    AddProperty('AuthorID', 'property: integer',
      'FAuthorID', 'FAuthorID');
    AddProperty('Author', 'property: string',
      'FAuthor', 'FAuthor');
    AddProperty('AllowEdit', 'property: boolean',
      'FAllowEdit', '');
    AddProperty('IsSaveguard', 'property: boolean',
      'FIsSaveguard', '');
    AddProperty('Master', 'property: TMaster',
      'FMaster', '');
    AddProperty('UnitFileCount', 'property: integer',
      'GetUnitFileCount', '');
    AddProperty('UnitFiles', 'property[index : integer] : TUnitFile',
      'GetUnitFiles', '');
    AddProperty('MapFileCount', 'property: integer',
      'GetMapFileCount', '');
    AddProperty('MapFiles', 'property[index : integer] : TMapFile',
      'GetMapFiles', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'FilesUtils',
    ['SysUtils', 'Classes', 'Contnrs', 'ScUtils', 'FunLabyUtils']);

  // Constants
  TSepiConstant.Create(Result, 'sInvalidFileFormat', sInvalidFileFormat);
  TSepiConstant.Create(Result, 'sVersionTooHigh', sVersionTooHigh);
  TSepiConstant.Create(Result, 'sFileNotFound', sFileNotFound);
  TSepiConstant.Create(Result, 'sUnknownUnitType', sUnknownUnitType);
  TSepiConstant.Create(Result, 'sThereMustBeOnePlayer', sThereMustBeOnePlayer);
  TSepiConstant.Create(Result, 'sEditingNotAllowed', sEditingNotAllowed);
  TSepiConstant.Create(Result, 'sCantEditSaveguard', sCantEditSaveguard);
  TSepiConstant.Create(Result, 'sNoFileName', sNoFileName);
  TSepiConstant.Create(Result, 'sTemporaryStatedMap', sTemporaryStatedMap);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TFileMode));
  TSepiImportsEFileError.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TMasterFile));
  TSepiImportsTDependantFile.SepiImport(Result);
  TSepiImportsTUnitFile.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TUnitFileClass', TypeInfo(TUnitFile), True);
  TSepiImportsTMapFile.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TMapFileClass', TypeInfo(TMapFile), True);
  TSepiImportsTMasterFile.SepiImport(Result);

  // Constants
  TSepiConstant.Create(Result, 'HRefDelim', HRefDelim);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('FilesUtils', ImportUnit);
end.

