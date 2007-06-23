{*
  Importe l'unité Generics dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsGenerics;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, FunLabyUtils, Generics;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTDecorativeEffect = class(TDecorativeEffect)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTObjectTool = class(TObjectTool)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTOverriddenScrew = class(TOverriddenScrew)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{--------------------------}
{ TDecorativeEffect import }
{--------------------------}

class function TSepiImportsTDecorativeEffect.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TDecorativeEffect));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTDecorativeEffect.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; const AName, AImgName : string )');

    Complete;
  end;
end;

{--------------------}
{ TObjectTool import }
{--------------------}

class function TSepiImportsTObjectTool.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TObjectTool));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FObjectDef', System.TypeInfo(TObjectDef));
    AddField('FFindMessage', System.TypeInfo(string));

    CurrentVisibility := mvProtected;

    AddMethod('DoDraw', @TSepiImportsTObjectTool.DoDraw,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTObjectTool.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; AObjectDef : TObjectDef ; const AFindMessage : string ; const AName : string = '''' ; const AImgName : string = '''' )');
    AddMethod('Find', @TSepiImportsTObjectTool.Find,
      'procedure(Player : TPlayer; const Pos : T3DPoint)',
      mlkOverride);

    AddProperty('ObjectDef', 'property: TObjectDef',
      'FObjectDef', '');
    AddProperty('FindMessage', 'property: string',
      'FFindMessage', '');

    Complete;
  end;
end;

{-------------------------}
{ TOverriddenScrew import }
{-------------------------}

class function TSepiImportsTOverriddenScrew.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TOverriddenScrew));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMap', System.TypeInfo(TMap));
    AddField('FPosition', 'T3DPoint');
    AddField('FOriginalScrew', System.TypeInfo(TScrew));

    CurrentVisibility := mvProtected;

    AddMethod('DoDraw', @TSepiImportsTOverriddenScrew.DoDraw,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTOverriddenScrew.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; AMap : TMap ; const APosition : T3DPoint ; AField : TField = nil ; AEffect : TEffect = nil ; ATool : TTool = nil ; AObstacle : TObstacle = nil )');
    AddMethod('Destroy', @TSepiImportsTOverriddenScrew.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('Map', 'property: TMap',
      'FMap', '');
    AddProperty('Position', 'property: T3DPoint',
      'FPosition', '');
    AddProperty('OriginalScrew', 'property: TScrew',
      'FOriginalScrew', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'Generics',
    ['Graphics', 'ScUtils', 'FunLabyUtils']);

  // Types
  TSepiImportsTDecorativeEffect.SepiImport(Result);
  TSepiImportsTObjectTool.SepiImport(Result);
  TSepiImportsTOverriddenScrew.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Generics', ImportUnit);
end.

