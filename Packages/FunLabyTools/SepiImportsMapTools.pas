{*
  Importe l'unité MapTools dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMapTools;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, FunLabyUtils, MapTools;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ChangeComp_0(Screw: TScrew; NewComp: TScrewComponent): TScrew;
begin
  Result := ChangeComp(Screw, NewComp);
end;

function ChangeComp_1(Screw: TScrew; const NewComp: TComponentID): TScrew;
begin
  Result := ChangeComp(Screw, NewComp);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'MapTools',
    ['ScUtils', 'FunLabyUtils']);

  // Types
  TSepiMethodRefType.Create(Result, 'TFindScrewProc',
    'procedure(Map : TMap; var Pos : T3DPoint; Component : TScrewComponent )');

  // Routines
  TSepiMethod.Create(Result, 'ChangeField', @ChangeField,
    'function(Screw : TScrew; const NewField : TComponentID = '''' ) : TScrew');
  TSepiMethod.Create(Result, 'ChangeEffect', @ChangeEffect,
    'function(Screw : TScrew; const NewEffect : TComponentID = '''' ) : TScrew');
  TSepiMethod.Create(Result, 'ChangeTool', @ChangeTool,
    'function(Screw : TScrew; const NewTool : TComponentID = '''' ) : TScrew');
  TSepiMethod.Create(Result, 'ChangeObstacle', @ChangeObstacle,
    'function(Screw : TScrew; const NewObstacle : TComponentID = '''' ) : TScrew');
  TSepiOverloadedMethod.Create(Result, 'ChangeComp');
  TSepiMethod.Create(Result, 'OL$ChangeComp$0', @ChangeComp_0,
    'function(Screw : TScrew; NewComp : TScrewComponent ) : TScrew');
  TSepiMethod.Create(Result, 'OL$ChangeComp$1', @ChangeComp_1,
    'function(Screw : TScrew; const NewComp : TComponentID ) : TScrew');
  TSepiMethod.Create(Result, 'FindNextScrew', @FindNextScrew,
    'procedure(Map : TMap; var Pos : T3DPoint; Component : TScrewComponent )');
  TSepiMethod.Create(Result, 'FindPreviousScrew', @FindPreviousScrew,
    'procedure(Map : TMap; var Pos : T3DPoint; Component : TScrewComponent )');
  TSepiMethod.Create(Result, 'FindScrewAtRandom', @FindScrewAtRandom,
    'procedure(Map : TMap; var Pos : T3DPoint; Component : TScrewComponent )');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('MapTools', ImportUnit);
end.

