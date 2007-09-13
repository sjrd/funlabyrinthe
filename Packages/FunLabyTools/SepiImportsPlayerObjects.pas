{*
  Importe l'unité PlayerObjects dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsPlayerObjects;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Controls, StdCtrls, ComCtrls, PlayerObjects;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTFormObjects = class(TFormObjects)
  private
    class function SepiImport(Owner: TSepiMetaUnit): TSepiClass;
  end;

{---------------------}
{ TFormObjects import }
{---------------------}

class function TSepiImportsTFormObjects.SepiImport(
  Owner: TSepiMetaUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFormObjects));

  with Result do
  begin
    AddField('LabelObjects', System.TypeInfo(TLabel));
    AddField('ListViewObjects', System.TypeInfo(TListView));
    AddField('ButtonOK', System.TypeInfo(TButton));
    AddField('ObjectsImages', System.TypeInfo(TImageList));

    CurrentVisibility := mvPrivate;
    CurrentVisibility := mvPublic;

    AddMethod('ShowObjects', @TSepiImportsTFormObjects.ShowObjects,
      'class procedure(Player : TPlayer)');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiMetaRoot): TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'PlayerObjects',
    ['Windows', 'Messages', 'SysUtils', 'Variants', 'Classes', 'Graphics',
    'Controls', 'Forms', 'Dialogs', 'ImgList', 'StdCtrls', 'ComCtrls',
    'FunLabyUtils']);

  // Types
  TSepiImportsTFormObjects.SepiImport(Result);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('PlayerObjects', ImportUnit);
end.

