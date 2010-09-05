unit EditFilers;

interface

uses
  Types, SysUtils, Classes, TypInfo, FunLabyUtils, FunLabyFilers;

type
  {*
    Pseudo-filer servant à purger les références à un composant donné
    @author sjrd
    @version 5.0
  *}
  TFunLabyPurgeRefFiler = class(TFunLabyWriter)
  private
    FReference: TFunLabyComponent; /// Référence à supprimer

    procedure HandleSubInstance(SubInstance: TFunLabyPersistent);
  protected
    procedure HandleProperty(PropInfo: PPropInfo; HasData: Boolean); override;

    procedure HandlePersistent(const Name: string;
      SubInstance: TFunLabyPersistent); override;

    procedure HandleCollection(const Name: string;
      Collection: TFunLabyCollection); override;

    procedure HandleComponent(const Name: string;
      Component: TFunLabyComponent); override;

    procedure HandleStrings(const Name: string; Strings: TStrings;
      ObjectType: PTypeInfo; HasData: Boolean); override;

    procedure HandleBinaryProperty(const Name: string;
      ReadProc, WriteProc: TStreamProc; HasData: Boolean); override;
  public
    constructor Create(AInstance: TFunLabyPersistent;
      AReference: TFunLabyComponent; AOwner: TFunLabyPurgeRefFiler = nil);

    class procedure PurgeReferences(AInstance: TFunLabyPersistent;
      AReference: TFunLabyComponent);

    property Reference: TFunLabyComponent read FReference;
  end;

implementation

{-----------------------------}
{ TFunLabyPurgeRefFiler class }
{-----------------------------}

{*
  Crée le pseudo-filer
  @param AInstance    Instance à traiter
  @param AReference   Référence à supprimer
  @param AOwner       Filer propriétaire
*}
constructor TFunLabyPurgeRefFiler.Create(AInstance: TFunLabyPersistent;
  AReference: TFunLabyComponent; AOwner: TFunLabyPurgeRefFiler = nil);
begin
  inherited Create(AInstance, AOwner);

  FReference := AReference;
end;

{*
  Traite une sous-instance
  @param SubInstance   Sous-instance à traiter
*}
procedure TFunLabyPurgeRefFiler.HandleSubInstance(
  SubInstance: TFunLabyPersistent);
var
  SubFiler: TFunLabyPurgeRefFiler;
begin
  SubFiler := TFunLabyPurgeRefFiler.Create(SubInstance, Reference, Self);
  try
    SubFiler.EnumProperties;
  finally
    SubFiler.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleProperty(PropInfo: PPropInfo;
  HasData: Boolean);
var
  PropType: PTypeInfo;
  PropValue: TFunLabyComponent;
begin
  PropType := PropInfo.PropType^;

  if (PropType.Kind = tkClass) and
    GetTypeData(PropType).ClassType.InheritsFrom(TFunLabyComponent) then
  begin
    PropValue := TFunLabyComponent(GetOrdProp(Instance, PropInfo));

    if PropValue = Reference then
      SetOrdProp(Instance, PropInfo, 0);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandlePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
begin
  HandleSubInstance(SubInstance);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleCollection(const Name: string;
  Collection: TFunLabyCollection);
begin
  HandleSubInstance(Collection);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleComponent(const Name: string;
  Component: TFunLabyComponent);
begin
  HandleSubInstance(Component);
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleStrings(const Name: string;
  Strings: TStrings; ObjectType: PTypeInfo; HasData: Boolean);
begin
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPurgeRefFiler.HandleBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean);
begin
end;

{*
  Élimine les références à un composant dans un objet persistant
  @param AInstance    Objet persistant à traiter
  @param AReference   Référence à éliminer
*}
class procedure TFunLabyPurgeRefFiler.PurgeReferences(
  AInstance: TFunLabyPersistent; AReference: TFunLabyComponent);
begin
  with Create(AInstance, AReference) do
  try
    EnumProperties;
  finally
    Free;
  end;
end;

end.

