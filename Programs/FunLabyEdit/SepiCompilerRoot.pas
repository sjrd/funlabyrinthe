unit SepiCompilerRoot;

interface

uses
  SepiReflectionCore, SepiReflectionConsts;

type
  {*
    Fork d'une racine Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiRootFork = class(TSepiRoot)
  private
    FOriginalRoot: TSepiRoot;
  public
    constructor Create(AOriginalRoot: TSepiRoot);

    procedure BeforeDestruction; override;

    function LoadUnit(const UnitName: string): TSepiUnit; override;
    procedure UnloadUnit(const UnitName: string); override;

    property OriginalRoot: TSepiRoot read FOriginalRoot;
  end;

implementation

{---------------------}
{ TSepiRootFork class }
{---------------------}

{*
  Crée un nouveau fork de racine Sepi
  @param AOriginalRoot   Racine Sepi originale
*}
constructor TSepiRootFork.Create(AOriginalRoot: TSepiRoot);
begin
  FOriginalRoot := AOriginalRoot;

  inherited Create;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.BeforeDestruction;
var
  I: Integer;
  SepiUnit: TSepiUnit;
begin
  for I := UnitCount-1 downto 0 do
  begin
    SepiUnit := Units[I];
    if SepiUnit.Owner <> Self then
    begin
      ChildRemoving(SepiUnit);
      RemoveChild(SepiUnit);

      OriginalRoot.UnloadUnit(SepiUnit.Name);
    end;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiRootFork.LoadUnit(const UnitName: string): TSepiUnit;
var
  ImportFunc: TSepiImportUnitFunc;
begin
  Result := TSepiUnit(GetMeta(UnitName));

  if Result = nil then
  begin
    ImportFunc := SepiImportedUnit(UnitName);

    // For imported units, check the original root directly
    if Assigned(ImportFunc) then
    begin
      Result := OriginalRoot.LoadUnit(UnitName);

      if Result <> nil then
      begin
        AddChild(Result);
        ChildAdded(Result);
      end else
        Result := ImportFunc(Self);
    end;

    // Otherwise, load the unit in this fork
    if (Result = nil) and Assigned(OnLoadUnit) then
      Result := OnLoadUnit(Self, UnitName);

    // If we didn't find anything, check the original root
    if Result = nil then
    begin
      Result := OriginalRoot.LoadUnit(UnitName);

      if Result <> nil then
      begin
        AddChild(Result);
        ChildAdded(Result);
      end;
    end;
  end;

  // Unit not found
  if Result = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  // Increment ref-count
  if Result.Owner = Self then
    inherited LoadUnit(UnitName);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.UnloadUnit(const UnitName: string);
var
  MetaUnit: TSepiUnit;
begin
  if State = msDestroying then
    Exit;

  MetaUnit := TSepiUnit(GetMeta(UnitName));
  if MetaUnit = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  if MetaUnit.Owner = Self then
    inherited UnloadUnit(UnitName);
end;

end.

