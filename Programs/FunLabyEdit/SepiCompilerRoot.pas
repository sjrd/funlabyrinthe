unit SepiCompilerRoot;

interface

uses
  Windows, Classes, SepiReflectionCore, SepiReflectionConsts;

type
  {*
    Fork d'une racine Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiRootFork = class(TSepiRoot)
  private
    FOriginalRoot: TSepiRoot;    /// Racine de base
    FForeignRefCounts: TStrings; /// Ref-counts des unités étrangères
    FDestroying: Boolean;        /// True lorsqu'en destruction

    procedure IncForeignRefCount(SepiUnit: TSepiUnit);
    procedure DecForeignRefCount(SepiUnit: TSepiUnit);
  protected
    procedure AddChild(Child: TSepiMeta); override;
    procedure RemoveChild(Child: TSepiMeta); override;
  public
    constructor Create(AOriginalRoot: TSepiRoot);
    destructor Destroy; override;

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
  FForeignRefCounts := TStringList.Create;

  inherited Create;
end;

{*
  [@inheritDoc]
*}
destructor TSepiRootFork.Destroy;
begin
  inherited;

  FForeignRefCounts.Free;
end;

{*
  Incrémente le ref-count d'une unité étrangère
  @param SepiUnit   Unité concernée
*}
procedure TSepiRootFork.IncForeignRefCount(SepiUnit: TSepiUnit);
var
  Index: Integer;
begin
  with FForeignRefCounts do
  begin
    Index := IndexOf(SepiUnit.Name);
    Assert(Index >= 0);
    Objects[Index] := TObject(Integer(Objects[Index]) + 1);
  end;
end;

{*
  Décrémente le ref-count d'une unité étrangère
  @param SepiUnit   Unité concernée
*}
procedure TSepiRootFork.DecForeignRefCount(SepiUnit: TSepiUnit);
var
  Index, RefCount: Integer;
begin
  with FForeignRefCounts do
  begin
    Index := IndexOf(SepiUnit.Name);
    Assert(Index >= 0);

    RefCount := Integer(Objects[Index]);
    Dec(RefCount);

    if RefCount > 0 then
      Objects[Index] := TObject(RefCount)
    else
    begin
      // Remove the unit
      ChildRemoving(SepiUnit);
      RemoveChild(SepiUnit);
      OriginalRoot.UnloadUnit(SepiUnit.Name);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.AddChild(Child: TSepiMeta);
begin
  inherited;

  FForeignRefCounts.Add(Child.Name);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.RemoveChild(Child: TSepiMeta);
begin
  FForeignRefCounts.Delete(FForeignRefCounts.IndexOf(Child.Name));

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.BeforeDestruction;
var
  I: Integer;
  SepiUnit: TSepiUnit;
begin
  FDestroying := True;

  // Mark my own units as destructing
  for I := UnitCount-1 downto 0 do
    if Units[I].Owner = Self then
      TSepiRootFork(Units[I]).Destroying;

  // Free units the way I want to
  for I := UnitCount-1 downto 0 do
  begin
    SepiUnit := Units[I];

    if SepiUnit.Owner = Self then
    begin
      OutputDebugString(PChar('Freeing own unit: '+SepiUnit.Name));
      SepiUnit.Free;
    end else
    begin
      OutputDebugString(PChar('Unloading foreign unit: '+SepiUnit.Name));
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
      Result := (OriginalRoot.GetMeta(UnitName) as TSepiUnit);

      if Result <> nil then
      begin
        OutputDebugString(PChar('Loading foreing unit: '+UnitName));
        OriginalRoot.LoadUnit(UnitName);
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
      Result := (OriginalRoot.GetMeta(UnitName) as TSepiUnit);

      if Result <> nil then
      begin
        OutputDebugString(PChar('Loading foreign unit: '+UnitName));
        OriginalRoot.LoadUnit(UnitName);
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
  begin
    OutputDebugString(PChar('Loading own unit: '+UnitName));
    inherited LoadUnit(UnitName);
  end else
    IncForeignRefCount(Result);
end;

{*
  [@inheritDoc]
*}
procedure TSepiRootFork.UnloadUnit(const UnitName: string);
var
  SepiUnit: TSepiUnit;
begin
  if FDestroying or (State = msDestroying) then
    Exit;

  SepiUnit := TSepiUnit(GetMeta(UnitName));
  if SepiUnit = nil then
    raise ESepiUnitNotFoundError.CreateFmt(SSepiUnitNotFound, [UnitName]);

  if SepiUnit.Owner = Self then
  begin
    OutputDebugString(PChar('Unloading own unit: '+UnitName));
    inherited UnloadUnit(UnitName);
  end else
    DecForeignRefCount(SepiUnit);
end;

end.

