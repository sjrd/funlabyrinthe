{*
  Utilitaires pour la compilation d'une unit� FunDelphi dans Sepi
  @author sjrd
  @version 5.0
*}
unit SepiFunDelphiCompilerUtils;

interface

uses
  Windows, SysUtils, StrUtils, TypInfo, ScUtils, SepiReflectionCore,
  SepiOrdTypes, SepiStrTypes, SepiMembers, SepiSystemUnit, SepiCompiler,
  SepiExpressions, SepiInstructions, SepiCompilerConsts,
  SepiDelphiLikeCompilerUtils;

type
  {*
    Valeur composant FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TSepiComponentValue = class(TSepiCustomComputedValue,
    ISepiTypeForceableValue)
  private
    FMasterExpr: ISepiExpression; /// Valeur Master
    FIDConstant: TSepiConstant;   /// Constante repr�sentant l'ID du composant
    FFunLabyUtilsUnit: TSepiUnit; /// Unit� FunLabyUtils
  protected
    function CanForceType(AValueType: TSepiType;
      Explicit: Boolean = False): Boolean;
    procedure ForceType(AValueType: TSepiType);

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;

    property MasterExpr: ISepiExpression read FMasterExpr;
    property IDConstant: TSepiConstant read FIDConstant;
    property FunLabyUtilsUnit: TSepiUnit read FFunLabyUtilsUnit;
  public
    constructor Create(const AMasterExpr: ISepiExpression;
      AIDConstant: TSepiConstant);
  end;

function UnitResolveIdent(UnitCompiler: TSepiUnitCompiler;
  const Identifier: string): ISepiExpression;
function MethodResolveIdent(Compiler: TSepiMethodCompiler;
  const Identifier: string): ISepiExpression;

function FieldSelection(SepiContext: TSepiMeta;
  const BaseExpression: ISepiExpression;
  const FieldName: string): ISepiExpression;

const {don't localize}
  FunLabyUtilsUnitName = 'FunLabyUtils';
  ComponentIDPrefix = 'id';
  ActionPrefix = 'act';
  AttributePrefix = 'attr';

  MasterName = 'Master';
  CreateName = 'Create';
  AbleToName = 'AbleTo';
  UseForName = 'UseFor';
  ActionName = 'Action';

implementation

const {don't localize}
  ComponentClassNames: array[0..10] of string = (
    'TFunLabyComponent', 'TSquareComponent', 'TPlugin', 'TObjectDef',
    'TField', 'TEffect', 'TTool', 'TObstacle', 'TSquare', 'TMap', 'TPlayer'
  );

  MasterPropNames: array[0..10] of string = (
    'Component', 'SquareComponent', 'Plugin', 'ObjectDef',
    'Field', 'Effect', 'Tool', 'Obstacle', 'Square', 'Map', 'Player'
  );

{-----------------}
{ Global routines }
{-----------------}

{*
  Recherche une constante cha�ne
  @param Context   Contexte
  @param Name      Nom de la constante
  @return Constante cha�ne recherch�e, ou nil si non trouv�e
*}
function LookForStringConstant(Context: TSepiMeta;
  const Name: string): TSepiConstant;
var
  Meta: TSepiMeta;
begin
  Result := nil;

  Meta := Context.LookFor(Name);
  if not (Meta is TSepiConstant) then
    Exit;

  Result := TSepiConstant(Meta);
  if not Result.ConstType.Equals(
    (Result.Root.SystemUnit as TSepiSystemUnit).LongString) then
    Result := nil;
end;

{*
  R�soud un identificateur de composant
  @param SepiContext   Contexte
  @param Identifier    Identificateur recherch�
  @param Expression    Expression � construire
  @return True en cas de succ�s, False sinon
*}
function ResolveComponentID(SepiContext: TSepiMeta; const Identifier: string;
  const Expression: ISepiExpression): Boolean;
var
  Constant: TSepiConstant;
  MasterExpr: ISepiExpression;
begin
  Result := False;

  Constant := LookForStringConstant(SepiContext, ComponentIDPrefix+Identifier);
  if Constant = nil then
    Exit;

  MasterExpr := MethodResolveIdent(Expression.MethodCompiler, MasterName);
  if MasterExpr = nil then
    Exit;

  ISepiExpressionPart(TSepiComponentValue.Create(MasterExpr,
    Constant)).AttachToExpression(Expression);
  Result := True;
end;

{*
  R�soud une action
  @param SepiContext   Contexte
  @param Identifier    Identificateur recherch�
  @param Expression    Expression � construire
  @return True en cas de succ�s, False sinon
*}
function ResolveAction(SepiContext: TSepiMeta; const Identifier: string;
  const Expression: ISepiExpression): Boolean;
var
  Constant: TSepiConstant;
begin
  Result := False;

  Constant := LookForStringConstant(SepiContext, ActionPrefix+Identifier);
  if Constant = nil then
    Exit;

  ISepiExpressionPart(TSepiTrueConstValue.Create(
    Constant)).AttachToExpression(Expression);
  Result := True;
end;

{*
  R�soud un identificateur dans le contexte d'une unit�
  @param UnitCompiler   Compilateur d'unit�
  @param Identifier     Identificateur recherch�
  @return Expression repr�sentant l'identificateur, ou nil si non trouv�
*}
function UnitResolveIdent(UnitCompiler: TSepiUnitCompiler;
  const Identifier: string): ISepiExpression;
begin
  // Standart Delphi expression
  Result := SepiDelphiLikeCompilerUtils.UnitResolveIdent(UnitCompiler,
    Identifier);
  if Result <> nil then
    Exit;

  // Special FunDelphi expression
  Result := TSepiExpression.Create(UnitCompiler);
  if ResolveAction(UnitCompiler.SepiUnit, Identifier, Result) then
    Exit;

  Result := nil;
end;

{*
  R�soud un identificateur dans le contexte d'une m�thode
  @param Compiler     Compilateur de m�thode
  @param Identifier   Identificateur recherch�
  @return Expression repr�sentant l'identificateur, ou nil si non trouv�
*}
function MethodResolveIdent(Compiler: TSepiMethodCompiler;
  const Identifier: string): ISepiExpression;
begin
  // Standart Delphi expression
  Result := SepiDelphiLikeCompilerUtils.MethodResolveIdent(Compiler,
    Identifier);
  if Result <> nil then
    Exit;

  // Special FunDelphi expression
  Result := TSepiExpression.Create(Compiler);
  if ResolveComponentID(Compiler.SepiMethod, Identifier, Result) then
    Exit;
  if ResolveAction(Compiler.SepiMethod, Identifier, Result) then
    Exit;

  Result := nil;
end;

{*
  S�lection d'un attribut d'une expression selon les r�gles du FunDelphi
  @param SepiContext      Contexte Sepi depuis lequel chercher
  @param BaseExpression   Expression de base
  @param AttrName         Nom de l'attribut
  @return Expression repr�sentant l'attribut s�lectionn� (ou nil si inexistant)
*}
function AttributeSelection(SepiContext: TSepiMeta;
  const BaseExpression: ISepiExpression;
  const AttrName: string): ISepiExpression;
var
  Constant: TSepiConstant;
  Prop: ISepiProperty;
  Expression: ISepiExpression;
begin
  Constant := LookForStringConstant(SepiContext, AttributePrefix+AttrName);
  if Constant = nil then
    Exit;

  Expression := SepiDelphiLikeCompilerUtils.FieldSelection(SepiContext,
    BaseExpression, 'Attribute');

  if not Supports(Expression, ISepiProperty, Prop) then
    Exit;
  if (Prop.ParamsCompleted) or (Prop.ParamCount <> 1) then
    Exit;

  Expression := TSepiExpression.Create(Expression);
  ISepiExpressionPart(TSepiTrueConstValue.Create(
    Constant)).AttachToExpression(Expression);

  Prop.Params[0] := Expression;
  Prop.CompleteParams;

  Result := Prop as ISepiExpression;
end;

{*
  S�lection de champ d'une expression selon les r�gles du FunDelphi
  @param SepiContext      Contexte Sepi depuis lequel chercher
  @param BaseExpression   Expression de base
  @param FieldName        Nom du champ
  @return Expression repr�sentant le champ s�lectionn� (ou nil si inexistant)
*}
function FieldSelection(SepiContext: TSepiMeta;
  const BaseExpression: ISepiExpression;
  const FieldName: string): ISepiExpression;
begin
  // Standart Delphi field
  Result := SepiDelphiLikeCompilerUtils.FieldSelection(SepiContext,
    BaseExpression, FieldName);
  if Result <> nil then
    Exit;

  // Player attribute
  Result := AttributeSelection(SepiContext, BaseExpression, FieldName);
end;

{---------------------------}
{ TSepiComponentValue class }
{---------------------------}

{*
  Cr�e une valeur composant
  @param AMasterExpr   Expression Master
  @param AIDConstant   Constante repr�sentant l'ID du composant
*}
constructor TSepiComponentValue.Create(const AMasterExpr: ISepiExpression;
  AIDConstant: TSepiConstant);
begin
  inherited Create;

  FMasterExpr := AMasterExpr;
  FIDConstant := AIDConstant;
  FFunLabyUtilsUnit := FIDConstant.Root.FindMeta(
    FunLabyUtilsUnitName) as TSepiUnit;

  SetValueType(FunLabyUtilsUnit.FindMeta('TFunLabyComponent') as TSepiType);
end;

{*
  [@inheritDoc]
*}
function TSepiComponentValue.CanForceType(AValueType: TSepiType;
  Explicit: Boolean = False): Boolean;
begin
  Result := (AValueType is TSepiClass) and
    TSepiClass(AValueType).ClassInheritsFrom(ValueType as TSepiClass) and
    (AValueType.Owner = FunLabyUtilsUnit) and
    AnsiMatchStr(AValueType.Name, ComponentClassNames);
end;

{*
  [@inheritDoc]
*}
procedure TSepiComponentValue.ForceType(AValueType: TSepiType);
begin
  Assert(CanForceType(AValueType));

  SetValueType(AValueType);
end;

{*
  [@inheritDoc]
*}
procedure TSepiComponentValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  Index: Integer;
  PropName: string;
  Expression: ISepiExpression;
  Prop: ISepiProperty;
begin
  Index := AnsiIndexStr(ValueType.Name, ComponentClassNames);
  Assert(Index >= 0);
  PropName := MasterPropNames[Index];

  Expression := SepiDelphiLikeCompilerUtils.FieldSelection(
    Compiler.SepiMethod, MasterExpr, PropName);
  Prop := Expression as ISepiProperty;

  Expression := TSepiExpression.Create(Expression);
  ISepiExpressionPart(TSepiTrueConstValue.Create(
    IDConstant)).AttachToExpression(Expression);

  Assert(Prop.ParamCount = 1);
  Prop.Params[0] := Expression;
  Prop.CompleteParams;

  (Prop as ISepiReadableValue).CompileRead(Compiler, Instructions,
    Destination, TempVars);
end;

end.

