{*
  Utilitaires pour la compilation d'une unité FunDelphi dans Sepi
  @author sjrd
  @version 5.0
*}
unit SepiFunDelphiCompilerUtils;

interface

uses
  Windows, SysUtils, Classes, StrUtils, TypInfo, ScUtils, FunLabyUtils,
  SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes, SepiMembers,
  SepiSystemUnit,
  SepiOpCodes, SepiCompiler, SepiExpressions, SepiInstructions,
  SepiAsmInstructions, SepiCompilerConsts, SepiDelphiLikeCompilerUtils,
  SepiFunDelphiCompilerConsts;

type
  {*
    Valeur construction de case
    @author sjrd
    @version 5.0
  *}
  ISepiMakeSquareValue = interface(ISepiReadableValue)
    ['{1E77E9E9-BDAD-4B1D-9C4E-F2E47DBC00DA}']

    procedure AddComponent(const AComponent: ISepiReadableValue);
  end;

  {*
    Valeur composant FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TSepiComponentValue = class(TSepiCustomComputedValue,
    ISepiTypeForceableValue)
  private
    FMasterExpr: ISepiExpression; /// Valeur Master
    FIDConstant: TSepiConstant;   /// Constante représentant l'ID du composant
    FFunLabyUtilsUnit: TSepiUnit; /// Unité FunLabyUtils

    FMakeSquareValue: ISepiMakeSquareValue; /// Valeur constructeur de case
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

  {*
    Valeur construction de case
    @author sjrd
    @version 5.0
  *}
  TSepiMakeSquareValue = class(TSepiCustomComputedValue, ISepiMakeSquareValue)
  private
    FComponents: IInterfaceList; /// Liste des composants
  protected
    function CheckComponentType(const AComponent: ISepiReadableValue): Boolean;

    procedure AddComponent(const AComponent: ISepiReadableValue);

    procedure CompileParams(CallInstr: TSepiAsmCall;
      Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
      Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;
  public
    constructor Create(ASepiRoot: TSepiRoot);
  end;

  {*
    Règles sémantiques du langage FunDelphi
    @author sjrd
    @version 1.0
  *}
  TSepiFunDelphiLanguageRules = class(TSepiDelphiLanguageRules)
  protected
    function LookForStringConstant(Context: TSepiComponent;
      const Name: string): TSepiConstant; virtual;

    function ResolveComponentID(SepiContext: TSepiComponent;
      const Identifier: string;
      const Expression: ISepiExpression): Boolean; virtual;

    function ResolveAction(SepiContext: TSepiComponent;
      const Identifier: string;
      const Expression: ISepiExpression): Boolean; virtual;

    function AttributeSelection(SepiContext: TSepiComponent;
      const BaseExpression: ISepiExpression;
      const AttrName: string): ISepiExpression; virtual;

    function ObjectCountSelection(SepiContext: TSepiComponent;
      const BaseExpression: ISepiExpression;
      const ObjectID: string): ISepiExpression; virtual;
  public
    function ResolveIdent(Context: TSepiComponent;
      const Identifier: string): ISepiExpression; override;

    function ResolveIdentInMethod(Compiler: TSepiMethodCompiler;
      const Identifier: string): ISepiExpression; override;

    function FieldSelection(SepiContext: TSepiComponent;
      const BaseExpression: ISepiExpression;
      const FieldName: string): ISepiExpression; override;
  end;

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
  ContextName = 'Context';

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
  Recherche une constante chaîne
  @param Context   Contexte
  @param Name      Nom de la constante
  @return Constante chaîne recherchée, ou nil si non trouvée
*}
function TSepiFunDelphiLanguageRules.LookForStringConstant(
  Context: TSepiComponent; const Name: string): TSepiConstant;
var
  Meta: TSepiComponent;
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
  Résoud un identificateur de composant
  @param SepiContext   Contexte
  @param Identifier    Identificateur recherché
  @param Expression    Expression à construire
  @return True en cas de succès, False sinon
*}
function TSepiFunDelphiLanguageRules.ResolveComponentID(
  SepiContext: TSepiComponent; const Identifier: string;
  const Expression: ISepiExpression): Boolean;
var
  Constant: TSepiConstant;
  MasterExpr: ISepiExpression;
begin
  Result := False;

  Constant := LookForStringConstant(SepiContext, ComponentIDPrefix+Identifier);
  if Constant = nil then
    Exit;

  MasterExpr := ResolveIdentInMethod(Expression.MethodCompiler, MasterName);
  if MasterExpr = nil then
    Exit;

  ISepiExpressionPart(TSepiComponentValue.Create(MasterExpr,
    Constant)).AttachToExpression(Expression);
  Result := True;
end;

{*
  Résoud une action
  @param SepiContext   Contexte
  @param Identifier    Identificateur recherché
  @param Expression    Expression à construire
  @return True en cas de succès, False sinon
*}
function TSepiFunDelphiLanguageRules.ResolveAction(SepiContext: TSepiComponent;
  const Identifier: string; const Expression: ISepiExpression): Boolean;
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
  Sélection d'un attribut d'une expression selon les règles du FunDelphi
  @param SepiContext      Contexte Sepi depuis lequel chercher
  @param BaseExpression   Expression de base
  @param AttrName         Nom de l'attribut
  @return Expression représentant l'attribut sélectionné (ou nil si inexistant)
*}
function TSepiFunDelphiLanguageRules.AttributeSelection(
  SepiContext: TSepiComponent; const BaseExpression: ISepiExpression;
  const AttrName: string): ISepiExpression;
var
  Constant: TSepiConstant;
  Prop: ISepiProperty;
  Expression: ISepiExpression;
begin
  Constant := LookForStringConstant(SepiContext, AttributePrefix+AttrName);
  if Constant = nil then
    Exit;

  Expression := inherited FieldSelection(SepiContext, BaseExpression,
    'Attribute');

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
  Sélection du nombre d'un certain objet possédés par un joueur
  @param SepiContext      Contexte Sepi depuis lequel chercher
  @param BaseExpression   Expression de base
  @param ObjectID         ID de l'objet
  @return Expression représentant l'attribut sélectionné (ou nil si inexistant)
*}
function TSepiFunDelphiLanguageRules.ObjectCountSelection(
  SepiContext: TSepiComponent; const BaseExpression: ISepiExpression;
  const ObjectID: string): ISepiExpression;
var
  TPlayerType: TSepiType;
  ReadableBase: ISepiReadableValue;
  ObjectExpr, CountExpr: ISepiExpression;
  CountProp: ISepiProperty;
begin
  TPlayerType := SepiContext.Root.FindClass(TPlayer);

  if not Supports(BaseExpression, ISepiReadableValue, ReadableBase) then
    Exit;
  if not TSepiConvertOperation.ConvertionExists(TPlayerType, ReadableBase) then
    Exit;

  ObjectExpr := TSepiExpression.Create(BaseExpression);
  if not ResolveComponentID(BaseExpression.MethodCompiler.SepiMethod,
    ObjectID, ObjectExpr) then
    Exit;

  ((ObjectExpr as ISepiReadableValue) as ISepiTypeForceableValue).ForceType(
    SepiContext.Root.FindClass(TObjectDef));

  CountExpr := inherited FieldSelection(SepiContext, ObjectExpr, 'Count');

  if not Supports(CountExpr, ISepiProperty, CountProp) then
    Exit;
  if (CountProp.ParamsCompleted) or (CountProp.ParamCount <> 1) then
    Exit;

  CountProp.Params[0] :=
    TSepiConvertOperation.ConvertValue(TPlayerType,
      ReadableBase) as ISepiExpression;
  CountProp.CompleteParams;

  Result := CountProp as ISepiExpression;
end;

{*
  [@inheritDoc]
*}
function TSepiFunDelphiLanguageRules.ResolveIdent(Context: TSepiComponent;
  const Identifier: string): ISepiExpression;
begin
  // Standart Delphi expression
  Result := inherited ResolveIdent(Context, Identifier);
  if Result <> nil then
    Exit;

  // Special FunDelphi expression
  Result := TSepiExpression.Create(UnitCompiler);
  if ResolveAction(UnitCompiler.SepiUnit, Identifier, Result) then
    Exit;

  Result := nil;
end;

{*
  [@inheritDoc]
*}
function TSepiFunDelphiLanguageRules.ResolveIdentInMethod(
  Compiler: TSepiMethodCompiler; const Identifier: string): ISepiExpression;
var
  LocalVar: TSepiLocalVar;
  ContextExpression, FieldExpression: ISepiExpression;
begin
  // Field selection on the Context parameter
  if (Compiler.SepiMethod.Signature.GetParam(ContextName) <> nil) and
    (Compiler.Locals.GetVarByName(Identifier) = nil) then
  begin
    LocalVar := Compiler.Locals.GetVarByName(ContextName);

    ContextExpression := TSepiExpression.Create(Compiler);
    ISepiExpressionPart(TSepiLocalVarValue.Create(
      LocalVar)).AttachToExpression(ContextExpression);

    FieldExpression := FieldSelection(Compiler.SepiMethod,
      ContextExpression, Identifier);

    if FieldExpression <> nil then
    begin
      Result := FieldExpression;
      Exit;
    end;
  end;

  // Standart Delphi expression
  Result := inherited ResolveIdentInMethod(Compiler, Identifier);
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
  [@inheritDoc]
*}
function TSepiFunDelphiLanguageRules.FieldSelection(SepiContext: TSepiComponent;
  const BaseExpression: ISepiExpression;
  const FieldName: string): ISepiExpression;
begin
  // Standart Delphi field
  Result := inherited FieldSelection(SepiContext, BaseExpression, FieldName);
  if Result <> nil then
    Exit;

  // Player attribute
  Result := AttributeSelection(SepiContext, BaseExpression, FieldName);
  if Result <> nil then
    Exit;

  // Object count
  Result := ObjectCountSelection(SepiContext, BaseExpression, FieldName);
end;

{---------------------------}
{ TSepiComponentValue class }
{---------------------------}

{*
  Crée une valeur composant
  @param AMasterExpr   Expression Master
  @param AIDConstant   Constante représentant l'ID du composant
*}
constructor TSepiComponentValue.Create(const AMasterExpr: ISepiExpression;
  AIDConstant: TSepiConstant);
begin
  inherited Create;

  FMasterExpr := AMasterExpr;
  FIDConstant := AIDConstant;
  FFunLabyUtilsUnit := FIDConstant.Root.FindComponent(
    FunLabyUtilsUnitName) as TSepiUnit;

  SetValueType(FunLabyUtilsUnit.FindClass(TFunLabyComponent));
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
    AnsiMatchStr(AValueType.Name, ComponentClassNames) and
    (FMakeSquareValue = nil);
end;

{*
  [@inheritDoc]
*}
procedure TSepiComponentValue.ForceType(AValueType: TSepiType);
var
  SubComponentValue: ISepiReadableValue;
begin
  Assert(CanForceType(AValueType));

  SetValueType(AValueType);

  if (AValueType as TSepiClass).DelphiClass = TSquare then
  begin
    SubComponentValue := TSepiComponentValue.Create(MasterExpr, IDConstant);
    SubComponentValue.AttachToExpression(TSepiExpression.Create(Expression));

    FMakeSquareValue := TSepiMakeSquareValue.Create(SepiRoot);
    FMakeSquareValue.AttachToExpression(TSepiExpression.Create(Expression));
    FMakeSquareValue.AddComponent(SubComponentValue);
  end;
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
  if FMakeSquareValue <> nil then
  begin
    FMakeSquareValue.CompileRead(Compiler, Instructions, Destination, TempVars);
    Exit;
  end;

  Index := AnsiIndexStr(ValueType.Name, ComponentClassNames);
  Assert(Index >= 0);
  PropName := MasterPropNames[Index];

  Expression := LanguageRules.FieldSelection(Compiler.SepiMethod, MasterExpr, PropName);
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

{----------------------------}
{ TSepiMakeSquareValue class }
{----------------------------}

{*
  Crée un constructeur de case
*}
constructor TSepiMakeSquareValue.Create(ASepiRoot: TSepiRoot);
begin
  inherited Create;

  FComponents := TInterfaceList.Create;

  SetValueType(ASepiRoot.FindClass(TSquare));
end;

{*
  Vérifie le type d'un composant
  @param AComponent   Composant à vérifier
  @return True si le type est valide, False sinon
*}
function TSepiMakeSquareValue.CheckComponentType(
  const AComponent: ISepiReadableValue): Boolean;
var
  TSquareComponentType: TSepiType;
  TypeForceable: ISepiTypeForceableValue;
begin
  Result := True;
  TSquareComponentType := SepiRoot.FindClass(TSquareComponent);

  if TSquareComponentType.CompatibleWith(AComponent.ValueType) then
    Exit;

  if Supports(AComponent, ISepiTypeForceableValue, TypeForceable) and
    TypeForceable.CanForceType(TSquareComponentType) then
  begin
    TypeForceable.ForceType(TSquareComponentType);
    Exit;
  end;

  (AComponent as ISepiExpression).MakeError(SSquareComponentValueRequired);
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMakeSquareValue.AddComponent(
  const AComponent: ISepiReadableValue);
begin
  if CheckComponentType(AComponent) then
    FComponents.Add(AComponent);
end;

{*
  Compile les paramètres dans une instruction assembleur CALL
  @param CallInstr      Instruction CALL
  @param Compiler       Compilateur de méthode
  @param Instructions   Liste d'instructions
  @param Destination    Référence mémoire à la variable résultat
  @parma TempVars       Gestionnaire de variables temporaires
*}
procedure TSepiMakeSquareValue.CompileParams(CallInstr: TSepiAsmCall;
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
var
  Parameters: TSepiAsmCallParams;
  OpenArrayTempVar: TSepiLocalVar;
  OpenArrayValue: ISepiValue;
  OpenArrayItemValue: ISepiWritableValue;
  I, HighValue: Integer;
  InstrParamMemory: TSepiMemoryReference;
begin
  Parameters := CallInstr.Parameters;

  // Set result memory space
  Parameters.Result.Assign(Destination);

  // Make open array temp var
  OpenArrayTempVar := Compiler.Locals.AddTempVar(
    TSepiStaticArrayType.Create(Compiler.LocalNamespace, '',
      UnitCompiler.SystemUnit.Integer, 0, FComponents.Count-1,
      SepiRoot.FindClass(TSquareComponent)));

  // Begin life of open array temp var
  OpenArrayTempVar.HandleLife;
  OpenArrayTempVar.Life.BeginInstrInterval(Instructions.GetCurrentEndRef);
  TempVars.Acquire(OpenArrayTempVar);

  // Make open array value
  OpenArrayValue := TSepiLocalVarValue.MakeValue(Compiler, OpenArrayTempVar);

  // Read components
  for I := 0 to FComponents.Count-1 do
  begin
    OpenArrayItemValue := TSepiArrayItemValue.MakeArrayItemValue(
      OpenArrayValue, TSepiTrueConstValue.MakeIntegerLiteral(
      Compiler, I)) as ISepiWritableValue;

    OpenArrayItemValue.CompileWrite(Compiler, Instructions,
      FComponents[I] as ISepiReadableValue);
  end;

  // Open array parameter
  InstrParamMemory := Parameters.Parameters[0].MemoryRef;
  InstrParamMemory.SetSpace(OpenArrayTempVar);
  InstrParamMemory.Seal;

  // High hidden parameter
  HighValue := FComponents.Count-1;
  InstrParamMemory := Parameters.Parameters[1].MemoryRef;
  InstrParamMemory.SetSpace(msConstant, SizeOf(Integer));
  InstrParamMemory.SetConstant(HighValue);
  InstrParamMemory.Seal;
end;

{*
  [@inheritDoc]
*}
procedure TSepiMakeSquareValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  SepiMethod: TSepiMethod;
  CallInstr: TSepiAsmRefCall;
begin
  NeedDestination(Destination, ValueType, Compiler, TempVars,
    Instructions.GetCurrentEndRef);

  if FComponents.Count = 0 then
    Exit;

  SepiMethod := SepiRoot.FindComponent('MapTools.MakeSquare') as TSepiMethod;

  CallInstr := TSepiAsmStaticCall.Create(Compiler);
  CallInstr.SourcePos := Expression.SourcePos;
  CallInstr.SetMethod(SepiMethod, False);
  CallInstr.Prepare(SepiMethod.Signature);

  CompileParams(CallInstr, Compiler, Instructions, Destination, TempVars);

  Instructions.Add(CallInstr);
end;

end.

