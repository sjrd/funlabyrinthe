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
  SepiFunDelphiCompilerConsts, SepiCompilerErrors;

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
    FIsTypeKnown: Boolean;        /// Indique si le type est connu

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
    Valeur propriété composant de case
    @author sjrd
    @version 5.0
  *}
  TSepiSquareCompPropertyValue = class(TSepiPropertyValue)
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue); override;
  public
    constructor Create(const AObjectValue: ISepiReadableValue;
      AProperty: TSepiProperty);
  end;

  {*
    Règles sémantiques du langage FunDelphi
    @author sjrd
    @version 1.0
  *}
  TSepiFunDelphiLanguageRules = class(TSepiDelphiLanguageRules)
  protected
    function AddMemberToExpression(const Expression: ISepiExpression;
      Context: TSepiComponent; const BaseValue: ISepiReadableValue;
      Member: TSepiMember): Boolean; override;

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
  AttributeTypePrefix = 'attrtype';
  ComponentTypePrefix = 'comp';

  MasterName = 'Master';
  CreateName = 'Create';
  AbleToName = 'AbleTo';
  UseForName = 'UseFor';
  ActionName = 'Action';
  ContextName = 'Context';
  ComponentPropName = 'Component';

implementation

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
var
  StrID: string;
  CompVarComponent: TSepiComponent;
  CompVar: TSepiVariable;
begin
  inherited Create;

  FMasterExpr := AMasterExpr;
  FIDConstant := AIDConstant;
  FFunLabyUtilsUnit := FIDConstant.Root.FindComponent(
    FunLabyUtilsUnitName) as TSepiUnit;

  SetValueType(FunLabyUtilsUnit.FindClass(TFunLabyComponent));

  StrID := string(IDConstant.ValuePtr^);
  CompVarComponent := IDConstant.LookFor(ComponentTypePrefix+StrID);
  if CompVarComponent is TSepiVariable then
  begin
    CompVar := TSepiVariable(CompVarComponent);
    Assert((CompVar.VarType is TSepiClass) and (CompVar.VarType.IsForward or
      TSepiClass(CompVar.VarType).ClassInheritsFrom(ValueType as TSepiClass)));

    SetValueType(CompVar.VarType);
    FIsTypeKnown := True;
  end;
end;

{*
  [@inheritDoc]
*}
function TSepiComponentValue.CanForceType(AValueType: TSepiType;
  Explicit: Boolean = False): Boolean;
var
  ClassType, AClassType: TSepiClass;
begin
  if (not (AValueType is TSepiClass)) or (FMakeSquareValue <> nil) then
  begin
    Result := False;
    Exit;
  end;

  ClassType := TSepiClass(ValueType);
  AClassType := TSepiClass(AValueType);

  if FIsTypeKnown then
  begin
    Result := ClassType.IsForward and
      (AClassType.DelphiClass.InheritsFrom(TFunLabyComponent) or
      TFunLabyComponent.InheritsFrom(AClassType.DelphiClass));

    if not Result then
    begin
      Result := (AClassType.DelphiClass = TSquare) and
        ClassType.DelphiClass.InheritsFrom(TSquareComponent);
    end;
  end else
  begin
    Result := AClassType.ClassInheritsFrom(ClassType);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiComponentValue.ForceType(AValueType: TSepiType);
var
  SubComponentValue: ISepiReadableValue;
begin
  Assert(CanForceType(AValueType));

  if AValueType = ValueType then
    Exit;

  if FIsTypeKnown and ValueType.IsForward and (not AValueType.IsForward) then
  begin
    { TODO 1 : Keep track of forward typecasts }
  end;

  SetValueType(AValueType);

  if (AValueType as TSepiClass).DelphiClass = TSquare then
  begin
    SubComponentValue := TSepiComponentValue.Create(MasterExpr, IDConstant);
    SubComponentValue.AttachToExpression(TSepiExpression.Create(Expression));

    (SubComponentValue as ISepiExpression).SourcePos := Expression.SourcePos;

    FMakeSquareValue := TSepiMakeSquareValue.Create(SepiRoot);
    FMakeSquareValue.AttachToExpression(TSepiExpression.Create(Expression));
    FMakeSquareValue.AddComponent(SubComponentValue);

    (FMakeSquareValue as ISepiExpression).SourcePos := Expression.SourcePos;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiComponentValue.CompileCompute(Compiler: TSepiMethodCompiler;
  Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
  TempVars: TSepiTempVarsLifeManager);
var
  Expression: ISepiExpression;
  Prop: ISepiProperty;
begin
  if FMakeSquareValue <> nil then
  begin
    FMakeSquareValue.CompileRead(Compiler, Instructions, Destination, TempVars);
    Exit;
  end;

  Expression := LanguageRules.FieldSelection(Compiler.SepiMethod, MasterExpr,
    ComponentPropName);
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
  [@inheritDoc]
*}
procedure TSepiMakeSquareValue.AddComponent(
  const AComponent: ISepiReadableValue);
var
  TSquareComponentType: TSepiType;
  Component: ISepiReadableValue;
begin
  TSquareComponentType := SepiRoot.FindClass(TSquareComponent);
  Component := AComponent;

  if not Component.ValueType.Equals(TSquareComponentType) then
  begin
    if TSepiConvertOperation.ConversionExists(TSquareComponentType,
      Component) then
    begin
      Component := TSepiConvertOperation.ConvertValue(
        TSquareComponentType, Component);
    end else
    begin
      (AComponent as ISepiExpression).MakeError(SSquareComponentValueRequired);
      Exit;
    end;
  end;

  FComponents.Add(Component);
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
      OpenArrayValue, TSepiIntegerLiteralValue.MakeValue(
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

{------------------------------------}
{ TSepiSquareCompPropertyValue class }
{------------------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiSquareCompPropertyValue.Create(
  const AObjectValue: ISepiReadableValue; AProperty: TSepiProperty);
begin
  inherited Create(AObjectValue, AProperty);

  Assert(Supports(AObjectValue, ISepiWritableValue));
  Assert(AObjectValue.ValueType = AProperty.Root.FindClass(TSquare));
  Assert(AProperty.WriteAccess.Kind = pakNone);
  Assert(ParamsCompleted);
end;

{*
  [@inheritDoc]
*}
function TSepiSquareCompPropertyValue.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if SameGUID(IID, ISepiWritableValue) then
  begin
    ISepiWritableValue(Obj) := Self;
    Result := S_OK;
    Exit;
  end;

  Result := inherited QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSquareCompPropertyValue.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;

  inherited;

  Expression.Attach(ISepiWritableValue, AsExpressionPart);
end;

{*
  [@inheritDoc]
*}
procedure TSepiSquareCompPropertyValue.CompileWrite(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  Source: ISepiReadableValue);
var
  ChangeCompMethod: TSepiMethodBase;
  ChangeCompCall: ISepiWantingParams;
begin
  ChangeCompMethod := SepiRoot.FindComponent(
    'MapTools.Change'+ObjectProperty.Name) as TSepiMethodBase;

  ChangeCompCall := TSepiMethodCall.Create(ChangeCompMethod);
  ChangeCompCall.AttachToExpression(TSepiExpression.Create(Compiler));

  ChangeCompCall.AddParam(ObjectValue as ISepiExpression);
  ChangeCompCall.AddParam(Source as ISepiExpression);
  ChangeCompCall.CompleteParams;

  (ObjectValue as ISepiWritableValue).CompileWrite(Compiler, Instructions,
    ChangeCompCall as ISepiReadableValue);
end;

{-----------------------------------}
{ TSepiFunDelphiLanguageRules class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
function TSepiFunDelphiLanguageRules.AddMemberToExpression(
  const Expression: ISepiExpression; Context: TSepiComponent;
  const BaseValue: ISepiReadableValue; Member: TSepiMember): Boolean;
begin
  if (Member is TSepiProperty) and
    (BaseValue.ValueType = SepiRoot.FindClass(TSquare)) and
    Supports(BaseValue, ISepiWritableValue) and
    AnsiMatchText(Member.Name, ['Field', 'Effect', 'Tool', 'Obstacle']) then
  begin
    ISepiExpressionPart(TSepiSquareCompPropertyValue.Create(
      BaseValue, TSepiProperty(Member))).AttachToExpression(Expression);
    Result := True;
    Exit;
  end;

  Result := inherited AddMemberToExpression(Expression, Context,
    BaseValue, Member);
end;

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
  Component: TSepiComponent;
  AttrType: TSepiType;
  Prop: ISepiProperty;
  AttributesExpr, Expression: ISepiExpression;
  PointedValue: ISepiValue;
begin
  // Player.SomeAttribute
  // -> TAttrType(Player.Attributes.Properties[attrSomeAttribute]^)

  Constant := LookForStringConstant(SepiContext, AttributePrefix+AttrName);
  if Constant = nil then
    Exit;

  Component := SepiContext.LookFor(AttributeTypePrefix+AttrName);
  if Component is TSepiVariable then
    AttrType := TSepiVariable(Component).VarType
  else
    AttrType := SystemUnit.Integer;

  AttributesExpr := inherited FieldSelection(SepiContext, BaseExpression,
    'Attributes');

  if not Supports(AttributesExpr, ISepiProperty, Prop) then
    Exit;
  if not Prop.ParamsCompleted then
    Exit;

  Expression := inherited FieldSelection(SepiContext, AttributesExpr,
    'ValueByName');

  if not Supports(Expression, ISepiProperty, Prop) then
    Exit;
  if Prop.ParamsCompleted or (Prop.ParamCount <> 1) then
    Exit;

  Expression := TSepiExpression.Create(Expression);
  ISepiExpressionPart(TSepiTrueConstValue.Create(
    Constant)).AttachToExpression(Expression);

  Prop.Params[0] := Expression;
  Prop.CompleteParams;

  PointedValue := TSepiDereferenceValue.MakeDereference(
    Prop as ISepiReadableValue);

  Result := TSepiCastOperator.CastValue(AttrType,
    PointedValue) as ISepiExpression;
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
  TPlayerType, TObjectDefType: TSepiType;
  ReadableBase: ISepiReadableValue;
  ObjectExpr, CountExpr: ISepiExpression;
  TypeForceableObjectExpr: ISepiTypeForceableValue;
  CountProp: ISepiProperty;
begin
  TPlayerType := SepiContext.Root.FindClass(TPlayer);
  TObjectDefType := SepiContext.Root.FindClass(TObjectDef);

  if not Supports(BaseExpression, ISepiReadableValue, ReadableBase) then
    Exit;
  if not TSepiConvertOperation.ConversionExists(TPlayerType, ReadableBase) then
    Exit;

  ObjectExpr := TSepiExpression.Create(BaseExpression);
  if not ResolveComponentID(BaseExpression.MethodCompiler.SepiMethod,
    ObjectID, ObjectExpr) then
    Exit;

  if not TObjectDefType.CompatibleWith(
    (ObjectExpr as ISepiReadableValue).ValueType) then
  begin
    TypeForceableObjectExpr :=
      ((ObjectExpr as ISepiReadableValue) as ISepiTypeForceableValue);

    if not TypeForceableObjectExpr.CanForceType(
      SepiContext.Root.FindClass(TObjectDef)) then
      Exit;
    TypeForceableObjectExpr.ForceType(SepiContext.Root.FindClass(TObjectDef));
  end;

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
  ContextExpression, FieldExpression, StdResult: ISepiExpression;
  ComponentExpr: ISepiComponentExpression;
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
  StdResult := inherited ResolveIdentInMethod(Compiler, Identifier);
  if (StdResult <> nil) and
    (not (Supports(StdResult, ISepiComponentExpression, ComponentExpr) and
    (ComponentExpr.Component is TSepiUnit))) then
  begin
    Result := StdResult;
    Exit;
  end;

  // Special FunDelphi expression
  Result := TSepiExpression.Create(Compiler);
  if ResolveComponentID(Compiler.SepiMethod, Identifier, Result) then
    Exit;
  if ResolveAction(Compiler.SepiMethod, Identifier, Result) then
    Exit;

  Result := StdResult;
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

end.

