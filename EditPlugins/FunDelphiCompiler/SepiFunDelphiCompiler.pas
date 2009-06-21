{*
  Classes de compilation d'une unité FunDelphi dans Sepi
  @author sjrd
  @version 5.0
*}
unit SepiFunDelphiCompiler;

interface

uses
  Windows, Types, SysUtils, Classes, StrUtils, TypInfo, Contnrs, SysConst,
  ScUtils, ScStrUtils, ScDelphiLanguage, FunLabyUtils,
  SepiCore,
  SepiReflectionCore, SepiMembers, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiOpCodes,
  SepiCompiler, SepiCompilerErrors, SepiExpressions, SepiInstructions,
  SepiAsmInstructions, SepiCompilerConsts,
  SepiParseTrees, SepiCompilerUtils, SepiDelphiLikeCompilerUtils,
  SepiStdCompilerNodes,
  SepiDelphiCompiler, SepiFunDelphiLexer, SepiFunDelphiParser,
  SepiFunDelphiCompilerConsts, SepiFunDelphiCompilerUtils;

type
  TFunDelphiComponentDeclNode = class;

  {*
    Noeud racine
    @author sjrd
    @version 5.0
  *}
  TFunDelphiRootNode = class(TSepiParseTreeRootNode)
  private
    FComponentDeclNodes: TObjectList; /// Liste des noeuds déclaration de compo

    procedure PrepareInitializeUnitProc;
    procedure MakeInitializeUnitProc;
    procedure MakeRegisterComponentsProc;

    procedure MakeTopLevelProcs;
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    destructor Destroy; override;

    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    procedure AddComponentDeclNode(Node: TFunDelphiComponentDeclNode);
  end;

  {*
    Noeud section uses
    @author sjrd
    @version 5.0
  *}
  TFunDelphiUsesNode = class(TSepiUsesNode)
  protected
    function IsRedeclared(const UnitName: string): Boolean; override;
  end;

  {*
    Noeud d'expression d'initialisation
    @author sjrd
    @version 5.0
  *}
  TFunDelphiInitializationExpressionNode = class(
    TDelphiInitializationExpressionNode)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud opérateur binaire
    @author sjrd
    @version 1.0
  *}
  TFunDelphiBinaryOpNode = class(TSepiDelphiLikeBinaryOpNode)
  protected
    function GetOperation: TSepiOperation; override;
  end;

  {*
    Noeud opération is
    @author sjrd
    @version 5.0
  *}
  TFunDelphiIsOperationNode = class(TSepiIsOperationNode)
  protected
    function MakeSquareOperation(const ALeftValue: ISepiReadableValue;
      const RightValue: ISepiReadableValue): ISepiExpression;

    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud opération binaire can
    @author sjrd
    @version 5.0
  *}
  TFunDelphiCanOpNode = class(TSepiBinaryOpNode)
  protected
    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud opération binaire has
    @author sjrd
    @version 5.0
  *}
  TFunDelphiHasOpNode = class(TSepiBinaryOpNode)
  protected
    function GetPriority: Integer; override;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;
  end;

  {*
    Noeud indiquant le type de comparaison d'un has
    @author sjrd
    @version 5.0
  *}
  TFunDelphiHasComparisonNode = class(TSepiDelphiLikeBinaryOpNode)
  protected
    function GetOperation: TSepiOperation; override;
  end;

  {*
    Noeud opérateur unaire
    @author sjrd
    @version 5.0
  *}
  TFunDelphiUnaryOpNode = class(TSepiDelphiLikeUnaryOpNode)
  protected
    function GetOperation: TSepiOperation; override;
  end;

  {*
    Noeud index de tableau ou de propriété tableau
    @author sjrd
    @version 5.0
  *}
  TFunDelphiArrayIndicesModifierNode = class(TSepiArrayIndicesModifierNode)
  protected
    procedure CompileProperty(const Prop: ISepiProperty); override;
  end;

  {*
    Noeud sélection de champ
    @author sjrd
    @version 5.0
  *}
  TFunDelphiFieldSelectionModifierNode = class(TSepiFieldSelectionModifierNode)
  protected
    function MakeFieldSelection: ISepiExpression; override;
  end;

  {*
    Noeud déclarations de types d'actions
    @author sjrd
    @version 5.0
  *}
  TFunDelphiActionsNode = class(TSepiNonTerminal)
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud déclarations d'attributs
    @author sjrd
    @version 5.0
  *}
  TFunDelphiAttributesNode = class(TSepiNonTerminal)
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud déclaration d'un composant
    @author sjrd
    @version 5.0
  *}
  TFunDelphiComponentDeclNode = class(TSepiNonTerminal)
  private
    FInitializeUnitCompiler: TSepiMethodCompiler; /// Compilateur InitializeUnit

    FIDConstant: TSepiConstant; /// Constante représentant l'ID du composant
    FRegisterable: Boolean;     /// True si peut être recensé
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    procedure MakeInitialize(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);
    procedure MakeRegister(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);

    property IDConstant: TSepiConstant read FIDConstant;
  end;

  {*
    Noeud définition de classe
    @author sjrd
    @version 5.0
  *}
  TFunDelphiClassDefNode = class(TSepiNonTerminal)
  private
    FSepiClass: TSepiClass; /// Classe Sepi compilée

    procedure CompleteMethods;
  protected
    function GetSepiContext: TSepiComponent; override;
    function GetMasterClass: TSepiClass; virtual;

    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property MasterClass: TSepiClass read GetMasterClass;
  public
    procedure EndParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property SepiClass: TSepiClass read FSepiClass;
  end;

  {*
    Noeud définition de plug-in
    @author sjrd
    @version 5.0
  *}
  TFunDelphiPluginNode = class(TFunDelphiClassDefNode)
  protected
    function GetMasterClass: TSepiClass; override;
  end;

  {*
    Noeud définition d'objet
    @author sjrd
    @version 5.0
  *}
  TFunDelphiObjectNode = class(TFunDelphiClassDefNode)
  protected
    function GetMasterClass: TSepiClass; override;
  end;

  {*
    Noeud définition de terrain
    @author sjrd
    @version 5.0
  *}
  TFunDelphiFieldNode = class(TFunDelphiClassDefNode)
  protected
    function GetMasterClass: TSepiClass; override;
  end;

  {*
    Noeud définition d'effet
    @author sjrd
    @version 5.0
  *}
  TFunDelphiEffectNode = class(TFunDelphiClassDefNode)
  protected
    function GetMasterClass: TSepiClass; override;
  end;

  {*
    Noeud définition d'outil
    @author sjrd
    @version 5.0
  *}
  TFunDelphiToolNode = class(TFunDelphiClassDefNode)
  protected
    function GetMasterClass: TSepiClass; override;
  end;

  {*
    Noeud définition d'obstacle
    @author sjrd
    @version 5.0
  *}
  TFunDelphiObstacleNode = class(TFunDelphiClassDefNode)
  protected
    function GetMasterClass: TSepiClass; override;
  end;

  {*
    Noeud classe parent
    @author sjrd
    @version 5.0
  *}
  TFunDelphiParentClassNode = class(TSepiNonTerminal)
  public
    function CompileParentClass(MasterClass: TSepiClass): TSepiClass;
  end;

  {*
    Noeud champ de classe
    @author sjrd
    @version 5.0
  *}
  TFunDelphiClassFieldNode = class(TSepiClassFieldNode)
  public
    procedure BeginParsing; override;
    procedure EndParsing; override;
  end;

  {*
    Noeud méthode (déclaration et implémentation)
    @author sjrd
    @version 5.0
  *}
  TFunDelphiMethodDeclAndImplNode = class(TSepiNonTerminal)
  private
    FSepiMethod: TSepiMethod; /// Méthode déclarée et implémentée
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    property SepiMethod: TSepiMethod read FSepiMethod;
  end;

  {*
    Noeud déclaration de méthode
    @author sjrd
    @version 5.0
  *}
  TFunDelphiMethodDeclNode = class(TSepiMethodDeclarationNode)
  private
    function GetSepiMethod: TSepiMethod;
  public
    procedure EndParsing; override;

    property SepiMethod: TSepiMethod read GetSepiMethod;
  end;

  {*
    Noeud en-tête de constructeur
    @author sjrd
    @version 5.0
  *}
  TFunDelphiConstructorHeaderNode = class(TFunDelphiMethodDeclNode)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud corps de constructeur
    @author sjrd
    @version 5.0
  *}
  TFunDelphiConstructorBodyBode = class(TSepiMethodBodyNode)
  end;

  {*
    Noeud en-tête de destructeur
    @author sjrd
    @version 5.0
  *}
  TFunDelphiDestructorHeaderNode = class(TFunDelphiMethodDeclNode)
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud corps de destructeur
    @author sjrd
    @version 5.0
  *}
  TFunDelphiDestructorBodyBode = class(TSepiMethodBodyNode)
  end;

  {*
    Noeud auto-override (met override sans marque dans le code source)
    @author sjrd
    @version 5.0
  *}
  TFunDelphiAutoOverrideNode = class(TSepiMethodLinkKindNode)
  protected
    function GetLinkKind: TMethodLinkKind; override;
  end;

  {*
    Noeud image d'un composant visuel
    @author sjrd
    @version 5.0
  *}
  TFunDelphiImageNode = class(TSepiNonTerminal)
  private
    FCompiler: TSepiMethodCompiler;      /// Compilateur du constructeur
    FInstructions: TSepiInstructionList; /// Instructions d'ajout d'images

    function MakeEmptyConstructor: TSepiMethod;
    procedure AddImage(const ImageValue: ISepiReadableValue);
  protected
    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  public
    procedure BeginParsing; override;
  end;

  {*
    Noeud en-tête d'événement
    @author sjrd
    @version 5.0
  *}
  TFunDelphiEventHeaderNode = class(TFunDelphiMethodDeclNode)
  private
    procedure FillSignature;
  protected
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;
  end;

  {*
    Noeud action de plug-in
    @author sjrd
    @version 5.0
  *}
  TFunDelphiPluginActionNode = class(TSepiNonTerminal)
  private
    FAbleToCompiler: TSepiMethodCompiler; /// Compilateur de la méthode AbleTo

    FAbleToInstructions: TSepiInstructionList; /// Instructions dans AbleTo

    procedure BuildSuccess;
  protected
    function BuildIfStatement(
      Instructions: TSepiInstructionList): TSepiInstructionList;
    procedure BuildCondition(const Expression: ISepiExpression); virtual;

    function GetMethodCompiler: TSepiMethodCompiler; override;

    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property AbleToCompiler: TSepiMethodCompiler read FAbleToCompiler;
    property AbleToInstructions: TSepiInstructionList read FAbleToInstructions;
  public
    procedure BeginParsing; override;

    function ResolveIdent(const Identifier: string): ISepiExpression; override;
  end;

  {*
    Noeud action d'objet
    @author sjrd
    @version 5.0
  *}
  TFunDelphiObjectActionNode = class(TFunDelphiPluginActionNode)
  private
    FUseForCompiler: TSepiMethodCompiler; /// Compilateur de la méthode UseFor
  protected
    procedure BuildCondition(const Expression: ISepiExpression); override;

    function GetMethodCompiler: TSepiMethodCompiler; override;

    procedure ChildBeginParsing(Child: TSepiParseTreeNode); override;
    procedure ChildEndParsing(Child: TSepiParseTreeNode); override;

    property UseForCompiler: TSepiMethodCompiler read FUseForCompiler;
  end;

  {*
    Noeud condition d'action
    @author sjrd
    @version 5.0
  *}
  TFunDelphiActionConditionNode = class(TSepiExpressionNode)
  private
    function GetExpression: ISepiExpression;
  public
    property Expression: ISepiExpression read GetExpression;
  end;

  {*
    Noeud routine
    @author sjrd
    @version 1.0
  *}
  TFunDelphiRoutineNode = class(TSepiMethodImplementationNode)
  public
    procedure EndParsing; override;
  end;

  {*
    Noeud du corps d'une méthode
    @author sjrd
    @version 1.0
  *}
  TFunDelphiMethodBodyNode = class(TSepiMethodBodyNode)
  public
    function ResolveIdent(const Identifier: string): ISepiExpression; override;
  end;

  {*
    Classe de base pour les noeuds opérateur receives et discards
    @author sjrd
    @version 5.0
  *}
  TFunDelphiReceivesDiscardsOpNode = class(TSepiBinaryOpNode)
  protected
    function GetOperation: TSepiOperation; virtual; abstract;
  public
    function MakeOperation(
      const Left, Right: ISepiExpression): ISepiExpression; override;

    property Operation: TSepiOperation read GetOperation;
  end;

  {*
    Noeud opérateur receives
    @author sjrd
    @version 5.0
  *}
  TFunDelphiReceivesOpNode = class(TFunDelphiReceivesDiscardsOpNode)
  protected
    function GetOperation: TSepiOperation; override;
  end;

  {*
    Noeud opérateur discards
    @author sjrd
    @version 5.0
  *}
  TFunDelphiDiscardsOpNode = class(TFunDelphiReceivesDiscardsOpNode)
  public
    function GetOperation: TSepiOperation; override;
  end;

function CompileFunDelphiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName): TSepiUnit;

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Compile un fichier source FunDelphi
  @param SepiRoot       Racine Sepi
  @param Errors         Gestionnaire d'erreurs
  @param SourceFile     Source à compiler
  @param DestFileName   Nom du fichier de sortie
  @return Unité Sepi compilée (interface seulement, pas d'implémentation)
*}
function CompileFunDelphiSource(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList; SourceFile: TStrings;
  const DestFileName: TFileName): TSepiUnit;
begin
  Result := SepiCompilerUtils.CompileSepiSource(SepiRoot, Errors, SourceFile,
    DestFileName, TFunDelphiRootNode, ntSource, TSepiFunDelphiLexer,
    TSepiFunDelphiParser);
end;

{*
  Initialise le tableau NonTerminalClasses
*}
procedure InitNonTerminalClasses;
begin
  NonTerminalClasses[ntSource]      := TFunDelphiRootNode;
  NonTerminalClasses[ntUsesSection] := TFunDelphiUsesNode;

  NonTerminalClasses[ntCommaIdentDeclList] := TSepiIdentifierDeclListNode;
  NonTerminalClasses[ntQualifiedIdent]     := TSepiQualifiedIdentNode;
  NonTerminalClasses[ntIdentifierDecl]     := TSepiIdentifierDeclarationNode;
  NonTerminalClasses[ntTypeName]           := TSepiTypeNameNode;

  NonTerminalClasses[ntInitializationExpression] :=
    TFunDelphiInitializationExpressionNode;
  NonTerminalClasses[ntArrayInitialization]  := TDelphiArrayInitializationNode;
  NonTerminalClasses[ntRecordInitialization] := TDelphiRecordInitializationNode;
  NonTerminalClasses[ntGUIDInitialization]   := TDelphiGUIDInitializationNode;
  NonTerminalClasses[ntOtherInitialization]  := TDelphiOtherInitializationNode;

  NonTerminalClasses[ntExpression]      := TSepiBinaryOpTreeNode;
  NonTerminalClasses[ntConstExpression] := TSepiConstExpressionNode;

  NonTerminalClasses[ntDelphiBinaryOp] := TFunDelphiBinaryOpNode;
  NonTerminalClasses[ntIsOperation]    := TFunDelphiIsOperationNode;
  NonTerminalClasses[ntAsOperation]    := TSepiAsOperationNode;
  NonTerminalClasses[ntCanOp]          := TFunDelphiCanOpNode;
  NonTerminalClasses[ntHasOp]          := TFunDelphiHasOpNode;
  NonTerminalClasses[ntHasComparison]  := TFunDelphiHasComparisonNode;
  NonTerminalClasses[ntUnaryOp]        := TFunDelphiUnaryOpNode;
  NonTerminalClasses[ntAddressOfOp]    := TSepiAddressOfOpNode;

  NonTerminalClasses[ntSingleExpr]        := TDelphiSingleExprNode;
  NonTerminalClasses[ntParenthesizedExpr] := TSepiSameAsChildExpressionNode;
  NonTerminalClasses[ntUnaryOpExpr]       := TSepiUnaryOperationNode;

  NonTerminalClasses[ntSingleValue]           := TSepiSameAsChildExpressionNode;
  NonTerminalClasses[ntIntegerConst]          := TSepiConstIntegerNode;
  NonTerminalClasses[ntFloatConst]            := TSepiConstFloatNode;
  NonTerminalClasses[ntStringConst]           := TSepiConstStringNode;
  NonTerminalClasses[ntIdentifierSingleValue] := TSepiIdentifierExpressionNode;
  NonTerminalClasses[ntInheritedExpression]   := TSepiInheritedExpressionNode;
  NonTerminalClasses[ntPureInheritedExpression] :=
    TSepiPureInheritedExpressionNode;
  NonTerminalClasses[ntNilValue]              := TSepiNilValueNode;
  NonTerminalClasses[ntSetValue]              := TSepiSetValueNode;

  NonTerminalClasses[ntUnaryOpModifier] := TSepiUnaryOpModifierNode;
  NonTerminalClasses[ntDereferenceOp]   := TSepiDereferenceOpNode;
  NonTerminalClasses[ntParameters]      := TDelphiParametersNode;
  NonTerminalClasses[ntArrayIndices]    := TFunDelphiArrayIndicesModifierNode;
  NonTerminalClasses[ntFieldSelection]  := TFunDelphiFieldSelectionModifierNode;

  NonTerminalClasses[ntConstDecl]         := TDelphiConstantDeclNode;
  NonTerminalClasses[ntGlobalVar]         := TDelphiVariableDeclNode;
  NonTerminalClasses[ntActionsSection]    := TFunDelphiActionsNode;
  NonTerminalClasses[ntAttributesSection] := TFunDelphiAttributesNode;
  NonTerminalClasses[ntComponent]         := TFunDelphiComponentDeclNode;
  NonTerminalClasses[ntPluginSection]     := TFunDelphiPluginNode;
  NonTerminalClasses[ntObjectSection]     := TFunDelphiObjectNode;
  NonTerminalClasses[ntFieldSection]      := TFunDelphiFieldNode;
  NonTerminalClasses[ntEffectSection]     := TFunDelphiEffectNode;
  NonTerminalClasses[ntToolSection]       := TFunDelphiToolNode;
  NonTerminalClasses[ntObstacleSection]   := TFunDelphiObstacleNode;

  NonTerminalClasses[ntParentClass]       := TFunDelphiParentClassNode;
  NonTerminalClasses[ntField]             := TFunDelphiClassFieldNode;
  NonTerminalClasses[ntConstructor]       := TFunDelphiMethodDeclAndImplNode;
  NonTerminalClasses[ntConstructorHeader] := TFunDelphiConstructorHeaderNode;
  NonTerminalClasses[ntDestructor]        := TFunDelphiMethodDeclAndImplNode;
  NonTerminalClasses[ntDestructorHeader]  := TFunDelphiDestructorHeaderNode;
  NonTerminalClasses[ntAutoOverride]      := TFunDelphiAutoOverrideNode;
  NonTerminalClasses[ntImage]             := TFunDelphiImageNode;
  NonTerminalClasses[ntEvent]             := TFunDelphiMethodDeclAndImplNode;
  NonTerminalClasses[ntEventHeader]       := TFunDelphiEventHeaderNode;
  NonTerminalClasses[ntPluginAction]      := TFunDelphiPluginActionNode;
  NonTerminalClasses[ntObjectAction]      := TFunDelphiObjectActionNode;
  NonTerminalClasses[ntActionCondition]   := TFunDelphiActionConditionNode;

  NonTerminalClasses[ntRoutineImpl]       := TFunDelphiRoutineNode;
  NonTerminalClasses[ntRoutineImplHeader] := TSepiMethodImplHeaderNode;
  NonTerminalClasses[ntRoutineKind]       := TSepiSignatureKindNode;
  NonTerminalClasses[ntRoutineName]       := TSepiQualifiedIdentNode;
  NonTerminalClasses[ntRoutineVisibility] := TSepiChangeVisibilityNode;

  NonTerminalClasses[ntConstructorSignature] := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntDestructorSignature]  := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntRoutineSignature]     := TSepiSignatureBuilderNode;
  NonTerminalClasses[ntParam]                := TSepiParamNode;
  NonTerminalClasses[ntParamKind]            := TSepiParamKindNode;
  NonTerminalClasses[ntParamNameList]        := TSepiIdentifierDeclListNode;
  NonTerminalClasses[ntParamName]            := TSepiParamNameNode;
  NonTerminalClasses[ntReturnType]           := TSepiSignatureReturnTypeNode;

  NonTerminalClasses[ntForwardMarker] := TSepiForwardMarkerNode;
  NonTerminalClasses[ntMethodBody]    := TFunDelphiMethodBodyNode;
  NonTerminalClasses[ntLocalVar]      := TSepiLocalVarNode;

  NonTerminalClasses[ntInstructionList]       := TSepiInstructionListNode;
  NonTerminalClasses[ntNoInstruction]         := TSepiNoInstructionNode;
  NonTerminalClasses[ntBeginEndBlock]         := TSepiBeginEndBlockNode;
  NonTerminalClasses[ntIfThenElseInstruction] := TSepiIfThenElseInstructionNode;
  NonTerminalClasses[ntWhileInstruction]      := TSepiWhileInstructionNode;
  NonTerminalClasses[ntRepeatInstruction] :=
    TSepiRepeatUntilInstructionNode;
  NonTerminalClasses[ntForInstruction]        := TSepiForInstructionNode;
  NonTerminalClasses[ntForControlVar]         := TSepiForControlVarNode;
  NonTerminalClasses[ntForTo]                 := TSepiForToNode;
  NonTerminalClasses[ntForDownTo]             := TSepiForDownToNode;
  NonTerminalClasses[ntTryInstruction]        := TSepiTryInstructionNode;
  NonTerminalClasses[ntExceptClause]          := TSepiExceptClauseNode;
  NonTerminalClasses[ntMultiOn]               := TSepiMultiOnNode;
  NonTerminalClasses[ntOnClause]              := TSepiOnClauseNode;
  NonTerminalClasses[ntExceptionVarAndType]   := TSepiExceptionVarAndTypeNode;
  NonTerminalClasses[ntMultiOnElseClause]     := TSepiMultiOnElseClauseNode;
  NonTerminalClasses[ntFinallyClause]         := TSepiFinallyClauseNode;
  NonTerminalClasses[ntRaiseInstruction]      := TSepiRaiseInstructionNode;

  NonTerminalClasses[ntExpressionInstruction] :=
    TSepiExecuteExpressionInstructionNode;
  NonTerminalClasses[ntExecutableExpression] := TSepiBinaryOpTreeNode;
  NonTerminalClasses[ntAssignmentOp]         := TSepiAssignmentOpNode;
  NonTerminalClasses[ntReceivesOp]           := TFunDelphiReceivesOpNode;
  NonTerminalClasses[ntDiscardsOp]           := TFunDelphiDiscardsOpNode;
end;

{*
  Compile un appel inherited pur
  @param Compiler   Compilateur
*}
procedure CompilePureInheritedCall(Compiler: TSepiMethodCompiler);
var
  Executable: ISepiExecutable;
  Instruction: TSepiExecuteExpression;
begin
  Executable := TSepiPureInheritedCall.Create;
  Executable.AttachToExpression(TSepiExpression.Create(Compiler));

  Instruction := TSepiExecuteExpression.Create(Compiler);
  Instruction.Executable := Executable;
  Compiler.Instructions.Add(Instruction);
end;

{--------------------------}
{ TFunDelphiRootNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
destructor TFunDelphiRootNode.Destroy;
begin
  FComponentDeclNodes.Free;

  inherited;
end;

{*
  Prépare la procédure InitializeUnit
*}
procedure TFunDelphiRootNode.PrepareInitializeUnitProc;
var
  SepiMethod: TSepiMethod;
begin
  SepiContext.CurrentVisibility := mvPrivate;

  SepiMethod := TSepiMethod.Create(SepiContext, 'InitializeUnit', nil,
    'procedure(Master: FunLabyUtils.TMaster; Params: Classes.TStrings)');
  UnitCompiler.FindMethodCompiler(SepiMethod, True);

  SepiContext.CurrentVisibility := mvPublic;
end;

{*
  Construit la procédure InitializeUnit
*}
procedure TFunDelphiRootNode.MakeInitializeUnitProc;
var
  SepiMethod: TSepiMethod;
  Compiler: TSepiMethodCompiler;
  I: Integer;
begin
  SepiMethod := SepiContext.FindComponent('InitializeUnit') as TSepiMethod;
  Compiler := UnitCompiler.FindMethodCompiler(SepiMethod);

  for I := 0 to FComponentDeclNodes.Count-1 do
    TFunDelphiComponentDeclNode(FComponentDeclNodes[I]).MakeInitialize(
      Compiler, Compiler.Instructions);
end;

{*
  Construit la procédure RegisterComponents
*}
procedure TFunDelphiRootNode.MakeRegisterComponentsProc;
var
  SepiMethod: TSepiMethod;
  Compiler: TSepiMethodCompiler;
  I: Integer;
begin
  SepiMethod := TSepiMethod.Create(SepiContext, 'RegisterComponents', nil,
    'procedure(Master: TMaster; '+
    'RegisterSingleComponentProc: TRegisterSingleComponentProc; '+
    'RegisterComponentSetProc: TRegisterComponentSetProc)');
  Compiler := UnitCompiler.FindMethodCompiler(SepiMethod, True);

  for I := 0 to FComponentDeclNodes.Count-1 do
    TFunDelphiComponentDeclNode(FComponentDeclNodes[I]).MakeRegister(
      Compiler, Compiler.Instructions);
end;

{*
  Construit les procédures top-level
*}
procedure TFunDelphiRootNode.MakeTopLevelProcs;
begin
  SepiContext.CurrentVisibility := mvPrivate;

  MakeInitializeUnitProc;
  MakeRegisterComponentsProc;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiRootNode.ChildEndParsing(Child: TSepiParseTreeNode);
const
  ImpliedUses: array[0..10] of string = (
    'Types', 'SysUtils', 'Classes', 'Graphics', 'Contnrs', 'Controls',
    'Dialogs', 'TypInfo', 'ScUtils', 'SdDialogs', 'FunLabyUtils'
  );
begin
  if Child.SymbolClass = ntIdentifier then
  begin
    SetSepiUnit(TSepiUnit.Create(SepiRoot, Child.AsText, ImpliedUses));
    PrepareInitializeUnitProc;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiRootNode.EndParsing;
var
  I: Integer;
begin
  if FComponentDeclNodes <> nil then
    MakeTopLevelProcs;

  for I := 0 to UnitCompiler.MethodCount-1 do
    UnitCompiler.Methods[I].Complete;

  SepiUnit.Complete;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiRootNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  Result := UnitResolveIdent(UnitCompiler, Identifier);
end;

{*
  Ajoute un noeud déclaration de composant
  @param Node   Noeud à ajouter
*}
procedure TFunDelphiRootNode.AddComponentDeclNode(
  Node: TFunDelphiComponentDeclNode);
begin
  if FComponentDeclNodes = nil then
    FComponentDeclNodes := TObjectList.Create(False);

  FComponentDeclNodes.Add(Node);
end;

{--------------------------}
{ TFunDelphiUsesNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiUsesNode.IsRedeclared(const UnitName: string): Boolean;
var
  I: Integer;
begin
  // Same as the current unit name?
  if AnsiSameText(UnitName, SepiUnit.Name) then
  begin
    Result := True;
    Exit;
  end;

  // Already present in this list?
  for I := 0 to ChildCount-2 do
  begin
    if AnsiSameText(UnitName, Children[I].AsText) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

{----------------------------------------------}
{ TFunDelphiInitializationExpressionNode class }
{----------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiInitializationExpressionNode.BeginParsing;
begin
  inherited;

  if ValueType is TSepiStaticArrayType then
    SetSymbolClass(ntArrayInitializationExpression)
  else if ValueType is TSepiRecordType then
  begin
    if ValueType = SystemUnit.TGUID then
      SetSymbolClass(ntGUIDInitializationExpression)
    else
      SetSymbolClass(ntRecordInitializationExpression);
  end else
    SetSymbolClass(ntOtherInitializationExpression);
end;

{------------------------------}
{ TFunDelphiBinaryOpNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiBinaryOpNode.GetOperation: TSepiOperation;
const
  SymbolClassToOperation: array[tkPlus..tkNotEqual] of TSepiOperation = (
    opAdd, opSubtract, opMultiply, opDivide, opIntDivide, opModulus,
    opShiftLeft, opShiftRight, opOr, opAnd, opXor, 0,
    opCmpLT, opCmpLE, opCmpGT, opCmpGE, opCmpNE
  );
var
  SymbolClass: TSepiSymbolClass;
begin
  SymbolClass := Children[0].SymbolClass;

  Assert(SymbolClass in ([tkEquals, tkPlus..tkNotEqual]-[tkNot]));

  if SymbolClass = tkEquals then
    Result := opCmpEQ
  else
    Result := SymbolClassToOperation[SymbolClass];
end;

{---------------------------------}
{ TFunDelphiIsOperationNode class }
{---------------------------------}

{*
  Construit une opération avec des cases
  @param LeftValue   Valeur gauche
  @param RightValue   Valeur droite
  @return Expression résultat, ou nil si non applicable
*}
function TFunDelphiIsOperationNode.MakeSquareOperation(
  const ALeftValue, RightValue: ISepiReadableValue): ISepiExpression;
const
  SqCompClasses: array[0..3] of TClass = (TField, TEffect, TTool, TObstacle);
  SqCompFields: array[0..3] of string = ('Field', 'Effect', 'Tool', 'Obstacle');
var
  LeftValue: ISepiReadableValue;
  TFunLabyComponentType, TSquareComponentType, TSquareType: TSepiClass;
  LeftClass, RightClass: TSepiClass;
  TypeForceable: ISepiTypeForceableValue;
  I: Integer;
  WantingParams: ISepiWantingParams;
begin
  LeftValue := ALeftValue;

  // Fetch TSquareComponent and TSquare classes
  TFunLabyComponentType := TSepiClass(SepiRoot.FindClass(TFunLabyComponent));
  TSquareComponentType := TSepiClass(SepiRoot.FindClass(TSquareComponent));
  TSquareType := TSepiClass(SepiRoot.FindClass(TSquare));

  // Fetch left class
  if LeftValue.ValueType is TSepiClass then
    LeftClass := TSepiClass(LeftValue.ValueType)
  else
    Exit;

  // Left class must be descendant of TSquareComponent
  if not LeftClass.ClassInheritsFrom(TSquareComponentType) then
    Exit;

  // Fetch right class
  if RightValue.ValueType is TSepiClass then
  begin
    RightClass := TSepiClass(RightValue.ValueType);

    // SquareComp is Component may force the component to be a TSquareComponent
    if RightClass = TFunLabyComponentType then
    begin
      if Supports(RightValue, ISepiTypeForceableValue, TypeForceable) and
        TypeForceable.CanForceType(TSquareComponentType) then
      begin
        TypeForceable.ForceType(TSquareComponentType);
        RightClass := TSquareComponentType;
      end;
    end;
  end else if RightValue.ValueType is TSepiMetaClass then
    RightClass := TSepiMetaClass(RightValue.ValueType).SepiClass
  else
    Exit;

  // Right class must be descendant of TSquareComponent
  if not RightClass.ClassInheritsFrom(TSquareComponentType) then
    Exit;

  // Select the subpart to the left following the right class
  if LeftClass.ClassInheritsFrom(TSquareType) then
  begin
    for I := Low(SqCompClasses) to High(SqCompClasses) do
    begin
      if RightClass.ClassInheritsFrom(TSepiClass(
        SepiRoot.FindClass(SqCompClasses[I]))) then
      begin
        LeftValue := FieldSelection(SepiContext, LeftValue as ISepiExpression,
          SqCompFields[I]) as ISepiReadableValue;
        Break;
      end;
    end;
  end;

  // SquareComponent is TClass: use standart is operation
  if RightValue.ValueType is TSepiMetaClass then
  begin
    Result := TSepiIsOperation.MakeOperation(LeftValue,
      RightValue) as ISepiExpression;
  end else

  // Square is Component: use TSquare.Contains
  if LeftClass.ClassInheritsFrom(TSquareType) then
  begin
    WantingParams := FieldSelection(SepiContext, LeftValue as ISepiExpression,
      'Contains') as ISepiWantingParams;
    WantingParams.AddParam(RightValue as ISepiExpression);
    WantingParams.CompleteParams;

    Result := WantingParams as ISepiExpression;
  end else

  // Component is Component: use = comparison
  begin
    Result := TSepiBinaryOperation.MakeOperation(opCmpEQ,
      LeftValue, RightValue) as ISepiExpression;
  end;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiIsOperationNode.GetPriority: Integer;
begin
  Result := 2;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiIsOperationNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue, RightValue: ISepiReadableValue;
begin
  if RequireReadableValue(Left, LeftValue) and
    RequireReadableValue(Right, RightValue) then
  begin
    Result := MakeSquareOperation(LeftValue, RightValue);
    if Result <> nil then
      Result.SourcePos := SourcePos;
  end;

  if Result = nil then
    Result := inherited MakeOperation(Left, Right);
end;

{---------------------------}
{ TFunDelphiCanOpNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiCanOpNode.GetPriority: Integer;
begin
  Result := 5;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiCanOpNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue, RightValue: ISepiReadableValue;
  WantingParams: ISepiWantingParams;
begin
  RequireReadableValue(Left, LeftValue);
  RequireReadableValue(Right, RightValue);

  LeftValue := TSepiConvertOperation.ConvertValue(
    SepiRoot.FindClass(TPlayer), LeftValue);
  RightValue := TSepiConvertOperation.ConvertValue(
    SystemUnit.LongString, RightValue);

  WantingParams := FieldSelection(SepiContext, LeftValue as ISepiExpression,
    'DoAction') as ISepiWantingParams;
  WantingParams.AddParam(RightValue as ISepiExpression);
  WantingParams.CompleteParams;

  Result := WantingParams as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{---------------------------}
{ TFunDelphiHasOpNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiHasOpNode.GetPriority: Integer;
begin
  Result := 5;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiHasOpNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  LeftValue, RightValue: ISepiReadableValue;
  CountProp: ISepiProperty;
begin
  RequireReadableValue(Left, LeftValue);
  RequireReadableValue(Right, RightValue);

  LeftValue := TSepiConvertOperation.ConvertValue(
    SepiRoot.FindClass(TPlayer), LeftValue);
  RightValue := TSepiConvertOperation.ConvertValue(
    SepiRoot.FindClass(TObjectDef), RightValue);

  CountProp := FieldSelection(SepiContext, RightValue as ISepiExpression,
    'Count') as ISepiProperty;
  Assert(CountProp.ParamCount = 1);
  CountProp.Params[0] := LeftValue as ISepiExpression;
  CountProp.CompleteParams;

  Result := CountProp as ISepiExpression;
  Result.SourcePos := SourcePos;

  Result := (Children[0] as TSepiBinaryOpNode).MakeOperation(
    Result, (Children[1] as TSepiExpressionNode).Expression);
end;

{-----------------------------------}
{ TFunDelphiHasComparisonNode class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiHasComparisonNode.GetOperation: TSepiOperation;
begin
  if ChildCount = 0 then
  begin
    Result := opCmpGE;
    Exit;
  end;

  case Children[0].SymbolClass of
    ntAtLeast:  Result := opCmpGE;
    ntAtMost:   Result := opCmpLE;
    ntMoreThan: Result := opCmpGT;
    ntLessThan: Result := opCmpLT;
    ntExactly:  Result := opCmpEQ;
  else
    Assert(False);
    Result := opCmpGE;
  end;
end;

{-----------------------------}
{ TFunDelphiUnaryOpNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiUnaryOpNode.GetOperation: TSepiOperation;
begin
  case Children[0].SymbolClass of
    tkMinus:
      Result := opNegate;
    tkNot:
      Result := opNot;
  else
    Result := opAdd;
  end;
end;

{------------------------------------------}
{ TFunDelphiArrayIndicesModifierNode class }
{------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiArrayIndicesModifierNode.CompileProperty(
  const Prop: ISepiProperty);
var
  WantingParams: ISepiWantingParams;
  I: Integer;
begin
  if (ChildCount = 3) and (Prop.ParamCount = 1) then
  begin
    WantingParams := TSepiMethodCall.Create(
      SepiRoot.FindComponent('ScUtils.Point3D') as TSepiMethod);
    WantingParams.AttachToExpression(TSepiExpression.Create(Base));

    for I := 0 to 2 do
      WantingParams.AddParam((Children[I] as TSepiExpressionNode).Expression);
    WantingParams.CompleteParams;

    Prop.Params[0] := WantingParams as ISepiExpression;
    Prop.CompleteParams;

    SetExpression(Base);
    Exit;
  end;

  inherited;
end;

{--------------------------------------------}
{ TFunDelphiFieldSelectionModifierNode class }
{--------------------------------------------}

function TFunDelphiFieldSelectionModifierNode.MakeFieldSelection: ISepiExpression;
begin
  Result := FieldSelection(SepiContext, Base, Children[0].AsText);
end;

{-----------------------------}
{ TFunDelphiActionsNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiActionsNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  ActionNames: TStringDynArray;
  I: Integer;
  ActionName: string;
begin
  (Child as TSepiIdentifierDeclListNode).GetIdentifierList(ActionNames);

  for I := 0 to Length(ActionNames)-1 do
  begin
    ActionName := ActionNames[I];
    TSepiConstant.Create(SepiContext, ActionPrefix+ActionName, ActionName);
  end;

  inherited;
end;

{--------------------------------}
{ TFunDelphiAttributesNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiAttributesNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  AttributeNames: TStringDynArray;
  I: Integer;
  AttributeName: string;
begin
  (Child as TSepiIdentifierDeclListNode).GetIdentifierList(AttributeNames);

  for I := 0 to Length(AttributeNames)-1 do
  begin
    AttributeName := AttributeNames[I];
    TSepiConstant.Create(SepiContext, AttributePrefix+AttributeName,
      AttributeName);
  end;

  inherited;
end;

{-----------------------------------}
{ TFunDelphiComponentDeclNode class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiComponentDeclNode.BeginParsing;
var
  SepiMethod: TSepiMethod;
begin
  inherited;

  SepiMethod := SepiContext.FindComponent('InitializeUnit') as TSepiMethod;
  FInitializeUnitCompiler := UnitCompiler.FindMethodCompiler(SepiMethod);
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiComponentDeclNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
var
  ComponentID: string;
begin
  if Child is TSepiIdentifierDeclarationNode then
  begin
    ComponentID := TSepiIdentifierDeclarationNode(Child).Identifier;
    FIDConstant := TSepiConstant.Create(SepiContext,
      ComponentIDPrefix+ComponentID, ComponentID);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiComponentDeclNode.EndParsing;
begin
  (RootNode as TFunDelphiRootNode).AddComponentDeclNode(Self);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiComponentDeclNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  Result := MethodResolveIdent(FInitializeUnitCompiler, Identifier);
end;

{*
  Construit les instructions d'initialisation
  @param Compiler       Compilateur
  @param Instructions   Instructions
*}
procedure TFunDelphiComponentDeclNode.MakeInitialize(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
var
  Expression, IDExpr: ISepiExpression;
  ComponentClass: TSepiClass;
  TypeExpr: ISepiTypeExpression;
  WantingParams: ISepiWantingParams;
  ParamsNode: TSepiParseTreeNode;
  I: Integer;
  Executable: ISepiExecutable;
begin
  Expression := MethodResolveIdent(Compiler, Children[1].AsText);
  if not CheckIdentFound(Expression, Children[1].AsText, Children[1]) then
    Exit;
  Expression.SourcePos := Children[1].SourcePos;

  ComponentClass := TSepiClass(SepiRoot.FindClass(TFunLabyComponent));

  if (not Supports(Expression, ISepiTypeExpression, TypeExpr)) or
    (not (TypeExpr.ExprType is TSepiClass)) or
    (not TSepiClass(TypeExpr.ExprType).ClassInheritsFrom(ComponentClass)) then
  begin
    Expression.MakeError(Format(SClassOfRequired, [ComponentClass.Name]));
    Exit;
  end;

  if TSepiClass(TypeExpr.ExprType).ClassInheritsFrom(
    TSepiClass(SepiRoot.FindClass(TSquareComponent))) then
  begin
    FRegisterable := True;
  end;

  Expression := FieldSelection(Compiler.SepiMethod, Expression, CreateName);
  Assert(Expression <> nil);
  Expression.SourcePos := Children[1].SourcePos;

  WantingParams := Expression as ISepiWantingParams;

  IDExpr := TSepiExpression.Create(Compiler);
  IDExpr.SourcePos := Children[0].SourcePos;
  ISepiExpressionPart(TSepiTrueConstValue.Create(
    IDConstant)).AttachToExpression(IDExpr);

  WantingParams.AddParam(MethodResolveIdent(Compiler, MasterName));
  WantingParams.AddParam(IDExpr);

  ParamsNode := Children[2];
  for I := 0 to ParamsNode.ChildCount-1 do
    WantingParams.AddParam(
      (ParamsNode.Children[I] as TSepiExpressionNode).Expression);

  WantingParams.CompleteParams;

  Executable := WantingParams as ISepiExecutable;
  Executable.CompileExecute(Compiler, Instructions);
end;

{*
  Construit les instructions d'enregistrement
  @param Compiler       Compilateur
  @param Instructions   Instructions
*}
procedure TFunDelphiComponentDeclNode.MakeRegister(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList);
var
  ComponentExpr: ISepiExpression;
  ProcValue: ISepiReadableValue;
  WantingParams: ISepiWantingParams;
  Executable: ISepiExecutable;
begin
  if not FRegisterable then
    Exit;

  ComponentExpr := MethodResolveIdent(Compiler, Children[0].AsText);
  Assert(ComponentExpr <> nil);
  ComponentExpr.SourcePos := Children[0].SourcePos;

  ProcValue := MethodResolveIdent(Compiler,
    'RegisterSingleComponentProc') as ISepiReadableValue;
  Assert(ProcValue.ValueType is TSepiMethodRefType);

  WantingParams := TSepiMethodRefCall.Create(ProcValue, True);
  WantingParams.AttachToExpression(TSepiExpression.Create(ComponentExpr));

  WantingParams.AddParam(ComponentExpr);

  WantingParams.CompleteParams;

  Executable := WantingParams as ISepiExecutable;
  Executable.CompileExecute(Compiler, Instructions);
end;

{------------------------------}
{ TFunDelphiClassDefNode class }
{------------------------------}

{*
  Complète les méthodes qui doivent l'être
*}
procedure TFunDelphiClassDefNode.CompleteMethods;
const
  MethodNames: array[0..1] of string = (AbleToName, UseForName);
var
  I: Integer;
  SepiMethod: TSepiMethod;
  Compiler: TSepiMethodCompiler;
begin
  for I := Low(MethodNames) to High(MethodNames) do
  begin
    SepiMethod := SepiClass.GetComponent(MethodNames[I]) as TSepiMethod;
    if SepiMethod = nil then
      Continue;

    Compiler := UnitCompiler.FindMethodCompiler(SepiMethod);

    CompilePureInheritedCall(Compiler);
  end;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiClassDefNode.GetSepiContext: TSepiComponent;
begin
  if SepiClass <> nil then
    Result := SepiClass
  else
    Result := inherited GetSepiContext;
end;

{*
  Classe maître
  @return Classe maître
*}
function TFunDelphiClassDefNode.GetMasterClass: TSepiClass;
begin
  Result := SystemUnit.TObject;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiClassDefNode.ChildEndParsing(Child: TSepiParseTreeNode);
var
  ClassName: string;
  ParentClass: TSepiClass;
begin
  if Child is TFunDelphiParentClassNode then
  begin
    ClassName := Children[0].AsText;
    ParentClass := TFunDelphiParentClassNode(Child).CompileParentClass(
      MasterClass);

    FSepiClass := TSepiClass.Create(SepiContext, ClassName, ParentClass);
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiClassDefNode.EndParsing;
begin
  CompleteMethods;
  SepiClass.Complete;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiClassDefNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
var
  Component: TSepiComponent;
begin
  if SepiClass <> nil then
  begin
    Component := SepiContext.LookFor(Identifier);
    if Component <> nil then
    begin
      Result := MakeExpression;
      AddComponentToExpression(Result, Component);
      Exit;
    end;
  end;

  Result := inherited ResolveIdent(Identifier);
end;

{----------------------------}
{ TFunDelphiPluginNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiPluginNode.GetMasterClass: TSepiClass;
begin
  Result := TSepiClass(SepiRoot.FindClass(TPlugin));
end;

{----------------------------}
{ TFunDelphiObjectNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiObjectNode.GetMasterClass: TSepiClass;
begin
  Result := TSepiClass(SepiRoot.FindClass(TObjectDef));
end;

{---------------------------}
{ TFunDelphiFieldNode class }
{---------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiFieldNode.GetMasterClass: TSepiClass;
begin
  Result := TSepiClass(SepiRoot.FindClass(TField));
end;

{----------------------------}
{ TFunDelphiEffectNode class }
{----------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiEffectNode.GetMasterClass: TSepiClass;
begin
  Result := TSepiClass(SepiRoot.FindClass(TEffect));
end;

{--------------------------}
{ TFunDelphiToolNode class }
{--------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiToolNode.GetMasterClass: TSepiClass;
begin
  Result := TSepiClass(SepiRoot.FindClass(TTool));
end;

{------------------------------}
{ TFunDelphiObstacleNode class }
{------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiObstacleNode.GetMasterClass: TSepiClass;
begin
  Result := TSepiClass(SepiRoot.FindClass(TObstacle));
end;

{---------------------------------}
{ TFunDelphiParentClassNode class }
{---------------------------------}

{*
  Compile la classe parent
  @param MasterClass   Classe maître (par défaut)
  @return Classe compilée
*}
function TFunDelphiParentClassNode.CompileParentClass(
  MasterClass: TSepiClass): TSepiClass;
begin
  if ChildCount = 0 then
    Result := nil
  else
    Result := TSepiClass(LookForOrError(Children[0], TSepiClass,
      SClassTypeRequired));

  if Result = nil then
    Result := MasterClass
  else if not Result.ClassInheritsFrom(MasterClass) then
  begin
    Children[0].MakeError(Format(
      SClassOfRequired, [MasterClass.DisplayName]));
    Result := MasterClass;
  end;
end;

{--------------------------------}
{ TFunDelphiClassFieldNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiClassFieldNode.BeginParsing;
begin
  inherited;

  SepiContext.CurrentVisibility := mvProtected;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiClassFieldNode.EndParsing;
begin
  inherited;

  SepiContext.CurrentVisibility := mvPublic;
end;

{---------------------------------------}
{ TFunDelphiMethodDeclAndImplNode class }
{---------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiMethodDeclAndImplNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiMethodBodyNode then
    TSepiMethodBodyNode(Child).SetSepiMethod(SepiMethod);
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiMethodDeclAndImplNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  if Child is TFunDelphiMethodDeclNode then
    FSepiMethod := TFunDelphiMethodDeclNode(Child).SepiMethod;

  inherited;
end;

{--------------------------------}
{ TFunDelphiMethodDeclNode class }
{--------------------------------}

{*
  Méthode déclarée
  @return Méthode déclarée
*}
function TFunDelphiMethodDeclNode.GetSepiMethod: TSepiMethod;
var
  Created: TSepiComponent;
begin
  Created := SepiContext.FindComponent(Name);

  if Created is TSepiOverloadedMethod then
    with TSepiOverloadedMethod(Created) do
      Created := Methods[MethodCount-1];

  Result := Created as TSepiMethod;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiMethodDeclNode.EndParsing;
begin
  inherited;

  SepiContext.CurrentVisibility := mvPublic;
end;

{---------------------------------------}
{ TFunDelphiConstructorHeaderNode class }
{---------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiConstructorHeaderNode.BeginParsing;
begin
  inherited;

  Signature.Kind := skConstructor;

  TSepiParam.Create(Signature, 'AMaster', SepiRoot.FindClass(TMaster));
  TSepiParam.Create(Signature, 'AID',
    SepiRoot.FindType('FunLabyUtils.TComponentID'), pkConst);
end;

{--------------------------------------}
{ TFunDelphiDestructorHeaderNode class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiDestructorHeaderNode.BeginParsing;
begin
  inherited;

  Signature.Kind := skDestructor;
end;

{----------------------------------}
{ TFunDelphiAutoOverrideNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiAutoOverrideNode.GetLinkKind: TMethodLinkKind;
begin
  Result := mlkOverride;
end;

{---------------------------}
{ TFunDelphiImageNode class }
{---------------------------}

{*
  Construit un constructeur vide
  @return Constructeur créé
*}
function TFunDelphiImageNode.MakeEmptyConstructor: TSepiMethod;
var
  Previous: TSepiMethod;
  Compiler: TSepiMethodCompiler;
begin
  Previous := (SepiContext as TSepiClass).LookForMember(
    CreateName) as TSepiMethod;

  Result := TSepiMethod.Create(SepiContext, CreateName, nil,
    Previous.Signature);
  Compiler := UnitCompiler.FindMethodCompiler(Result, True);

  CompilePureInheritedCall(Compiler);
end;

{*
  Ajoute une image
  @param ImageValue   Valeur chaîne de l'image à ajouter
*}
procedure TFunDelphiImageNode.AddImage(const ImageValue: ISepiReadableValue);
var
  Expression: ISepiExpression;
  WantingParams: ISepiWantingParams;
  Executable: ISepiExecutable;
begin
  Expression := MethodResolveIdent(FCompiler, 'Painter');
  Expression := FieldSelection(SepiContext, Expression, 'ImgNames');
  Expression := FieldSelection(SepiContext, Expression, 'Add');

  WantingParams := Expression as ISepiWantingParams;
  WantingParams.AddParam(ImageValue as ISepiExpression);
  WantingParams.CompleteParams;
  WantingParams.AttachToExpression(Expression);

  Executable := Expression as ISepiExecutable;
  Executable.CompileExecute(FCompiler, FInstructions);
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiImageNode.BeginParsing;
var
  SepiConstructor: TSepiMethod;
begin
  inherited;

  SepiConstructor := SepiContext.GetComponent('Create') as TSepiMethod;

  if SepiConstructor = nil then
    SepiConstructor := MakeEmptyConstructor;

  FCompiler := UnitCompiler.FindMethodCompiler(SepiConstructor);
  FInstructions := FCompiler.Instructions;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiImageNode.ChildBeginParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  (Child as TSepiConstExpressionNode).ValueType := SystemUnit.LongString;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiImageNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  AddImage((Child as TSepiConstExpressionNode).AsReadableValue);

  inherited;
end;

{---------------------------------}
{ TFunDelphiEventHeaderNode class }
{---------------------------------}

{*
  Remplit la signature
*}
procedure TFunDelphiEventHeaderNode.FillSignature;
var
  InheritedMeta: TSepiComponent;
  InheritedSignature: TSepiSignature;
  I: Integer;
begin
  InheritedMeta := (SepiContext as TSepiClass).LookForMember(Name);
  if not (InheritedMeta is TSepiMethod) then
    Exit; // Error message will be produced by TSepiMethodDeclarationNode

  InheritedSignature := TSepiMethod(InheritedMeta).Signature;

  if not (InheritedSignature.Kind in [skObjectProcedure, skObjectFunction]) then
  begin
    MakeError(SMethodNotFoundInBaseClass);
    Signature.Kind := skObjectProcedure;
    Exit;
  end;

  Signature.Kind := InheritedSignature.Kind;
  Signature.ReturnType := InheritedSignature.ReturnType;
  Signature.CallingConvention := InheritedSignature.CallingConvention;

  for I := 0 to InheritedSignature.ParamCount-1 do
    TSepiParam.Clone(Signature, InheritedSignature.Params[I]);

  SepiContext.CurrentVisibility := InheritedMeta.Visibility;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiEventHeaderNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiIdentifierDeclarationNode then
    FillSignature;
end;

{----------------------------------}
{ TFunDelphiPluginActionNode class }
{----------------------------------}

{*
  Construit l'instruction en cas de succès
*}
procedure TFunDelphiPluginActionNode.BuildSuccess;
const
  TrueValue: Boolean = True;
var
  MoveInstr: TSepiAsmMove;
  ExitInstr: TSepiExit;
begin
  MoveInstr := TSepiAsmMove.Create(AbleToCompiler, SizeOf(Boolean));
  MoveInstr.Destination.SetSpace(msResult);
  MoveInstr.Source.SetSpace(msConstant);
  MoveInstr.Source.SetConstant(TrueValue);
  AbleToInstructions.Add(MoveInstr);

  ExitInstr := TSepiExit.Create(AbleToCompiler);
  AbleToInstructions.Add(ExitInstr);
end;

{*
  Construit l'instruction if
  @param Instructions   Instructions contenantes
  @return Instructions exécutées si le test est True
*}
function TFunDelphiPluginActionNode.BuildIfStatement(
  Instructions: TSepiInstructionList): TSepiInstructionList;
var
  Compiler: TSepiMethodCompiler;
  ActionVarValue, ActionNameValue, CmpValue, TestValue: ISepiReadableValue;
  I: Integer;
  IfInstr: TSepiIfThenElse;
begin
  Compiler := Instructions.MethodCompiler;
  ActionVarValue := MethodResolveIdent(Compiler,
    ActionName) as ISepiReadableValue;
  TestValue := nil;

  for I := 0 to Children[0].ChildCount-1 do
  begin
    ActionNameValue :=
      (Children[0].Children[I] as TSepiConstExpressionNode).AsReadableValue(
      SystemUnit.LongString);

    CmpValue := TSepiBinaryOperation.MakeOperation(opCmpEQ,
      ActionVarValue, ActionNameValue);

    if TestValue = nil then
      TestValue := CmpValue
    else
      TestValue := TSepiBinaryOperation.MakeOperation(opOr,
        TestValue, CmpValue);
  end;

  IfInstr := TSepiIfThenElse.Create(Compiler);
  IfInstr.TestValue := TestValue;
  Instructions.Add(IfInstr);

  Result := IfInstr.TrueInstructions;
end;

{*
  Construit la condition
  @param Expression   Expression
*}
procedure TFunDelphiPluginActionNode.BuildCondition(
  const Expression: ISepiExpression);
var
  IfInstr: TSepiIfThenElse;
begin
  if Expression = nil then
    Exit;

  IfInstr := TSepiIfThenElse.Create(AbleToCompiler);
  IfInstr.TestValue := Expression as ISepiReadableValue;
  AbleToInstructions.Add(IfInstr);

  FAbleToInstructions := IfInstr.TrueInstructions;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiPluginActionNode.GetMethodCompiler: TSepiMethodCompiler;
begin
  if AbleToCompiler <> nil then
    Result := AbleToCompiler
  else
    Result := inherited GetMethodCompiler;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiPluginActionNode.BeginParsing;
var
  SepiClass: TSepiClass;
  AbleToMethod: TSepiMethod;
begin
  inherited;

  SepiClass := SepiContext as TSepiClass;
  AbleToMethod := SepiClass.LookForMember(AbleToName) as TSepiMethod;
  Assert(AbleToMethod <> nil);

  if AbleToMethod.Owner <> SepiClass then
  begin
    AbleToMethod := TSepiMethod.Create(SepiClass, AbleToName, nil,
      AbleToMethod.Signature, mlkOverride);
  end;

  FAbleToCompiler := UnitCompiler.FindMethodCompiler(AbleToMethod, True);

  FAbleToInstructions := TSepiInstructionList.Create(AbleToCompiler);
  AbleToCompiler.Instructions.Add(AbleToInstructions);
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiPluginActionNode.ChildEndParsing(Child: TSepiParseTreeNode);
begin
  if Child.SymbolClass = ntActionList then
    FAbleToInstructions := BuildIfStatement(AbleToInstructions)
  else if Child is TFunDelphiActionConditionNode then
  begin
    BuildCondition(TFunDelphiActionConditionNode(Child).Expression);
    BuildSuccess;
  end;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TFunDelphiPluginActionNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  if MethodCompiler <> nil then
    Result := MethodResolveIdent(MethodCompiler, Identifier)
  else
    Result := inherited ResolveIdent(Identifier);
end;

{----------------------------------}
{ TFunDelphiObjectActionNode class }
{----------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiObjectActionNode.BuildCondition(
  const Expression: ISepiExpression);
var
  ActualExpression: ISepiExpression;
  CountProp: ISepiProperty;
  Zero: ISepiReadableValue;
begin
  if Expression <> nil then
    ActualExpression := Expression
  else
  begin
    CountProp := ResolveIdent('Count') as ISepiProperty;
    Assert((not CountProp.ParamsCompleted) and (CountProp.ParamCount = 1));

    CountProp.Params[0] := ResolveIdent('Player');
    CountProp.CompleteParams;

    Zero := TSepiTrueConstValue.Create(SystemUnit.Integer, 0);
    Zero.AttachToExpression(TSepiExpression.Create(CountProp.Params[0]));

    ActualExpression := TSepiBinaryOperation.MakeOperation(opCmpGT,
      CountProp as ISepiReadableValue, Zero) as ISepiExpression;
  end;

  inherited BuildCondition(ActualExpression);
end;

{*
  [@inheritDoc]
*}
function TFunDelphiObjectActionNode.GetMethodCompiler: TSepiMethodCompiler;
begin
  if UseForCompiler <> nil then
    Result := UseForCompiler
  else
    Result := inherited GetMethodCompiler;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiObjectActionNode.ChildBeginParsing(
  Child: TSepiParseTreeNode);
var
  SepiClass: TSepiClass;
  UseForMethod: TSepiMethod;
  UseForInstructions: TSepiInstructionList;
begin
  inherited;

  if Child is TSepiInstructionNode then
  begin
    SepiClass := SepiContext as TSepiClass;
    UseForMethod := SepiClass.LookForMember(UseForName) as TSepiMethod;
    Assert(UseForMethod <> nil);

    if UseForMethod.Owner <> SepiClass then
    begin
      UseForMethod := TSepiMethod.Create(SepiClass, UseForName, nil,
        UseForMethod.Signature, mlkOverride);
    end;

    FUseForCompiler := UnitCompiler.FindMethodCompiler(UseForMethod, True);

    UseForInstructions := TSepiInstructionList.Create(UseForCompiler);
    UseForCompiler.Instructions.Add(UseForInstructions);

    UseForInstructions := BuildIfStatement(UseForInstructions);

    TSepiInstructionNode(Child).InstructionList := UseForInstructions;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFunDelphiObjectActionNode.ChildEndParsing(
  Child: TSepiParseTreeNode);
begin
  inherited;

  if Child is TSepiInstructionNode then
  begin
    TSepiInstructionNode(Child).InstructionList.Add(
      TSepiExit.Create(MethodCompiler));
  end;
end;

{-------------------------------------}
{ TFunDelphiActionConditionNode class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiActionConditionNode.GetExpression: ISepiExpression;
begin
  if ChildCount = 0 then
    Result := nil
  else
    Result := (Children[0] as TSepiExpressionNode).Expression;
end;

{-----------------------------}
{ TFunDelphiRoutineNode class }
{-----------------------------}

{*
  [@inheritDoc]
*}
procedure TFunDelphiRoutineNode.EndParsing;
begin
  SepiContext.CurrentVisibility := mvPublic;

  inherited;
end;

{--------------------------------}
{ TFunDelphiMethodBodyNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiMethodBodyNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  Result := MethodResolveIdent(MethodCompiler, Identifier);
end;

{----------------------------------------}
{ TFunDelphiReceivesDiscardsOpNode class }
{----------------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiReceivesDiscardsOpNode.MakeOperation(
  const Left, Right: ISepiExpression): ISepiExpression;
var
  PlayerValue, ObjectValue, CountValue, NewCountValue: ISepiReadableValue;
  CountProp: ISepiProperty;
  WritableCount: ISepiWritableValue;
begin
  RequireReadableValue(Left, PlayerValue);
  RequireReadableValue(Right, ObjectValue);

  PlayerValue := TSepiConvertOperation.ConvertValue(
    SepiRoot.FindClass(TPlayer), PlayerValue);
  ObjectValue := TSepiConvertOperation.ConvertValue(
    SepiRoot.FindClass(TObjectDef), ObjectValue);

  CountProp := FieldSelection(SepiContext, ObjectValue as ISepiExpression,
    'Count') as ISepiProperty;
  Assert(CountProp.ParamCount = 1);
  CountProp.Params[0] := PlayerValue as ISepiExpression;
  CountProp.CompleteParams;

  Result := CountProp as ISepiExpression;
  Result.SourcePos := SourcePos;
  CountValue := Result as ISepiReadableValue;
  WritableCount := Result as ISepiWritableValue;

  NewCountValue := TSepiBinaryOperation.MakeOperation(Operation,
    CountValue, (Children[0] as TSepiExpressionNode).AsReadableValue(
    SystemUnit.Integer));

  Result := TSepiAssignmentOperation.MakeOperation(
    WritableCount, NewCountValue) as ISepiExpression;
  Result.SourcePos := SourcePos;
end;

{--------------------------------}
{ TFunDelphiReceivesOpNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiReceivesOpNode.GetOperation: TSepiOperation;
begin
  Result := opAdd;
end;

{--------------------------------}
{ TFunDelphiDiscardsOpNode class }
{--------------------------------}

{*
  [@inheritDoc]
*}
function TFunDelphiDiscardsOpNode.GetOperation: TSepiOperation;
begin
  Result := opSubtract;
end;

initialization
  InitNonTerminalClasses;
end.

