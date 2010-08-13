unit SepiFunDelphiParser;

interface

{$D-,L-}

uses
  SysUtils, Contnrs, SepiCompilerErrors, SepiParseTrees, SepiParserUtils,
  SepiLL1ParserUtils, SepiFunDelphiLexer;

const
  ChoiceCount = 315;
  FirstNonTerminal = 107;
  LastNonTerminal = 319;

  ntSource = 107; // Source
  ntImplSection = 108; // ImplSection
  ntIdentifier = 109; // Identifier
  ntUsesSection = 110; // UsesSection
  ntCommaIdentList = 111; // CommaIdentList
  ntCommaIdentDeclList = 112; // CommaIdentDeclList
  ntQualifiedIdent = 113; // QualifiedIdent
  ntIdentifierDecl = 114; // IdentifierDecl
  ntTypeName = 115; // TypeName
  ntInitializationExpression = 116; // InitializationExpression
  ntArrayInitializationExpression = 117; // ArrayInitializationExpression
  ntArrayInitialization = 118; // ArrayInitialization
  ntRecordInitializationExpression = 119; // RecordInitializationExpression
  ntRecordInitialization = 120; // RecordInitialization
  ntGUIDInitializationExpression = 121; // GUIDInitializationExpression
  ntGUIDInitialization = 122; // GUIDInitialization
  ntOtherInitializationExpression = 123; // OtherInitializationExpression
  ntOtherInitialization = 124; // OtherInitialization
  ntExpression = 125; // Expression
  ntNextExpression = 126; // NextExpression
  ntConstExpression = 127; // ConstExpression
  ntSingleExpr = 128; // SingleExpr
  ntUnaryOpExpr = 129; // UnaryOpExpr
  ntParenthesizedExpr = 130; // ParenthesizedExpr
  ntNextExprList = 131; // NextExprList
  ntNextExpr = 132; // NextExpr
  ntUnaryOpModifier = 133; // UnaryOpModifier
  ntDereferenceOp = 134; // DereferenceOp
  ntParameters = 135; // Parameters
  ntInnerParameters = 136; // InnerParameters
  ntParameter = 137; // Parameter
  ntSetOrOpenArrayBuilder = 138; // SetOrOpenArrayBuilder
  ntSetOrOpenArrayRange = 139; // SetOrOpenArrayRange
  ntExprListOrEmpty = 140; // ExprListOrEmpty
  ntArrayIndices = 141; // ArrayIndices
  ntExprList = 142; // ExprList
  ntFieldSelection = 143; // FieldSelection
  ntSingleValue = 144; // SingleValue
  ntIntegerConst = 145; // IntegerConst
  ntFloatConst = 146; // FloatConst
  ntStringConst = 147; // StringConst
  ntIdentifierSingleValue = 148; // IdentifierSingleValue
  ntInheritedSingleValue = 149; // InheritedSingleValue
  ntInheritedExpression = 150; // InheritedExpression
  ntPureInheritedExpression = 151; // PureInheritedExpression
  ntNilValue = 152; // NilValue
  ntSetValue = 153; // SetValue
  ntCaseOfSetValue = 154; // CaseOfSetValue
  ntSetRange = 155; // SetRange
  ntBinaryOp = 156; // BinaryOp
  ntDelphiBinaryOp = 157; // DelphiBinaryOp
  ntInOperation = 158; // InOperation
  ntIsOperation = 159; // IsOperation
  ntAsOperation = 160; // AsOperation
  ntCanOp = 161; // CanOp
  ntHasOp = 162; // HasOp
  ntHasComparison = 163; // HasComparison
  ntAtLeastOrAtMost = 164; // AtLeastOrAtMost
  ntAtLeast = 165; // AtLeast
  ntAtMost = 166; // AtMost
  ntMoreThan = 167; // MoreThan
  ntLessThan = 168; // LessThan
  ntExactly = 169; // Exactly
  ntUnaryOp = 170; // UnaryOp
  ntAddressOfOp = 171; // AddressOfOp
  ntConstSection = 172; // ConstSection
  ntConstKeyWord = 173; // ConstKeyWord
  ntConstDecl = 174; // ConstDecl
  ntInnerConstDecl = 175; // InnerConstDecl
  ntActionsSection = 176; // ActionsSection
  ntAttributesSection = 177; // AttributesSection
  ntMessagesSection = 178; // MessagesSection
  ntMessageDecl = 179; // MessageDecl
  ntComponentsSection = 180; // ComponentsSection
  ntComponent = 181; // Component
  ntComponentParameters = 182; // ComponentParameters
  ntComponentParameter = 183; // ComponentParameter
  ntPluginSection = 184; // PluginSection
  ntPluginMembers = 185; // PluginMembers
  ntPluginMember = 186; // PluginMember
  ntObjectSection = 187; // ObjectSection
  ntObjectMembers = 188; // ObjectMembers
  ntObjectMember = 189; // ObjectMember
  ntFieldSection = 190; // FieldSection
  ntEffectSection = 191; // EffectSection
  ntToolSection = 192; // ToolSection
  ntObstacleSection = 193; // ObstacleSection
  ntSquareCompMembers = 194; // SquareCompMembers
  ntSquareCompMember = 195; // SquareCompMember
  ntPosComponentSection = 196; // PosComponentSection
  ntVehicleSection = 197; // VehicleSection
  ntPosCompMembers = 198; // PosCompMembers
  ntPosCompMember = 199; // PosCompMember
  ntCreatorSection = 200; // CreatorSection
  ntCreatorItemClass = 201; // CreatorItemClass
  ntCreatorMembers = 202; // CreatorMembers
  ntCreatorMember = 203; // CreatorMember
  ntClassSection = 204; // ClassSection
  ntClassMembers = 205; // ClassMembers
  ntClassMember = 206; // ClassMember
  ntParentClass = 207; // ParentClass
  ntFields = 208; // Fields
  ntField = 209; // Field
  ntAutoOverride = 210; // AutoOverride
  ntName = 211; // Name
  ntHint = 212; // Hint
  ntCategory = 213; // Category
  ntZIndex = 214; // ZIndex
  ntImage = 215; // Image
  ntEvent = 216; // Event
  ntEventHeader = 217; // EventHeader
  ntInnerEventHeader = 218; // InnerEventHeader
  ntMethodEventHeader = 219; // MethodEventHeader
  ntMessageEventHeader = 220; // MessageEventHeader
  ntMessageMethodName = 221; // MessageMethodName
  ntMessageEventLinkKind = 222; // MessageEventLinkKind
  ntPluginAction = 223; // PluginAction
  ntObjectAction = 224; // ObjectAction
  ntActionList = 225; // ActionList
  ntActionCondition = 226; // ActionCondition
  ntActionOnUse = 227; // ActionOnUse
  ntRoutineImpl = 228; // RoutineImpl
  ntRoutineImplHeader = 229; // RoutineImplHeader
  ntRoutineKind = 230; // RoutineKind
  ntRoutineName = 231; // RoutineName
  ntRoutineVisibility = 232; // RoutineVisibility
  ntForwardOrMethodBody = 233; // ForwardOrMethodBody
  ntForwardMarker = 234; // ForwardMarker
  ntRoutineSignature = 235; // RoutineSignature
  ntParamList = 236; // ParamList
  ntReturnType = 237; // ReturnType
  ntParam = 238; // Param
  ntParamKind = 239; // ParamKind
  ntParamNameList = 240; // ParamNameList
  ntParamName = 241; // ParamName
  ntParamType = 242; // ParamType
  ntMethodBody = 243; // MethodBody
  ntCommonMethodBody = 244; // CommonMethodBody
  ntInMethodSection = 245; // InMethodSection
  ntLocalVarSection = 246; // LocalVarSection
  ntLocalVar = 247; // LocalVar
  ntInstructionList = 248; // InstructionList
  ntInstruction = 249; // Instruction
  ntNoInstruction = 250; // NoInstruction
  ntBeginEndBlock = 251; // BeginEndBlock
  ntIfThenElseInstruction = 252; // IfThenElseInstruction
  ntElseBranch = 253; // ElseBranch
  ntCaseOfInstruction = 254; // CaseOfInstruction
  ntCaseOfClause = 255; // CaseOfClause
  ntCaseOfElseClause = 256; // CaseOfElseClause
  ntWhileInstruction = 257; // WhileInstruction
  ntRepeatInstruction = 258; // RepeatInstruction
  ntForInstruction = 259; // ForInstruction
  ntForControlVar = 260; // ForControlVar
  ntForToDownTo = 261; // ForToDownTo
  ntForTo = 262; // ForTo
  ntForDownTo = 263; // ForDownTo
  ntTryInstruction = 264; // TryInstruction
  ntNextTryInstruction = 265; // NextTryInstruction
  ntExceptClause = 266; // ExceptClause
  ntNextExceptClause = 267; // NextExceptClause
  ntMultiOn = 268; // MultiOn
  ntOnClause = 269; // OnClause
  ntExceptionVarAndType = 270; // ExceptionVarAndType
  ntMultiOnElseClause = 271; // MultiOnElseClause
  ntFinallyClause = 272; // FinallyClause
  ntRaiseInstruction = 273; // RaiseInstruction
  ntExpressionInstruction = 274; // ExpressionInstruction
  ntExecutableExpression = 275; // ExecutableExpression
  ntExecutableOp = 276; // ExecutableOp
  ntAssignmentOp = 277; // AssignmentOp
  ntReceivesOp = 278; // ReceivesOp
  ntDiscardsOp = 279; // DiscardsOp
  ntWithInstruction = 280; // WithInstruction
  ntWithEx = 281; // WithEx
  ntInnerWith = 282; // InnerWith
  ntPriv0 = 283; // Priv0
  ntPriv1 = 284; // Priv1
  ntPriv2 = 285; // Priv2
  ntPriv3 = 286; // Priv3
  ntPriv4 = 287; // Priv4
  ntPriv5 = 288; // Priv5
  ntPriv6 = 289; // Priv6
  ntPriv7 = 290; // Priv7
  ntPriv8 = 291; // Priv8
  ntPriv9 = 292; // Priv9
  ntPriv10 = 293; // Priv10
  ntPriv11 = 294; // Priv11
  ntPriv12 = 295; // Priv12
  ntPriv13 = 296; // Priv13
  ntPriv14 = 297; // Priv14
  ntPriv15 = 298; // Priv15
  ntPriv16 = 299; // Priv16
  ntPriv17 = 300; // Priv17
  ntPriv18 = 301; // Priv18
  ntPriv19 = 302; // Priv19
  ntPriv20 = 303; // Priv20
  ntPriv21 = 304; // Priv21
  ntPriv22 = 305; // Priv22
  ntPriv23 = 306; // Priv23
  ntPriv24 = 307; // Priv24
  ntPriv25 = 308; // Priv25
  ntPriv26 = 309; // Priv26
  ntPriv27 = 310; // Priv27
  ntPriv28 = 311; // Priv28
  ntPriv29 = 312; // Priv29
  ntPriv30 = 313; // Priv30
  ntPriv31 = 314; // Priv31
  ntPriv32 = 315; // Priv32
  ntPriv33 = 316; // Priv33
  ntPriv34 = 317; // Priv34
  ntPriv35 = 318; // Priv35
  ntPriv36 = 319; // Priv36

type
  {*
    Analyseur syntaxique
    @author sjrd
    @version 1.0
  *}
  TSepiFunDelphiParser = class(TSepiCustomLL1Parser)
  private
    procedure PushChoice1;
    procedure PushChoice2;
    procedure PushChoice3;
    procedure PushChoice4;
    procedure PushChoice5;
    procedure PushChoice6;
    procedure PushChoice7;
    procedure PushChoice8;
    procedure PushChoice9;
    procedure PushChoice10;
    procedure PushChoice11;
    procedure PushChoice12;
    procedure PushChoice13;
    procedure PushChoice14;
    procedure PushChoice15;
    procedure PushChoice16;
    procedure PushChoice17;
    procedure PushChoice18;
    procedure PushChoice19;
    procedure PushChoice20;
    procedure PushChoice21;
    procedure PushChoice22;
    procedure PushChoice23;
    procedure PushChoice24;
    procedure PushChoice25;
    procedure PushChoice26;
    procedure PushChoice27;
    procedure PushChoice28;
    procedure PushChoice29;
    procedure PushChoice30;
    procedure PushChoice31;
    procedure PushChoice32;
    procedure PushChoice33;
    procedure PushChoice34;
    procedure PushChoice35;
    procedure PushChoice36;
    procedure PushChoice37;
    procedure PushChoice38;
    procedure PushChoice39;
    procedure PushChoice40;
    procedure PushChoice41;
    procedure PushChoice42;
    procedure PushChoice43;
    procedure PushChoice44;
    procedure PushChoice45;
    procedure PushChoice46;
    procedure PushChoice47;
    procedure PushChoice48;
    procedure PushChoice49;
    procedure PushChoice50;
    procedure PushChoice51;
    procedure PushChoice52;
    procedure PushChoice53;
    procedure PushChoice54;
    procedure PushChoice55;
    procedure PushChoice56;
    procedure PushChoice57;
    procedure PushChoice58;
    procedure PushChoice59;
    procedure PushChoice60;
    procedure PushChoice61;
    procedure PushChoice62;
    procedure PushChoice63;
    procedure PushChoice64;
    procedure PushChoice65;
    procedure PushChoice66;
    procedure PushChoice67;
    procedure PushChoice68;
    procedure PushChoice69;
    procedure PushChoice70;
    procedure PushChoice71;
    procedure PushChoice72;
    procedure PushChoice73;
    procedure PushChoice74;
    procedure PushChoice75;
    procedure PushChoice76;
    procedure PushChoice77;
    procedure PushChoice78;
    procedure PushChoice79;
    procedure PushChoice80;
    procedure PushChoice81;
    procedure PushChoice82;
    procedure PushChoice83;
    procedure PushChoice84;
    procedure PushChoice85;
    procedure PushChoice86;
    procedure PushChoice87;
    procedure PushChoice88;
    procedure PushChoice89;
    procedure PushChoice90;
    procedure PushChoice91;
    procedure PushChoice92;
    procedure PushChoice93;
    procedure PushChoice94;
    procedure PushChoice95;
    procedure PushChoice96;
    procedure PushChoice97;
    procedure PushChoice98;
    procedure PushChoice99;
    procedure PushChoice100;
    procedure PushChoice101;
    procedure PushChoice102;
    procedure PushChoice103;
    procedure PushChoice104;
    procedure PushChoice105;
    procedure PushChoice106;
    procedure PushChoice107;
    procedure PushChoice108;
    procedure PushChoice109;
    procedure PushChoice110;
    procedure PushChoice111;
    procedure PushChoice112;
    procedure PushChoice113;
    procedure PushChoice114;
    procedure PushChoice115;
    procedure PushChoice116;
    procedure PushChoice117;
    procedure PushChoice118;
    procedure PushChoice119;
    procedure PushChoice120;
    procedure PushChoice121;
    procedure PushChoice122;
    procedure PushChoice123;
    procedure PushChoice124;
    procedure PushChoice125;
    procedure PushChoice126;
    procedure PushChoice127;
    procedure PushChoice128;
    procedure PushChoice129;
    procedure PushChoice130;
    procedure PushChoice131;
    procedure PushChoice132;
    procedure PushChoice133;
    procedure PushChoice134;
    procedure PushChoice135;
    procedure PushChoice136;
    procedure PushChoice137;
    procedure PushChoice138;
    procedure PushChoice139;
    procedure PushChoice140;
    procedure PushChoice141;
    procedure PushChoice142;
    procedure PushChoice143;
    procedure PushChoice144;
    procedure PushChoice145;
    procedure PushChoice146;
    procedure PushChoice147;
    procedure PushChoice148;
    procedure PushChoice149;
    procedure PushChoice150;
    procedure PushChoice151;
    procedure PushChoice152;
    procedure PushChoice153;
    procedure PushChoice154;
    procedure PushChoice155;
    procedure PushChoice156;
    procedure PushChoice157;
    procedure PushChoice158;
    procedure PushChoice159;
    procedure PushChoice160;
    procedure PushChoice161;
    procedure PushChoice162;
    procedure PushChoice163;
    procedure PushChoice164;
    procedure PushChoice165;
    procedure PushChoice166;
    procedure PushChoice167;
    procedure PushChoice168;
    procedure PushChoice169;
    procedure PushChoice170;
    procedure PushChoice171;
    procedure PushChoice172;
    procedure PushChoice173;
    procedure PushChoice174;
    procedure PushChoice175;
    procedure PushChoice176;
    procedure PushChoice177;
    procedure PushChoice178;
    procedure PushChoice179;
    procedure PushChoice180;
    procedure PushChoice181;
    procedure PushChoice182;
    procedure PushChoice183;
    procedure PushChoice184;
    procedure PushChoice185;
    procedure PushChoice186;
    procedure PushChoice187;
    procedure PushChoice188;
    procedure PushChoice189;
    procedure PushChoice190;
    procedure PushChoice191;
    procedure PushChoice192;
    procedure PushChoice193;
    procedure PushChoice194;
    procedure PushChoice195;
    procedure PushChoice196;
    procedure PushChoice197;
    procedure PushChoice198;
    procedure PushChoice199;
    procedure PushChoice200;
    procedure PushChoice201;
    procedure PushChoice202;
    procedure PushChoice203;
    procedure PushChoice204;
    procedure PushChoice205;
    procedure PushChoice206;
    procedure PushChoice207;
    procedure PushChoice208;
    procedure PushChoice209;
    procedure PushChoice210;
    procedure PushChoice211;
    procedure PushChoice212;
    procedure PushChoice213;
    procedure PushChoice214;
    procedure PushChoice215;
    procedure PushChoice216;
    procedure PushChoice217;
    procedure PushChoice218;
    procedure PushChoice219;
    procedure PushChoice220;
    procedure PushChoice221;
    procedure PushChoice222;
    procedure PushChoice223;
    procedure PushChoice224;
    procedure PushChoice225;
    procedure PushChoice226;
    procedure PushChoice227;
    procedure PushChoice228;
    procedure PushChoice229;
    procedure PushChoice230;
    procedure PushChoice231;
    procedure PushChoice232;
    procedure PushChoice233;
    procedure PushChoice234;
    procedure PushChoice235;
    procedure PushChoice236;
    procedure PushChoice237;
    procedure PushChoice238;
    procedure PushChoice239;
    procedure PushChoice240;
    procedure PushChoice241;
    procedure PushChoice242;
    procedure PushChoice243;
    procedure PushChoice244;
    procedure PushChoice245;
    procedure PushChoice246;
    procedure PushChoice247;
    procedure PushChoice248;
    procedure PushChoice249;
    procedure PushChoice250;
    procedure PushChoice251;
    procedure PushChoice252;
    procedure PushChoice253;
    procedure PushChoice254;
    procedure PushChoice255;
    procedure PushChoice256;
    procedure PushChoice257;
    procedure PushChoice258;
    procedure PushChoice259;
    procedure PushChoice260;
    procedure PushChoice261;
    procedure PushChoice262;
    procedure PushChoice263;
    procedure PushChoice264;
    procedure PushChoice265;
    procedure PushChoice266;
    procedure PushChoice267;
    procedure PushChoice268;
    procedure PushChoice269;
    procedure PushChoice270;
    procedure PushChoice271;
    procedure PushChoice272;
    procedure PushChoice273;
    procedure PushChoice274;
    procedure PushChoice275;
    procedure PushChoice276;
    procedure PushChoice277;
    procedure PushChoice278;
    procedure PushChoice279;
    procedure PushChoice280;
    procedure PushChoice281;
    procedure PushChoice282;
    procedure PushChoice283;
    procedure PushChoice284;
    procedure PushChoice285;
    procedure PushChoice286;
    procedure PushChoice287;
    procedure PushChoice288;
    procedure PushChoice289;
    procedure PushChoice290;
    procedure PushChoice291;
    procedure PushChoice292;
    procedure PushChoice293;
    procedure PushChoice294;
    procedure PushChoice295;
    procedure PushChoice296;
    procedure PushChoice297;
    procedure PushChoice298;
    procedure PushChoice299;
    procedure PushChoice300;
    procedure PushChoice301;
    procedure PushChoice302;
    procedure PushChoice303;
    procedure PushChoice304;
    procedure PushChoice305;
    procedure PushChoice306;
    procedure PushChoice307;
    procedure PushChoice308;
    procedure PushChoice309;
    procedure PushChoice310;
    procedure PushChoice311;
    procedure PushChoice312;
    procedure PushChoice313;
    procedure PushChoice314;
  protected
    function IsTerminal(Symbol: TSepiSymbolClass): Boolean; override;
    function IsNonTerminal(
      Symbol: TSepiSymbolClass): Boolean; override;

    procedure InitPushChoiceProcs; override;

    function GetExpectedString(
      ExpectedSymbol: TSepiSymbolClass): string; override;

    function GetParsingTable(NonTerminalClass: TSepiSymbolClass;
      TerminalClass: TSepiSymbolClass): TRuleID; override;

    function GetNonTerminalClass(
      Symbol: TSepiSymbolClass): TSepiNonTerminalClass; override;
  end;

var
  NonTerminalClasses:
    array[FirstNonTerminal..LastNonTerminal] of TSepiNonTerminalClass;

implementation

type
  TParsingTable = array[FirstNonTerminal..LastNonTerminal,
    FirstTerminal..LastTerminal] of TRuleID;

const
  ParsingTable: TParsingTable = (
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  16,  16,  -1,  -1,  17,  17,  -1,  -1,  -1,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,   5,   3,   2,   4,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  18,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  19,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  20,   0,   0,  -1,  -1,   0,   0,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  21,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  21,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  22,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  22,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  23,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  23,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  24,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  24,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  25,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  25,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  29,  29,  29,  29,  29,  -1,  29,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  29,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  29,  29,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  29,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  29,  29,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  29,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  31,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  32,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  33,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  34,  34,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  36,  35,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  37,  37,  37,  37,  37,  -1,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  37,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  37,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  38,  38,  38,  38,  38,  -1,  38,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  38,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  38,  38,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  38,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  38,  38,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  38,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  39,  39,  39,  39,  39,  -1,  39,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  39,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  39,  39,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  39,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  39,  39,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  39,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  40,   0,   0,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  40,  40,  40,  40,  40,  40,  40,  40,  40,  40,  40,  -1,  40,  40,  40,  40,  40,  40,  40,  40,  -1,  -1,  -1,  -1,   0,   0,   0,  -1,   0,  -1,  -1,  -1,   0,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  40,  40,  40,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0),
    ( -1,  -1,  -1,  41,  41,  41,  41,  41,  -1,  41,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  41,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  41,  41,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  41,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  41,  41,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  41,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  43,  43,  43,  43,  42,  -1,  43,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  44,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  44,  44,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  44,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  43,  43,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  43,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  46,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  45,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  47,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,   0,   0,   0,   0,  48,   0,  48,   0,   0,   0,   0,   0,  48,   0,  48,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,   0,   0,   0,   0,   0,  -1,   0,  -1,  -1,  -1,   0,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  -1,  51,  -1,  -1,  -1,  -1,  -1,  52,  -1,  49,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  53,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  54,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  55,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  56,  56,  56,  56,  56,   0,  56,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  56,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  56,  56,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  56,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  56,  56,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  56,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  57,  57,  57,  57,  57,  -1,  58,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  57,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  57,  57,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  57,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  57,  57,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  57,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  59,  59,  59,  59,  59,  -1,  59,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  60,  60,  60,  60,  60,  -1,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  61,  61,  61,  61,  61,  -1,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  62,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  63,  63,  63,  63,  63,  -1,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  64,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  68,  65,  66,  67,  -1,  -1,  71,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  68,  70,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  69,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  72,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  73,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  74,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  75,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  75,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  76,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  77,  -1,  -1,  76,  77,  77,  77,  77,  -1,  77,  -1,  -1,  -1,  77,  77,  -1,  77,  -1,  -1,  -1,  -1,  -1,  77,  -1,  77,  77,  77,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  77,  77),
    ( -1,  -1,  -1,  78,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  78,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,   0,   0,   0,   0,   0,  -1,   0,  -1,  -1,  -1,   0,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  79,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  80,  80,  80,  80,  80,  -1,  80,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  80,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  80,  80,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  80,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  80,  80,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  80,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  81,  81,  81,  81,  81,  -1,  81,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  81,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  81,  81,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  81,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  81,  81,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  81,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  82,  82,  82,  82,  82,  -1,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  83,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  -1,  83,  83,  83,  83,  83,  84,  85,  86,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  87,  87,  88,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 100,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  89,  90,  91,  92,  93,  94,  95,  96,  97,  98,  99,  -1, 101, 102, 103, 104, 105,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 106,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 107,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 108,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 109, 110,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 111,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,   0,   0,   0,   0,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1, 112,  -1,  -1, 113, 114,  -1, 115,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 116, 117,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 118,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 119,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 120,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 121,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 122,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 123, 124,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 125,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 126,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 127, 127,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 128, 129,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 130,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 130,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 132,  -1, 131,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 133,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 134,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 135,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 136,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 136,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 137,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 138,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 138,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 139,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 139, 139,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 140,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 140,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 141,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 142, 142, 142,  -1, 142,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 142,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 142,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 143, 144, 145,  -1, 147,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 146,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 148,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 149,  -1, 149,  -1, 149, 149,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 149,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 149,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 150,  -1, 151,  -1, 152, 154,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 153,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 155,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 156,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 157,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 158,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 159,  -1, 159,  -1, 159,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 159,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 159,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 160,  -1, 161,  -1, 162,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 163,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 164,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 165,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 166,  -1, 166,  -1, 166,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 166,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 166,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 167,  -1, 168,  -1, 169,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 170,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 171,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 172,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 173, 173,  -1, 173,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 173,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 173,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 174, 175,  -1, 176,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 177,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 178,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 179,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 179,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 180,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 181,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 182,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 183,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 183,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 184,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 185,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 186,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 187,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 188,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 189,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 190,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 191,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 192,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 191,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 193,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 193,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 194,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 195,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 195,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 196,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 197,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 198, 198, 198, 198, 198,  -1, 198,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 198,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 198, 198,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 198,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 198, 198,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 198,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 199,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 200,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 201, 201,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 202, 202,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 203, 204,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 205,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 205,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 206, 207,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 209, 209, 209,  -1,  -1,  -1,  -1,  -1, 208,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 209,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 210,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 211,  -1,  -1,  -1,  -1,  -1, 211, 211,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 211, 211,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 212,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 212,  -1, 212, 212,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 212,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 213,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 214,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 214,  -1, 214, 214,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 214,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 215,  -1, 216, 217,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 218,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 218,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 219,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 219,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 220,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 221, 221, 221,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 221,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 222, 222, 222,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 222,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 223, 223, 224,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 225,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 226,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 226,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 227, 227, 227, 227, 227,  -1, 227,  -1,  -1,  -1,  -1, 227,  -1,  -1,  -1, 227,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 227, 227,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 227,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 227, 227, 227, 227, 227,  -1, 227, 227,  -1, 227, 227, 227,  -1,  -1, 227,  -1, 227, 227, 227, 227, 227, 227, 227,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 237, 237, 237, 237, 237,  -1, 237,  -1,  -1,  -1,  -1, 228,  -1,  -1,  -1, 237,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 237, 237,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 237,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 229,  -1, 237, 237, 230,  -1, 228, 232,  -1, 233,  -1, 234,  -1,  -1, 231,  -1, 235,  -1,  -1,  -1, 236, 237, 238,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 239,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 240,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 242,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 241,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 243,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 244, 244, 244, 244, 244,  -1, 244,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 244,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 244, 244,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 244,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 244, 244,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 244,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1, 245,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 246,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 247,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 248,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 249,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 249,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 250, 251,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 252,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 253,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 254,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 255,  -1, 256,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 257,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 258, 258, 258, 258, 258,  -1, 258,  -1,  -1,  -1,  -1, 258,  -1,  -1,  -1, 258,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 258, 258,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 258,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 258, 258, 258, 258, 258,  -1,  -1, 258,  -1, 258,  -1, 258,  -1,  -1, 258,  -1, 258,  -1, 259,  -1, 258, 258, 258,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 260,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 261,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 262,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 262,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1, 263,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 264,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 265,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 266, 266, 266, 266, 266,  -1, 266,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 266,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 266, 266,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 266,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 266, 266,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 266,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 267, 267, 267, 267, 267,  -1, 267,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 267,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 267, 267,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 267,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 267, 267,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 267,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 268,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 269, 270),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 271,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 272,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 273),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 274,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 275,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 276,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 277,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 278, 278,  -1,  -1, 278, 278,  -1,  -1,  -1, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 279,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 280,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,   0,   0, 281,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1, 282,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1, 283,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1, 284,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1, 285,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,  -1,  -1, 286,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1, 287,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1, 288,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 289,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,   0,  -1,  -1, 290,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 291,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,  -1,   0,   0,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0, 291,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 292,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,   0,   0,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, 292,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 293,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,   0,   0,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, 293,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 294,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, 294,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 295, 295, 295,  -1, 295,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 295,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 296,  -1, 296,  -1, 296, 296,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 296,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 297,  -1, 297,  -1, 297,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 297,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 298,  -1, 298,  -1, 298,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 298,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 299, 299,  -1, 299,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 299,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 300,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 301,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 301,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 302,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 303,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 304,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1, 305,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 306,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 307, 307, 307,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 308,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1, 308,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 309, 309, 309, 309, 309,  -1, 309,  -1,  -1,  -1,  -1, 309,  -1,  -1,  -1, 309,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 309, 309,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 309,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 309,   0, 309, 309, 309,  -1,   0, 309,  -1, 309,   0, 309,  -1,  -1, 309,  -1, 309,   0,   0,   0, 309, 309, 309,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 310, 310, 310, 310, 310,  -1, 310,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 310,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 310, 310,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 310,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, 310, 310,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 310,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 311,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 312,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1, 313, 313, 313, 313, 313,  -1, 313,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1, 313,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 313, 313,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 313,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 313, 313,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 313,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1, 314,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 314, 314)
  );

{ TSepiFunDelphiParser class }

procedure TSepiFunDelphiParser.PushChoice1;
begin
  PushBackToParent;
  PushFakeSymbol(tkEof);
  PushFakeSymbol(tkDot);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntPriv0);
  PushSymbol(ntUsesSection);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkUnit);
end;

procedure TSepiFunDelphiParser.PushChoice2;
begin
  PushBackToParent;
  PushSymbol(ntActionsSection);
end;

procedure TSepiFunDelphiParser.PushChoice3;
begin
  PushBackToParent;
  PushSymbol(ntAttributesSection);
end;

procedure TSepiFunDelphiParser.PushChoice4;
begin
  PushBackToParent;
  PushSymbol(ntMessagesSection);
end;

procedure TSepiFunDelphiParser.PushChoice5;
begin
  PushBackToParent;
  PushSymbol(ntComponentsSection);
end;

procedure TSepiFunDelphiParser.PushChoice6;
begin
  PushBackToParent;
  PushSymbol(ntPluginSection);
end;

procedure TSepiFunDelphiParser.PushChoice7;
begin
  PushBackToParent;
  PushSymbol(ntObjectSection);
end;

procedure TSepiFunDelphiParser.PushChoice8;
begin
  PushBackToParent;
  PushSymbol(ntFieldSection);
end;

procedure TSepiFunDelphiParser.PushChoice9;
begin
  PushBackToParent;
  PushSymbol(ntEffectSection);
end;

procedure TSepiFunDelphiParser.PushChoice10;
begin
  PushBackToParent;
  PushSymbol(ntToolSection);
end;

procedure TSepiFunDelphiParser.PushChoice11;
begin
  PushBackToParent;
  PushSymbol(ntObstacleSection);
end;

procedure TSepiFunDelphiParser.PushChoice12;
begin
  PushBackToParent;
  PushSymbol(ntPosComponentSection);
end;

procedure TSepiFunDelphiParser.PushChoice13;
begin
  PushBackToParent;
  PushSymbol(ntVehicleSection);
end;

procedure TSepiFunDelphiParser.PushChoice14;
begin
  PushBackToParent;
  PushSymbol(ntCreatorSection);
end;

procedure TSepiFunDelphiParser.PushChoice15;
begin
  PushBackToParent;
  PushSymbol(ntClassSection);
end;

procedure TSepiFunDelphiParser.PushChoice16;
begin
  PushBackToParent;
  PushSymbol(ntConstSection);
end;

procedure TSepiFunDelphiParser.PushChoice17;
begin
  PushBackToParent;
  PushSymbol(ntRoutineImpl);
end;

procedure TSepiFunDelphiParser.PushChoice18;
begin
  PushBackToParent;
  PushSymbol(tkIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice19;
begin
  PushBackToParent;
  PushSymbol(tkString);
end;

procedure TSepiFunDelphiParser.PushChoice20;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntCommaIdentList);
  PushFakeSymbol(tkUses);
end;

procedure TSepiFunDelphiParser.PushChoice21;
begin
  PushBackToParent;
  PushSymbol(ntPriv1);
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice22;
begin
  PushBackToParent;
  PushSymbol(ntPriv2);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiFunDelphiParser.PushChoice23;
begin
  PushBackToParent;
  PushSymbol(ntPriv3);
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice24;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice25;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
end;

procedure TSepiFunDelphiParser.PushChoice26;
begin
  PushBackToParent;
  PushSymbol(ntArrayInitializationExpression);
end;

procedure TSepiFunDelphiParser.PushChoice27;
begin
  PushBackToParent;
  PushSymbol(ntRecordInitializationExpression);
end;

procedure TSepiFunDelphiParser.PushChoice28;
begin
  PushBackToParent;
  PushSymbol(ntGUIDInitializationExpression);
end;

procedure TSepiFunDelphiParser.PushChoice29;
begin
  PushBackToParent;
  PushSymbol(ntOtherInitializationExpression);
end;

procedure TSepiFunDelphiParser.PushChoice30;
begin
  PushBackToParent;
  PushSymbol(ntArrayInitialization);
end;

procedure TSepiFunDelphiParser.PushChoice31;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntPriv4);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiFunDelphiParser.PushChoice32;
begin
  PushBackToParent;
  PushSymbol(ntRecordInitialization);
end;

procedure TSepiFunDelphiParser.PushChoice33;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntPriv5);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkColon);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiFunDelphiParser.PushChoice34;
begin
  PushBackToParent;
  PushSymbol(ntGUIDInitialization);
end;

procedure TSepiFunDelphiParser.PushChoice35;
begin
  PushBackToParent;
  PushSymbol(ntRecordInitialization);
end;

procedure TSepiFunDelphiParser.PushChoice36;
begin
  PushBackToParent;
  PushSymbol(tkStringCst);
end;

procedure TSepiFunDelphiParser.PushChoice37;
begin
  PushBackToParent;
  PushSymbol(ntOtherInitialization);
end;

procedure TSepiFunDelphiParser.PushChoice38;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
end;

procedure TSepiFunDelphiParser.PushChoice39;
begin
  PushBackToParent;
  PushSymbol(ntNextExpression);
  PushSymbol(ntSingleExpr);
end;

procedure TSepiFunDelphiParser.PushChoice40;
begin
  PushBackToParent;
  PushSymbol(ntNextExpression);
  PushSymbol(ntSingleExpr);
  PushSymbol(ntBinaryOp);
end;

procedure TSepiFunDelphiParser.PushChoice41;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
end;

procedure TSepiFunDelphiParser.PushChoice42;
begin
  PushBackToParent;
  PushSymbol(ntNextExprList);
  PushSymbol(ntParenthesizedExpr);
end;

procedure TSepiFunDelphiParser.PushChoice43;
begin
  PushBackToParent;
  PushSymbol(ntNextExprList);
  PushSymbol(ntSingleValue);
end;

procedure TSepiFunDelphiParser.PushChoice44;
begin
  PushBackToParent;
  PushSymbol(ntUnaryOpExpr);
end;

procedure TSepiFunDelphiParser.PushChoice45;
begin
  PushBackToParent;
  PushSymbol(ntSingleExpr);
  PushSymbol(ntUnaryOp);
end;

procedure TSepiFunDelphiParser.PushChoice46;
begin
  PushBackToParent;
  PushSymbol(ntSingleExpr);
  PushSymbol(ntAddressOfOp);
end;

procedure TSepiFunDelphiParser.PushChoice47;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiFunDelphiParser.PushChoice48;
begin
  PushBackToParent;
  PushSymbol(ntNextExprList);
  PushSymbol(ntNextExpr);
end;

procedure TSepiFunDelphiParser.PushChoice49;
begin
  PushBackToParent;
  PushSymbol(ntUnaryOpModifier);
end;

procedure TSepiFunDelphiParser.PushChoice50;
begin
  PushBackToParent;
  PushSymbol(ntParameters);
end;

procedure TSepiFunDelphiParser.PushChoice51;
begin
  PushBackToParent;
  PushSymbol(ntArrayIndices);
end;

procedure TSepiFunDelphiParser.PushChoice52;
begin
  PushBackToParent;
  PushSymbol(ntFieldSelection);
end;

procedure TSepiFunDelphiParser.PushChoice53;
begin
  PushBackToParent;
  PushSymbol(ntDereferenceOp);
end;

procedure TSepiFunDelphiParser.PushChoice54;
begin
  PushBackToParent;
  PushSymbol(tkHat);
end;

procedure TSepiFunDelphiParser.PushChoice55;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntInnerParameters);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiFunDelphiParser.PushChoice56;
begin
  PushBackToParent;
  PushSymbol(ntPriv6);
  PushSymbol(ntParameter);
end;

procedure TSepiFunDelphiParser.PushChoice57;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
end;

procedure TSepiFunDelphiParser.PushChoice58;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntSetOrOpenArrayBuilder);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiFunDelphiParser.PushChoice59;
begin
  PushBackToParent;
  PushSymbol(ntPriv7);
  PushSymbol(ntSetOrOpenArrayRange);
end;

procedure TSepiFunDelphiParser.PushChoice60;
begin
  PushBackToParent;
  PushSymbol(ntPriv8);
  PushSymbol(ntExpression);
end;

procedure TSepiFunDelphiParser.PushChoice61;
begin
  PushBackToParent;
  PushSymbol(ntExprList);
end;

procedure TSepiFunDelphiParser.PushChoice62;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntExprList);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiFunDelphiParser.PushChoice63;
begin
  PushBackToParent;
  PushSymbol(ntPriv9);
  PushSymbol(ntExpression);
end;

procedure TSepiFunDelphiParser.PushChoice64;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkDot);
end;

procedure TSepiFunDelphiParser.PushChoice65;
begin
  PushBackToParent;
  PushSymbol(ntIntegerConst);
end;

procedure TSepiFunDelphiParser.PushChoice66;
begin
  PushBackToParent;
  PushSymbol(ntFloatConst);
end;

procedure TSepiFunDelphiParser.PushChoice67;
begin
  PushBackToParent;
  PushSymbol(ntStringConst);
end;

procedure TSepiFunDelphiParser.PushChoice68;
begin
  PushBackToParent;
  PushSymbol(ntIdentifierSingleValue);
end;

procedure TSepiFunDelphiParser.PushChoice69;
begin
  PushBackToParent;
  PushSymbol(ntInheritedSingleValue);
  PushFakeSymbol(tkInherited);
end;

procedure TSepiFunDelphiParser.PushChoice70;
begin
  PushBackToParent;
  PushSymbol(ntNilValue);
end;

procedure TSepiFunDelphiParser.PushChoice71;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntSetValue);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiFunDelphiParser.PushChoice72;
begin
  PushBackToParent;
  PushSymbol(tkInteger);
end;

procedure TSepiFunDelphiParser.PushChoice73;
begin
  PushBackToParent;
  PushSymbol(tkFloat);
end;

procedure TSepiFunDelphiParser.PushChoice74;
begin
  PushBackToParent;
  PushSymbol(tkStringCst);
end;

procedure TSepiFunDelphiParser.PushChoice75;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice76;
begin
  PushBackToParent;
  PushSymbol(ntInheritedExpression);
end;

procedure TSepiFunDelphiParser.PushChoice77;
begin
  PushBackToParent;
  PushSymbol(ntPureInheritedExpression);
end;

procedure TSepiFunDelphiParser.PushChoice78;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice79;
begin
  PushBackToParent;
  PushSymbol(tkNil);
end;

procedure TSepiFunDelphiParser.PushChoice80;
begin
  PushBackToParent;
  PushSymbol(ntPriv10);
  PushSymbol(ntSetRange);
end;

procedure TSepiFunDelphiParser.PushChoice81;
begin
  PushBackToParent;
  PushSymbol(ntPriv11);
  PushSymbol(ntSetRange);
end;

procedure TSepiFunDelphiParser.PushChoice82;
begin
  PushBackToParent;
  PushSymbol(ntPriv12);
  PushSymbol(ntExpression);
end;

procedure TSepiFunDelphiParser.PushChoice83;
begin
  PushBackToParent;
  PushSymbol(ntDelphiBinaryOp);
end;

procedure TSepiFunDelphiParser.PushChoice84;
begin
  PushBackToParent;
  PushSymbol(ntInOperation);
end;

procedure TSepiFunDelphiParser.PushChoice85;
begin
  PushBackToParent;
  PushSymbol(ntIsOperation);
end;

procedure TSepiFunDelphiParser.PushChoice86;
begin
  PushBackToParent;
  PushSymbol(ntAsOperation);
end;

procedure TSepiFunDelphiParser.PushChoice87;
begin
  PushBackToParent;
  PushSymbol(ntCanOp);
end;

procedure TSepiFunDelphiParser.PushChoice88;
begin
  PushBackToParent;
  PushSymbol(ntHasOp);
end;

procedure TSepiFunDelphiParser.PushChoice89;
begin
  PushBackToParent;
  PushSymbol(tkPlus);
end;

procedure TSepiFunDelphiParser.PushChoice90;
begin
  PushBackToParent;
  PushSymbol(tkMinus);
end;

procedure TSepiFunDelphiParser.PushChoice91;
begin
  PushBackToParent;
  PushSymbol(tkTimes);
end;

procedure TSepiFunDelphiParser.PushChoice92;
begin
  PushBackToParent;
  PushSymbol(tkDivide);
end;

procedure TSepiFunDelphiParser.PushChoice93;
begin
  PushBackToParent;
  PushSymbol(tkDiv);
end;

procedure TSepiFunDelphiParser.PushChoice94;
begin
  PushBackToParent;
  PushSymbol(tkMod);
end;

procedure TSepiFunDelphiParser.PushChoice95;
begin
  PushBackToParent;
  PushSymbol(tkShl);
end;

procedure TSepiFunDelphiParser.PushChoice96;
begin
  PushBackToParent;
  PushSymbol(tkShr);
end;

procedure TSepiFunDelphiParser.PushChoice97;
begin
  PushBackToParent;
  PushSymbol(tkOr);
end;

procedure TSepiFunDelphiParser.PushChoice98;
begin
  PushBackToParent;
  PushSymbol(tkAnd);
end;

procedure TSepiFunDelphiParser.PushChoice99;
begin
  PushBackToParent;
  PushSymbol(tkXor);
end;

procedure TSepiFunDelphiParser.PushChoice100;
begin
  PushBackToParent;
  PushSymbol(tkEquals);
end;

procedure TSepiFunDelphiParser.PushChoice101;
begin
  PushBackToParent;
  PushSymbol(tkLowerThan);
end;

procedure TSepiFunDelphiParser.PushChoice102;
begin
  PushBackToParent;
  PushSymbol(tkLowerEq);
end;

procedure TSepiFunDelphiParser.PushChoice103;
begin
  PushBackToParent;
  PushSymbol(tkGreaterThan);
end;

procedure TSepiFunDelphiParser.PushChoice104;
begin
  PushBackToParent;
  PushSymbol(tkGreaterEq);
end;

procedure TSepiFunDelphiParser.PushChoice105;
begin
  PushBackToParent;
  PushSymbol(tkNotEqual);
end;

procedure TSepiFunDelphiParser.PushChoice106;
begin
  PushBackToParent;
  PushSymbol(tkIn);
end;

procedure TSepiFunDelphiParser.PushChoice107;
begin
  PushBackToParent;
  PushSymbol(tkIs);
end;

procedure TSepiFunDelphiParser.PushChoice108;
begin
  PushBackToParent;
  PushSymbol(tkAs);
end;

procedure TSepiFunDelphiParser.PushChoice109;
begin
  PushBackToParent;
  PushSymbol(tkCan);
end;

procedure TSepiFunDelphiParser.PushChoice110;
begin
  PushBackToParent;
  PushSymbol(tkCannot);
end;

procedure TSepiFunDelphiParser.PushChoice111;
begin
  PushBackToParent;
  PushSymbol(ntSingleExpr);
  PushSymbol(ntHasComparison);
  PushFakeSymbol(tkHas);
end;

procedure TSepiFunDelphiParser.PushChoice112;
begin
  PushBackToParent;
  PushSymbol(ntAtLeastOrAtMost);
  PushFakeSymbol(tkAtKw);
end;

procedure TSepiFunDelphiParser.PushChoice113;
begin
  PushBackToParent;
  PushSymbol(ntMoreThan);
end;

procedure TSepiFunDelphiParser.PushChoice114;
begin
  PushBackToParent;
  PushSymbol(ntLessThan);
end;

procedure TSepiFunDelphiParser.PushChoice115;
begin
  PushBackToParent;
  PushSymbol(ntExactly);
end;

procedure TSepiFunDelphiParser.PushChoice116;
begin
  PushBackToParent;
  PushSymbol(ntAtLeast);
end;

procedure TSepiFunDelphiParser.PushChoice117;
begin
  PushBackToParent;
  PushSymbol(ntAtMost);
end;

procedure TSepiFunDelphiParser.PushChoice118;
begin
  PushBackToParent;
  PushSymbol(tkLeast);
end;

procedure TSepiFunDelphiParser.PushChoice119;
begin
  PushBackToParent;
  PushSymbol(tkMost);
end;

procedure TSepiFunDelphiParser.PushChoice120;
begin
  PushBackToParent;
  PushFakeSymbol(tkThan);
  PushSymbol(tkMore);
end;

procedure TSepiFunDelphiParser.PushChoice121;
begin
  PushBackToParent;
  PushFakeSymbol(tkThan);
  PushSymbol(tkLess);
end;

procedure TSepiFunDelphiParser.PushChoice122;
begin
  PushBackToParent;
  PushSymbol(tkExactly);
end;

procedure TSepiFunDelphiParser.PushChoice123;
begin
  PushBackToParent;
  PushSymbol(tkPlus);
end;

procedure TSepiFunDelphiParser.PushChoice124;
begin
  PushBackToParent;
  PushSymbol(tkMinus);
end;

procedure TSepiFunDelphiParser.PushChoice125;
begin
  PushBackToParent;
  PushSymbol(tkNot);
end;

procedure TSepiFunDelphiParser.PushChoice126;
begin
  PushBackToParent;
  PushSymbol(tkAt);
end;

procedure TSepiFunDelphiParser.PushChoice127;
begin
  PushBackToParent;
  PushSymbol(ntPriv13);
  PushSymbol(ntConstDecl);
  PushFakeSymbol(ntConstKeyWord);
end;

procedure TSepiFunDelphiParser.PushChoice128;
begin
  PushBackToParent;
  PushSymbol(tkConst);
end;

procedure TSepiFunDelphiParser.PushChoice129;
begin
  PushBackToParent;
  PushSymbol(tkResourceString);
end;

procedure TSepiFunDelphiParser.PushChoice130;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntInnerConstDecl);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiFunDelphiParser.PushChoice131;
begin
  PushBackToParent;
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkEquals);
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkColon);
end;

procedure TSepiFunDelphiParser.PushChoice132;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkEquals);
end;

procedure TSepiFunDelphiParser.PushChoice133;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntCommaIdentDeclList);
  PushFakeSymbol(tkActions);
end;

procedure TSepiFunDelphiParser.PushChoice134;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntCommaIdentDeclList);
  PushFakeSymbol(tkAttributes);
end;

procedure TSepiFunDelphiParser.PushChoice135;
begin
  PushBackToParent;
  PushSymbol(ntPriv14);
  PushSymbol(ntMessageDecl);
  PushFakeSymbol(tkMessages);
end;

procedure TSepiFunDelphiParser.PushChoice136;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkEquals);
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkColon);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiFunDelphiParser.PushChoice137;
begin
  PushBackToParent;
  PushSymbol(ntPriv15);
  PushSymbol(ntComponent);
  PushFakeSymbol(tkComponents);
end;

procedure TSepiFunDelphiParser.PushChoice138;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntComponentParameters);
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkColon);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiFunDelphiParser.PushChoice139;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntPriv16);
end;

procedure TSepiFunDelphiParser.PushChoice140;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkColon);
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice141;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntPluginMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkPlugin);
end;

procedure TSepiFunDelphiParser.PushChoice142;
begin
  PushBackToParent;
  PushSymbol(ntPriv17);
end;

procedure TSepiFunDelphiParser.PushChoice143;
begin
  PushBackToParent;
  PushSymbol(ntHint);
end;

procedure TSepiFunDelphiParser.PushChoice144;
begin
  PushBackToParent;
  PushSymbol(ntCategory);
end;

procedure TSepiFunDelphiParser.PushChoice145;
begin
  PushBackToParent;
  PushSymbol(ntZIndex);
end;

procedure TSepiFunDelphiParser.PushChoice146;
begin
  PushBackToParent;
  PushSymbol(ntEvent);
end;

procedure TSepiFunDelphiParser.PushChoice147;
begin
  PushBackToParent;
  PushSymbol(ntPluginAction);
end;

procedure TSepiFunDelphiParser.PushChoice148;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntObjectMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkObject);
end;

procedure TSepiFunDelphiParser.PushChoice149;
begin
  PushBackToParent;
  PushSymbol(ntPriv18);
end;

procedure TSepiFunDelphiParser.PushChoice150;
begin
  PushBackToParent;
  PushSymbol(ntName);
end;

procedure TSepiFunDelphiParser.PushChoice151;
begin
  PushBackToParent;
  PushSymbol(ntCategory);
end;

procedure TSepiFunDelphiParser.PushChoice152;
begin
  PushBackToParent;
  PushSymbol(ntImage);
end;

procedure TSepiFunDelphiParser.PushChoice153;
begin
  PushBackToParent;
  PushSymbol(ntEvent);
end;

procedure TSepiFunDelphiParser.PushChoice154;
begin
  PushBackToParent;
  PushSymbol(ntObjectAction);
end;

procedure TSepiFunDelphiParser.PushChoice155;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntSquareCompMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkField);
end;

procedure TSepiFunDelphiParser.PushChoice156;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntSquareCompMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkEffect);
end;

procedure TSepiFunDelphiParser.PushChoice157;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntSquareCompMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkTool);
end;

procedure TSepiFunDelphiParser.PushChoice158;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntSquareCompMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkObstacle);
end;

procedure TSepiFunDelphiParser.PushChoice159;
begin
  PushBackToParent;
  PushSymbol(ntPriv19);
end;

procedure TSepiFunDelphiParser.PushChoice160;
begin
  PushBackToParent;
  PushSymbol(ntName);
end;

procedure TSepiFunDelphiParser.PushChoice161;
begin
  PushBackToParent;
  PushSymbol(ntCategory);
end;

procedure TSepiFunDelphiParser.PushChoice162;
begin
  PushBackToParent;
  PushSymbol(ntImage);
end;

procedure TSepiFunDelphiParser.PushChoice163;
begin
  PushBackToParent;
  PushSymbol(ntEvent);
end;

procedure TSepiFunDelphiParser.PushChoice164;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntPosCompMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkPosComponent);
end;

procedure TSepiFunDelphiParser.PushChoice165;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntPosCompMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkVehicle);
end;

procedure TSepiFunDelphiParser.PushChoice166;
begin
  PushBackToParent;
  PushSymbol(ntPriv20);
end;

procedure TSepiFunDelphiParser.PushChoice167;
begin
  PushBackToParent;
  PushSymbol(ntName);
end;

procedure TSepiFunDelphiParser.PushChoice168;
begin
  PushBackToParent;
  PushSymbol(ntCategory);
end;

procedure TSepiFunDelphiParser.PushChoice169;
begin
  PushBackToParent;
  PushSymbol(ntImage);
end;

procedure TSepiFunDelphiParser.PushChoice170;
begin
  PushBackToParent;
  PushSymbol(ntEvent);
end;

procedure TSepiFunDelphiParser.PushChoice171;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntCreatorMembers);
  PushSymbol(ntCreatorItemClass);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkCreator);
end;

procedure TSepiFunDelphiParser.PushChoice172;
begin
  PushBackToParent;
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkFor);
end;

procedure TSepiFunDelphiParser.PushChoice173;
begin
  PushBackToParent;
  PushSymbol(ntPriv21);
end;

procedure TSepiFunDelphiParser.PushChoice174;
begin
  PushBackToParent;
  PushSymbol(ntHint);
end;

procedure TSepiFunDelphiParser.PushChoice175;
begin
  PushBackToParent;
  PushSymbol(ntCategory);
end;

procedure TSepiFunDelphiParser.PushChoice176;
begin
  PushBackToParent;
  PushSymbol(ntImage);
end;

procedure TSepiFunDelphiParser.PushChoice177;
begin
  PushBackToParent;
  PushSymbol(ntEvent);
end;

procedure TSepiFunDelphiParser.PushChoice178;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntClassMembers);
  PushSymbol(ntParentClass);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkClass);
end;

procedure TSepiFunDelphiParser.PushChoice179;
begin
  PushBackToParent;
  PushSymbol(ntPriv22);
end;

procedure TSepiFunDelphiParser.PushChoice180;
begin
  PushBackToParent;
  PushSymbol(ntEvent);
end;

procedure TSepiFunDelphiParser.PushChoice181;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiFunDelphiParser.PushChoice182;
begin
  PushBackToParent;
  PushSymbol(ntPriv23);
  PushSymbol(ntField);
  PushFakeSymbol(tkVar);
end;

procedure TSepiFunDelphiParser.PushChoice183;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkColon);
  PushSymbol(ntCommaIdentDeclList);
end;

procedure TSepiFunDelphiParser.PushChoice184;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkName);
end;

procedure TSepiFunDelphiParser.PushChoice185;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkHint);
end;

procedure TSepiFunDelphiParser.PushChoice186;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkCategory);
end;

procedure TSepiFunDelphiParser.PushChoice187;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkZIndex);
end;

procedure TSepiFunDelphiParser.PushChoice188;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntPriv24);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkImage);
end;

procedure TSepiFunDelphiParser.PushChoice189;
begin
  PushBackToParent;
  PushSymbol(ntMethodBody);
  PushSymbol(ntEventHeader);
end;

procedure TSepiFunDelphiParser.PushChoice190;
begin
  PushBackToParent;
  PushFakeSymbol(tkDo);
  PushSymbol(ntInnerEventHeader);
  PushFakeSymbol(tkOn);
end;

procedure TSepiFunDelphiParser.PushChoice191;
begin
  PushBackToParent;
  PushSymbol(ntMethodEventHeader);
end;

procedure TSepiFunDelphiParser.PushChoice192;
begin
  PushBackToParent;
  PushSymbol(ntMessageEventHeader);
end;

procedure TSepiFunDelphiParser.PushChoice193;
begin
  PushBackToParent;
  PushSymbol(ntAutoOverride);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiFunDelphiParser.PushChoice194;
begin
  PushBackToParent;
  PushSymbol(ntMessageEventLinkKind);
  PushSymbol(ntMessageMethodName);
  PushFakeSymbol(tkMessage);
end;

procedure TSepiFunDelphiParser.PushChoice195;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice196;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntActionCondition);
  PushSymbol(ntActionList);
  PushFakeSymbol(tkAction);
end;

procedure TSepiFunDelphiParser.PushChoice197;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntActionOnUse);
  PushSymbol(ntActionCondition);
  PushSymbol(ntActionList);
  PushFakeSymbol(tkAction);
end;

procedure TSepiFunDelphiParser.PushChoice198;
begin
  PushBackToParent;
  PushSymbol(ntPriv25);
  PushSymbol(ntConstExpression);
end;

procedure TSepiFunDelphiParser.PushChoice199;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkIf);
end;

procedure TSepiFunDelphiParser.PushChoice200;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkThen);
end;

procedure TSepiFunDelphiParser.PushChoice201;
begin
  PushBackToParent;
  PushSymbol(ntForwardOrMethodBody);
  PushSymbol(ntRoutineImplHeader);
end;

procedure TSepiFunDelphiParser.PushChoice202;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntRoutineVisibility);
  PushSymbol(ntRoutineSignature);
  PushSymbol(ntRoutineName);
  PushSymbol(ntRoutineKind);
end;

procedure TSepiFunDelphiParser.PushChoice203;
begin
  PushBackToParent;
  PushSymbol(tkProcedure);
end;

procedure TSepiFunDelphiParser.PushChoice204;
begin
  PushBackToParent;
  PushSymbol(tkFunction);
end;

procedure TSepiFunDelphiParser.PushChoice205;
begin
  PushBackToParent;
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiFunDelphiParser.PushChoice206;
begin
  PushBackToParent;
  PushSymbol(tkPrivate);
end;

procedure TSepiFunDelphiParser.PushChoice207;
begin
  PushBackToParent;
  PushSymbol(tkPublic);
end;

procedure TSepiFunDelphiParser.PushChoice208;
begin
  PushBackToParent;
  PushSymbol(ntForwardMarker);
end;

procedure TSepiFunDelphiParser.PushChoice209;
begin
  PushBackToParent;
  PushSymbol(ntMethodBody);
end;

procedure TSepiFunDelphiParser.PushChoice210;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(tkForward);
end;

procedure TSepiFunDelphiParser.PushChoice211;
begin
  PushBackToParent;
  PushSymbol(ntReturnType);
  PushSymbol(ntPriv26);
end;

procedure TSepiFunDelphiParser.PushChoice212;
begin
  PushBackToParent;
  PushSymbol(ntPriv27);
  PushSymbol(ntParam);
end;

procedure TSepiFunDelphiParser.PushChoice213;
begin
  PushBackToParent;
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkColon);
end;

procedure TSepiFunDelphiParser.PushChoice214;
begin
  PushBackToParent;
  PushSymbol(ntParamType);
  PushSymbol(ntParamNameList);
  PushSymbol(ntParamKind);
end;

procedure TSepiFunDelphiParser.PushChoice215;
begin
  PushBackToParent;
  PushSymbol(tkConst);
end;

procedure TSepiFunDelphiParser.PushChoice216;
begin
  PushBackToParent;
  PushSymbol(tkVar);
end;

procedure TSepiFunDelphiParser.PushChoice217;
begin
  PushBackToParent;
  PushSymbol(tkOut);
end;

procedure TSepiFunDelphiParser.PushChoice218;
begin
  PushBackToParent;
  PushSymbol(ntPriv28);
  PushSymbol(ntParamName);
end;

procedure TSepiFunDelphiParser.PushChoice219;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice220;
begin
  PushBackToParent;
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkColon);
end;

procedure TSepiFunDelphiParser.PushChoice221;
begin
  PushBackToParent;
  PushSymbol(ntCommonMethodBody);
end;

procedure TSepiFunDelphiParser.PushChoice222;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntBeginEndBlock);
  PushSymbol(ntPriv29);
end;

procedure TSepiFunDelphiParser.PushChoice223;
begin
  PushBackToParent;
  PushSymbol(ntConstSection);
end;

procedure TSepiFunDelphiParser.PushChoice224;
begin
  PushBackToParent;
  PushSymbol(ntLocalVarSection);
end;

procedure TSepiFunDelphiParser.PushChoice225;
begin
  PushBackToParent;
  PushSymbol(ntPriv30);
  PushSymbol(ntLocalVar);
  PushFakeSymbol(tkVar);
end;

procedure TSepiFunDelphiParser.PushChoice226;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkColon);
  PushSymbol(ntCommaIdentDeclList);
end;

procedure TSepiFunDelphiParser.PushChoice227;
begin
  PushBackToParent;
  PushSymbol(ntPriv31);
end;

procedure TSepiFunDelphiParser.PushChoice228;
begin
  PushBackToParent;
  PushSymbol(ntNoInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice229;
begin
  PushBackToParent;
  PushSymbol(ntBeginEndBlock);
end;

procedure TSepiFunDelphiParser.PushChoice230;
begin
  PushBackToParent;
  PushSymbol(ntIfThenElseInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice231;
begin
  PushBackToParent;
  PushSymbol(ntCaseOfInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice232;
begin
  PushBackToParent;
  PushSymbol(ntWhileInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice233;
begin
  PushBackToParent;
  PushSymbol(ntRepeatInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice234;
begin
  PushBackToParent;
  PushSymbol(ntForInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice235;
begin
  PushBackToParent;
  PushSymbol(ntTryInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice236;
begin
  PushBackToParent;
  PushSymbol(ntRaiseInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice237;
begin
  PushBackToParent;
  PushSymbol(ntExpressionInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice238;
begin
  PushBackToParent;
  PushSymbol(ntWithInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice239;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkBegin);
end;

procedure TSepiFunDelphiParser.PushChoice240;
begin
  PushBackToParent;
  PushSymbol(ntElseBranch);
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkThen);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkIf);
end;

procedure TSepiFunDelphiParser.PushChoice241;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkElse);
end;

procedure TSepiFunDelphiParser.PushChoice242;
begin
  PushBackToParent;
  PushSymbol(ntNoInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice243;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntCaseOfElseClause);
  PushSymbol(ntPriv32);
  PushSymbol(ntCaseOfClause);
  PushFakeSymbol(tkOf);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkCase);
end;

procedure TSepiFunDelphiParser.PushChoice244;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkColon);
  PushSymbol(ntCaseOfSetValue);
end;

procedure TSepiFunDelphiParser.PushChoice245;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkElse);
end;

procedure TSepiFunDelphiParser.PushChoice246;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkDo);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkWhile);
end;

procedure TSepiFunDelphiParser.PushChoice247;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkUntil);
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkRepeat);
end;

procedure TSepiFunDelphiParser.PushChoice248;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkDo);
  PushSymbol(ntExpression);
  PushSymbol(ntForToDownTo);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkAssign);
  PushSymbol(ntForControlVar);
  PushFakeSymbol(tkFor);
end;

procedure TSepiFunDelphiParser.PushChoice249;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiFunDelphiParser.PushChoice250;
begin
  PushBackToParent;
  PushSymbol(ntForTo);
end;

procedure TSepiFunDelphiParser.PushChoice251;
begin
  PushBackToParent;
  PushSymbol(ntForDownTo);
end;

procedure TSepiFunDelphiParser.PushChoice252;
begin
  PushBackToParent;
  PushSymbol(tkTo);
end;

procedure TSepiFunDelphiParser.PushChoice253;
begin
  PushBackToParent;
  PushSymbol(tkDownTo);
end;

procedure TSepiFunDelphiParser.PushChoice254;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntNextTryInstruction);
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkTry);
end;

procedure TSepiFunDelphiParser.PushChoice255;
begin
  PushBackToParent;
  PushSymbol(ntExceptClause);
end;

procedure TSepiFunDelphiParser.PushChoice256;
begin
  PushBackToParent;
  PushSymbol(ntFinallyClause);
end;

procedure TSepiFunDelphiParser.PushChoice257;
begin
  PushBackToParent;
  PushSymbol(ntNextExceptClause);
  PushFakeSymbol(tkExcept);
end;

procedure TSepiFunDelphiParser.PushChoice258;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
end;

procedure TSepiFunDelphiParser.PushChoice259;
begin
  PushBackToParent;
  PushSymbol(ntMultiOn);
end;

procedure TSepiFunDelphiParser.PushChoice260;
begin
  PushBackToParent;
  PushSymbol(ntMultiOnElseClause);
  PushSymbol(ntPriv33);
  PushSymbol(ntOnClause);
end;

procedure TSepiFunDelphiParser.PushChoice261;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkDo);
  PushSymbol(ntExceptionVarAndType);
  PushFakeSymbol(tkOn);
end;

procedure TSepiFunDelphiParser.PushChoice262;
begin
  PushBackToParent;
  PushSymbol(ntPriv34);
  PushSymbol(ntQualifiedIdent);
end;

procedure TSepiFunDelphiParser.PushChoice263;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkElse);
end;

procedure TSepiFunDelphiParser.PushChoice264;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkFinally);
end;

procedure TSepiFunDelphiParser.PushChoice265;
begin
  PushBackToParent;
  PushSymbol(ntPriv35);
  PushFakeSymbol(tkRaise);
end;

procedure TSepiFunDelphiParser.PushChoice266;
begin
  PushBackToParent;
  PushSymbol(ntExecutableExpression);
end;

procedure TSepiFunDelphiParser.PushChoice267;
begin
  PushBackToParent;
  PushSymbol(ntPriv36);
  PushSymbol(ntExpression);
end;

procedure TSepiFunDelphiParser.PushChoice268;
begin
  PushBackToParent;
  PushSymbol(ntAssignmentOp);
end;

procedure TSepiFunDelphiParser.PushChoice269;
begin
  PushBackToParent;
  PushSymbol(ntReceivesOp);
end;

procedure TSepiFunDelphiParser.PushChoice270;
begin
  PushBackToParent;
  PushSymbol(ntDiscardsOp);
end;

procedure TSepiFunDelphiParser.PushChoice271;
begin
  PushBackToParent;
  PushSymbol(tkAssign);
end;

procedure TSepiFunDelphiParser.PushChoice272;
begin
  PushBackToParent;
  PushSymbol(ntSingleExpr);
  PushFakeSymbol(tkReceives);
end;

procedure TSepiFunDelphiParser.PushChoice273;
begin
  PushBackToParent;
  PushSymbol(ntSingleExpr);
  PushFakeSymbol(tkDiscards);
end;

procedure TSepiFunDelphiParser.PushChoice274;
begin
  PushBackToParent;
  PushSymbol(ntWithEx);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkWith);
end;

procedure TSepiFunDelphiParser.PushChoice275;
begin
  PushBackToParent;
  PushSymbol(ntInnerWith);
end;

procedure TSepiFunDelphiParser.PushChoice276;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkDo);
end;

procedure TSepiFunDelphiParser.PushChoice277;
begin
  PushBackToParent;
  PushSymbol(ntWithEx);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice278;
begin
  PushBackToParent;
  PushSymbol(ntPriv0);
  PushSymbol(ntImplSection);
end;

procedure TSepiFunDelphiParser.PushChoice279;
begin
  PushBackToParent;
  PushSymbol(ntPriv1);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice280;
begin
  PushBackToParent;
  PushSymbol(ntPriv2);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice281;
begin
  PushBackToParent;
  PushSymbol(ntPriv3);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkDot);
end;

procedure TSepiFunDelphiParser.PushChoice282;
begin
  PushBackToParent;
  PushSymbol(ntPriv4);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice283;
begin
  PushBackToParent;
  PushSymbol(ntPriv5);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkColon);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkSemiColon);
end;

procedure TSepiFunDelphiParser.PushChoice284;
begin
  PushBackToParent;
  PushSymbol(ntPriv6);
  PushSymbol(ntParameter);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice285;
begin
  PushBackToParent;
  PushSymbol(ntPriv7);
  PushSymbol(ntSetOrOpenArrayRange);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice286;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkRange);
end;

procedure TSepiFunDelphiParser.PushChoice287;
begin
  PushBackToParent;
  PushSymbol(ntPriv9);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice288;
begin
  PushBackToParent;
  PushSymbol(ntPriv10);
  PushSymbol(ntSetRange);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice289;
begin
  PushBackToParent;
  PushSymbol(ntPriv11);
  PushSymbol(ntSetRange);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice290;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkRange);
end;

procedure TSepiFunDelphiParser.PushChoice291;
begin
  PushBackToParent;
  PushSymbol(ntPriv13);
  PushSymbol(ntConstDecl);
end;

procedure TSepiFunDelphiParser.PushChoice292;
begin
  PushBackToParent;
  PushSymbol(ntPriv14);
  PushSymbol(ntMessageDecl);
end;

procedure TSepiFunDelphiParser.PushChoice293;
begin
  PushBackToParent;
  PushSymbol(ntPriv15);
  PushSymbol(ntComponent);
end;

procedure TSepiFunDelphiParser.PushChoice294;
begin
  PushBackToParent;
  PushSymbol(ntPriv16);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntComponentParameter);
end;

procedure TSepiFunDelphiParser.PushChoice295;
begin
  PushBackToParent;
  PushSymbol(ntPriv17);
  PushSymbol(ntPluginMember);
end;

procedure TSepiFunDelphiParser.PushChoice296;
begin
  PushBackToParent;
  PushSymbol(ntPriv18);
  PushSymbol(ntObjectMember);
end;

procedure TSepiFunDelphiParser.PushChoice297;
begin
  PushBackToParent;
  PushSymbol(ntPriv19);
  PushSymbol(ntSquareCompMember);
end;

procedure TSepiFunDelphiParser.PushChoice298;
begin
  PushBackToParent;
  PushSymbol(ntPriv20);
  PushSymbol(ntPosCompMember);
end;

procedure TSepiFunDelphiParser.PushChoice299;
begin
  PushBackToParent;
  PushSymbol(ntPriv21);
  PushSymbol(ntCreatorMember);
end;

procedure TSepiFunDelphiParser.PushChoice300;
begin
  PushBackToParent;
  PushSymbol(ntPriv22);
  PushSymbol(ntClassMember);
end;

procedure TSepiFunDelphiParser.PushChoice301;
begin
  PushBackToParent;
  PushSymbol(ntPriv23);
  PushSymbol(ntField);
end;

procedure TSepiFunDelphiParser.PushChoice302;
begin
  PushBackToParent;
  PushSymbol(ntPriv24);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice303;
begin
  PushBackToParent;
  PushSymbol(ntPriv25);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice304;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntParamList);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiFunDelphiParser.PushChoice305;
begin
  PushBackToParent;
  PushSymbol(ntPriv27);
  PushSymbol(ntParam);
  PushFakeSymbol(tkSemiColon);
end;

procedure TSepiFunDelphiParser.PushChoice306;
begin
  PushBackToParent;
  PushSymbol(ntPriv28);
  PushSymbol(ntParamName);
  PushFakeSymbol(tkComma);
end;

procedure TSepiFunDelphiParser.PushChoice307;
begin
  PushBackToParent;
  PushSymbol(ntPriv29);
  PushSymbol(ntInMethodSection);
end;

procedure TSepiFunDelphiParser.PushChoice308;
begin
  PushBackToParent;
  PushSymbol(ntPriv30);
  PushSymbol(ntLocalVar);
end;

procedure TSepiFunDelphiParser.PushChoice309;
begin
  PushBackToParent;
  PushSymbol(ntPriv31);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntInstruction);
end;

procedure TSepiFunDelphiParser.PushChoice310;
begin
  PushBackToParent;
  PushSymbol(ntPriv32);
  PushSymbol(ntCaseOfClause);
end;

procedure TSepiFunDelphiParser.PushChoice311;
begin
  PushBackToParent;
  PushSymbol(ntPriv33);
  PushSymbol(ntOnClause);
end;

procedure TSepiFunDelphiParser.PushChoice312;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkColon);
end;

procedure TSepiFunDelphiParser.PushChoice313;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
end;

procedure TSepiFunDelphiParser.PushChoice314;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushSymbol(ntExecutableOp);
end;

{*
  [@inheritDoc]
*}
function TSepiFunDelphiParser.IsTerminal(Symbol: TSepiSymbolClass): Boolean;
begin
  Result := (Symbol >= FirstTerminal) and (Symbol <= LastTerminal);
end;

{*
  [@inheritDoc]
*}
function TSepiFunDelphiParser.IsNonTerminal(Symbol: TSepiSymbolClass): Boolean;
begin
  Result := (Symbol >= FirstNonTerminal) and (Symbol <= LastNonTerminal);
end;

{*
  [@inheritDoc]
*}
procedure TSepiFunDelphiParser.InitPushChoiceProcs;
begin
  SetLength(PushChoiceProcs, ChoiceCount);

  inherited;

  PushChoiceProcs[1] := PushChoice1;
  PushChoiceProcs[2] := PushChoice2;
  PushChoiceProcs[3] := PushChoice3;
  PushChoiceProcs[4] := PushChoice4;
  PushChoiceProcs[5] := PushChoice5;
  PushChoiceProcs[6] := PushChoice6;
  PushChoiceProcs[7] := PushChoice7;
  PushChoiceProcs[8] := PushChoice8;
  PushChoiceProcs[9] := PushChoice9;
  PushChoiceProcs[10] := PushChoice10;
  PushChoiceProcs[11] := PushChoice11;
  PushChoiceProcs[12] := PushChoice12;
  PushChoiceProcs[13] := PushChoice13;
  PushChoiceProcs[14] := PushChoice14;
  PushChoiceProcs[15] := PushChoice15;
  PushChoiceProcs[16] := PushChoice16;
  PushChoiceProcs[17] := PushChoice17;
  PushChoiceProcs[18] := PushChoice18;
  PushChoiceProcs[19] := PushChoice19;
  PushChoiceProcs[20] := PushChoice20;
  PushChoiceProcs[21] := PushChoice21;
  PushChoiceProcs[22] := PushChoice22;
  PushChoiceProcs[23] := PushChoice23;
  PushChoiceProcs[24] := PushChoice24;
  PushChoiceProcs[25] := PushChoice25;
  PushChoiceProcs[26] := PushChoice26;
  PushChoiceProcs[27] := PushChoice27;
  PushChoiceProcs[28] := PushChoice28;
  PushChoiceProcs[29] := PushChoice29;
  PushChoiceProcs[30] := PushChoice30;
  PushChoiceProcs[31] := PushChoice31;
  PushChoiceProcs[32] := PushChoice32;
  PushChoiceProcs[33] := PushChoice33;
  PushChoiceProcs[34] := PushChoice34;
  PushChoiceProcs[35] := PushChoice35;
  PushChoiceProcs[36] := PushChoice36;
  PushChoiceProcs[37] := PushChoice37;
  PushChoiceProcs[38] := PushChoice38;
  PushChoiceProcs[39] := PushChoice39;
  PushChoiceProcs[40] := PushChoice40;
  PushChoiceProcs[41] := PushChoice41;
  PushChoiceProcs[42] := PushChoice42;
  PushChoiceProcs[43] := PushChoice43;
  PushChoiceProcs[44] := PushChoice44;
  PushChoiceProcs[45] := PushChoice45;
  PushChoiceProcs[46] := PushChoice46;
  PushChoiceProcs[47] := PushChoice47;
  PushChoiceProcs[48] := PushChoice48;
  PushChoiceProcs[49] := PushChoice49;
  PushChoiceProcs[50] := PushChoice50;
  PushChoiceProcs[51] := PushChoice51;
  PushChoiceProcs[52] := PushChoice52;
  PushChoiceProcs[53] := PushChoice53;
  PushChoiceProcs[54] := PushChoice54;
  PushChoiceProcs[55] := PushChoice55;
  PushChoiceProcs[56] := PushChoice56;
  PushChoiceProcs[57] := PushChoice57;
  PushChoiceProcs[58] := PushChoice58;
  PushChoiceProcs[59] := PushChoice59;
  PushChoiceProcs[60] := PushChoice60;
  PushChoiceProcs[61] := PushChoice61;
  PushChoiceProcs[62] := PushChoice62;
  PushChoiceProcs[63] := PushChoice63;
  PushChoiceProcs[64] := PushChoice64;
  PushChoiceProcs[65] := PushChoice65;
  PushChoiceProcs[66] := PushChoice66;
  PushChoiceProcs[67] := PushChoice67;
  PushChoiceProcs[68] := PushChoice68;
  PushChoiceProcs[69] := PushChoice69;
  PushChoiceProcs[70] := PushChoice70;
  PushChoiceProcs[71] := PushChoice71;
  PushChoiceProcs[72] := PushChoice72;
  PushChoiceProcs[73] := PushChoice73;
  PushChoiceProcs[74] := PushChoice74;
  PushChoiceProcs[75] := PushChoice75;
  PushChoiceProcs[76] := PushChoice76;
  PushChoiceProcs[77] := PushChoice77;
  PushChoiceProcs[78] := PushChoice78;
  PushChoiceProcs[79] := PushChoice79;
  PushChoiceProcs[80] := PushChoice80;
  PushChoiceProcs[81] := PushChoice81;
  PushChoiceProcs[82] := PushChoice82;
  PushChoiceProcs[83] := PushChoice83;
  PushChoiceProcs[84] := PushChoice84;
  PushChoiceProcs[85] := PushChoice85;
  PushChoiceProcs[86] := PushChoice86;
  PushChoiceProcs[87] := PushChoice87;
  PushChoiceProcs[88] := PushChoice88;
  PushChoiceProcs[89] := PushChoice89;
  PushChoiceProcs[90] := PushChoice90;
  PushChoiceProcs[91] := PushChoice91;
  PushChoiceProcs[92] := PushChoice92;
  PushChoiceProcs[93] := PushChoice93;
  PushChoiceProcs[94] := PushChoice94;
  PushChoiceProcs[95] := PushChoice95;
  PushChoiceProcs[96] := PushChoice96;
  PushChoiceProcs[97] := PushChoice97;
  PushChoiceProcs[98] := PushChoice98;
  PushChoiceProcs[99] := PushChoice99;
  PushChoiceProcs[100] := PushChoice100;
  PushChoiceProcs[101] := PushChoice101;
  PushChoiceProcs[102] := PushChoice102;
  PushChoiceProcs[103] := PushChoice103;
  PushChoiceProcs[104] := PushChoice104;
  PushChoiceProcs[105] := PushChoice105;
  PushChoiceProcs[106] := PushChoice106;
  PushChoiceProcs[107] := PushChoice107;
  PushChoiceProcs[108] := PushChoice108;
  PushChoiceProcs[109] := PushChoice109;
  PushChoiceProcs[110] := PushChoice110;
  PushChoiceProcs[111] := PushChoice111;
  PushChoiceProcs[112] := PushChoice112;
  PushChoiceProcs[113] := PushChoice113;
  PushChoiceProcs[114] := PushChoice114;
  PushChoiceProcs[115] := PushChoice115;
  PushChoiceProcs[116] := PushChoice116;
  PushChoiceProcs[117] := PushChoice117;
  PushChoiceProcs[118] := PushChoice118;
  PushChoiceProcs[119] := PushChoice119;
  PushChoiceProcs[120] := PushChoice120;
  PushChoiceProcs[121] := PushChoice121;
  PushChoiceProcs[122] := PushChoice122;
  PushChoiceProcs[123] := PushChoice123;
  PushChoiceProcs[124] := PushChoice124;
  PushChoiceProcs[125] := PushChoice125;
  PushChoiceProcs[126] := PushChoice126;
  PushChoiceProcs[127] := PushChoice127;
  PushChoiceProcs[128] := PushChoice128;
  PushChoiceProcs[129] := PushChoice129;
  PushChoiceProcs[130] := PushChoice130;
  PushChoiceProcs[131] := PushChoice131;
  PushChoiceProcs[132] := PushChoice132;
  PushChoiceProcs[133] := PushChoice133;
  PushChoiceProcs[134] := PushChoice134;
  PushChoiceProcs[135] := PushChoice135;
  PushChoiceProcs[136] := PushChoice136;
  PushChoiceProcs[137] := PushChoice137;
  PushChoiceProcs[138] := PushChoice138;
  PushChoiceProcs[139] := PushChoice139;
  PushChoiceProcs[140] := PushChoice140;
  PushChoiceProcs[141] := PushChoice141;
  PushChoiceProcs[142] := PushChoice142;
  PushChoiceProcs[143] := PushChoice143;
  PushChoiceProcs[144] := PushChoice144;
  PushChoiceProcs[145] := PushChoice145;
  PushChoiceProcs[146] := PushChoice146;
  PushChoiceProcs[147] := PushChoice147;
  PushChoiceProcs[148] := PushChoice148;
  PushChoiceProcs[149] := PushChoice149;
  PushChoiceProcs[150] := PushChoice150;
  PushChoiceProcs[151] := PushChoice151;
  PushChoiceProcs[152] := PushChoice152;
  PushChoiceProcs[153] := PushChoice153;
  PushChoiceProcs[154] := PushChoice154;
  PushChoiceProcs[155] := PushChoice155;
  PushChoiceProcs[156] := PushChoice156;
  PushChoiceProcs[157] := PushChoice157;
  PushChoiceProcs[158] := PushChoice158;
  PushChoiceProcs[159] := PushChoice159;
  PushChoiceProcs[160] := PushChoice160;
  PushChoiceProcs[161] := PushChoice161;
  PushChoiceProcs[162] := PushChoice162;
  PushChoiceProcs[163] := PushChoice163;
  PushChoiceProcs[164] := PushChoice164;
  PushChoiceProcs[165] := PushChoice165;
  PushChoiceProcs[166] := PushChoice166;
  PushChoiceProcs[167] := PushChoice167;
  PushChoiceProcs[168] := PushChoice168;
  PushChoiceProcs[169] := PushChoice169;
  PushChoiceProcs[170] := PushChoice170;
  PushChoiceProcs[171] := PushChoice171;
  PushChoiceProcs[172] := PushChoice172;
  PushChoiceProcs[173] := PushChoice173;
  PushChoiceProcs[174] := PushChoice174;
  PushChoiceProcs[175] := PushChoice175;
  PushChoiceProcs[176] := PushChoice176;
  PushChoiceProcs[177] := PushChoice177;
  PushChoiceProcs[178] := PushChoice178;
  PushChoiceProcs[179] := PushChoice179;
  PushChoiceProcs[180] := PushChoice180;
  PushChoiceProcs[181] := PushChoice181;
  PushChoiceProcs[182] := PushChoice182;
  PushChoiceProcs[183] := PushChoice183;
  PushChoiceProcs[184] := PushChoice184;
  PushChoiceProcs[185] := PushChoice185;
  PushChoiceProcs[186] := PushChoice186;
  PushChoiceProcs[187] := PushChoice187;
  PushChoiceProcs[188] := PushChoice188;
  PushChoiceProcs[189] := PushChoice189;
  PushChoiceProcs[190] := PushChoice190;
  PushChoiceProcs[191] := PushChoice191;
  PushChoiceProcs[192] := PushChoice192;
  PushChoiceProcs[193] := PushChoice193;
  PushChoiceProcs[194] := PushChoice194;
  PushChoiceProcs[195] := PushChoice195;
  PushChoiceProcs[196] := PushChoice196;
  PushChoiceProcs[197] := PushChoice197;
  PushChoiceProcs[198] := PushChoice198;
  PushChoiceProcs[199] := PushChoice199;
  PushChoiceProcs[200] := PushChoice200;
  PushChoiceProcs[201] := PushChoice201;
  PushChoiceProcs[202] := PushChoice202;
  PushChoiceProcs[203] := PushChoice203;
  PushChoiceProcs[204] := PushChoice204;
  PushChoiceProcs[205] := PushChoice205;
  PushChoiceProcs[206] := PushChoice206;
  PushChoiceProcs[207] := PushChoice207;
  PushChoiceProcs[208] := PushChoice208;
  PushChoiceProcs[209] := PushChoice209;
  PushChoiceProcs[210] := PushChoice210;
  PushChoiceProcs[211] := PushChoice211;
  PushChoiceProcs[212] := PushChoice212;
  PushChoiceProcs[213] := PushChoice213;
  PushChoiceProcs[214] := PushChoice214;
  PushChoiceProcs[215] := PushChoice215;
  PushChoiceProcs[216] := PushChoice216;
  PushChoiceProcs[217] := PushChoice217;
  PushChoiceProcs[218] := PushChoice218;
  PushChoiceProcs[219] := PushChoice219;
  PushChoiceProcs[220] := PushChoice220;
  PushChoiceProcs[221] := PushChoice221;
  PushChoiceProcs[222] := PushChoice222;
  PushChoiceProcs[223] := PushChoice223;
  PushChoiceProcs[224] := PushChoice224;
  PushChoiceProcs[225] := PushChoice225;
  PushChoiceProcs[226] := PushChoice226;
  PushChoiceProcs[227] := PushChoice227;
  PushChoiceProcs[228] := PushChoice228;
  PushChoiceProcs[229] := PushChoice229;
  PushChoiceProcs[230] := PushChoice230;
  PushChoiceProcs[231] := PushChoice231;
  PushChoiceProcs[232] := PushChoice232;
  PushChoiceProcs[233] := PushChoice233;
  PushChoiceProcs[234] := PushChoice234;
  PushChoiceProcs[235] := PushChoice235;
  PushChoiceProcs[236] := PushChoice236;
  PushChoiceProcs[237] := PushChoice237;
  PushChoiceProcs[238] := PushChoice238;
  PushChoiceProcs[239] := PushChoice239;
  PushChoiceProcs[240] := PushChoice240;
  PushChoiceProcs[241] := PushChoice241;
  PushChoiceProcs[242] := PushChoice242;
  PushChoiceProcs[243] := PushChoice243;
  PushChoiceProcs[244] := PushChoice244;
  PushChoiceProcs[245] := PushChoice245;
  PushChoiceProcs[246] := PushChoice246;
  PushChoiceProcs[247] := PushChoice247;
  PushChoiceProcs[248] := PushChoice248;
  PushChoiceProcs[249] := PushChoice249;
  PushChoiceProcs[250] := PushChoice250;
  PushChoiceProcs[251] := PushChoice251;
  PushChoiceProcs[252] := PushChoice252;
  PushChoiceProcs[253] := PushChoice253;
  PushChoiceProcs[254] := PushChoice254;
  PushChoiceProcs[255] := PushChoice255;
  PushChoiceProcs[256] := PushChoice256;
  PushChoiceProcs[257] := PushChoice257;
  PushChoiceProcs[258] := PushChoice258;
  PushChoiceProcs[259] := PushChoice259;
  PushChoiceProcs[260] := PushChoice260;
  PushChoiceProcs[261] := PushChoice261;
  PushChoiceProcs[262] := PushChoice262;
  PushChoiceProcs[263] := PushChoice263;
  PushChoiceProcs[264] := PushChoice264;
  PushChoiceProcs[265] := PushChoice265;
  PushChoiceProcs[266] := PushChoice266;
  PushChoiceProcs[267] := PushChoice267;
  PushChoiceProcs[268] := PushChoice268;
  PushChoiceProcs[269] := PushChoice269;
  PushChoiceProcs[270] := PushChoice270;
  PushChoiceProcs[271] := PushChoice271;
  PushChoiceProcs[272] := PushChoice272;
  PushChoiceProcs[273] := PushChoice273;
  PushChoiceProcs[274] := PushChoice274;
  PushChoiceProcs[275] := PushChoice275;
  PushChoiceProcs[276] := PushChoice276;
  PushChoiceProcs[277] := PushChoice277;
  PushChoiceProcs[278] := PushChoice278;
  PushChoiceProcs[279] := PushChoice279;
  PushChoiceProcs[280] := PushChoice280;
  PushChoiceProcs[281] := PushChoice281;
  PushChoiceProcs[282] := PushChoice282;
  PushChoiceProcs[283] := PushChoice283;
  PushChoiceProcs[284] := PushChoice284;
  PushChoiceProcs[285] := PushChoice285;
  PushChoiceProcs[286] := PushChoice286;
  PushChoiceProcs[287] := PushChoice287;
  PushChoiceProcs[288] := PushChoice288;
  PushChoiceProcs[289] := PushChoice289;
  PushChoiceProcs[290] := PushChoice290;
  PushChoiceProcs[291] := PushChoice291;
  PushChoiceProcs[292] := PushChoice292;
  PushChoiceProcs[293] := PushChoice293;
  PushChoiceProcs[294] := PushChoice294;
  PushChoiceProcs[295] := PushChoice295;
  PushChoiceProcs[296] := PushChoice296;
  PushChoiceProcs[297] := PushChoice297;
  PushChoiceProcs[298] := PushChoice298;
  PushChoiceProcs[299] := PushChoice299;
  PushChoiceProcs[300] := PushChoice300;
  PushChoiceProcs[301] := PushChoice301;
  PushChoiceProcs[302] := PushChoice302;
  PushChoiceProcs[303] := PushChoice303;
  PushChoiceProcs[304] := PushChoice304;
  PushChoiceProcs[305] := PushChoice305;
  PushChoiceProcs[306] := PushChoice306;
  PushChoiceProcs[307] := PushChoice307;
  PushChoiceProcs[308] := PushChoice308;
  PushChoiceProcs[309] := PushChoice309;
  PushChoiceProcs[310] := PushChoice310;
  PushChoiceProcs[311] := PushChoice311;
  PushChoiceProcs[312] := PushChoice312;
  PushChoiceProcs[313] := PushChoice313;
  PushChoiceProcs[314] := PushChoice314;
end;

{*
  [@inheritDoc]
*}
function TSepiFunDelphiParser.GetExpectedString(
  ExpectedSymbol: TSepiSymbolClass): string;
begin
  Result := SymbolClassNames[ExpectedSymbol];
end;

{*
  [@inheritDoc]
*}
function TSepiFunDelphiParser.GetParsingTable(NonTerminalClass,
  TerminalClass: TSepiSymbolClass): TRuleID;
begin
  Result := ParsingTable[NonTerminalClass, TerminalClass];
end;

{*
  [@inheritDoc]
*}
function TSepiFunDelphiParser.GetNonTerminalClass(
  Symbol: TSepiSymbolClass): TSepiNonTerminalClass;
begin
  Result := NonTerminalClasses[Symbol];
end;

{*
  Initializes SymbolClassNames array
*}
procedure InitSymbolClassNames;
begin
  SymbolClassNames[ntSource] := 'ntSource';
  SymbolClassNames[ntImplSection] := 'ntImplSection';
  SymbolClassNames[ntIdentifier] := 'ntIdentifier';
  SymbolClassNames[ntUsesSection] := 'ntUsesSection';
  SymbolClassNames[ntCommaIdentList] := 'ntCommaIdentList';
  SymbolClassNames[ntCommaIdentDeclList] := 'ntCommaIdentDeclList';
  SymbolClassNames[ntQualifiedIdent] := 'ntQualifiedIdent';
  SymbolClassNames[ntIdentifierDecl] := 'ntIdentifierDecl';
  SymbolClassNames[ntTypeName] := 'ntTypeName';
  SymbolClassNames[ntInitializationExpression] := 'ntInitializationExpression';
  SymbolClassNames[ntArrayInitializationExpression] := 'ntArrayInitializationExpression';
  SymbolClassNames[ntArrayInitialization] := 'ntArrayInitialization';
  SymbolClassNames[ntRecordInitializationExpression] := 'ntRecordInitializationExpression';
  SymbolClassNames[ntRecordInitialization] := 'ntRecordInitialization';
  SymbolClassNames[ntGUIDInitializationExpression] := 'ntGUIDInitializationExpression';
  SymbolClassNames[ntGUIDInitialization] := 'ntGUIDInitialization';
  SymbolClassNames[ntOtherInitializationExpression] := 'ntOtherInitializationExpression';
  SymbolClassNames[ntOtherInitialization] := 'ntOtherInitialization';
  SymbolClassNames[ntExpression] := 'ntExpression';
  SymbolClassNames[ntNextExpression] := 'ntNextExpression';
  SymbolClassNames[ntConstExpression] := 'ntConstExpression';
  SymbolClassNames[ntSingleExpr] := 'ntSingleExpr';
  SymbolClassNames[ntUnaryOpExpr] := 'ntUnaryOpExpr';
  SymbolClassNames[ntParenthesizedExpr] := 'ntParenthesizedExpr';
  SymbolClassNames[ntNextExprList] := 'ntNextExprList';
  SymbolClassNames[ntNextExpr] := 'ntNextExpr';
  SymbolClassNames[ntUnaryOpModifier] := 'ntUnaryOpModifier';
  SymbolClassNames[ntDereferenceOp] := 'ntDereferenceOp';
  SymbolClassNames[ntParameters] := 'ntParameters';
  SymbolClassNames[ntInnerParameters] := 'ntInnerParameters';
  SymbolClassNames[ntParameter] := 'ntParameter';
  SymbolClassNames[ntSetOrOpenArrayBuilder] := 'ntSetOrOpenArrayBuilder';
  SymbolClassNames[ntSetOrOpenArrayRange] := 'ntSetOrOpenArrayRange';
  SymbolClassNames[ntExprListOrEmpty] := 'ntExprListOrEmpty';
  SymbolClassNames[ntArrayIndices] := 'ntArrayIndices';
  SymbolClassNames[ntExprList] := 'ntExprList';
  SymbolClassNames[ntFieldSelection] := 'ntFieldSelection';
  SymbolClassNames[ntSingleValue] := 'ntSingleValue';
  SymbolClassNames[ntIntegerConst] := 'ntIntegerConst';
  SymbolClassNames[ntFloatConst] := 'ntFloatConst';
  SymbolClassNames[ntStringConst] := 'ntStringConst';
  SymbolClassNames[ntIdentifierSingleValue] := 'ntIdentifierSingleValue';
  SymbolClassNames[ntInheritedSingleValue] := 'ntInheritedSingleValue';
  SymbolClassNames[ntInheritedExpression] := 'ntInheritedExpression';
  SymbolClassNames[ntPureInheritedExpression] := 'ntPureInheritedExpression';
  SymbolClassNames[ntNilValue] := 'ntNilValue';
  SymbolClassNames[ntSetValue] := 'ntSetValue';
  SymbolClassNames[ntCaseOfSetValue] := 'ntCaseOfSetValue';
  SymbolClassNames[ntSetRange] := 'ntSetRange';
  SymbolClassNames[ntBinaryOp] := 'ntBinaryOp';
  SymbolClassNames[ntDelphiBinaryOp] := 'ntDelphiBinaryOp';
  SymbolClassNames[ntInOperation] := 'ntInOperation';
  SymbolClassNames[ntIsOperation] := 'ntIsOperation';
  SymbolClassNames[ntAsOperation] := 'ntAsOperation';
  SymbolClassNames[ntCanOp] := 'ntCanOp';
  SymbolClassNames[ntHasOp] := 'ntHasOp';
  SymbolClassNames[ntHasComparison] := 'ntHasComparison';
  SymbolClassNames[ntAtLeastOrAtMost] := 'ntAtLeastOrAtMost';
  SymbolClassNames[ntAtLeast] := 'ntAtLeast';
  SymbolClassNames[ntAtMost] := 'ntAtMost';
  SymbolClassNames[ntMoreThan] := 'ntMoreThan';
  SymbolClassNames[ntLessThan] := 'ntLessThan';
  SymbolClassNames[ntExactly] := 'ntExactly';
  SymbolClassNames[ntUnaryOp] := 'ntUnaryOp';
  SymbolClassNames[ntAddressOfOp] := 'ntAddressOfOp';
  SymbolClassNames[ntConstSection] := 'ntConstSection';
  SymbolClassNames[ntConstKeyWord] := 'ntConstKeyWord';
  SymbolClassNames[ntConstDecl] := 'ntConstDecl';
  SymbolClassNames[ntInnerConstDecl] := 'ntInnerConstDecl';
  SymbolClassNames[ntActionsSection] := 'ntActionsSection';
  SymbolClassNames[ntAttributesSection] := 'ntAttributesSection';
  SymbolClassNames[ntMessagesSection] := 'ntMessagesSection';
  SymbolClassNames[ntMessageDecl] := 'ntMessageDecl';
  SymbolClassNames[ntComponentsSection] := 'ntComponentsSection';
  SymbolClassNames[ntComponent] := 'ntComponent';
  SymbolClassNames[ntComponentParameters] := 'ntComponentParameters';
  SymbolClassNames[ntComponentParameter] := 'ntComponentParameter';
  SymbolClassNames[ntPluginSection] := 'ntPluginSection';
  SymbolClassNames[ntPluginMembers] := 'ntPluginMembers';
  SymbolClassNames[ntPluginMember] := 'ntPluginMember';
  SymbolClassNames[ntObjectSection] := 'ntObjectSection';
  SymbolClassNames[ntObjectMembers] := 'ntObjectMembers';
  SymbolClassNames[ntObjectMember] := 'ntObjectMember';
  SymbolClassNames[ntFieldSection] := 'ntFieldSection';
  SymbolClassNames[ntEffectSection] := 'ntEffectSection';
  SymbolClassNames[ntToolSection] := 'ntToolSection';
  SymbolClassNames[ntObstacleSection] := 'ntObstacleSection';
  SymbolClassNames[ntSquareCompMembers] := 'ntSquareCompMembers';
  SymbolClassNames[ntSquareCompMember] := 'ntSquareCompMember';
  SymbolClassNames[ntPosComponentSection] := 'ntPosComponentSection';
  SymbolClassNames[ntVehicleSection] := 'ntVehicleSection';
  SymbolClassNames[ntPosCompMembers] := 'ntPosCompMembers';
  SymbolClassNames[ntPosCompMember] := 'ntPosCompMember';
  SymbolClassNames[ntCreatorSection] := 'ntCreatorSection';
  SymbolClassNames[ntCreatorItemClass] := 'ntCreatorItemClass';
  SymbolClassNames[ntCreatorMembers] := 'ntCreatorMembers';
  SymbolClassNames[ntCreatorMember] := 'ntCreatorMember';
  SymbolClassNames[ntClassSection] := 'ntClassSection';
  SymbolClassNames[ntClassMembers] := 'ntClassMembers';
  SymbolClassNames[ntClassMember] := 'ntClassMember';
  SymbolClassNames[ntParentClass] := 'ntParentClass';
  SymbolClassNames[ntFields] := 'ntFields';
  SymbolClassNames[ntField] := 'ntField';
  SymbolClassNames[ntAutoOverride] := 'ntAutoOverride';
  SymbolClassNames[ntName] := 'ntName';
  SymbolClassNames[ntHint] := 'ntHint';
  SymbolClassNames[ntCategory] := 'ntCategory';
  SymbolClassNames[ntZIndex] := 'ntZIndex';
  SymbolClassNames[ntImage] := 'ntImage';
  SymbolClassNames[ntEvent] := 'ntEvent';
  SymbolClassNames[ntEventHeader] := 'ntEventHeader';
  SymbolClassNames[ntInnerEventHeader] := 'ntInnerEventHeader';
  SymbolClassNames[ntMethodEventHeader] := 'ntMethodEventHeader';
  SymbolClassNames[ntMessageEventHeader] := 'ntMessageEventHeader';
  SymbolClassNames[ntMessageMethodName] := 'ntMessageMethodName';
  SymbolClassNames[ntMessageEventLinkKind] := 'ntMessageEventLinkKind';
  SymbolClassNames[ntPluginAction] := 'ntPluginAction';
  SymbolClassNames[ntObjectAction] := 'ntObjectAction';
  SymbolClassNames[ntActionList] := 'ntActionList';
  SymbolClassNames[ntActionCondition] := 'ntActionCondition';
  SymbolClassNames[ntActionOnUse] := 'ntActionOnUse';
  SymbolClassNames[ntRoutineImpl] := 'ntRoutineImpl';
  SymbolClassNames[ntRoutineImplHeader] := 'ntRoutineImplHeader';
  SymbolClassNames[ntRoutineKind] := 'ntRoutineKind';
  SymbolClassNames[ntRoutineName] := 'ntRoutineName';
  SymbolClassNames[ntRoutineVisibility] := 'ntRoutineVisibility';
  SymbolClassNames[ntForwardOrMethodBody] := 'ntForwardOrMethodBody';
  SymbolClassNames[ntForwardMarker] := 'ntForwardMarker';
  SymbolClassNames[ntRoutineSignature] := 'ntRoutineSignature';
  SymbolClassNames[ntParamList] := 'ntParamList';
  SymbolClassNames[ntReturnType] := 'ntReturnType';
  SymbolClassNames[ntParam] := 'ntParam';
  SymbolClassNames[ntParamKind] := 'ntParamKind';
  SymbolClassNames[ntParamNameList] := 'ntParamNameList';
  SymbolClassNames[ntParamName] := 'ntParamName';
  SymbolClassNames[ntParamType] := 'ntParamType';
  SymbolClassNames[ntMethodBody] := 'ntMethodBody';
  SymbolClassNames[ntCommonMethodBody] := 'ntCommonMethodBody';
  SymbolClassNames[ntInMethodSection] := 'ntInMethodSection';
  SymbolClassNames[ntLocalVarSection] := 'ntLocalVarSection';
  SymbolClassNames[ntLocalVar] := 'ntLocalVar';
  SymbolClassNames[ntInstructionList] := 'ntInstructionList';
  SymbolClassNames[ntInstruction] := 'ntInstruction';
  SymbolClassNames[ntNoInstruction] := 'ntNoInstruction';
  SymbolClassNames[ntBeginEndBlock] := 'ntBeginEndBlock';
  SymbolClassNames[ntIfThenElseInstruction] := 'ntIfThenElseInstruction';
  SymbolClassNames[ntElseBranch] := 'ntElseBranch';
  SymbolClassNames[ntCaseOfInstruction] := 'ntCaseOfInstruction';
  SymbolClassNames[ntCaseOfClause] := 'ntCaseOfClause';
  SymbolClassNames[ntCaseOfElseClause] := 'ntCaseOfElseClause';
  SymbolClassNames[ntWhileInstruction] := 'ntWhileInstruction';
  SymbolClassNames[ntRepeatInstruction] := 'ntRepeatInstruction';
  SymbolClassNames[ntForInstruction] := 'ntForInstruction';
  SymbolClassNames[ntForControlVar] := 'ntForControlVar';
  SymbolClassNames[ntForToDownTo] := 'ntForToDownTo';
  SymbolClassNames[ntForTo] := 'ntForTo';
  SymbolClassNames[ntForDownTo] := 'ntForDownTo';
  SymbolClassNames[ntTryInstruction] := 'ntTryInstruction';
  SymbolClassNames[ntNextTryInstruction] := 'ntNextTryInstruction';
  SymbolClassNames[ntExceptClause] := 'ntExceptClause';
  SymbolClassNames[ntNextExceptClause] := 'ntNextExceptClause';
  SymbolClassNames[ntMultiOn] := 'ntMultiOn';
  SymbolClassNames[ntOnClause] := 'ntOnClause';
  SymbolClassNames[ntExceptionVarAndType] := 'ntExceptionVarAndType';
  SymbolClassNames[ntMultiOnElseClause] := 'ntMultiOnElseClause';
  SymbolClassNames[ntFinallyClause] := 'ntFinallyClause';
  SymbolClassNames[ntRaiseInstruction] := 'ntRaiseInstruction';
  SymbolClassNames[ntExpressionInstruction] := 'ntExpressionInstruction';
  SymbolClassNames[ntExecutableExpression] := 'ntExecutableExpression';
  SymbolClassNames[ntExecutableOp] := 'ntExecutableOp';
  SymbolClassNames[ntAssignmentOp] := 'ntAssignmentOp';
  SymbolClassNames[ntReceivesOp] := 'ntReceivesOp';
  SymbolClassNames[ntDiscardsOp] := 'ntDiscardsOp';
  SymbolClassNames[ntWithInstruction] := 'ntWithInstruction';
  SymbolClassNames[ntWithEx] := 'ntWithEx';
  SymbolClassNames[ntInnerWith] := 'ntInnerWith';
  SymbolClassNames[ntPriv0] := 'ntPriv0';
  SymbolClassNames[ntPriv1] := 'ntPriv1';
  SymbolClassNames[ntPriv2] := 'ntPriv2';
  SymbolClassNames[ntPriv3] := 'ntPriv3';
  SymbolClassNames[ntPriv4] := 'ntPriv4';
  SymbolClassNames[ntPriv5] := 'ntPriv5';
  SymbolClassNames[ntPriv6] := 'ntPriv6';
  SymbolClassNames[ntPriv7] := 'ntPriv7';
  SymbolClassNames[ntPriv8] := 'ntPriv8';
  SymbolClassNames[ntPriv9] := 'ntPriv9';
  SymbolClassNames[ntPriv10] := 'ntPriv10';
  SymbolClassNames[ntPriv11] := 'ntPriv11';
  SymbolClassNames[ntPriv12] := 'ntPriv12';
  SymbolClassNames[ntPriv13] := 'ntPriv13';
  SymbolClassNames[ntPriv14] := 'ntPriv14';
  SymbolClassNames[ntPriv15] := 'ntPriv15';
  SymbolClassNames[ntPriv16] := 'ntPriv16';
  SymbolClassNames[ntPriv17] := 'ntPriv17';
  SymbolClassNames[ntPriv18] := 'ntPriv18';
  SymbolClassNames[ntPriv19] := 'ntPriv19';
  SymbolClassNames[ntPriv20] := 'ntPriv20';
  SymbolClassNames[ntPriv21] := 'ntPriv21';
  SymbolClassNames[ntPriv22] := 'ntPriv22';
  SymbolClassNames[ntPriv23] := 'ntPriv23';
  SymbolClassNames[ntPriv24] := 'ntPriv24';
  SymbolClassNames[ntPriv25] := 'ntPriv25';
  SymbolClassNames[ntPriv26] := 'ntPriv26';
  SymbolClassNames[ntPriv27] := 'ntPriv27';
  SymbolClassNames[ntPriv28] := 'ntPriv28';
  SymbolClassNames[ntPriv29] := 'ntPriv29';
  SymbolClassNames[ntPriv30] := 'ntPriv30';
  SymbolClassNames[ntPriv31] := 'ntPriv31';
  SymbolClassNames[ntPriv32] := 'ntPriv32';
  SymbolClassNames[ntPriv33] := 'ntPriv33';
  SymbolClassNames[ntPriv34] := 'ntPriv34';
  SymbolClassNames[ntPriv35] := 'ntPriv35';
  SymbolClassNames[ntPriv36] := 'ntPriv36';
end;

{*
  Initializes NonTerminalClasses array
*}
procedure InitNonTerminalClasses;
const
  ClassesToSimplify: array[0..72] of TSepiSymbolClass = (
    -1, 108, 111, 126, 131, 132, 136, 137, 140, 142, 149, 156, 164, 175, 186, 189, 195, 199, 203, 206, 208, 217, 218, 227, 233, 236, 242, 244, 245, 249, 253, 261, 265, 267, 276, 281, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319
  );
var
  I: TSepiSymbolClass;
begin
  for I := FirstNonTerminal to LastNonTerminal do
    NonTerminalClasses[I] := TSepiNonTerminal;

  for I := 1 to High(ClassesToSimplify) do
    NonTerminalClasses[ClassesToSimplify[I]] := TSepiChildThroughNonTerminal;
end;

initialization
  if Length(SymbolClassNames) < LastNonTerminal+1 then
    SetLength(SymbolClassNames, LastNonTerminal+1);

  InitSymbolClassNames;
  InitNonTerminalClasses;
end.

