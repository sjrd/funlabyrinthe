unit SimpleSquaresUtils;

interface

uses
  SysUtils, Classes, Graphics, Contnrs, ScUtils, ScDelphiLanguage, FunLabyUtils,
  FunLabyEditOTA;

resourcestring
  SSimpleEffectTitle = 'Effet simple';
  SSimpleObjectTitle = 'Objet simple';
  SSimpleObstacleTitle = 'Obstacle simple';

type
  {*
    Interface de toute partie de l'éditeur de composants de cases simples
    @author sjrd
    @version 5.0
  *}
  ISimpleSquaresEditor = interface
    ['{8A27FBD3-9660-4129-9F28-3C2855CE2CD7}']

    {*
      Fiche principale de FunLabyEdit
      @return Fiche principale de FunLabyEdit
    *}
    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    {*
      Marque le fichier source comme modifié
    *}
    procedure MarkModified;

    property FunLabyEditMainForm: IOTAFunLabyEditMainForm50
      read GetFunLabyEditMainForm;
  end;

  {*
    Définition d'un composant de case simple
    TSimpleSquare est la classe de base de toutes les définitions de composants
    de cases simples.
    @author sjrd
    @version 5.0
  *}
  TSimpleSquare = class(TPersistent)
  private
    FID: TComponentID;   /// ID du composant
    FName: string;       /// Nom
    FPainter: TPainter;  /// Peintre
    FImgNames: TStrings; /// Noms des images
  protected
    procedure Save(Stream: TStream); virtual;

    procedure ProduceDefaultClassHeader(Code: TStrings);
    procedure ProduceDefaultClassFooter(Code: TStrings);

    procedure ProduceDefaultImgNames(Code: TStrings);

    function GetParentClassName: string; virtual; abstract;

    function GetCanEditImgNames: Boolean; virtual;
  public
    constructor Load(AImagesMaster: TImagesMaster;
      Stream: TStream); virtual;
    constructor Create(AImagesMaster: TImagesMaster; const AID: TComponentID;
      const AName: string); virtual;
    destructor Destroy; override;

    class function ClassTitle: string; virtual;

    class function LoadFromStream(ImagesMaster: TImagesMaster;
      Stream: TStream): TSimpleSquare;
    procedure SaveToStream(Stream: TStream);

    procedure Draw(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0); virtual;

    procedure ProduceIDConst(Code: TStrings); virtual;
    procedure ProduceClassDef(Code: TStrings); virtual;
    procedure ProduceCreateComponents(Code: TStrings); virtual;
    procedure ProduceRegisterComponents(Code: TStrings); virtual;
    procedure ProduceImplementation(Code: TStrings); virtual;

    property ID: TComponentID read FID;
    property Name: string read FName write FName;
    property ParentClassName: string read GetParentClassName;

    property CanEditImgNames: Boolean read GetCanEditImgNames;
    property ImgNames: TStrings read FImgNames;
  end;

  /// Classe de TSimpleSquare
  TSimpleSquareClass = class of TSimpleSquare;

  {*
    Définition d'une action simple
    @author sjrd
    @version 5.0
  *}
  TSimpleAction = class(TPersistent)
  protected
    procedure Save(Stream: TStream); virtual;

    function GetTitle: string; virtual; abstract;
  public
    constructor Load(Stream: TStream); virtual;
    constructor Create; virtual;

    class function LoadFromStream(Stream: TStream): TSimpleAction;
    procedure SaveToStream(Stream: TStream);

    procedure ProduceDelphiCode(Code: TStrings; const Indent: string); virtual;

    property Title: string read GetTitle;
  end;

  /// Classe de TSimpleAction
  TSimpleActionClass = class of TSimpleAction;

  {*
    Liste d'actions simples
    @author sjrd
    @version 5.0
  *}
  TSimpleActionList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TSimpleAction;
  public
    constructor Load(Stream: TStream);
    constructor Create;

    procedure SaveToStream(Stream: TStream);

    function AddNew(ActionClass: TSimpleActionClass): TSimpleAction;

    procedure ProduceDelphiCode(Code: TStrings);

    property Items[Index: Integer]: TSimpleAction read GetItems; default;
  end;

  {*
    Définition d'un terrain simple
    @author sjrd
    @version 5.0
  *}
  TSimpleField = class(TSimpleSquare)
  public
    procedure ProduceClassDef(Code: TStrings); override;
  end;

  {*
    Définition d'un effet simple
    @author sjrd
    @version 5.0
  *}
  TSimpleEffect = class(TSimpleSquare)
  private
    FActions: TSimpleActionList; /// Actions de l'effet
  protected
    procedure Save(Stream: TStream); override;

    function GetParentClassName: string; override;
  public
    constructor Load(AImagesMaster: TImagesMaster;
      Stream: TStream); override;
    constructor Create(AImagesMaster: TImagesMaster; const AID: TComponentID;
      const AName: string); override;
    destructor Destroy; override;

    class function ClassTitle: string; override;

    procedure ProduceClassDef(Code: TStrings); override;
    procedure ProduceImplementation(Code: TStrings); override;

    property Actions: TSimpleActionList read FActions;
  end;

  {*
    Définition d'un objet simple
    @author sjrd
    @version 5.0
  *}
  TSimpleObject = class(TSimpleSquare)
  private
    FFindMessage: string; /// Message affiché quand on trouve cet objet

    FHandledAction: TPlayerAction; /// Action prise en charge par l'objet
    FMinimumCount: Integer;        /// Minimum d'objets requis
    FDecrementOnUse: Boolean;      /// True décrémente l'objet quand utilisé
  protected
    procedure Save(Stream: TStream); override;

    function GetParentClassName: string; override;
  public
    constructor Load(AImagesMaster: TImagesMaster;
      Stream: TStream); override;
    constructor Create(AImagesMaster: TImagesMaster; const AID: TComponentID;
      const AName: string); override;

    class function ClassTitle: string; override;

    procedure ProduceIDConst(Code: TStrings); override;
    procedure ProduceClassDef(Code: TStrings); override;
    procedure ProduceCreateComponents(Code: TStrings); override;
    procedure ProduceRegisterComponents(Code: TStrings); override;
    procedure ProduceImplementation(Code: TStrings); override;

    property FindMessage: string read FFindMessage write FFindMessage;

    property HandledAction: TPlayerAction
      read FHandledAction write FHandledAction;
    property MinimumCount: Integer read FMinimumCount write FMinimumCount;
    property DecrementOnUse: Boolean read FDecrementOnUse write FDecrementOnUse;
  end;

  {*
    Type de condition pour un obstacle
    - ockAlways : l'obstacle est toujours détruit
    - ockNever : l'obstacle n'est jamais détruit
    - ockCondition : l'obstacle est détruit si le joueur peut faire une action
  *}
  TObstacleConditionKind = (ockAlways, ockNever, ockPlayerAction);

  {*
    Définition d'un obstacle simple
    @author sjrd
    @version 5.0
  *}
  TSimpleObstacle = class(TSimpleSquare)
  private
    FConditionKind: TObstacleConditionKind; /// Type de condition
    FPlayerAction: TPlayerAction;           /// Action que doit faire le joueur

    FFailMessage: string; /// Message à afficher si raté
  protected
    procedure Save(Stream: TStream); override;

    function GetParentClassName: string; override;
  public
    constructor Load(AImagesMaster: TImagesMaster;
      Stream: TStream); override;
    constructor Create(AImagesMaster: TImagesMaster; const AID: TComponentID;
      const AName: string); override;

    class function ClassTitle: string; override;

    procedure ProduceClassDef(Code: TStrings); override;
    procedure ProduceImplementation(Code: TStrings); override;

    property ConditionKind: TObstacleConditionKind
      read FConditionKind write FConditionKind;
    property PlayerAction: TPlayerAction read FPlayerAction write FPlayerAction;

    property FailMessage: string read FFailMessage write FFailMessage;
  end;

const {don't localize}
  DefaultIndent = '  '; /// Indentation par défaut
  ToolSuffix = 'Tool';  /// Suffixe 'Tool' pour les outils des objets

implementation

{---------------------}
{ TSimpleSquare class }
{---------------------}

{*
  Crée un composant de case simple en chargeant depuis un flux
  @param AImagesMaster   Maître d'images
  @param Stream          Flux source
*}
constructor TSimpleSquare.Load(AImagesMaster: TImagesMaster;
  Stream: TStream);
var
  Count, I: Integer;
begin
  inherited Create;

  FID := ReadStrFromStream(Stream);
  FName := ReadStrFromStream(Stream);
  FPainter := TPainter.Create(AImagesMaster);
  FImgNames := FPainter.ImgNames;

  FImgNames.BeginUpdate;
  try
    Stream.ReadBuffer(Count, 4);
    for I := 0 to Count-1 do
      FImgNames.Add(ReadStrFromStream(Stream));
  finally
    FImgNames.EndUpdate;
  end;
end;

{*
  Crée un nouveau composant de case simple
  @param AImagesMaster   Maître d'images
  @param AID             ID du composant
  @param AName           Nom
*}
constructor TSimpleSquare.Create(AImagesMaster: TImagesMaster;
  const AID: TComponentID; const AName: string);
begin
  inherited Create;

  FID := AID;
  FName := AName;
  FPainter := TPainter.Create(AImagesMaster);
  FImgNames := FPainter.ImgNames;
end;

{*
  [@inheritDoc]
*}
destructor TSimpleSquare.Destroy;
begin
  FPainter.Free;

  inherited;
end;

{*
  Enregistre le composant dans un flux
  @param Stream   Flux destination
*}
procedure TSimpleSquare.Save(Stream: TStream);
var
  Count, I: Integer;
begin
  WriteStrToStream(Stream, ID);
  WriteStrToStream(Stream, Name);

  Count := ImgNames.Count;
  Stream.WriteBuffer(Count, 4);
  for I := 0 to Count-1 do
    WriteStrToStream(Stream, ImgNames[I]);
end;

{*
  Produit le code Delphi par défaut pour l'en-tête de la classe
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceDefaultClassHeader(Code: TStrings);
begin
  Code.Add('');
  Code.Add('type');
  Code.Add(Format('  T%s = class(%s)', [ID, ParentClassName]));
end;

{*
  Produit le code Delphi par défaut pour le pied de la classe
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceDefaultClassFooter(Code: TStrings);
begin
  Code.Add('  end;');
end;

{*
  Produit le code Delphi par défaut pour le remplissage de ImgNames
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceDefaultImgNames(Code: TStrings);
var
  I: Integer;
begin
  if (not CanEditImgNames) or (ImgNames.Count = 0) then
    Exit;

  Code.Add('');
  for I := 0 to ImgNames.Count-1 do
  begin
    Code.Add(Format('  Painter.ImgNames.Add(%s);',
      [StrToStrRepres(ImgNames[I])]));
  end;
end;

{*
  Indique si on peut modifier les noms des images à afficher
  @return True si on peut les modifier, False sinon
*}
function TSimpleSquare.GetCanEditImgNames: Boolean;
begin
  Result := True;
end;

{*
  Titre de la classe
  @return Titre de la classe
*}
class function TSimpleSquare.ClassTitle: string;
begin
  Result := '';
end;

{*
  Charge un composant depuis un flux
  @param ImagesMaster   Maître d'images
  @param Stream         Flux source
*}
class function TSimpleSquare.LoadFromStream(ImagesMaster: TImagesMaster;
  Stream: TStream): TSimpleSquare;
var
  SquareClass: TPersistentClass;
begin
  SquareClass := FindClass(ReadStrFromStream(Stream));
  Assert(SquareClass.InheritsFrom(TSimpleSquare));
  Result := TSimpleSquareClass(SquareClass).Load(ImagesMaster, Stream);
end;

{*
  Enregistre le composant dans un flux
  @param Stream   Flux destination
*}
procedure TSimpleSquare.SaveToStream(Stream: TStream);
begin
  WriteStrToStream(Stream, ClassName);
  Save(Stream);
end;

{*
  Dessine le composant
  @param Canvas   Canevas cible
  @param X        Abscisse
  @param Y        Ordonnée
*}
procedure TSimpleSquare.Draw(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
end;

{*
  Produit le code Delphi pour la partie constantes d'ID
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceIDConst(Code: TStrings);
begin
  Code.Add(Format('  id%s = ''%0:s'';', [ID]));
end;

{*
  Produit le code Delphi pour la partie définition de classe
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceClassDef(Code: TStrings);
begin
  ProduceDefaultClassHeader(Code);
  ProduceDefaultClassFooter(Code);
end;

{*
  Produit le code Delphi pour la partie création des composants
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceCreateComponents(Code: TStrings);
const
  LineFormat =
    '  T%s.Create(Master, id%0:s, %s);';
begin
  Code.Add(Format(LineFormat, [ID, StrToStrRepres(Name)]));
end;

{*
  Produit le code Delphi pour la partie enregistrement des composants
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceRegisterComponents(Code: TStrings);
const
  LineFormat =
    '  RegisterSingleComponentProc(Master.SquareComponent[id%s]);';
begin
  Code.Add(Format(LineFormat, [ID]));
end;

{*
  Produit le code Delphi pour la partie implémentation
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceImplementation(Code: TStrings);
begin
  Code.Add('');
  Code.Add('{--' + StringOfChar('-', Length(ID)) + '-------}');
  Code.Add('{ T' + ID                            + ' class }');
  Code.Add('{--' + StringOfChar('-', Length(ID)) + '-------}');
end;

{---------------------}
{ TSimpleAction class }
{---------------------}

{*
  Crée une action et la charge depuis un flux
  @param Stream   Flux source
*}
constructor TSimpleAction.Load(Stream: TStream);
begin
  inherited Create;
end;

{*
  Crée une nouvelle action
*}
constructor TSimpleAction.Create;
begin
  inherited Create;
end;

{*
  Enregistre l'action dans un flux
  @param Stream   Flux destination
*}
procedure TSimpleAction.Save(Stream: TStream);
begin
end;

{*
  Charge une action depuis un flux
  @param Stream   Flux source
  @return Action lue
*}
class function TSimpleAction.LoadFromStream(Stream: TStream): TSimpleAction;
var
  ActionClass: TPersistentClass;
begin
  ActionClass := FindClass(ReadStrFromStream(Stream));
  Assert(ActionClass.InheritsFrom(TSimpleAction));
  Result := TSimpleActionClass(ActionClass).Load(Stream);
end;

{*
  Enregistre l'action dans un flux
  @param Stream   Flux destination
*}
procedure TSimpleAction.SaveToStream(Stream: TStream);
begin
  WriteStrToStream(Stream, ClassName);
  Save(Stream);
end;

{*
  Produit le code Delphi de l'action
  @param Code     Code sortie
  @param Indent   Indentation
*}
procedure TSimpleAction.ProduceDelphiCode(Code: TStrings; const Indent: string);
begin
end;

{-------------------------}
{ TSimpleActionList class }
{-------------------------}

{*
  Charge une liste d'action depuis un flux
  @param Stream   Flux source
*}
constructor TSimpleActionList.Load(Stream: TStream);
var
  Count, I: Integer;
begin
  inherited Create;

  Stream.ReadBuffer(Count, 4);
  Capacity := Count;

  for I := 0 to Count-1 do
    Add(TSimpleAction.LoadFromStream(Stream));
end;

{*
  Crée une nouvelle liste d'actions
*}
constructor TSimpleActionList.Create;
begin
  inherited Create;
end;

{*
  Tableau zero-based des actions
  @param Index   Index d'une action
  @return Action à l'index spécifié
*}
function TSimpleActionList.GetItems(Index: Integer): TSimpleAction;
begin
  Result := GetItem(Index) as TSimpleAction;
end;

{*
  Enregistre la liste d'actions dans un flux
  @param Stream   Flux destination
*}
procedure TSimpleActionList.SaveToStream(Stream: TStream);
var
  Count, I: Integer;
begin
  Count := Self.Count;
  Stream.WriteBuffer(Count, 4);

  for I := 0 to Count-1 do
    Items[I].SaveToStream(Stream);
end;

{*
  Ajoute une nouvelle action
  @param ActionClass   Classe de l'action
  @return Action ajoutée
*}
function TSimpleActionList.AddNew(
  ActionClass: TSimpleActionClass): TSimpleAction;
begin
  Result := ActionClass.Create;
  Add(Result);
end;

{*
  Produit le code Delphi de la liste d'actions
  @param Code   Code sortie
*}
procedure TSimpleActionList.ProduceDelphiCode(Code: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Items[I].ProduceDelphiCode(Code, DefaultIndent);
end;

{--------------------}
{ TSimpleField class }
{--------------------}

{*
  [@inheritDoc]
*}
procedure TSimpleField.ProduceClassDef(Code: TStrings);
begin
  inherited;
end;

{---------------------}
{ TSimpleEffect class }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleEffect.Load(AImagesMaster: TImagesMaster; Stream: TStream);
begin
  inherited;

  FActions := TSimpleActionList.Load(Stream);
end;

{*
  [@inheritDoc]
*}
constructor TSimpleEffect.Create(AImagesMaster: TImagesMaster;
  const AID: TComponentID; const AName: string);
begin
  inherited;

  FActions := TSimpleActionList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TSimpleEffect.Destroy;
begin
  FActions.Free;
  
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleEffect.Save(Stream: TStream);
begin
  inherited;

  FActions.SaveToStream(Stream);
end;

{*
  [@inheritDoc]
*}
function TSimpleEffect.GetParentClassName: string;
begin
  Result := 'TCounterEffect'; {don't localize}
end;

{*
  [@inheritDoc]
*}
class function TSimpleEffect.ClassTitle: string;
begin
  Result := SSimpleEffectTitle;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleEffect.ProduceClassDef(Code: TStrings);
begin
  ProduceDefaultClassHeader(Code);

  Code.Add('  public');

  if CanEditImgNames and (ImgNames.Count > 0) then
  begin
    Code.Add(
      '    constructor Create(AMaster: TMaster; const AID: TComponentID;');
    Code.Add(
      '      const AName: string);');
    Code.Add('');
  end;

  Code.Add('    procedure Execute(Player: TPlayer; const Pos: T3DPoint;');
  Code.Add('      var GoOnMoving: Boolean); override;');

  ProduceDefaultClassFooter(Code);
end;

{*
  [@inheritDoc]
*}
procedure TSimpleEffect.ProduceImplementation(Code: TStrings);
begin
  inherited;

  if CanEditImgNames and (ImgNames.Count > 0) then
  begin
    Code.Add('');
    Code.Add('{*');
    Code.Add('  [@inheritDoc]');
    Code.Add('*}');
    Code.Add(Format(
      'constructor T%s.Create(AMaster: TMaster; const AID: TComponentID;',
      [ID]));
    Code.Add('  const AName: string);');
    Code.Add('begin');
    Code.Add('  inherited Create(AMaster, AID, AName);');

    ProduceDefaultImgNames(Code);

    Code.Add('end;');
  end;

  Code.Add('');
  Code.Add('{*');
  Code.Add('  [@inheritDoc]');
  Code.Add('*}');
  Code.Add(Format('procedure T%s.Execute(Player: TPlayer; const Pos: T3DPoint;',
    [ID]));
  Code.Add('  var GoOnMoving: Boolean);');
  Code.Add('begin');
  Code.Add('  inherited Execute(Player, Pos, GoOnMoving);');

  if Actions.Count > 0 then
  begin
    Code.Add('');
    Actions.ProduceDelphiCode(Code);
  end;

  Code.Add('end;');
end;

{---------------------}
{ TSimpleObject class }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleObject.Load(AImagesMaster: TImagesMaster; Stream: TStream);
begin
  inherited;

  FFindMessage := ReadStrFromStream(Stream);

  FHandledAction := ReadStrFromStream(Stream);

  if FHandledAction <> '' then
  begin
    Stream.ReadBuffer(FMinimumCount, 4);
    Stream.ReadBuffer(FDecrementOnUse, SizeOf(Boolean));
  end else
    FMinimumCount := 1;
end;

{*
  [@inheritDoc]
*}
constructor TSimpleObject.Create(AImagesMaster: TImagesMaster;
  const AID: TComponentID; const AName: string);
begin
  inherited;

  FMinimumCount := 1;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.Save(Stream: TStream);
begin
  inherited;

  WriteStrToStream(Stream, FFindMessage);

  WriteStrToStream(Stream, FHandledAction);

  if FHandledAction <> '' then
  begin
    Stream.WriteBuffer(FMinimumCount, 4);
    Stream.WriteBuffer(FDecrementOnUse, SizeOf(Boolean));
  end;
end;

{*
  [@inheritDoc]
*}
function TSimpleObject.GetParentClassName: string;
begin
  Result := 'TObjectDef'; {don't localize}
end;

{*
  [@inheritDoc]
*}
class function TSimpleObject.ClassTitle: string;
begin
  Result := SSimpleObjectTitle;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.ProduceIDConst(Code: TStrings);
begin
  inherited;

  Code.Add(Format('  id%sTool = ''%0:sTool'';', [ID]));
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.ProduceClassDef(Code: TStrings);
begin
  ProduceDefaultClassHeader(Code);

  Code.Add('  public');

  if CanEditImgNames and (ImgNames.Count > 0) then
  begin
    Code.Add(
      '    constructor Create(AMaster: TMaster; const AID: TComponentID;');
    Code.Add(
      '      const AName: string);');

    if HandledAction <> '' then
      Code.Add('');
  end;

  if HandledAction <> '' then
  begin
    Code.Add('    function AbleTo(Player: TPlayer;');
    Code.Add('      const Action: TPlayerAction): Boolean; override;');

    if DecrementOnUse then
    begin
      Code.Add('    procedure UseFor(Player: TPlayer; const Action: '+
        'TPlayerAction); override;');
    end;
  end;

  ProduceDefaultClassFooter(Code);
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.ProduceCreateComponents(Code: TStrings);
begin
  inherited;

  Code.Add(Format('  TObjectTool.Create(Master, id%0:sTool,', [ID]));
  Code.Add(Format('    Master.ObjectDef[id%s],', [ID]));
  Code.Add(Format('    %s, '''', '''');', [StrToStrRepres(FindMessage)]));
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.ProduceRegisterComponents(Code: TStrings);
const
  LineFormat =
    '  RegisterSingleComponentProc(Master.SquareComponent[id%sTool]);';
begin
  Code.Add(Format(LineFormat, [ID]));
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.ProduceImplementation(Code: TStrings);
begin
  inherited;

  // Constructor
  if CanEditImgNames and (ImgNames.Count > 0) then
  begin
    Code.Add('');
    Code.Add('{*');
    Code.Add('  [@inheritDoc]');
    Code.Add('*}');
    Code.Add(Format(
      'constructor T%s.Create(AMaster: TMaster; const AID: TComponentID;',
      [ID]));
    Code.Add('  const AName: string);');
    Code.Add('begin');
    Code.Add('  inherited Create(AMaster, AID, AName);');

    ProduceDefaultImgNames(Code);

    Code.Add('end;');
  end;

  if HandledAction <> '' then
  begin
    // AbleTo method
    Code.Add('');
    Code.Add('{*');
    Code.Add('  [@inheritDoc]');
    Code.Add('*}');
    Code.Add(Format('function T%s.AbleTo(Player: TPlayer;', [ID]));
    Code.Add('  const Action: TPlayerAction): Boolean;');
    Code.Add('begin');
    Code.Add(Format('  Result := ((Action = %s) and (Count[Player] >= %d)) or',
      [StrToStrRepres(HandledAction), MinimumCount]));
    Code.Add('    (inherited AbleTo(Player, Action));');
    Code.Add('end;');

    // UseFor method
    if DecrementOnUse then
    begin
      Code.Add('');
      Code.Add('{*');
      Code.Add('  [@inheritDoc]');
      Code.Add('*}');
      Code.Add(Format('procedure T%s.UseFor(Player: TPlayer; const Action: '+
        'TPlayerAction);', [ID]));
      Code.Add('begin');
      Code.Add(Format('  if Action = %s then',
        [StrToStrRepres(HandledAction)]));
      Code.Add('    Count[Player] := Count[Player]-1');
      Code.Add('  else');
      Code.Add('    inherited UseFor(Player, Action);');
      Code.Add('end;');
    end;
  end;
end;

{-----------------------}
{ TSimpleObstacle class }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleObstacle.Load(AImagesMaster: TImagesMaster; Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FConditionKind, SizeOf(TObstacleConditionKind));
  if FConditionKind = ockPlayerAction then
    FPlayerAction := ReadStrFromStream(Stream);

  FFailMessage := ReadStrFromStream(Stream);
end;

{*
  [@inheritDoc]
*}
constructor TSimpleObstacle.Create(AImagesMaster: TImagesMaster;
  const AID: TComponentID; const AName: string);
begin
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObstacle.Save(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FConditionKind, SizeOf(TObstacleConditionKind));
  if FConditionKind = ockPlayerAction then
    WriteStrToStream(Stream, FPlayerAction);

  WriteStrToStream(Stream, FFailMessage);
end;

{*
  [@inheritDoc]
*}
function TSimpleObstacle.GetParentClassName: string;
begin
  Result := 'TObstacle'; {don't localize}
end;

{*
  [@inheritDoc]
*}
class function TSimpleObstacle.ClassTitle: string;
begin
  Result := SSimpleObstacleTitle;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObstacle.ProduceClassDef(Code: TStrings);
begin
  ProduceDefaultClassHeader(Code);

  Code.Add('  public');

  if CanEditImgNames and (ImgNames.Count > 0) then
  begin
    Code.Add(
      '    constructor Create(AMaster: TMaster; const AID: TComponentID;');
    Code.Add(
      '      const AName: string);');
    Code.Add('');
  end;

  Code.Add('    procedure Pushing(Player: TPlayer; OldDirection: TDirection;');
  Code.Add('      KeyPressed: Boolean; const Src, Pos: T3DPoint;');
  Code.Add('      var Cancel, AbortExecute: Boolean); override;');

  ProduceDefaultClassFooter(Code);
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObstacle.ProduceImplementation(Code: TStrings);
begin
  inherited;

  if CanEditImgNames and (ImgNames.Count > 0) then
  begin
    Code.Add('');
    Code.Add('{*');
    Code.Add('  [@inheritDoc]');
    Code.Add('*}');
    Code.Add(Format(
      'constructor T%s.Create(AMaster: TMaster; const AID: TComponentID;',
      [ID]));
    Code.Add('  const AName: string);');
    Code.Add('begin');
    Code.Add('  inherited Create(AMaster, AID, AName);');

    ProduceDefaultImgNames(Code);

    Code.Add('end;');
  end;

  Code.Add('');
  Code.Add('{*');
  Code.Add('  [@inheritDoc]');
  Code.Add('*}');
  Code.Add(Format('procedure T%s.Pushing(Player: TPlayer; OldDirection: '+
    'TDirection;', [ID]));
  Code.Add('  KeyPressed: Boolean; const Src, Pos: T3DPoint;');
  Code.Add('  var Cancel, AbortExecute: Boolean);');
  Code.Add('begin');
  Code.Add('  inherited Pushing(Player, OldDirection, KeyPressed, Src, Pos,');
  Code.Add('    Cancel, AbortExecute);');

  if (ConditionKind <> ockNever) or (FailMessage <> '') then
  begin
    Code.Add('');
    Code.Add('  if not KeyPressed then');
    Code.Add('    Exit;');
    Code.Add('');
  end;

  case ConditionKind of
    ockAlways:
    begin
      Code.Add('  Player.Map.Map[Pos] := ChangeObstacle('+
        'Player.Map.Map[Pos], '''');');
    end;

    ockNever:
    begin
      Code.Add('  Player.ShowDialog(sBlindAlley,');
      Code.Add(Format('    %s,', [StrToStrRepres(FailMessage)]));
      Code.Add('    dtError, dbOK, 1, 0);');
    end;

    ockPlayerAction:
    begin
      Code.Add(Format('  if Player.DoAction(%s) then',
        [StrToStrRepres(PlayerAction)]));
      Code.Add('    Player.Map.Map[Pos] := ChangeObstacle('+
        'Player.Map.Map[Pos], '''')');
      Code.Add('  else');
      Code.Add('  begin');
      Code.Add('    Player.ShowDialog(sBlindAlley,');
      Code.Add(Format('      %s,', [StrToStrRepres(FailMessage)]));
      Code.Add('      dtError, dbOK, 1, 0);');
      Code.Add('  end;');
    end;
  end;

  Code.Add('end;');
end;

initialization
  RegisterClasses([
    TSimpleField, TSimpleEffect, TSimpleObject, TSimpleObstacle
  ]);
finalization
  UnRegisterClasses([
    TSimpleField, TSimpleEffect, TSimpleObject, TSimpleObstacle
  ]);
end.

