unit SimpleSquaresUtils;

interface

uses
  SysUtils, Classes, Graphics, Contnrs, ScUtils, ScTypInfo, FunLabyUtils,
  FunLabyEditOTA, GR32;

resourcestring
  SSimpleFieldTitle = 'Terrain simple';
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
  TSimpleSquare = class(TFunLabyPersistent)
  private
    FID: TComponentID;  /// ID du composant
    FName: string;      /// Nom
    FPainter: TPainter; /// Peintre
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetComponentType: string; virtual; abstract;
    function GetParentClassName: string; virtual;

    procedure ProduceDefaultImgNames(Code: TStrings);
    procedure ProduceInnerClass(Code: TStrings); virtual;

    function GetCanEditPainter: Boolean; virtual;
  public
    constructor Create(AImagesMaster: TImagesMaster); overload; virtual;
    constructor Create(AImagesMaster: TImagesMaster; const AID: TComponentID;
      const AName: string); overload;
    destructor Destroy; override;

    class function ClassTitle: string; virtual;

    procedure Draw(Canvas: TCanvas; BackgroundColor: TColor; X: Integer = 0;
      Y: Integer = 0); virtual;

    procedure RegisterActions(Actions: TStrings); virtual;
    procedure RegisterAttributes(Attributes: TStrings); virtual;
    procedure RegisterComponentIDs(ComponentIDs: TStrings); virtual;
    procedure ProduceComponents(Code: TStrings); virtual;
    procedure ProduceClass(Code: TStrings);

    property ID: TComponentID read FID;
    property ParentClassName: string read GetParentClassName;
    property CanEditPainter: Boolean read GetCanEditPainter;
  published
    property Name: string read FName write FName;
    property Painter: TPainter read FPainter;
  end;

  /// Classe de TSimpleSquare
  TSimpleSquareClass = class of TSimpleSquare;

  {*
    Définition d'une action simple
    @author sjrd
    @version 5.0
  *}
  TSimpleAction = class(TFunLabyPersistent)
  protected
    function GetTitle: string; virtual; abstract;

    procedure RegisterComponentIDs(ComponentIDs: TStrings); virtual;
  public
    constructor Create; virtual;

    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); virtual;

    property Title: string read GetTitle;
  end;

  /// Classe de TSimpleAction
  TSimpleActionClass = class of TSimpleAction;

  {*
    Liste d'actions simples
    @author sjrd
    @version 5.0
  *}
  TSimpleActionList = class(TFunLabyCollection)
  private
    function GetItems(Index: Integer): TSimpleAction;
  protected
    function CreateItem(ItemClass: TFunLabyPersistentClass):
      TFunLabyPersistent; override;
  public
    function AddNew(ActionClass: TSimpleActionClass): TSimpleAction;

    procedure ProduceFunDelphiCode(Code: TStrings);

    property Items[Index: Integer]: TSimpleAction read GetItems; default;
  end;

  {*
    Type de condition pour un terrain
    - fckAlways : le terrain accepte toujours le joueur
    - fckNever : le terrain n'accepte jamais le joueur
    - fckCondition : le terrain accepte le joueur s'il peut faire une action
  *}
  TFieldConditionKind = (fckAlways, fckNever, fckPlayerAction);

  {*
    Définition d'un terrain simple
    @author sjrd
    @version 5.0
  *}
  TSimpleField = class(TSimpleSquare)
  private
    FConditionKind: TFieldConditionKind; /// Type de condition
    FPlayerAction: TPlayerAction;        /// Action que doit faire le joueur

    FMessageText: string; /// Texte du message associé

    function IsPlayerActionStored: Boolean;
  protected
    function GetComponentType: string; override;

    procedure ProduceInnerClass(Code: TStrings); override;
  public
    class function ClassTitle: string; override;

    procedure RegisterActions(Actions: TStrings); override;
 published
    property ConditionKind: TFieldConditionKind
      read FConditionKind write FConditionKind default fckAlways;
    property PlayerAction: TPlayerAction read FPlayerAction write FPlayerAction
      stored IsPlayerActionStored;

    property MessageText: string read FMessageText write FMessageText;
  end;

  {*
    Définition d'un effet simple
    @author sjrd
    @version 5.0
  *}
  TSimpleEffect = class(TSimpleSquare)
  private
    FEvents: TStrings; /// Événements interceptés

    function GetEventCount: Integer;
    function GetEvents(Index: Integer): string;
    function GetEventActions(Index: Integer): TSimpleActionList;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetComponentType: string; override;
    function GetParentClassName: string; override;

    procedure EnumEvents(AEvents: TStrings); virtual;
    function GetDefaultEvent: string; virtual;

    procedure ProduceInnerClass(Code: TStrings); override;
  public
    constructor Create(AImagesMaster: TImagesMaster); override;
    destructor Destroy; override;

    procedure RegisterComponentIDs(ComponentIDs: TStrings); override;

    class function ClassTitle: string; override;

    procedure GetEventsAndActions(EventList: TStrings);

    property EventCount: Integer read GetEventCount;
    property Events[Index: Integer]: string read GetEvents;
    property EventActions[Index: Integer]: TSimpleActionList
      read GetEventActions;
    property DefaultEvent: string read GetDefaultEvent;
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

    function IsActionDataStored: Boolean;
  protected
    function GetComponentType: string; override;

    procedure ProduceInnerClass(Code: TStrings); override;
  public
    constructor Create(AImagesMaster: TImagesMaster); override;

    class function ClassTitle: string; override;

    procedure RegisterActions(Actions: TStrings); override;
    procedure ProduceComponents(Code: TStrings); override;
  published
    property FindMessage: string read FFindMessage write FFindMessage;

    property HandledAction: TPlayerAction
      read FHandledAction write FHandledAction;
    property MinimumCount: Integer read FMinimumCount write FMinimumCount
      stored IsActionDataStored default 1;
    property DecrementOnUse: Boolean read FDecrementOnUse write FDecrementOnUse
      stored IsActionDataStored default False;
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

    FMessageText: string; /// Texte du message associé

    function IsPlayerActionStored: Boolean;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetComponentType: string; override;

    procedure ProduceInnerClass(Code: TStrings); override;
  public
    class function ClassTitle: string; override;

    procedure RegisterActions(Actions: TStrings); override;
 published
    property ConditionKind: TObstacleConditionKind
      read FConditionKind write FConditionKind default ockAlways;
    property PlayerAction: TPlayerAction read FPlayerAction write FPlayerAction
      stored IsPlayerActionStored;

    property MessageText: string read FMessageText write FMessageText;
  end;

const {don't localize}
  DefaultIndent = '    '; /// Indentation par défaut
  ToolSuffix = 'Tool';    /// Suffixe 'Tool' pour les outils des objets

implementation

type
  TPainterAccess = class(TPainter)
  public
    property Description;
  end;

{---------------------}
{ TSimpleSquare class }
{---------------------}

{*
  Crée un nouveau composant de case simple
  @param AImagesMaster   Maître d'images
*}
constructor TSimpleSquare.Create(AImagesMaster: TImagesMaster);
begin
  inherited Create;

  FPainter := TPainter.Create(AImagesMaster);
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
  Create(AImagesMaster);

  FID := AID;
  FName := AName;
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
  [@inheritDoc]
*}
procedure TSimpleSquare.DefineProperties(Filer: TFunLabyFiler);
begin
  Filer.DefineFieldProperty('ID', TypeInfo(TComponentID), @FID, True);
  Filer.DefineStrings('Description', TPainterAccess(Painter).Description,
    nil, False);

  inherited;
end;

{*
  Nom de la classe parent
  @return Nom de la classe parent
*}
function TSimpleSquare.GetParentClassName: string;
begin
  Result := '';
end;

{*
  Produit le code Delphi par défaut pour le remplissage de ImgNames
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceDefaultImgNames(Code: TStrings);
var
  Line: string;
  I: Integer;
begin
  if (not CanEditPainter) or (Painter.IsEmpty) then
    Exit;

  Line := '  image';
  with TPainterAccess(Painter) do
    for I := 0 to Description.Count-1 do
      Line := Line + ' ' + StrToStrRepres(Description[I]) + ',';
  Line[Length(Line)] := ';';

  Code.Add(Line);
  Code.Add('');
end;

{*
  Produit le contenu de la classe
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceInnerClass(Code: TStrings);
begin
  Code.Add(Format('  name %s;', [StrToStrRepres(Name)]));
  ProduceDefaultImgNames(Code);
end;

{*
  Indique si on peut modifier les noms des images à afficher
  @return True si on peut les modifier, False sinon
*}
function TSimpleSquare.GetCanEditPainter: Boolean;
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
  Dessine le composant
  @param Canvas   Canevas cible
  @param X        Abscisse
  @param Y        Ordonnée
*}
procedure TSimpleSquare.Draw(Canvas: TCanvas; BackgroundColor: TColor;
  X: Integer = 0; Y: Integer = 0);
var
  Bitmap32: TBitmap32;
  Context: TDrawSquareContext;
begin
  Context := nil;
  Bitmap32 := TBitmap32.Create;
  try
    Bitmap32.SetSize(SquareSize, SquareSize);
    Bitmap32.Clear(Color32(BackgroundColor));

    Context := TDrawSquareContext.Create(Bitmap32);
    FPainter.Draw(Context);

    Canvas.CopyRect(SquareRect(X, Y), Bitmap32.Canvas, BaseSquareRect);
  finally
    Bitmap32.Free;
    Context.Free;
  end;
end;

{*
  Recense les actions utilisées
  @param Actions   Liste de toutes les actions utilisées
*}
procedure TSimpleSquare.RegisterActions(Actions: TStrings);
begin
end;

{*
  Recense les attributs utilisés
  @param Attributs   Liste de tous les attributs utilisés
*}
procedure TSimpleSquare.RegisterAttributes(Attributes: TStrings);
begin
end;

{*
  Recense les ID de composants utilisés
  @param Attributs   Liste de tous les ID de composants utilisés
*}
procedure TSimpleSquare.RegisterComponentIDs(ComponentIDs: TStrings);
begin
end;

{*
  Produit la définition des composants
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceComponents(Code: TStrings);
const
  Statement = '  %s: T%0:s;';
begin
  Code.Add(Format(Statement, [ID, StrToStrRepres(Name)]));
end;

{*
  Produit la définition de la classe
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceClass(Code: TStrings);
var
  ParentClassName: string;
begin
  ParentClassName := GetParentClassName;
  if ParentClassName <> '' then
    ParentClassName := '('+ParentClassName+')';

  Code.Add(GetComponentType + ' T' + ID + ParentClassName);

  ProduceInnerClass(Code);

  if Code[Code.Count-1] = '' then
    Code.Delete(Code.Count-1);
  Code.Add('end;');
  Code.Add('');
end;

{---------------------}
{ TSimpleAction class }
{---------------------}

{*
  Crée une nouvelle action
*}
constructor TSimpleAction.Create;
begin
  inherited Create;
end;

{*
  Recense les ID de composants utilisés
  @param Attributs   Liste de tous les ID de composants utilisés
*}
procedure TSimpleAction.RegisterComponentIDs(ComponentIDs: TStrings);
begin
end;

{*
  Produit le code FunDelphi de l'action
  @param Code     Code sortie
  @param Indent   Indentation
*}
procedure TSimpleAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
begin
end;

{-------------------------}
{ TSimpleActionList class }
{-------------------------}

{*
  Tableau zero-based des actions
  @param Index   Index d'une action
  @return Action à l'index spécifié
*}
function TSimpleActionList.GetItems(Index: Integer): TSimpleAction;
begin
  Result := (inherited Items[Index]) as TSimpleAction;
end;

{*
  [@inheritDoc]
*}
function TSimpleActionList.CreateItem(
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Result := TSimpleActionClass(ItemClass).Create;
end;

{*
  Ajoute une nouvelle action
  @param ActionClass   Classe de l'action
  @return Action ajoutée
*}
function TSimpleActionList.AddNew(
  ActionClass: TSimpleActionClass): TSimpleAction;
begin
  Result := TSimpleAction(inherited Add(ActionClass));
end;

{*
  Produit le code Delphi de la liste d'actions
  @param Code   Code sortie
*}
procedure TSimpleActionList.ProduceFunDelphiCode(Code: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Items[I].ProduceFunDelphiCode(Code, DefaultIndent);
end;

{--------------------}
{ TSimpleField class }
{--------------------}

{*
  Teste si la propriété PlayerAction doit être stockée
  @return True si la propriété PlayerAction doit être stockée, False sinon
*}
function TSimpleField.IsPlayerActionStored: Boolean;
begin
  Result := ConditionKind = fckPlayerAction;
end;

{*
  [@inheritDoc]
*}
function TSimpleField.GetComponentType: string;
begin
  Result := 'field'; {don't localize}
end;

{*
  [@inheritDoc]
*}
procedure TSimpleField.ProduceInnerClass(Code: TStrings);
begin
  inherited;

  if ConditionKind = fckAlways then
    Exit;

  Code.Add('  on Entering do');
  Code.Add('  begin');
  Code.Add('    inherited;');
  Code.Add('');

  case ConditionKind of
    fckNever:
    begin
      Code.Add('    Cancel;');

      if MessageText <> '' then
      begin
        Code.Add('');
        Code.Add('    if KeyPressed then');
        Code.Add(Format('      Player.ShowMessage(%s);',
          [StrToStrRepres(MessageText)]));
      end;
    end;

    fckPlayerAction:
    begin
      Code.Add(Format('    if Player cannot %s then', [PlayerAction]));
      Code.Add('    begin');
      Code.Add('      Cancel;');

      if MessageText <> '' then
      begin
        Code.Add('');
        Code.Add('      if KeyPressed then');
        Code.Add(Format('        Player.ShowMessage(%s);',
          [StrToStrRepres(MessageText)]));
      end;

      Code.Add('    end;');
    end;
  end;

  Code.Add('  end;');
  Code.Add('');
end;

{*
  [@inheritDoc]
*}
class function TSimpleField.ClassTitle: string;
begin
  Result := SSimpleFieldTitle;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleField.RegisterActions(Actions: TStrings);
begin
  if ConditionKind = fckPlayerAction then
    Actions.Add(PlayerAction);
end;

{---------------------}
{ TSimpleEffect class }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleEffect.Create(AImagesMaster: TImagesMaster);
var
  I: Integer;
begin
  inherited;

  FEvents := TStringList.Create;
  with TStringList(FEvents) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;

  EnumEvents(FEvents);
  for I := 0 to FEvents.Count-1 do
    FEvents.Objects[I] := TSimpleActionList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TSimpleEffect.Destroy;
var
  I: Integer;
begin
  if Assigned(FEvents) then
  begin
    for I := 0 to FEvents.Count-1 do
      FEvents.Objects[I].Free;
    FEvents.Free;
  end;

  inherited;
end;

{*
  Nombre d'événements
  @return Nombre d'événements
*}
function TSimpleEffect.GetEventCount: Integer;
begin
  Result := FEvents.Count;
end;

{*
  Tableau zero-based des noms des événements
  @param Index   Index compris entre 0 inclus et EventCount exclu
  @return Nom de l'événement spécifié
*}
function TSimpleEffect.GetEvents(Index: Integer): string;
begin
  Result := FEvents[Index];
end;

{*
  Tableau zero-based des actions correspondant aux événements
  @param Index   Index compris entre 0 inclus et EventCount exclu
  @return Action de l'événement spécifié
*}
function TSimpleEffect.GetEventActions(Index: Integer): TSimpleActionList;
begin
  Result := TSimpleActionList(FEvents.Objects[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TSimpleEffect.DefineProperties(Filer: TFunLabyFiler);
var
  I: Integer;
begin
  inherited;

  if psReading in PersistentState then
  begin {compatibility}
    I := FEvents.IndexOf(DefaultEvent);
    if I >= 0 then
      Filer.DefinePersistent('Actions', EventActions[I]);
  end; {compatibility}

  for I := 0 to EventCount-1 do
    Filer.DefinePersistent('On'+Events[I], EventActions[I]);
end;

{*
  [@inheritDoc]
*}
function TSimpleEffect.GetComponentType: string;
begin
  Result := 'effect'; {don't localize}
end;

{*
  [@inheritDoc]
*}
procedure TSimpleEffect.EnumEvents(AEvents: TStrings);
begin
  AEvents.Add('Execute'); {don't localize}
end;

{*
  [@inheritDoc]
*}
function TSimpleEffect.GetDefaultEvent: string;
begin
  Result := 'Execute'; {don't localize}
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
procedure TSimpleEffect.RegisterComponentIDs(ComponentIDs: TStrings);
var
  I, J: Integer;
  Actions: TSimpleActionList;
begin
  inherited;

  for I := 0 to EventCount-1 do
  begin
    Actions := EventActions[I];

    for J := 0 to Actions.Count-1 do
      Actions[J].RegisterComponentIDs(ComponentIDs);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleEffect.ProduceInnerClass(Code: TStrings);
var
  I: Integer;
  Actions: TSimpleActionList;
begin
  inherited;

  for I := 0 to EventCount-1 do
  begin
    Actions := EventActions[I];

    if Actions.Count > 0 then
    begin
      Code.Add(Format('  on %s do', [Events[I]]));
      Code.Add('  begin');
      Code.Add('    inherited;');
      Code.Add('');
      Actions.ProduceFunDelphiCode(Code);
      Code.Add('  end;');
      Code.Add('');
    end;
  end;
end;

{*
  [@inheritDoc]
*}
class function TSimpleEffect.ClassTitle: string;
begin
  Result := SSimpleEffectTitle;
end;

{*
  Récupère une liste des événements et leurs actions
  @param EventList   Remplie avec les noms des événements et leurs actions
*}
procedure TSimpleEffect.GetEventsAndActions(EventList: TStrings);
begin
  EventList.Assign(FEvents);
end;

{---------------------}
{ TSimpleObject class }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleObject.Create(AImagesMaster: TImagesMaster);
begin
  inherited;

  FMinimumCount := 1;
end;

{*
  Teste si les données liées à l'action doivent être enregistrées
*}
function TSimpleObject.IsActionDataStored: Boolean;
begin
  Result := FHandledAction <> '';
end;

{*
  [@inheritDoc]
*}
function TSimpleObject.GetComponentType: string;
begin
  Result := 'object'; {don't localize}
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.ProduceInnerClass(Code: TStrings);
var
  Line: string;
begin
  inherited;

  if HandledAction <> '' then
  begin
    Line := '  action '+HandledAction;
    if MinimumCount = 0 then
      Line := Line + ' if True'
    else if MinimumCount > 1 then
      Line := Line + Format(' if Player has %d Self', [MinimumCount]);
    if DecrementOnUse then
      Line := Line + ' then'
    else
      Line := Line + ';';
    Code.Add(Line);

    if DecrementOnUse then
      Code.Add(Format('    Player discards %d Self;',
        [MinimumCount]));

    Code.Add('');
  end;
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
procedure TSimpleObject.RegisterActions(Actions: TStrings);
begin
  if HandledAction <> '' then
    Actions.Add(HandledAction);
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObject.ProduceComponents(Code: TStrings);
begin
  inherited;

  Code.Add(Format('  %sTool: TObjectTool', [ID]));
  Code.Add(Format('    ObjectDef: %s;', [ID]));
  Code.Add(Format('    FindMessage: %s;', [StrToStrRepres(FindMessage)]));
  Code.Add('  end;');
end;

{-----------------------}
{ TSimpleObstacle class }
{-----------------------}

{*
  Teste si la propriété PlayerAction doit être stockée
  @return True si la propriété PlayerAction doit être stockée, False sinon
*}
function TSimpleObstacle.IsPlayerActionStored: Boolean;
begin
  Result := ConditionKind = ockPlayerAction;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObstacle.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  // Backward compatibility
  if psReading in PersistentState then
  begin
    Filer.DefineFieldProperty('FailMessage', TypeInfo(string),
      @FMessageText, False);
  end;
end;

{*
  [@inheritDoc]
*}
function TSimpleObstacle.GetComponentType: string;
begin
  Result := 'obstacle'; {don't localize}
end;

{*
  [@inheritDoc]
*}
procedure TSimpleObstacle.ProduceInnerClass(Code: TStrings);
begin
  inherited;

  if (ConditionKind = ockNever) and (MessageText = '') then
    Exit;

  Code.Add('  on Pushing do');
  Code.Add('  begin');
  Code.Add('    inherited;');
  Code.Add('');
  Code.Add('    if not KeyPressed then');
  Code.Add('      Exit;');
  Code.Add('');

  case ConditionKind of
    ockAlways:
    begin
      if MessageText <> '' then
        Code.Add(Format('    Player.ShowMessage(%s);',
          [StrToStrRepres(MessageText)]));
      Code.Add('    Square.Obstacle := nil;');
    end;

    ockNever:
    begin
      Assert(MessageText <> '');
      Code.Add(Format('    Player.ShowMessage(%s);',
        [StrToStrRepres(MessageText)]));
    end;

    ockPlayerAction:
    begin
      Code.Add(Format('    if Player can %s then', [PlayerAction]));
      Code.Add('      Square.Obstacle := nil' + IIF(MessageText = '', ';', ''));

      if MessageText <> '' then
      begin
        Code.Add('    else');
        Code.Add(Format('      Player.ShowMessage(%s);',
          [StrToStrRepres(MessageText)]));
      end;
    end;
  end;

  Code.Add('  end;');
  Code.Add('');
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
procedure TSimpleObstacle.RegisterActions(Actions: TStrings);
begin
  if ConditionKind = ockPlayerAction then
    Actions.Add(PlayerAction);
end;

initialization
  FunLabyRegisterClasses([
    TSimpleField, TSimpleEffect, TSimpleObject, TSimpleObstacle
  ]);
finalization
  FunLabyUnRegisterClasses([
    TSimpleField, TSimpleEffect, TSimpleObject, TSimpleObstacle
  ]);
end.

