unit SimpleSquaresUtils;

interface

uses
  SysUtils, Classes, Graphics, Contnrs, ScUtils, ScDelphiLanguage, FunLabyUtils,
  FunLabyEditOTA, GR32;

resourcestring
  SSimpleEffectTitle = 'Effet simple';
  SSimpleObjectTitle = 'Objet simple';
  SSimpleObstacleTitle = 'Obstacle simple';

type
  {*
    Interface de toute partie de l'�diteur de composants de cases simples
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
      Marque le fichier source comme modifi�
    *}
    procedure MarkModified;

    property FunLabyEditMainForm: IOTAFunLabyEditMainForm50
      read GetFunLabyEditMainForm;
  end;

  {*
    D�finition d'un composant de case simple
    TSimpleSquare est la classe de base de toutes les d�finitions de composants
    de cases simples.
    @author sjrd
    @version 5.0
  *}
  TSimpleSquare = class(TFunLabyPersistent)
  private
    FID: TComponentID;   /// ID du composant
    FName: string;       /// Nom
    FPainter: TPainter;  /// Peintre
    FImgNames: TStrings; /// Noms des images
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetComponentType: string; virtual; abstract;
    function GetParentClassName: string; virtual;

    procedure ProduceDefaultImgNames(Code: TStrings);
    procedure ProduceInnerClass(Code: TStrings); virtual;

    function GetCanEditImgNames: Boolean; virtual;
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
    procedure ProduceComponents(Code: TStrings); virtual;
    procedure ProduceClass(Code: TStrings);

    property ID: TComponentID read FID;
    property ParentClassName: string read GetParentClassName;
    property Painter: TPainter read FPainter;
    property CanEditImgNames: Boolean read GetCanEditImgNames;
  published
    property Name: string read FName write FName;
    property ImgNames: TStrings read FImgNames stored GetCanEditImgNames;
  end;

  /// Classe de TSimpleSquare
  TSimpleSquareClass = class of TSimpleSquare;

  {*
    D�finition d'une action simple
    @author sjrd
    @version 5.0
  *}
  TSimpleAction = class(TFunLabyPersistent)
  protected
    function GetTitle: string; virtual; abstract;
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
    D�finition d'un terrain simple
    @author sjrd
    @version 5.0
  *}
  TSimpleField = class(TSimpleSquare)
  protected
    function GetComponentType: string; override;
  end;

  {*
    D�finition d'un effet simple
    @author sjrd
    @version 5.0
  *}
  TSimpleEffect = class(TSimpleSquare)
  private
    FActions: TSimpleActionList; /// Actions de l'effet
  protected
    function GetComponentType: string; override;
    function GetParentClassName: string; override;

    procedure ProduceInnerClass(Code: TStrings); override;
  public
    constructor Create(AImagesMaster: TImagesMaster); override;
    destructor Destroy; override;

    class function ClassTitle: string; override;
  published
    property Actions: TSimpleActionList read FActions;
  end;

  {*
    D�finition d'un objet simple
    @author sjrd
    @version 5.0
  *}
  TSimpleObject = class(TSimpleSquare)
  private
    FFindMessage: string; /// Message affich� quand on trouve cet objet

    FHandledAction: TPlayerAction; /// Action prise en charge par l'objet
    FMinimumCount: Integer;        /// Minimum d'objets requis
    FDecrementOnUse: Boolean;      /// True d�cr�mente l'objet quand utilis�

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
    - ockAlways : l'obstacle est toujours d�truit
    - ockNever : l'obstacle n'est jamais d�truit
    - ockCondition : l'obstacle est d�truit si le joueur peut faire une action
  *}
  TObstacleConditionKind = (ockAlways, ockNever, ockPlayerAction);

  {*
    D�finition d'un obstacle simple
    @author sjrd
    @version 5.0
  *}
  TSimpleObstacle = class(TSimpleSquare)
  private
    FConditionKind: TObstacleConditionKind; /// Type de condition
    FPlayerAction: TPlayerAction;           /// Action que doit faire le joueur

    FFailMessage: string; /// Message � afficher si rat�

    function IsPlayerActionStored: Boolean;
  protected
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

    property FailMessage: string read FFailMessage write FFailMessage;
  end;

const {don't localize}
  DefaultIndent = '    '; /// Indentation par d�faut
  ToolSuffix = 'Tool';    /// Suffixe 'Tool' pour les outils des objets

implementation

{---------------------}
{ TSimpleSquare class }
{---------------------}

{*
  Cr�e un nouveau composant de case simple
  @param AImagesMaster   Ma�tre d'images
*}
constructor TSimpleSquare.Create(AImagesMaster: TImagesMaster);
begin
  inherited Create;

  FPainter := TPainter.Create(AImagesMaster);
  FImgNames := FPainter.ImgNames;
end;

{*
  Cr�e un nouveau composant de case simple
  @param AImagesMaster   Ma�tre d'images
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
  Produit le code Delphi par d�faut pour le remplissage de ImgNames
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceDefaultImgNames(Code: TStrings);
var
  Line: string;
  I: Integer;
begin
  if (not CanEditImgNames) or (ImgNames.Count = 0) then
    Exit;

  Line := '  image';
  for I := 0 to ImgNames.Count-1 do
    Line := Line + ' ' + StrToStrRepres(ImgNames[I]) + ',';
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
  Indique si on peut modifier les noms des images � afficher
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
  Dessine le composant
  @param Canvas   Canevas cible
  @param X        Abscisse
  @param Y        Ordonn�e
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
  Recense les actions utilis�es
  @param Actions   Liste de toutes les actions utilis�es
*}
procedure TSimpleSquare.RegisterActions(Actions: TStrings);
begin
end;

{*
  Recense les attributs utilis�s
  @param Attributs   Liste de tous les attributs utilis�s
*}
procedure TSimpleSquare.RegisterAttributes(Attributes: TStrings);
begin
end;

{*
  Produit la d�finition des composants
  @param Code   Code produit
*}
procedure TSimpleSquare.ProduceComponents(Code: TStrings);
const
  Statement = '  %s: T%0:s;';
begin
  Code.Add(Format(Statement, [ID, StrToStrRepres(Name)]));
end;

{*
  Produit la d�finition de la classe
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
  Cr�e une nouvelle action
*}
constructor TSimpleAction.Create;
begin
  inherited Create;
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
  @return Action � l'index sp�cifi�
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
  @return Action ajout�e
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
  [@inheritDoc]
*}
function TSimpleField.GetComponentType: string;
begin
  Result := 'field'; {don't localize}
end;

{---------------------}
{ TSimpleEffect class }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleEffect.Create(AImagesMaster: TImagesMaster);
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
function TSimpleEffect.GetComponentType: string;
begin
  Result := 'effect'; {don't localize}
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
procedure TSimpleEffect.ProduceInnerClass(Code: TStrings);
begin
  inherited;

  if Actions.Count > 0 then
  begin
    Code.Add('  on Execute do');
    Code.Add('  begin');
    Code.Add('    inherited;');
    Code.Add('');
    Actions.ProduceFunDelphiCode(Code);
    Code.Add('  end;');
    Code.Add('');
  end;
end;

{*
  [@inheritDoc]
*}
class function TSimpleEffect.ClassTitle: string;
begin
  Result := SSimpleEffectTitle;
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
  Teste si les donn�es li�es � l'action doivent �tre enregistr�es
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

  Code.Add(Format('  %sTool: TObjectTool(', [ID]));
  Code.Add(Format('    ObjectDef: %s,', [ID]));
  Code.Add(Format('    FindMessage: %s', [StrToStrRepres(FindMessage)]));
  Code.Add('  );');
end;

{-----------------------}
{ TSimpleObstacle class }
{-----------------------}

{*
  Teste si la propri�t� PlayerAction doit �tre stock�e
  @return True si la propri�t� PlayerAction doit �tre stock�e, False sinon
*}
function TSimpleObstacle.IsPlayerActionStored: Boolean;
begin
  Result := ConditionKind = ockPlayerAction;
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

  if (ConditionKind = ockNever) and (FailMessage = '') then
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
      Code.Add('    Square.Obstacle := nil;');
    end;

    ockNever:
    begin
      Assert(FailMessage <> '');
      Code.Add(Format('    Player.ShowMessage(%s);',
        [StrToStrRepres(FailMessage)]));
    end;

    ockPlayerAction:
    begin
      Code.Add(Format('    if Player can %s then', [PlayerAction]));
      Code.Add('      Square.Obstacle := nil' + IIF(FailMessage = '', ';', ''));

      if FailMessage <> '' then
      begin
        Code.Add('    else');
        Code.Add(Format('      Player.ShowMessage(%s);',
          [StrToStrRepres(FailMessage)]));
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

