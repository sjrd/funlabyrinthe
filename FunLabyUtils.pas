unit FunLabyUtils;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, Controls, IniFiles, ScUtils;

resourcestring
  sDefaultObjectInfos = '%s : %d';

const
  ScrewSize = 30;
  clTransparent = clTeal;

type
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);
  P3DPoint = ^T3DPoint;

  TMaster = class;
  TPlayer = class;

  TImagesMaster = class
  private
    FImgList : TImageList;
    FImgNames : TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const ImgName : string) : integer;
    procedure Draw(Index : integer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); overload;
    procedure Draw(const ImgName : string; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); overload;
  end;

  TPainter = class
  private
    FMaster : TImagesMaster;
    FImgNames : TStrings;
    FCachedImg : TBitmap;

    procedure ImgNamesChange(Sender : TObject);
  public
    constructor Create(AMaster : TImagesMaster);
    destructor Destroy; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);

    property ImgNames : TStrings read FImgNames;
  end;

  TFunLabyComponent = class
  private
    FMaster : TMaster;
    FName : string;
    FPainter : TPainter;
  protected
    property Painter : TPainter read FPainter;
  public
    constructor Create(AMaster : TMaster; const AName : string);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0); virtual;

    property Master : TMaster read FMaster;
    property Name : string read FName;
  end;

  TPlayerPlugin = class
  private
    FMaster : TMaster;
    FPainterBefore : TPainter;
    FPainterAfter : TPainter;
  protected
    property PainterBefore : TPainter read FPainterBefore;
    property PainterAfter : TPainter read FPainterAfter;
  public
    constructor Create(AMaster : TMaster);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure DrawBefore(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;
    procedure DrawAfter(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;

    procedure Moving(Player : TPlayer; OldDirection : TDirection;
      Src, Dest : T3DPoint; var Cancel : boolean); virtual;

    function CanYou(Player : TPlayer; Action : integer) : boolean; virtual;
  end;

  TPlayerObject = class(TFunLabyComponent)
  private
    FPlayer : TPlayer;
    FCount : integer;
  protected
    function GetShownInfos : string; virtual;
  public
    constructor Create(AMaster : TMaster; const AName : string;
      APlayer : TPlayer);

    function CanYou(Action : integer) : boolean; virtual;
    procedure UseFor(Action : integer); virtual;

    property Player : TPlayer read FPlayer;
    property Count : integer read FCount write FCount;
    property ShownInfos : string read GetShownInfos;
  end;

  TPlayer = class(TFunLabyComponent)
  private
    FPosition : T3DPoint;
    FPosAddr : P3DPoint;
    FDirection : TDirection;
    FDirPainters : array[diNorth..diWest] of TPainter;
    FColor : TColor;
    FPlugins : TObjectList;
    FObjects : TObjectList;

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlayerPlugin;
    function GetObjectCount : integer;
    function GetObjects(Index : integer) : TPlayerObject;

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlayerPlugin read GetPlugins;
  public
    constructor Create(AMaster : TMaster; const AName : string);
    destructor Destroy; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    procedure AddPlugin(Plugin : TPlayerPlugin);
    procedure RemovePlugin(Plugin : TPlayerPlugin);

    function CanYou(Action : integer) : boolean;

    procedure MoveTo(Dir : TDirection);

    property Position : P3DPoint read FPosAddr;
    property Direction : TDirection read FDirection write FDirection;
    property Color : TColor read FColor write FColor;
    property ObjectCount : integer read GetObjectCount;
    property Objects[index : integer] : TPlayerObject read GetObjects;
  end;

  TScrew = class(TFunLabyComponent)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      Src, Pos : T3DPoint; var Cancel, AbortEnter : boolean); virtual;
    procedure Entered(Player : TPlayer; Src, Pos : T3DPoint;
      var GoOnMoving : boolean); virtual;
    procedure Exiting(Player : TPlayer; OldDirection : TDirection;
      Pos, Dest : T3DPoint; var Cancel : boolean); virtual;
    procedure Exited(Player : TPlayer; Pos, Dest : T3DPoint); virtual;
  end;

  TMap = class
  private
    FMaster : TMaster;
    FDimensions : T3DPoint;

    function GetMap(Position : T3DPoint) : TScrew;
  public
    constructor Create(AMaster : TMaster; ADimensions : T3DPoint);

    property Map[Position : T3DPoint] : TScrew read GetMap; default;
  end;

  TMaster = class
  private
    FImagesMaster : TImagesMaster;
    FMap : TMap;
    FPlayers : TObjectList;

    function GetPlayerCount : integer;
    function GetPlayers(Index : integer) : TPlayer;
  public
    constructor Create;
    destructor Destroy; override;

    property ImagesMaster : TImagesMaster read FImagesMaster;
    property Map : TMap read FMap;
    property PlayerCount : integer read GetPlayerCount;
    property Players[index : integer] : TPlayer read GetPlayers;
  end;

var
  sScrewFileName : string = 'Cases\%s.bmp';

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;

implementation

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;
begin
  Result := Rect(X, Y, X+ScrewSize, Y+ScrewSize);
end;

////////////////////////////
/// Classe TImagesMaster ///
////////////////////////////

constructor TImagesMaster.Create;
begin
  inherited Create;
  FImgList := TImageList.CreateSize(ScrewSize, ScrewSize);
  FImgNames := THashedStringList.Create;
end;

destructor TImagesMaster.Destroy;
begin
  FImgNames.Free;
  FImgList.Free;
  inherited;
end;

function TImagesMaster.IndexOf(const ImgName : string) : integer;
var NewImg : TBitmap;
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
  begin
    NewImg := TBitmap.Create;
    try
      NewImg.LoadFromFile(Format(sScrewFileName, [ImgName]));
      FImgList.AddMasked(NewImg, clTransparent);
      Result := FImgNames.Add(ImgName);
    finally
      NewImg.Free;
    end;
  end;
end;

procedure TImagesMaster.Draw(Index : integer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FImgList.Draw(Canvas, X, Y, Index);
end;

procedure TImagesMaster.Draw(const ImgName : string; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  Draw(IndexOf(ImgName), Canvas, X, Y);
end;

///////////////////////
/// Classe TPainter ///
///////////////////////

constructor TPainter.Create(AMaster : TImagesMaster);
begin
  inherited Create;
  FMaster := AMaster;
  FImgNames := TStringList.Create;
  TStringList(FImgNames).OnChange := ImgNamesChange;
  FCachedImg := TBitmap.Create;
  with FCachedImg do
  begin
    Width := ScrewSize;
    Height := ScrewSize;
    Canvas.Brush.Color := clTransparent;
    Canvas.Pen.Style := psClear;
  end;
  ImgNamesChange(nil);
end;

destructor TPainter.Destroy;
begin
  FCachedImg.Free;
  FImgNames.Free;
  inherited;
end;

procedure TPainter.ImgNamesChange(Sender : TObject);
var I : integer;
begin
  FCachedImg.Canvas.Rectangle(0, 0, ScrewSize, ScrewSize);
  for I := 0 to FImgNames.Count-1 do
    FMaster.Draw(FImgNames[I], FCachedImg.Canvas);
end;

procedure TPainter.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.BrushCopy(ScrewRect(X, Y), FCachedImg, ScrewRect, clTransparent);
end;

////////////////////////////////
/// Classe TFunLabyComponent ///
////////////////////////////////

constructor TFunLabyComponent.Create(AMaster : TMaster; const AName : string);
begin
  inherited Create;
  FMaster := AMaster;
  FName := AName;
  FPainter := TPainter.Create(FMaster.ImagesMaster);
  FPainter.FImgNames.BeginUpdate;
end;

destructor TFunLabyComponent.Destroy;
begin
  FPainter.Free;
  inherited;
end;

procedure TFunLabyComponent.AfterConstruction;
begin
  inherited;
  FPainter.FImgNames.EndUpdate;
end;

procedure TFunLabyComponent.Draw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
end;

////////////////////////////
/// Classe TPlayerPlugin ///
////////////////////////////

constructor TPlayerPlugin.Create(AMaster : TMaster);
begin
  inherited Create;
  FMaster := AMaster;
  FPainterBefore := TPainter.Create(FMaster.ImagesMaster);
  FPainterBefore.FImgNames.BeginUpdate;
  FPainterAfter := TPainter.Create(FMaster.ImagesMaster);
  FPainterAfter.FImgNames.BeginUpdate;
end;

destructor TPlayerPlugin.Destroy;
begin
  FPainterAfter.Free;
  FPainterBefore.Free;
  inherited;
end;

procedure TPlayerPlugin.AfterConstruction;
begin
  inherited;
  FPainterBefore.FImgNames.EndUpdate;
  FPainterAfter.FImgNames.EndUpdate;
end;

procedure TPlayerPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterBefore.Draw(Canvas, X, Y);
end;

procedure TPlayerPlugin.DrawAfter(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterAfter.Draw(Canvas, X, Y);
end;

procedure TPlayerPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  Src, Dest : T3DPoint; var Cancel : boolean);
begin
end;

function TPlayerPlugin.CanYou(Player : TPlayer; Action : integer) : boolean;
begin
  Result := False;
end;

////////////////////////////
/// Classe TPlayerObject ///
////////////////////////////

constructor TPlayerObject.Create(AMaster : TMaster; const AName : string;
  APlayer : TPlayer);
begin
  inherited Create(AMaster, AName);
  FPlayer := APlayer;
  FCount := 0;
end;

function TPlayerObject.GetShownInfos : string;
begin
  Result := Format(sDefaultObjectInfos, [FName, FCount]);
end;

function TPlayerObject.CanYou(Action : integer) : boolean;
begin
  Result := False;
end;

procedure TPlayerObject.UseFor(Action : integer);
begin
end;

//////////////////////
/// Classe TPlayer ///
//////////////////////

constructor TPlayer.Create(AMaster : TMaster; const AName : string);
var Dir : TDirection;
begin
  inherited;
  FPosition := Point3D(0, 0, 0);
  FPosAddr := @FPosition;
  FDirection := diNone;
  for Dir in [diNorth..diWest] do
    FDirPainters[Dir] := nil;
  FColor := clBlue;
  FPlugins := TObjectList.Create(False);
  FObjects := TObjectList.Create;
end;

destructor TPlayer.Destroy;
var Dir : TDirection;
begin
  FObjects.Free;
  FPlugins.Free;
  for Dir in [diNorth..diWest] do if Assigned(FDirPainters[Dir]) then
    FDirPainters[Dir].Free;
  inherited;
end;

function TPlayer.GetPluginCount : integer;
begin
  Result := FPlugins.Count;
end;

function TPlayer.GetPlugins(Index : integer) : TPlayerPlugin;
begin
  Result := TPlayerPlugin(FPlugins[Index]);
end;

function TPlayer.GetObjectCount : integer;
begin
  Result := FObjects.Count;
end;

function TPlayer.GetObjects(Index : integer) : TPlayerObject;
begin
  Result := TPlayerObject(FObjects[Index]);
end;

procedure TPlayer.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
var I : integer;
begin
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawBefore(Self, Canvas, X, Y);

  if FColor = clDefault then
  begin
    if (FDirection = diNone) or (not Assigned(FDirPainters[FDirection])) then
      FPainter.Draw(Canvas, X, Y)
    else
      FDirPainters[FDirection].Draw(Canvas, X, Y);
  end else
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      Pen.Style := psClear;
      Ellipse(X+6, Y+6, X+24, Y+24);
    end;
  end;

  for I := 0 to PluginCount-1 do
    Plugins[I].DrawAfter(Self, Canvas, X, Y);
end;

procedure TPlayer.AddPlugin(Plugin : TPlayerPlugin);
begin
  FPlugins.Add(Plugin);
end;

procedure TPlayer.RemovePlugin(Plugin : TPlayerPlugin);
begin
  FPlugins.Remove(Plugin);
end;

function TPlayer.CanYou(Action : integer) : boolean;
var I, GoodObjectCount : integer;
    GoodObjects : array of TPlayerObject;
    GoodObject : TPlayerObject;
begin
  Result := True;

  // Les plug-in ont la priorité, puisqu'ils n'ont pas d'effet de bord
  for I := 0 to PluginCount-1 do if Plugins[I].CanYou(Self, Action) then exit;

  // Listage des objets susceptibles d'aider le joueur
  SetLength(GoodObjects, ObjectCount);
  GoodObjectCount := 0;
  for I := 0 to ObjectCount-1 do if Objects[I].CanYou(Action) then
  begin
    GoodObjects[GoodObjectCount] := Objects[I];
    inc(GoodObjectCount);
  end;

  // Aucun objet trouvé : échec
  if GoodObjectCount = 0 then
  begin
    Result := False;
    exit;
  end;

  // Si plusieurs objets, demande au joueur lequel utiliser
  if GoodObjectCount = 1 then GoodObject := GoodObjects[0] else
  begin
    { TODO 1 : Demander au joueur quel objet utiliser }
    GoodObject := GoodObjects[0];
  end;

  // Utilisation de l'objet
  GoodObject.UseFor(Action);
end;

procedure TPlayer.MoveTo(Dir : TDirection);
begin
  { TODO 2 : Déplacement du joueur }
end;

/////////////////////
/// Classe TScrew ///
/////////////////////

procedure TScrew.Entering(Player : TPlayer; OldDirection : TDirection;
  Src, Pos : T3DPoint; var Cancel, AbortEnter : boolean);
begin
end;

procedure TScrew.Entered(Player : TPlayer; Src, Pos : T3DPoint;
  var GoOnMoving : boolean);
begin
end;

procedure TScrew.Exiting(Player : TPlayer; OldDirection : TDirection;
  Pos, Dest : T3DPoint; var Cancel : boolean);
begin
end;

procedure TScrew.Exited(Player : TPlayer; Pos, Dest : T3DPoint);
begin
end;

///////////////////
/// Classe TMap ///
///////////////////

constructor TMap.Create(AMaster : TMaster; ADimensions : T3DPoint);
begin
  inherited Create;
  FMaster := AMaster;
  FDimensions := ADimensions;
end;

function TMap.GetMap(Position : T3DPoint) : TScrew;
begin
  Result := nil;
end;

//////////////////////
/// Classe TMaster ///
//////////////////////

constructor TMaster.Create;
begin
  inherited Create;
  FImagesMaster := TImagesMaster.Create;
  FMap := TMap.Create(Self, Point3D(1, 1, 1));
  FPlayers := TObjectList.Create;
end;

destructor TMaster.Destroy;
begin
  FPlayers.Free;
  FMap.Free;
  FImagesMaster.Free;
  inherited;
end;

function TMaster.GetPlayerCount : integer;
begin
  Result := FPlayers.Count;
end;

function TMaster.GetPlayers(Index : integer) : TPlayer;
begin
  Result := TPlayer(FPlayers[Index]);
end;

initialization
  sScrewFileName := Dir+sScrewFileName;
end.

