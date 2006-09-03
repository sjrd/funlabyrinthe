unit FunLabyUtils;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, Controls, IniFiles, ScUtils,
  Forms, Dialogs, StdCtrls, Math;

resourcestring
  sDefaultObjectInfos = '%s : %d';
  sWhichObject = 'Quel objet voulez-vous utiliser ?';

const
  ScrewSize = 30;
  clTransparent = clTeal;

type
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);
  P3DPoint = ^T3DPoint;
  TScrewCode = type Byte;

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
      KeyPressed : boolean; Src, Dest : T3DPoint;
      var Cancel : boolean); virtual;
    procedure Moved(Player : TPlayer; KeyPressed : boolean;
      Src, Dest : T3DPoint); virtual;

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

    function Move(Dir : TDirection; KeyPressed : boolean;
      out Redo : boolean) : boolean;

    property Position : P3DPoint read FPosAddr;
    property Direction : TDirection read FDirection write FDirection;
    property Color : TColor read FColor write FColor;
    property ObjectCount : integer read GetObjectCount;
    property Objects[index : integer] : TPlayerObject read GetObjects;
  end;

  TScrew = class(TFunLabyComponent)
  private
    FCode : TScrewCode;
  public
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); virtual;
    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); virtual;
    procedure Exiting(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Pos, Dest : T3DPoint;
      var Cancel : boolean); virtual;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); virtual;

    property Code : TScrewCode read FCode;
  end;

  TScrewsMaster = class
  private
    FMaster : TMaster;
    FScrews : array[33..255] of TScrew;

    function GetScrews(Code : TScrewCode) : TScrew;
  public
    constructor Create(AMaster : TMaster);
    destructor Destroy; override;

    property Master : TMaster read FMaster;
    property Screws[Code : TScrewCode] : TScrew read GetScrews; default;
  end;

  TMap = class
  private
    FMaster : TMaster;
    FDimensions : T3DPoint;
    FMap : array of TScrewCode;
    FOutside : array of TScrewCode;

    function GetCodeMap(Position : T3DPoint) : TScrewCode;
    procedure SetCodeMap(Position : T3DPoint; Value : TScrewCode);
    function GetMap(Position : T3DPoint) : TScrew;
  public
    constructor Create(AMaster : TMaster; ADimensions : T3DPoint);

    function InMap(Position : T3DPoint) : boolean;

    property Master : TMaster read FMaster;
    property Dimensions : T3DPoint read FDimensions;
    property CodeMap[Position : T3DPoint] : TScrewCode
      read GetCodeMap write SetCodeMap;
    property Map[Position : T3DPoint] : TScrew read GetMap; default;
  end;

  TMaster = class
  private
    FImagesMaster : TImagesMaster;
    FScrewsMaster : TScrewsMaster;
    FMap : TMap;
    FPlayers : TObjectList;

    function GetPlayerCount : integer;
    function GetPlayers(Index : integer) : TPlayer;
  public
    constructor Create;
    destructor Destroy; override;

    property ImagesMaster : TImagesMaster read FImagesMaster;
    property ScrewsMaster : TScrewsMaster read FScrewsMaster;
    property Map : TMap read FMap;
    property PlayerCount : integer read GetPlayerCount;
    property Players[index : integer] : TPlayer read GetPlayers;
  end;

var
  sScrewFileName : string = 'Screws\%s.bmp';

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;

implementation

uses
  Screws;

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X+ScrewSize;
  Result.Bottom := Y+ScrewSize;
end;

function MessageDlgRadio(const Msg : string; DlgType : TMsgDlgType;
  Buttons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Création de la boite de dialogue
  Form := CreateMessageDialog(Msg, DlgType, Buttons);

  with Form do
  try
    // On augmente la taille de la boite de dialogue
    Height := Height + Length(RadioTitles) * 25;

    // Création des boutons radio et détermination de la largeur minimale
    MaxWidth := 0;
    for I := High(RadioTitles) downto Low(RadioTitles) do
    with TRadioButton.Create(Form) do
    begin
      FreeNotification(Form);
      Parent := Form;
      Width := Canvas.TextWidth(RadioTitles[I]) + 20;
      MaxWidth := Max(MaxWidth, Width-20);
      Caption := RadioTitles[I];
      Checked := I = Selected;
      Tag := I;
      Left := 8;

      // OverButtons indique si les RadioBox sont au-dessus ou en-dessous des
      // boutons
      if OverButtons then
        Top := Form.Height - 90 - (High(RadioTitles) - I) * 25
      else
        Top := Form.Height - 50 - (High(RadioTitles) - I) * 25;
    end;

    // Il faut aussi vérifier que la fiche peut afficher les textes des RadioBox
    // en entier
    OldWidth := 0;
    if (MaxWidth + 40) > Width then
    begin
      OldWidth := Width;
      Width := MaxWidth +40;
    end;

    for I := 0 to ComponentCount-1 do
    begin
      // On récupère chaque bouton
      if Components[I] is TButton then
      begin
        Button := TButton(Components[I]);

        // On met le bon bouton par défaut et on le sélectionne
        Button.Default := Button.ModalResult = DefButton;
        if Button.Default then ActiveControl := Button;

        // S'il le faut, décaler tous les boutons vers le bas
        if OverButtons then
          Button.Top := Button.Top + Length(RadioTitles) * 25;

        // S'il le faut, décaler tous les boutons vers la droite
        if OldWidth > 0 then
          Button.Left := Button.Left + (Width - OldWidth) div 2;
      end;
    end;

    // On centre la boite de dialogue
    Position := poScreenCenter;

    // Affichage de la boîte de dialogue
    Result := ShowModal;

    // Récupération du choix de l'utilisateur
    Selected := -1;
    for I := 0 to ControlCount-1 do
      if (Controls[I] is TRadioButton) and TRadioButton(Controls[I]).Checked then
        Selected := Controls[I].Tag;
  finally
    Free;
  end;
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
  FPainter.ImgNames.BeginUpdate;
end;

destructor TFunLabyComponent.Destroy;
begin
  FPainter.Free;
  inherited;
end;

procedure TFunLabyComponent.AfterConstruction;
begin
  inherited;
  FPainter.ImgNames.EndUpdate;
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
  FPainterBefore.ImgNames.BeginUpdate;
  FPainterAfter := TPainter.Create(FMaster.ImagesMaster);
  FPainterAfter.ImgNames.BeginUpdate;
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
  FPainterBefore.ImgNames.EndUpdate;
  FPainterAfter.ImgNames.EndUpdate;
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
  KeyPressed : boolean; Src, Dest : T3DPoint; var Cancel : boolean);
begin
end;

procedure TPlayerPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
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
    RadioTitles : array of string;
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
    SetLength(RadioTitles, GoodObjectCount);
    for I := 0 to GoodObjectCount-1 do
      RadioTitles[I] := GoodObjects[I].Name;
    I := 0;
    MessageDlgRadio(sWhichObject, mtConfirmation, [mbOK], mrOK,
      RadioTitles, I, True);
    GoodObject := GoodObjects[I];
  end;

  // Utilisation de l'objet
  GoodObject.UseFor(Action);
end;

function TPlayer.Move(Dir : TDirection; KeyPressed : boolean;
  out Redo : boolean) : boolean;
var I : integer;
    Src, Dest : T3DPoint;
    OldDir : TDirection;
    Cancel, AbortEntered : boolean;
begin
  // Initialisation des variables
  Result := False;
  Redo := False;
  Src := FPosition;
  Dest := FPosition;
  case Dir of
    diNorth : dec(Dest.Y);
    diEast  : inc(Dest.X);
    diSouth : inc(Dest.Y);
    diWest  : dec(Dest.X);
    else exit;
  end;
  OldDir := FDirection;
  FDirection := Dir;
  Cancel := False;
  AbortEntered := False;

  // Premier passage : le déplacement est-il permis ?
  begin
    // Case source : exiting
    Master.Map[Src].Exiting(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Plug-in : moving
    for I := 0 to PluginCount-1 do
      Plugins[I].Moving(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Case destination : entering
    Master.Map[Dest].Entering(Self, OldDir, KeyPressed, Src, Dest, Cancel,
      AbortEntered);
    if Cancel then exit;
  end;

  // Déplacement du joueur (à moins qu'il ait été déplacé par ailleurs)
  if Same3DPoint(FPosition, Src) then
    FPosition := Dest
  else
    Dest := FPosition;

  // Second passage : le déplacement a été fait
  begin
    // Case source : exited
    Master.Map[Src].Exited(Self, KeyPressed, Src, Dest);
    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, KeyPressed, Src, Dest);
    // Case destination : entered (sauf si AbortEntered a été positionné à True)
    if not AbortEntered then
      Master.Map[Dest].Entered(Self, KeyPressed, Src, Dest, Redo);
  end;
end;

/////////////////////
/// Classe TScrew ///
/////////////////////

constructor TScrew.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName);
  FCode := ACode;
end;

procedure TScrew.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
end;

procedure TScrew.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
end;

procedure TScrew.Exiting(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Pos, Dest : T3DPoint; var Cancel : boolean);
begin
end;

procedure TScrew.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
end;

////////////////////////////
/// Classe TScrewsMaster ///
////////////////////////////

constructor TScrewsMaster.Create(AMaster : TMaster);
var Code : TScrewCode;
begin
  inherited Create;
  FMaster := AMaster;
  for Code := 33 to 255 do
    FScrews[Code] := nil;

  // Quelques exemples pour la route...
  FScrews[cGrass] := TGrass.Create(FMaster);
  FScrews[cDirectTurnstile] := TDirectTurnstile.Create(FMaster);
  FScrews[cIndirectTurnstile] := TIndirectTurnstile.Create(FMaster);
  FScrews[cOutside] := TOutside.Create(FMaster);
end;

destructor TScrewsMaster.Destroy;
var Code : TScrewCode;
begin
  for Code := 33 to 255 do if Assigned(FScrews[Code]) then
    FScrews[Code].Free;
  inherited;
end;

function TScrewsMaster.GetScrews(Code : TScrewCode) : TScrew;
begin
  Result := FScrews[Code];
end;

///////////////////
/// Classe TMap ///
///////////////////

constructor TMap.Create(AMaster : TMaster; ADimensions : T3DPoint);
var X, Y, Z : integer;
begin
  inherited Create;
  FMaster := AMaster;
  FDimensions := ADimensions;

  SetLength(FMap, FDimensions.X * FDimensions.Y * FDimensions.Z);
  for X := 0 to FDimensions.X-1 do
    for Y := 0 to FDimensions.Y-1 do
      for Z := 0 to FDimensions.Z-1 do
        FMap[Z*FDimensions.X*FDimensions.Y + Y*FDimensions.X + X] := cGrass;

  SetLength(FOutside, FDimensions.Z);
  for Z := 0 to FDimensions.Z-1 do
    FOutside[Z] := cOutside;
end;

function TMap.GetCodeMap(Position : T3DPoint) : TScrewCode;
var Index : integer;
begin
  if InMap(Position) then
  begin
    Index := Position.Z;
    Index := Index * FDimensions.Y;
    inc(Index, Position.Y);
    Index := Index * FDimensions.X;
    inc(Index, Position.X);

    Result := FMap[Index];
  end else
  if (Position.Z < 0) or (Position.Z >= FDimensions.Z) then
    Result := FOutside[0]
  else
    Result := FOutside[Position.Z];
end;

procedure TMap.SetCodeMap(Position : T3DPoint; Value : TScrewCode);
var Index : integer;
begin
  if (not InMap(Position)) or (Master.ScrewsMaster[Value] = nil) then exit;

  Index := Position.Z;
  Index := Index * FDimensions.Y;
  inc(Index, Position.Y);
  Index := Index * FDimensions.X;
  inc(Index, Position.X);

  FMap[Index] := Value;
end;

function TMap.GetMap(Position : T3DPoint) : TScrew;
begin
  Result := Master.ScrewsMaster[CodeMap[Position]];
end;

function TMap.InMap(Position : T3DPoint) : boolean;
begin
  Result :=
    (Position.X >= 0) and (Position.X < FDimensions.X) and
    (Position.Y >= 0) and (Position.Y < FDimensions.Y) and
    (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

//////////////////////
/// Classe TMaster ///
//////////////////////

constructor TMaster.Create;
begin
  inherited Create;
  FImagesMaster := TImagesMaster.Create;
  FScrewsMaster := TScrewsMaster.Create(Self);
  FMap := TMap.Create(Self, Point3D(1, 1, 1));
  FPlayers := TObjectList.Create;
end;

destructor TMaster.Destroy;
begin
  FPlayers.Free;
  FMap.Free;
  FScrewsMaster.Free;
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

