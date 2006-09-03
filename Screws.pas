unit Screws;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

resourcestring
  sGrass = 'Herbe';
  sDirectTurnstile = 'Tourniquet direct';
  sIndirectTurnstile = 'Tourniquet indirect';
  sOutside = 'Dehors';

const {don't localize}
  fGrass = 'Grass';
  fDirectTurnstile = 'DirectTurnstile';
  fIndirectTurnstile = 'IndirectTurnstile';
  fOutside = 'Outside';

const
  cGrass                  = 48;
  cWater                  = 49;
  cWall                   = 50;
  cHole                   = 51;
  cSilverBlock            = 52;
  cGoldenBlock            = 53;
  cNorthArrow             = 54;
  cEastArrow              = 55;
  cSouthArrow             = 56;
  cWestArrow              = 57;
  cCrossroads             = 58;
  cTreasure               = 59;
  cSecretPassage          = 63;
  cDirectTurnstile        = 224;
  cIndirectTurnstile      = 225;
  cButton                 = [33..47, 161..190];
  cSunkenButton           = 64;
  cInactiveTransporter    = 96;
  cNextOneTransporter     = [97..109];
  cPreviousOneTransporter = [110..122];
  cRandomTransporter      = [123..126];
  cStairs                 = [71..90];
  cDownStairs             = 60;
  cLift                   = 61;
  cOpenLift               = 5;
  cUpStairs               = 62;
  cStart                  = 65;
  cOutside                = 66;
  cBuoy                   = 67;
  cPlank                  = 68;
  cSilverKey              = 69;
  cGoldenKey              = 70;
  cSky                    = 226;
  cBoat                   = [193..202];

type
  TGrass = class(TScrew)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;
  end;

  TDirectTurnstile = class(TGrass)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  TIndirectTurnstile = class(TGrass)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  TOutside = class(TGrass)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

implementation

/////////////////////
/// Classe TGrass ///
/////////////////////

constructor TGrass.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fGrass);
end;

constructor TGrass.Create(AMaster : TMaster);
begin
  Create(AMaster, sGrass, cGrass);
end;

///////////////////////////////
/// Classe TDirectTurnstile ///
///////////////////////////////

constructor TDirectTurnstile.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fDirectTurnstile);
end;

constructor TDirectTurnstile.Create(AMaster : TMaster);
begin
  Create(AMaster, sDirectTurnstile, cDirectTurnstile);
end;

procedure TDirectTurnstile.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;
  repeat
    if Player.Direction = diNorth then
      Dir := diWest
    else
      Dir := Pred(Player.Direction);
  until Player.Move(Dir, KeyPressed, GoOnMoving);
end;

procedure TDirectTurnstile.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
  inherited;
  Master.Map.CodeMap[Pos] := cIndirectTurnstile;
end;

/////////////////////////////////
/// Classe TIndirectTurnstile ///
/////////////////////////////////

constructor TIndirectTurnstile.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fIndirectTurnstile);
end;

constructor TIndirectTurnstile.Create(AMaster : TMaster);
begin
  Create(AMaster, sIndirectTurnstile, cIndirectTurnstile);
end;

procedure TIndirectTurnstile.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;
  repeat
    if Player.Direction = diWest then
      Dir := diNorth
    else
      Dir := Succ(Player.Direction);
  until Player.Move(Dir, KeyPressed, GoOnMoving);
end;

procedure TIndirectTurnstile.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
  inherited;
  Master.Map.CodeMap[Pos] := cDirectTurnstile;
end;

///////////////////////
/// Classe TOutside ///
///////////////////////

constructor TOutside.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fOutside);
end;

constructor TOutside.Create(AMaster : TMaster);
begin
  Create(AMaster, sOutside, cOutside);
end;

procedure TOutside.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  // Faire gagner le joueur
end;

end.

