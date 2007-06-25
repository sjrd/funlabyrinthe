{*
  Constantes et classes communes
  L'unité FLBCommon décrit les constantes et classes qui sont communes aux
  autres parties, ce qui inclut essentiellement les constantes d'actions et les
  plug-in « polyvalents ».
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FLBCommon;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils;

const {don't localize}
  /// Action d'aller sur l'eau
  actGoOnWater = 'GoOnWater';
  /// Action d'ouvrir une serrure en argent
  actOpenSilverLock = 'OpenSilverLock';
  /// Action d'ouvrir une serrure en or
  actOpenGoldenLock = 'OpenGoldenLock';

const
  /// Message lié à la planche
  msgPlank = 1;

type
  {*
    Type de message lié à la planche
    - plkPassOver : Test sur la case au-dessus de laquelle on passe
    - plkArriveAt : Test sur la case sur laquelle on arriverait
    - plkLeaveFrom : Test sur la case de laquelle on vient
  *}
  TPlankMessageKind = (pmkPassOver, pmkLeaveFrom, pmkArriveAt);

  {*
    Message lié à la planche
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlankMessage = record
    MsgID : Word;             /// ID du message
    Kind : TPlankMessageKind; /// Type de message
    Result : boolean;         /// True pour autoriser, False sinon
    Player : TPlayer;         /// Joueur concerné
    Pos : T3DPoint;           /// Case au-dessus de laquelle on passe
    Src : T3DPoint;           /// Case dont on vient
    Dest : T3DPoint;          /// Case vers laquelle on va
  end;

  {*
    Plug-in masque
    Filtre l'affichage du joueur au moyen d'un masque monochrome.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TMaskPlugin = class(TPlugin)
  private
    FMask : TBitmap;   /// Masque monochrome à appliquer
    FBefore : TBitmap; /// Ce qu'il y avait avant le joueur
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      AMask : TBitmap);
    destructor Destroy; override;

    procedure DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); override;
    procedure DrawAfter(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); override;

    property Mask : TBitmap read FMask;
    property Before : TBitmap read FBefore;
  end;

implementation

{--------------------}
{ Classe TMaskPlugin }
{--------------------}

{*
  Crée une instance de TMaskPlugin
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du plug-in
  @param AMaks     Masque à appliquer
*}
constructor TMaskPlugin.Create(AMaster : TMaster; const AID : TComponentID;
  AMask : TBitmap);
var X, Y : integer;
begin
  inherited Create(AMaster, AID);

  FMask := TBitmap.Create;
  with FMask do
  begin
    Assign(AMask);
    for X := 0 to ScrewSize do for Y := 0 to ScrewSize do
    begin
      if Canvas.Pixels[X, Y] = clTransparent then
        Canvas.Pixels[X, Y] := clBlack
      else
        Canvas.Pixels[X, Y] := clTransparent;
    end;
    TransparentColor := clBlack;
    Transparent := True;
  end;

  FBefore := TBitmap.Create;
  with FBefore do
  begin
    Width := ScrewSize;
    Height := ScrewSize;
    TransparentColor := clTransparent;
    Transparent := True;
  end;
end;

{*
  Détruit l'instance
*}
destructor TMaskPlugin.Destroy;
begin
  FBefore.Free;
  FMask.Free;
  inherited;
end;

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessiné
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TMaskPlugin.DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  Before.Canvas.CopyRect(ScrewRect, Canvas, ScrewRect(X, Y));
  Before.Canvas.Draw(0, 0, Mask);
end;

{*
  Dessine sur le joueur
  DrawAfter est exécuté lors du dessin du joueur, après celui-ci. Le dessin
  effectué dans DrawAfter se retrouve donc sur le joueur.
  @param Player   Joueur qui est dessiné
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TMaskPlugin.DrawAfter(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  Canvas.Draw(X, Y, Before);
end;

end.

