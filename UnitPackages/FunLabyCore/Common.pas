{*
  Constantes et classes communes
  L'unit� Common d�crit les constantes et classes qui sont communes aux autres
  parties, ce qui inclut essentiellement les constantes d'actions et les plug-in
  � polyvalents �.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit Common;

interface

uses
  SysUtils, Graphics, FunLabyUtils;

const {don't localize}
  /// Action d'aller sur l'eau
  actGoOnWater = 'GoOnWater';
  /// Action de passer au-dessus d'une case
  actPassOverScrew = 'PassOverScrew';
  /// Action d'ouvrir une serrure en argent
  actOpenSilverLock = 'OpenSilverLock';
  /// Action d'ouvrir une serrure en or
  actOpenGoldenLock = 'OpenGoldenLock';

const {don't localize}
  idAvoidShowPlugin = 'AvoidShowPlugin'; /// ID du plug-in masquant le joueur

type
  {*
    Plug-in masque
    Filtre l'affichage du joueur au moyen d'un masque monochrome.
  *}
  TMaskPlugin = class(TPlugin)
  private
    FMask : TBitmap;   /// Masque monochrome � appliquer
    FBefore : TBitmap; /// Ce qu'il y avait avant le joueur
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      AMask : TBitmap = nil);
    destructor Destroy; override;

    procedure DrawBefore(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;
    procedure DrawAfter(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    property Mask : TBitmap read FMask;
    property Before : TBitmap read FBefore;
  end;

procedure DrawScrewNumber(Canvas : TCanvas; X, Y, Number : integer;
  FontColor : TColor = clBlack);

implementation

{*
  Dessine un num�ro sur un canevas de case
  @param Canvas      Canevas sur lequel dessiner le num�ro
  @param X           Bord gauche de la case
  @param Y           Bord sup�rieur de la case
  @param Number      Num�ro � �crire
  @param FontColor   Couleur du texte
*}
procedure DrawScrewNumber(Canvas : TCanvas; X, Y, Number : integer;
  FontColor : TColor = clBlack);
begin
  with Canvas do
  begin
    Brush.Color := clWhite;
    Font.Name := 'Arial'; {don't localize}
    Font.Color := FontColor;
    TextOut(X+10, Y+8, IntToStr(Number));
  end;
end;

//////////////////////////
/// Classe TMaskPlugin ///
//////////////////////////

{*
  Cr�e une instance de TMaskPlugin
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du plug-in
  @param AMaks     Masque � appliquer
*}
constructor TMaskPlugin.Create(AMaster : TMaster; const AID : TComponentID;
  AMask : TBitmap = nil);
var X, Y : integer;
begin
  inherited Create(AMaster, AID);

  FMask := TBitmap.Create;
  with FMask do
  begin
    if Assigned(AMask) then
    begin
      Assign(AMask);
      for X := 0 to ScrewSize do for Y := 0 to ScrewSize do
      begin
        if Canvas.Pixels[X, Y] = clTransparent then
          Canvas.Pixels[X, Y] := clBlack
        else
          Canvas.Pixels[X, Y] := clTransparent;
      end;
    end else
    begin
      Width := ScrewSize;
      Height := ScrewSize;
      with Canvas do
      begin
        Brush.Color := clBlack;
        Pen.Color := clBlack;
        Rectangle(ScrewRect);
      end;
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
  D�truit l'instance
*}
destructor TMaskPlugin.Destroy;
begin
  FBefore.Free;
  FMask.Free;
  inherited;
end;

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TMaskPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  Before.Canvas.CopyRect(ScrewRect, Canvas, ScrewRect(X, Y));
  Before.Canvas.Draw(0, 0, Mask);
end;

{*
  Dessine sur le joueur
  DrawAfter est ex�cut� lors du dessin du joueur, apr�s celui-ci. Le dessin
  effectu� dans DrawAfter se retrouve donc sur le joueur.
  @param Player   Joueur qui est dessin�
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TMaskPlugin.DrawAfter(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  Canvas.Draw(X, Y, Before);
end;

end.

