{*
  Constantes et classes communes
  L'unit� FLBCommon d�crit les constantes et classes qui sont communes aux
  autres parties, ce qui inclut essentiellement les constantes d'actions et les
  plug-in � polyvalents �.
  @author sjrd
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
  /// Message li� � la planche
  msgPlank = $10;

type
  {*
    Type de message li� � la planche
    - plkPassOver : Test sur la case au-dessus de laquelle on passe
    - plkArriveAt : Test sur la case sur laquelle on arriverait
    - plkLeaveFrom : Test sur la case de laquelle on vient
  *}
  TPlankMessageKind = (pmkPassOver, pmkLeaveFrom, pmkArriveAt);

  {*
    Message li� � la planche
    @author sjrd
    @version 5.0
  *}
  TPlankMessage = record
    MsgID: Word;             /// ID du message
    Kind: TPlankMessageKind; /// Type de message
    Result: Boolean;         /// True pour autoriser, False sinon
    Player: TPlayer;         /// Joueur concern�
    Pos: T3DPoint;           /// Case au-dessus de laquelle on passe
    Src: T3DPoint;           /// Case dont on vient
    Dest: T3DPoint;          /// Case vers laquelle on va
  end;

  {*
    Plug-in masque
    Filtre l'affichage du joueur au moyen d'un masque monochrome.
    @author sjrd
    @version 5.0
  *}
  TMaskPlugin = class(TPlugin)
  private
    FMask: TBitmap;   /// Masque monochrome � appliquer
    FBefore: TBitmap; /// Ce qu'il y avait avant le joueur
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      AMask: TBitmap);
    destructor Destroy; override;

    procedure DrawBefore(Context: TDrawSquareContext); override;
    procedure DrawAfter(Context: TDrawSquareContext); override;

    property Mask: TBitmap read FMask;
    property Before: TBitmap read FBefore;
  end;

implementation

{--------------------}
{ Classe TMaskPlugin }
{--------------------}

{*
  Cr�e une instance de TMaskPlugin
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du plug-in
  @param AMaks     Masque � appliquer
*}
constructor TMaskPlugin.Create(AMaster: TMaster; const AID: TComponentID;
  AMask: TBitmap);
var
  X, Y: Integer;
begin
  inherited Create(AMaster, AID);

  FMask := TBitmap.Create;
  with FMask do
  begin
    Assign(AMask);
    for X := 0 to SquareSize do
    begin
      for Y := 0 to SquareSize do
      begin
        if Canvas.Pixels[X, Y] = clTransparent then
          Canvas.Pixels[X, Y] := clBlack
        else
          Canvas.Pixels[X, Y] := clTransparent;
      end;
    end;
    TransparentColor := clBlack;
    Transparent := True;
  end;

  FBefore := TBitmap.Create;
  with FBefore do
  begin
    Width := SquareSize;
    Height := SquareSize;
    TransparentColor := clTransparent;
    Transparent := True;
  end;
end;

{*
  [@inheritDoc]
*}
destructor TMaskPlugin.Destroy;
begin
  FBefore.Free;
  FMask.Free;
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TMaskPlugin.DrawBefore(Context: TDrawSquareContext);
begin
  Before.Canvas.CopyRect(BaseSquareRect, Context.Canvas, Context.SquareRect);
  Before.Canvas.Draw(0, 0, Mask);
end;

{*
  [@inheritDoc]
*}
procedure TMaskPlugin.DrawAfter(Context: TDrawSquareContext);
begin
  with Context do
    Canvas.Draw(X, Y, Before);
end;

end.

