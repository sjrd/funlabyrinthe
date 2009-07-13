{*
  Constantes et classes communes
  L'unité FLBCommon décrit les constantes et classes qui sont communes aux
  autres parties, ce qui inclut essentiellement les constantes d'actions et les
  plug-in « polyvalents ».
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
  /// Message lié à la planche
  msgPlank = $10;

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
    @author sjrd
    @version 5.0
  *}
  TPlankMessage = record
    MsgID: Word;             /// ID du message
    Kind: TPlankMessageKind; /// Type de message
    Result: Boolean;         /// True pour autoriser, False sinon
    Player: TPlayer;         /// Joueur concerné
    Pos: T3DPoint;           /// Case au-dessus de laquelle on passe
    Src: T3DPoint;           /// Case dont on vient
    Dest: T3DPoint;          /// Case vers laquelle on va
  end;

implementation

end.

