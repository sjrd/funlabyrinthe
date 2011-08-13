unit C4xSquaresTable;

interface

uses
  SysUtils, StrUtils, FunLabyUtils;

function GetSquaresTable(C: Char): TComponentID;
function SquareToInteger(const ID: TComponentID): Integer;

implementation

const
  SquaresTable: array[AnsiChar] of string = (
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    { $20 - 32 }
    '',
    'Grass-ActionsEffect1--ActionsObstacle1',   // !
    'Grass-ActionsEffect2--ActionsObstacle2',   // "
    'Grass-ActionsEffect3--ActionsObstacle3',   // #
    'Grass-ActionsEffect4--ActionsObstacle4',   // $
    'Grass-ActionsEffect5--ActionsObstacle5',   // %
    'Grass-ActionsEffect6--ActionsObstacle6',   // &
    'Grass-ActionsEffect7--ActionsObstacle7',   // '
    'Grass-ActionsEffect8--ActionsObstacle8',   // (
    'Grass-ActionsEffect9--ActionsObstacle9',   // )
    'Grass-ActionsEffect10--ActionsObstacle10', // *
    'Grass-ActionsEffect11--ActionsObstacle11', // +
    'Grass-ActionsEffect12--ActionsObstacle12', // ,
    'Grass-ActionsEffect13--ActionsObstacle13', // -
    'Grass-ActionsEffect14--ActionsObstacle14', // .
    'Grass-ActionsEffect15--ActionsObstacle15', // /
    { $30 - 48 }
    'Grass---',                                 // 0
    'Water---',                                 // 1
    'Wall---',                                  // 2
    'Hole---',                                  // 3
    'Grass---SilverBlock',                      // 4
    'Grass---GoldenBlock',                      // 5
    'Grass-NorthArrow--',                       // 6
    'Grass-EastArrow--',                        // 7
    'Grass-SouthArrow--',                       // 8
    'Grass-WestArrow--',                        // 9
    'Grass-Crossroads--',                       // :
    'Grass-ActionsEffect77--ActionsObstacle77', // ;
    'Grass-DownStairs--',                       // <
    'Grass-Lift--',                             // =
    'Grass-UpStairs--',                         // >
    'Grass---SecretWay',                        // ?
    { $40 - 64 }
    'Grass-SunkenButton--',                     // @
    '',
    'Grass-ActionsEffect76--ActionsObstacle76', // B
    'Grass--Buoy-',                             // C
    'Grass--Plank-',                            // D
    'Grass--SilverKey-',                        // E
    'Grass--GoldenKey-',                        // F
    'Grass-OldStairs1--',                       // G
    'Grass-OldStairs2--',                       // H
    'Grass-OldStairs3--',                       // I
    'Grass-OldStairs4--',                       // J
    'Grass-OldStairs5--',                       // K
    'Grass-OldStairs6--',                       // L
    'Grass-OldStairs7--',                       // M
    'Grass-OldStairs8--',                       // N
    'Grass-OldStairs9--',                       // O
    { $50 - 80 }
    'Grass-OldStairs10--',                      // P
    'Grass-OldStairs11--',                      // Q
    'Grass-OldStairs12--',                      // R
    'Grass-OldStairs13--',                      // S
    'Grass-OldStairs14--',                      // T
    'Grass-OldStairs15--',                      // U
    'Grass-OldStairs16--',                      // V
    'Grass-OldStairs17--',                      // W
    'Grass-OldStairs18--',                      // X
    'Grass-OldStairs19--',                      // Y
    'Grass-OldStairs20--',                      // Z
    '', '', '', '', '',
    { $60 - 96 }
    'Grass-InactiveTransporter--',              // `
    'Grass-ActionsEffect46--ActionsObstacle46', // a
    'Grass-ActionsEffect47--ActionsObstacle47', // b
    'Grass-ActionsEffect48--ActionsObstacle48', // c
    'Grass-ActionsEffect49--ActionsObstacle49', // d
    'Grass-ActionsEffect50--ActionsObstacle50', // e
    'Grass-ActionsEffect51--ActionsObstacle51', // f
    'Grass-ActionsEffect52--ActionsObstacle52', // g
    'Grass-ActionsEffect53--ActionsObstacle53', // h
    'Grass-ActionsEffect54--ActionsObstacle54', // i
    'Grass-ActionsEffect55--ActionsObstacle55', // j
    'Grass-ActionsEffect56--ActionsObstacle56', // k
    'Grass-ActionsEffect57--ActionsObstacle57', // l
    'Grass-ActionsEffect58--ActionsObstacle58', // m
    'Grass-ActionsEffect59--ActionsObstacle59', // n
    'Grass-ActionsEffect60--ActionsObstacle60', // o
    { $70 - 112 }
    'Grass-ActionsEffect61--ActionsObstacle61', // p
    'Grass-ActionsEffect62--ActionsObstacle62', // q
    'Grass-ActionsEffect63--ActionsObstacle63', // r
    'Grass-ActionsEffect64--ActionsObstacle64', // s
    'Grass-ActionsEffect65--ActionsObstacle65', // t
    'Grass-ActionsEffect66--ActionsObstacle66', // u
    'Grass-ActionsEffect67--ActionsObstacle67', // v
    'Grass-ActionsEffect68--ActionsObstacle68', // w
    'Grass-ActionsEffect69--ActionsObstacle69', // x
    'Grass-ActionsEffect70--ActionsObstacle70', // y
    'Grass-ActionsEffect71--ActionsObstacle71', // z
    'Grass-ActionsEffect72--ActionsObstacle72', // {
    'Grass-ActionsEffect73--ActionsObstacle73', // |
    'Grass-ActionsEffect74--ActionsObstacle74', // }
    'Grass-ActionsEffect75--ActionsObstacle75', // ~
    '',
    { $80 - 128 }
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    { $A0 - 160 }
    '',
    'Grass-ActionsEffect16--ActionsObstacle16', // ¡
    'Grass-ActionsEffect17--ActionsObstacle17', // ¢
    'Grass-ActionsEffect18--ActionsObstacle18', // £
    'Grass-ActionsEffect19--ActionsObstacle19', // ¤
    'Grass-ActionsEffect20--ActionsObstacle20', // ¥
    'Grass-ActionsEffect21--ActionsObstacle21', // ¦
    'Grass-ActionsEffect22--ActionsObstacle22', // §
    'Grass-ActionsEffect23--ActionsObstacle23', // ¨
    'Grass-ActionsEffect24--ActionsObstacle24', // ©
    'Grass-ActionsEffect25--ActionsObstacle25', // ª
    'Grass-ActionsEffect26--ActionsObstacle26', // «
    'Grass-ActionsEffect27--ActionsObstacle27', // ¬
    'Grass-ActionsEffect28--ActionsObstacle28', // --
    'Grass-ActionsEffect29--ActionsObstacle29', // ®
    'Grass-ActionsEffect30--ActionsObstacle30', // ¯
    { $B0 - 176 }
    'Grass-ActionsEffect31--ActionsObstacle31', // °
    'Grass-ActionsEffect32--ActionsObstacle32', // ±
    'Grass-ActionsEffect33--ActionsObstacle33', // ²
    'Grass-ActionsEffect34--ActionsObstacle34', // ³
    'Grass-ActionsEffect35--ActionsObstacle35', // ´
    'Grass-ActionsEffect36--ActionsObstacle36', // µ
    'Grass-ActionsEffect37--ActionsObstacle37', // ¶
    'Grass-ActionsEffect38--ActionsObstacle38', // ·
    'Grass-ActionsEffect39--ActionsObstacle39', // ¸
    'Grass-ActionsEffect40--ActionsObstacle40', // ¹
    'Grass-ActionsEffect41--ActionsObstacle41', // º
    'Grass-ActionsEffect42--ActionsObstacle42', // »
    'Grass-ActionsEffect43--ActionsObstacle43', // ¼
    'Grass-ActionsEffect44--ActionsObstacle44', // ½
    'Grass-ActionsEffect45--ActionsObstacle45', // ¾
    '',
    { $C0 - 192 }
    '',
    'Boat1---',                                 // Á
    'Boat2---',                                 // Â
    'Boat3---',                                 // Ã
    'Boat4---',                                 // Ä
    'Boat5---',                                 // Å
    'Boat6---',                                 // Æ
    'Boat7---',                                 // Ç
    'Boat8---',                                 // È
    'Boat9---',                                 // É
    'Boat10---',                                // Ê
    '', '', '', '', '',
    { $D0 - 208 }
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    { $E0 - 224 }
    'Grass-DirectTurnstile--',                  // à
    'Grass-IndirectTurnstile--',                // á
    'Sky---',                                   // â
    '', '', '', '', '', '', '', '', '', '', '', '', '',
    { $F0 - 240 }
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''
  );

{*
  Lit la table des cases
  @param C   Caractère représentant une case
  @return ID de la case correspondante
*}
function GetSquaresTable(C: Char): TComponentID;
var
  AnsiStr: AnsiString;
begin
  AnsiStr := AnsiString(string(C));
  Result := SquaresTable[AnsiStr[1]];
end;

{*
  Calcule la valeur entière d'une case
  @param ID   ID de la case
  @return Valeur entière représentant la case
*}
function SquareToInteger(const ID: TComponentID): Integer;
begin
  Result := AnsiIndexStr(ID, SquaresTable);
end;

end.

