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
    '',
    'Grass-ActionsEffect1--ActionsObstacle1',
    'Grass-ActionsEffect2--ActionsObstacle2',
    'Grass-ActionsEffect3--ActionsObstacle3',
    'Grass-ActionsEffect4--ActionsObstacle4',
    'Grass-ActionsEffect5--ActionsObstacle5',
    'Grass-ActionsEffect6--ActionsObstacle6',
    'Grass-ActionsEffect7--ActionsObstacle7',
    'Grass-ActionsEffect8--ActionsObstacle8',
    'Grass-ActionsEffect9--ActionsObstacle9',
    'Grass-ActionsEffect10--ActionsObstacle10',
    'Grass-ActionsEffect11--ActionsObstacle11',
    'Grass-ActionsEffect12--ActionsObstacle12',
    'Grass-ActionsEffect13--ActionsObstacle13',
    'Grass-ActionsEffect14--ActionsObstacle14',
    'Grass-ActionsEffect15--ActionsObstacle15',
    'Grass---',
    'Water---',
    'Wall---',
    'Hole---',
    'Grass---SilverBlock',
    'Grass---GoldenBlock',
    'Grass-NorthArrow--',
    'Grass-EastArrow--',
    'Grass-SouthArrow--',
    'Grass-WestArrow--',
    'Grass-Crossroads--',
    'Grass-ActionsEffect77--ActionsObstacle77',
    'Grass-DownStairs--',
    'Grass-Lift--',
    'Grass-UpStairs--',
    'Grass---SecretWay',
    'Grass-SunkenButton--',
    '',
    'Grass-ActionsEffect76--ActionsObstacle76',
    'Grass--Buoy-',
    'Grass--Plank-',
    'Grass--SilverKey-',
    'Grass--GoldenKey-',
    'Grass-OldStairs1--',
    'Grass-OldStairs2--',
    'Grass-OldStairs3--',
    'Grass-OldStairs4--',
    'Grass-OldStairs5--',
    'Grass-OldStairs6--',
    'Grass-OldStairs7--',
    'Grass-OldStairs8--',
    'Grass-OldStairs9--',
    'Grass-OldStairs10--',
    'Grass-OldStairs11--',
    'Grass-OldStairs12--',
    'Grass-OldStairs13--',
    'Grass-OldStairs14--',
    'Grass-OldStairs15--',
    'Grass-OldStairs16--',
    'Grass-OldStairs17--',
    'Grass-OldStairs18--',
    'Grass-OldStairs19--',
    'Grass-OldStairs20--',
    '',
    '',
    '',
    '',
    '',
    'Grass-InactiveTransporter--',
    'Grass-ActionsEffect46--ActionsObstacle46',
    'Grass-ActionsEffect47--ActionsObstacle47',
    'Grass-ActionsEffect48--ActionsObstacle48',
    'Grass-ActionsEffect49--ActionsObstacle49',
    'Grass-ActionsEffect50--ActionsObstacle50',
    'Grass-ActionsEffect51--ActionsObstacle51',
    'Grass-ActionsEffect52--ActionsObstacle52',
    'Grass-ActionsEffect53--ActionsObstacle53',
    'Grass-ActionsEffect54--ActionsObstacle54',
    'Grass-ActionsEffect55--ActionsObstacle55',
    'Grass-ActionsEffect56--ActionsObstacle56',
    'Grass-ActionsEffect57--ActionsObstacle57',
    'Grass-ActionsEffect58--ActionsObstacle58',
    'Grass-ActionsEffect59--ActionsObstacle59',
    'Grass-ActionsEffect60--ActionsObstacle60',
    'Grass-ActionsEffect61--ActionsObstacle61',
    'Grass-ActionsEffect62--ActionsObstacle62',
    'Grass-ActionsEffect63--ActionsObstacle63',
    'Grass-ActionsEffect64--ActionsObstacle64',
    'Grass-ActionsEffect65--ActionsObstacle65',
    'Grass-ActionsEffect66--ActionsObstacle66',
    'Grass-ActionsEffect67--ActionsObstacle67',
    'Grass-ActionsEffect68--ActionsObstacle68',
    'Grass-ActionsEffect69--ActionsObstacle69',
    'Grass-ActionsEffect70--ActionsObstacle70',
    'Grass-ActionsEffect71--ActionsObstacle71',
    'Grass-ActionsEffect72--ActionsObstacle72',
    'Grass-ActionsEffect73--ActionsObstacle73',
    'Grass-ActionsEffect74--ActionsObstacle74',
    'Grass-ActionsEffect75--ActionsObstacle75',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    'Grass-ActionsEffect16--ActionsObstacle16',
    'Grass-ActionsEffect17--ActionsObstacle17',
    'Grass-ActionsEffect18--ActionsObstacle18',
    'Grass-ActionsEffect19--ActionsObstacle19',
    'Grass-ActionsEffect20--ActionsObstacle20',
    'Grass-ActionsEffect21--ActionsObstacle21',
    'Grass-ActionsEffect22--ActionsObstacle22',
    'Grass-ActionsEffect23--ActionsObstacle23',
    'Grass-ActionsEffect24--ActionsObstacle24',
    'Grass-ActionsEffect25--ActionsObstacle25',
    'Grass-ActionsEffect26--ActionsObstacle26',
    'Grass-ActionsEffect27--ActionsObstacle27',
    'Grass-ActionsEffect28--ActionsObstacle28',
    'Grass-ActionsEffect29--ActionsObstacle29',
    'Grass-ActionsEffect30--ActionsObstacle30',
    'Grass-ActionsEffect31--ActionsObstacle31',
    'Grass-ActionsEffect32--ActionsObstacle32',
    'Grass-ActionsEffect33--ActionsObstacle33',
    'Grass-ActionsEffect34--ActionsObstacle34',
    'Grass-ActionsEffect35--ActionsObstacle35',
    'Grass-ActionsEffect36--ActionsObstacle36',
    'Grass-ActionsEffect37--ActionsObstacle37',
    'Grass-ActionsEffect38--ActionsObstacle38',
    'Grass-ActionsEffect39--ActionsObstacle39',
    'Grass-ActionsEffect40--ActionsObstacle40',
    'Grass-ActionsEffect41--ActionsObstacle41',
    'Grass-ActionsEffect42--ActionsObstacle42',
    'Grass-ActionsEffect43--ActionsObstacle43',
    'Grass-ActionsEffect44--ActionsObstacle44',
    'Grass-ActionsEffect45--ActionsObstacle45',
    '',
    '',
    'Boat1---',
    'Boat2---',
    'Boat3---',
    'Boat4---',
    'Boat5---',
    'Boat6---',
    'Boat7---',
    'Boat8---',
    'Boat9---',
    'Boat10---',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    'Grass-DirectTurnstile--',
    'Grass-IndirectTurnstile--',
    'Sky---',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    ''
  );

{*
  Lit la table des cases
  @param C   Caract�re repr�sentant une case
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
  Calcule la valeur enti�re d'une case
  @param ID   ID de la case
  @return Valeur enti�re repr�sentant la case
*}
function SquareToInteger(const ID: TComponentID): Integer;
begin
  Result := AnsiIndexStr(ID, SquaresTable);
end;

end.

