{*
  Classes métier de jeu
  @author sjrd
  @version 5.0
*}
unit PlayUtils;

interface

uses
  Windows, SysUtils, Forms, Classes, Controls, Graphics, Dialogs, StdCtrls,
  StrUtils, Math, TypInfo, ScUtils, ScLists, ScDelphiLanguage, SdDialogs,
  FunLabyUtils;

const {don't localize}
  attrViewSize = 'ViewSize'; /// Nom d'attribut pour la taille de la vue

type
  /// Type de boîte de dialogue
  TDialogKind = (dkShowDialog, dkShowDialogRadio, dkChooseNumber);

  {*
    Données d'une boîte de dialogue standart à afficher
    @author sjrd
    @version 5.0
  *}
  TStdDialogInfos = record
    DialogKind : TDialogKind;           /// Type de boîte de dialogue
    Title : string;                     /// Titre de la boîte de dialogue
    Text : string;                      /// Texte de la boîte de dialogue
    RadioTitles : array of string;      /// Libellés des boutons radio
    case TDialogKind of
      dkShowDialog : (
        DialogType : TDialogType;       /// Type de ShowDialog
        DialogButtons : TDialogButtons; /// Boutons de ShowDialog
        DefButton : Byte;               /// Bouton par défaut de ShowDialog
        AddFlags : LongWord;            /// Flags supplémentaires de ShowDialog
        DialogResult : TDialogResult;   /// Résultat de ShowDialog
      );
      dkShowDialogRadio : (
        MsgDlgType : TMsgDlgType;       /// Type de ShowDialogRadio
        MsgDlgButtons : TMsgDlgButtons; /// Boutons de ShowDialogRadio
        DefResult : TModalResult;       /// Bouton par défaut de ShowDialogRadio
        Selected : integer;             /// Bouton radio sélectionné
        OverButtons : boolean;          /// Indiques si les radio sont au-dessus
        ModalResult : TModalResult;     /// Résultat de ShowDialogRadio
      );
      dkChooseNumber : (
        Value : integer;                /// Valeur sélectionnée de ChooseNumber
        MinValue : integer;             /// Valeur minimale de ChooseNumber
        MaxValue : integer;             /// Valeur maximale de ChooseNumber
      );
  end;

  {*
    Vue du joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerView = class
  private
    FMaster : TMaster; /// Maître FunLabyrinthe
    FPlayer : TPlayer; /// Joueur lié

    Bitmap : TBitmap;          /// Bitmap
    OldOrigin : TQualifiedPos; /// Ancienne origine
    OldSize : TPoint;          /// Ancienne taille
    OldView : array of TScrew; /// Ancienne vue

    procedure Update;

    function GetMinSize : integer;
    function GetMaxSize : integer;

    function GetSize : integer;
    procedure SetSize(Value : integer);
    function GetWidth : integer;
    function GetHeight : integer;
  public
    constructor Create(APlayer : TPlayer);
    destructor Destroy; override;
    procedure Draw(Canvas : TCanvas);

    property Master : TMaster read FMaster;
    property Player : TPlayer read FPlayer;
    property MinSize : integer read GetMinSize;
    property MaxSize : integer read GetMaxSize;
    property Size : integer read GetSize write SetSize;
    property Width : integer read GetWidth;
    property Height : integer read GetHeight;
  end;

  {*
    Contrôleur de joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerController = class(TThread)
  private
    FPlayer : TPlayer;              /// Joueur contrôlé
    FNextDir : TDirection;          /// Prochaine direction à prendre
    FDialogInfos : TStdDialogInfos; /// Infos de la boîte de dialogue à afficher

    procedure ExecuteDialog;
    function ShowDialogCommand(const Params : string) : string;
    function ShowDialogRadioCommand(const Params : string) : string;
    function ChooseNumberCommand(const Params : string) : string;

    function PlayerCommand(Sender : TPlayer;
      const Command, Params : string) : string;
  protected
    procedure Execute; override;
  public
    constructor Create(APlayer : TPlayer);
    destructor Destroy; override;

    procedure PressKey(Key : Word);

    property Player : TPlayer read FPlayer;
  end;

implementation

{--------------------}
{ Classe TPlayerView }
{--------------------}

{*
  Crée une instance de TPlayerView
  @param APlayer   Joueur lié
*}
constructor TPlayerView.Create(APlayer : TPlayer);
begin
  inherited Create;
  FMaster := APlayer.Master;
  FPlayer := APlayer;
  Bitmap := TBitmap.Create;

  OldOrigin := NoQPos;
  OldSize := NoPoint;

  Size := Size; // Mettre Size dans les bornes [MinSize ; MaxSize]
end;

{*
  [@inheritDoc]
*}
destructor TPlayerView.Destroy;
begin
  Bitmap.Free;
  inherited;
end;

{*
  Met à jour le bitmap
*}
procedure TPlayerView.Update;
var Map : TMap;
    Size, Width, Height : integer;
    OrigX, OrigY : integer;
    Origin : T3DPoint;
    X, Y, Z : integer;
    QPos : TQualifiedPos;
    Screw : TScrew;
begin
  // Mettre à jour le tick count de la partie avant de dessiner
  Master.UpdateTickCount;

  // Simplifier et accélrer les accès aux informations
  Map := Player.Map;
  Size := GetSize;
  Width := GetWidth;
  Height := GetHeight;

  // Origine à la position du joueur
  OrigX := Player.Position.X;
  OrigY := Player.Position.Y;
  { Si le joueur a fini de jouer et qu'on est en bordure de carte, on affiche la
    zone la plus proche dans la carte. }
  if Player.PlayState <> psPlaying then
  begin
    if OrigX = -1 then OrigX := 0 else
    if OrigX = Map.Dimensions.X then dec(OrigX);
    if OrigY = -1 then OrigY := 0 else
    if OrigY = Map.Dimensions.Y then dec(OrigY);
  end;
  // Origine au niveau de la zone
  dec(OrigX, IntMod(OrigX, Map.ZoneWidth));
  dec(OrigY, IntMod(OrigY, Map.ZoneHeight));
  // Origine au niveau de la vue
  dec(OrigX, Size);
  dec(OrigY, Size);

  Origin := Point3D(OrigX, OrigY, Player.Position.Z);

  // Test de validité des anciennes informations
  if (OldOrigin.Map <> Map) or
     (not Same3DPoint(OldOrigin.Position, Origin)) or
     (not SamePoint(OldSize, Point(Width, Height))) then
  begin
    OldOrigin.Map := Map;
    OldOrigin.Position := Origin;
    OldSize := Point(Width, Height);

    SetLength(OldView, Width*Height);
    FillChar(OldView[0], 4*Width*Height, 0);

    Bitmap.Width := Width*ScrewSize;
    Bitmap.Height := Height*ScrewSize;
  end;

  // Dessin des cases
  QPos.Map := Map;
  Z := Player.Position.Z;
  for X := 0 to Width-1 do for Y := 0 to Height-1 do
  begin
    QPos.Position := Point3D(OrigX+X, OrigY+Y, Z);
    Screw := Map[QPos.Position];

    if OldView[Y*Width + X] <> Screw then
    begin
      Screw.Draw(QPos, Bitmap.Canvas, X*ScrewSize, Y*ScrewSize);
      if Screw.StaticDraw then
        OldView[Y*Width + X] := Screw
      else
        OldView[Y*Width + X] := nil;
    end;
  end;
end;

{*
  Taille minimale de la vue
  @return Taille minimale de la vue
*}
function TPlayerView.GetMinSize : integer;
begin
  Result := MinViewSize;
end;

{*
  Taille maximale de la vue
  @return Taille maximale de la vue
*}
function TPlayerView.GetMaxSize : integer;
begin
  Result := Player.Map.MaxViewSize;
end;

{*
  Taille de la vue
  @return Taille de la vue
*}
function TPlayerView.GetSize : integer;
begin
  Result := Player.Attribute[attrViewSize];
end;

{*
  Modifie la taille de la vue
  @param Nouvelle taille de la vue
*}
procedure TPlayerView.SetSize(Value : integer);
begin
  Player.Attribute[attrViewSize] := MinMax(Value, MinSize, MaxSize);
end;

{*
  Nombre de cases affichées en largeur par la vue
  @return Nombre de cases affichées en largeur par la vue
*}
function TPlayerView.GetWidth : integer;
begin
  Result := Player.Map.ZoneWidth + 2*Size;
end;

{*
  Nombre de cases affichées en hauteur par la vue
  @return Nombre de cases affichées en hauteur par la vue
*}
function TPlayerView.GetHeight : integer;
begin
  Result := Player.Map.ZoneHeight + 2*Size;
end;

{*
  Dessine la vue sur un canevas
  @param Canvas   Canevas sur lequel dessiner la vue
*}
procedure TPlayerView.Draw(Canvas : TCanvas);
var I : integer;
begin
  // Dessin des cases
  Update;
  Canvas.Draw(0, 0, Bitmap);

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
  begin
    if (Map = Player.Map) and (Position.Z = Player.Position.Z) then
    begin
      DrawInPlace(Canvas,
        (Position.X-OldOrigin.Position.X)*ScrewSize,
        (Position.Y-OldOrigin.Position.Y)*ScrewSize);
    end;
  end;
end;

{--------------------------}
{ Classe TPlayerController }
{--------------------------}

{*
  Crée une instance de TPlayerController
  @param APlayer   Joueur à contrôler
*}
constructor TPlayerController.Create(APlayer : TPlayer);
begin
  inherited Create(False);
  FPlayer := APlayer;
  FNextDir := diNone;
  FPlayer.OnSendCommand := PlayerCommand;
end;

{*
  Détruit l'instance
*}
destructor TPlayerController.Destroy;
begin
  FPlayer.OnSendCommand := nil;
  inherited;
end;

{*
  Affiche la boîte de dialogue programmée dans FDialogInfos
*}
procedure TPlayerController.ExecuteDialog;
begin
  with FDialogInfos do case DialogKind of
    dkShowDialog :
    begin
      DialogResult := ShowDialog(Title, Text, DialogType, DialogButtons,
        DefButton, AddFlags);
    end;
    dkShowDialogRadio :
    begin
      ModalResult := ShowDialogRadio(Title, Text, MsgDlgType, MsgDlgButtons,
        DefResult, RadioTitles, Selected, OverButtons);
    end;
    dkChooseNumber :
    begin
      Value := QueryNumber(Title, Text, Value, MinValue, MaxValue);
    end;
  end;
end;

{*
  Commande ShowDialog
  @param Params   Paramètres de la commande
  @return Résultat de la commande
*}
function TPlayerController.ShowDialogCommand(const Params : string) : string;
begin
  with TScStrings.CreateFromString(Params, #10), FDialogInfos do
  try
    DialogKind := dkShowDialog;
    Title := StrRepresToStr(NextString);
    Text := StrRepresToStr(NextString);
    DialogType := TDialogType(
      GetEnumValue(TypeInfo(TDialogType), NextString));
    DialogButtons := TDialogButtons(
      GetEnumValue(TypeInfo(TDialogButtons), NextString));
    DefButton := StrToInt(NextString);
    AddFlags := StrToInt(NextString);

    Synchronize(ExecuteDialog);

    Result := GetEnumName(TypeInfo(TDialogResult), integer(DialogResult));
  finally
    Free;
  end;
end;

{*
  Commande ShowDialog
  @param Params   Paramètres de la commande
  @return Résultat de la commande
*}
function TPlayerController.ShowDialogRadioCommand(
  const Params : string) : string;
var I : integer;
begin
  with TScStrings.CreateFromString(Params, #10), FDialogInfos do
  try
    DialogKind := dkShowDialogRadio;
    Title := StrRepresToStr(NextString);
    Text := StrRepresToStr(NextString);
    MsgDlgType := TMsgDlgType(
      GetEnumValue(TypeInfo(TMsgDlgType), NextString));
    StrToEnumSet(NextString, TypeInfo(TMsgDlgButtons), MsgDlgButtons);
    DefResult := StrToInt(NextString);

    SetLength(RadioTitles, StrToInt(NextString));
    for I := 0 to Length(RadioTitles)-1 do
      RadioTitles[I] := StrRepresToStr(NextString);

    Selected := StrToInt(NextString);
    StrToEnumSet(NextString, TypeInfo(boolean), OverButtons);

    Synchronize(ExecuteDialog);

    Result := IntToStr(Selected) + ' ' + IntToStr(ModalResult);
  finally
    Free;
  end;
end;

{*
  Commande ShowDialog
  @param Params   Paramètres de la commande
  @return Résultat de la commande
*}
function TPlayerController.ChooseNumberCommand(const Params : string) : string;
begin
  with TScStrings.CreateFromString(Params, #10), FDialogInfos do
  try
    DialogKind := dkChooseNumber;
    Title := StrRepresToStr(NextString);
    Text := StrRepresToStr(NextString);
    Value := StrToInt(NextString);
    MinValue := StrToInt(NextString);
    MaxValue := StrToInt(NextString);

    Synchronize(ExecuteDialog);

    Result := IntToStr(Value);
  finally
    Free;
  end;
end;

{*
  Gestionnaire d'événement OnSendCommand du joueur
  @param Sender    Joueur concerné
  @param Command   Commande à effectuer
  @param Params    Paramètres de la commande
  @return Résultat de la commande
  @throws EUnsupportedCommand : La commande demandée n'est pas supportée
*}
function TPlayerController.PlayerCommand(Sender : TPlayer;
  const Command, Params : string) : string;
begin
  case AnsiIndexStr(Command,
    [CommandShowDialog, CommandShowDialogRadio, CommandChooseNumber]) of
    0 : Result := ShowDialogCommand(Params);
    1 : Result := ShowDialogRadioCommand(Params);
    2 : Result := ChooseNumberCommand(Params);
    else raise EUnsupportedCommand.CreateFmt(sUnsupportedCommand, [Command]);
  end;
end;

{*
  Méthode d'exécution du thread
*}
procedure TPlayerController.Execute;
var Redo : boolean;
    Dir : TDirection;
begin
  while not Terminated do
  begin
    if FNextDir = diNone then Sleep(50) else
    begin
      try
        Dir := FNextDir;
        FNextDir := diNone;
        Player.Move(Dir, True, Redo);
        if Redo then
          Player.NaturalMoving;
      except
        on Error : Exception do
          Player.ShowDialog(Error.ClassName, Error.Message, dtError);
      end;
    end;
  end;
end;

{*
  Presse une touche
  @param Key   Code de la touche pressée
*}
procedure TPlayerController.PressKey(Key : Word);
begin
  if FNextDir <> diNone then exit;
  case Key of
    VK_UP    : FNextDir := diNorth;
    VK_RIGHT : FNextDir := diEast;
    VK_DOWN  : FNextDir := diSouth;
    VK_LEFT  : FNextDir := diWest;
  end;
end;

end.

