{*
  Classes m�tier de jeu
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
  /// Type de bo�te de dialogue
  TDialogKind = (dkShowDialog, dkShowDialogRadio, dkChooseNumber);

  {*
    Donn�es d'une bo�te de dialogue standart � afficher
    @author sjrd
    @version 5.0
  *}
  TStdDialogInfos = record
    DialogKind: TDialogKind;           /// Type de bo�te de dialogue
    Title: string;                     /// Titre de la bo�te de dialogue
    Text: string;                      /// Texte de la bo�te de dialogue
    RadioTitles: array of string;      /// Libell�s des boutons radio
    case TDialogKind of
      dkShowDialog: (
        DialogType: TDialogType;       /// Type de ShowDialog
        DialogButtons: TDialogButtons; /// Boutons de ShowDialog
        DefButton: Byte;               /// Bouton par d�faut de ShowDialog
        AddFlags: LongWord;            /// Flags suppl�mentaires de ShowDialog
        DialogResult: TDialogResult;   /// R�sultat de ShowDialog
      );
      dkShowDialogRadio: (
        MsgDlgType: TMsgDlgType;       /// Type de ShowDialogRadio
        MsgDlgButtons: TMsgDlgButtons; /// Boutons de ShowDialogRadio
        DefResult: TModalResult;       /// Bouton par d�faut de ShowDialogRadio
        Selected: Integer;             /// Bouton radio s�lectionn�
        OverButtons: Boolean;          /// Indiques si les radio sont au-dessus
        ModalResult: TModalResult;     /// R�sultat de ShowDialogRadio
      );
      dkChooseNumber: (
        Value: Integer;                /// Valeur s�lectionn�e de ChooseNumber
        MinValue: Integer;             /// Valeur minimale de ChooseNumber
        MaxValue: Integer;             /// Valeur maximale de ChooseNumber
      );
  end;

  {*
    Vue du joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerView = class
  private
    FMaster: TMaster; /// Ma�tre FunLabyrinthe
    FPlayer: TPlayer; /// Joueur li�

    Bitmap: TBitmap;          /// Bitmap
    OldOrigin: TQualifiedPos; /// Ancienne origine
    OldSize: TPoint;          /// Ancienne taille
    OldView: array of TSquare; /// Ancienne vue

    procedure Update;

    function GetMinSize: Integer;
    function GetMaxSize: Integer;

    function GetSize: Integer;
    procedure SetSize(Value: Integer);
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    constructor Create(APlayer: TPlayer);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas);

    property Master: TMaster read FMaster;
    property Player: TPlayer read FPlayer;
    property MinSize: Integer read GetMinSize;
    property MaxSize: Integer read GetMaxSize;
    property Size: Integer read GetSize write SetSize;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  {*
    Contr�leur de joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerController = class(TThread)
  private
    FPlayer: TPlayer;              /// Joueur contr�l�
    FNextDir: TDirection;          /// Prochaine direction � prendre
    FDialogInfos: TStdDialogInfos; /// Infos de la bo�te de dialogue � afficher

    procedure ExecuteDialog;
    function ShowDialogCommand(const Params: string): string;
    function ShowDialogRadioCommand(const Params: string): string;
    function ChooseNumberCommand(const Params: string): string;

    function PlayerCommand(Sender: TPlayer;
      const Command, Params: string): string;
  protected
    procedure Execute; override;
  public
    constructor Create(APlayer: TPlayer);
    destructor Destroy; override;

    procedure PressKey(Key: Word);

    property Player: TPlayer read FPlayer;
  end;

implementation

{--------------------}
{ Classe TPlayerView }
{--------------------}

{*
  Cr�e une instance de TPlayerView
  @param APlayer   Joueur li�
*}
constructor TPlayerView.Create(APlayer: TPlayer);
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
  Met � jour le bitmap
*}
procedure TPlayerView.Update;
var
  Map: TMap;
  Size, Width, Height: Integer;
  OrigX, OrigY: Integer;
  Origin: T3DPoint;
  X, Y, Z: Integer;
  QPos: TQualifiedPos;
  Square: TSquare;
begin
  // Mettre � jour le tick count de la partie avant de dessiner
  Master.UpdateTickCount;

  // Simplifier et acc�lrer les acc�s aux informations
  Map := Player.Map;
  Size := GetSize;
  Width := GetWidth;
  Height := GetHeight;

  // Origine � la position du joueur
  OrigX := Player.Position.X;
  OrigY := Player.Position.Y;
  { Si le joueur a fini de jouer et qu'on est en bordure de carte, on affiche la
    zone la plus proche dans la carte. }
  if Player.PlayState <> psPlaying then
  begin
    if OrigX = -1 then
      OrigX := 0
    else if OrigX = Map.Dimensions.X then
      Dec(OrigX);
    if OrigY = -1 then
      OrigY := 0
    else if OrigY = Map.Dimensions.Y then
      Dec(OrigY);
  end;
  // Origine au niveau de la zone
  Dec(OrigX, IntMod(OrigX, Map.ZoneWidth));
  Dec(OrigY, IntMod(OrigY, Map.ZoneHeight));
  // Origine au niveau de la vue
  Dec(OrigX, Size);
  Dec(OrigY, Size);

  Origin := Point3D(OrigX, OrigY, Player.Position.Z);

  // Test de validit� des anciennes informations
  if (OldOrigin.Map <> Map) or
    (not Same3DPoint(OldOrigin.Position, Origin)) or
    (not SamePoint(OldSize, Point(Width, Height))) then
  begin
    OldOrigin.Map := Map;
    OldOrigin.Position := Origin;
    OldSize := Point(Width, Height);

    SetLength(OldView, Width*Height);
    FillChar(OldView[0], 4*Width*Height, 0);

    Bitmap.Width := Width*SquareSize;
    Bitmap.Height := Height*SquareSize;
  end;

  // Dessin des cases
  QPos.Map := Map;
  Z := Player.Position.Z;
  for X := 0 to Width-1 do
  begin
    for Y := 0 to Height-1 do
    begin
      QPos.Position := Point3D(OrigX+X, OrigY+Y, Z);
      Square := Map[QPos.Position];

      if OldView[Y*Width + X] <> Square then
      begin
        Square.Draw(QPos, Bitmap.Canvas, X*SquareSize, Y*SquareSize);
        if Square.StaticDraw then
          OldView[Y*Width + X] := Square
        else
          OldView[Y*Width + X] := nil;
      end;
    end;
  end;
end;

{*
  Taille minimale de la vue
  @return Taille minimale de la vue
*}
function TPlayerView.GetMinSize: Integer;
begin
  Result := MinViewSize;
end;

{*
  Taille maximale de la vue
  @return Taille maximale de la vue
*}
function TPlayerView.GetMaxSize: Integer;
begin
  Result := Player.Map.MaxViewSize;
end;

{*
  Taille de la vue
  @return Taille de la vue
*}
function TPlayerView.GetSize: Integer;
begin
  Result := Player.Attribute[attrViewSize];
end;

{*
  Modifie la taille de la vue
  @param Nouvelle taille de la vue
*}
procedure TPlayerView.SetSize(Value: Integer);
begin
  Player.Attribute[attrViewSize] := MinMax(Value, MinSize, MaxSize);
end;

{*
  Nombre de cases affich�es en largeur par la vue
  @return Nombre de cases affich�es en largeur par la vue
*}
function TPlayerView.GetWidth: Integer;
begin
  Result := Player.Map.ZoneWidth + 2*Size;
end;

{*
  Nombre de cases affich�es en hauteur par la vue
  @return Nombre de cases affich�es en hauteur par la vue
*}
function TPlayerView.GetHeight: Integer;
begin
  Result := Player.Map.ZoneHeight + 2*Size;
end;

{*
  Dessine la vue sur un canevas
  @param Canvas   Canevas sur lequel dessiner la vue
*}
procedure TPlayerView.Draw(Canvas: TCanvas);
var
  I: Integer;
begin
  // Dessin des cases
  Update;
  Canvas.Draw(0, 0, Bitmap);

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I] do
    begin
      if (Map = Player.Map) and (Position.Z = Player.Position.Z) then
      begin
        DrawInPlace(Canvas,
          (Position.X-OldOrigin.Position.X)*SquareSize,
          (Position.Y-OldOrigin.Position.Y)*SquareSize);
      end;
    end;
  end;
end;

{--------------------------}
{ Classe TPlayerController }
{--------------------------}

{*
  Cr�e une instance de TPlayerController
  @param APlayer   Joueur � contr�ler
*}
constructor TPlayerController.Create(APlayer: TPlayer);
begin
  inherited Create(False);
  FPlayer := APlayer;
  FNextDir := diNone;
  FPlayer.OnSendCommand := PlayerCommand;
end;

{*
  D�truit l'instance
*}
destructor TPlayerController.Destroy;
begin
  FPlayer.OnSendCommand := nil;
  inherited;
end;

{*
  Affiche la bo�te de dialogue programm�e dans FDialogInfos
*}
procedure TPlayerController.ExecuteDialog;
begin
  with FDialogInfos do
  begin
    case DialogKind of
      dkShowDialog:
      begin
        DialogResult := ShowDialog(Title, Text, DialogType, DialogButtons,
          DefButton, AddFlags);
      end;
      dkShowDialogRadio:
      begin
        ModalResult := ShowDialogRadio(Title, Text, MsgDlgType, MsgDlgButtons,
          DefResult, RadioTitles, Selected, OverButtons);
      end;
      dkChooseNumber:
      begin
        Value := QueryNumber(Title, Text, Value, MinValue, MaxValue);
      end;
    end;
  end;
end;

{*
  Commande ShowDialog
  @param Params   Param�tres de la commande
  @return R�sultat de la commande
*}
function TPlayerController.ShowDialogCommand(const Params: string): string;
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

    Result := GetEnumName(TypeInfo(TDialogResult), Integer(DialogResult));
  finally
    Free;
  end;
end;

{*
  Commande ShowDialog
  @param Params   Param�tres de la commande
  @return R�sultat de la commande
*}
function TPlayerController.ShowDialogRadioCommand(
  const Params: string): string;
var
  I: Integer;
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
    StrToEnumSet(NextString, TypeInfo(Boolean), OverButtons);

    Synchronize(ExecuteDialog);

    Result := IntToStr(Selected) + ' ' + IntToStr(ModalResult);
  finally
    Free;
  end;
end;

{*
  Commande ShowDialog
  @param Params   Param�tres de la commande
  @return R�sultat de la commande
*}
function TPlayerController.ChooseNumberCommand(const Params: string): string;
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
  Gestionnaire d'�v�nement OnSendCommand du joueur
  @param Sender    Joueur concern�
  @param Command   Commande � effectuer
  @param Params    Param�tres de la commande
  @return R�sultat de la commande
  @throws EUnsupportedCommand : La commande demand�e n'est pas support�e
*}
function TPlayerController.PlayerCommand(Sender: TPlayer;
  const Command, Params: string): string;
begin
  case AnsiIndexStr(Command,
      [CommandShowDialog, CommandShowDialogRadio, CommandChooseNumber]) of
    0: Result := ShowDialogCommand(Params);
    1: Result := ShowDialogRadioCommand(Params);
    2: Result := ChooseNumberCommand(Params);
  else
    raise EUnsupportedCommand.CreateFmt(sUnsupportedCommand, [Command]);
  end;
end;

{*
  M�thode d'ex�cution du thread
*}
procedure TPlayerController.Execute;
var
  Redo: Boolean;
  Dir: TDirection;
begin
  while not Terminated do
  begin
    if FNextDir = diNone then
      Sleep(50)
    else
    begin
      try
        Dir := FNextDir;
        FNextDir := diNone;
        Player.Move(Dir, True, Redo);
        if Redo then
          Player.NaturalMoving;
      except
        on Error: Exception do
          Player.ShowDialog(Error.ClassName, Error.Message, dtError);
      end;
    end;
  end;
end;

{*
  Presse une touche
  @param Key   Code de la touche press�e
*}
procedure TPlayerController.PressKey(Key: Word);
begin
  if FNextDir <> diNone then
    Exit;
  case Key of
    VK_UP: FNextDir := diNorth;
    VK_RIGHT: FNextDir := diEast;
    VK_DOWN: FNextDir := diSouth;
    VK_LEFT: FNextDir := diWest;
  end;
end;

end.

