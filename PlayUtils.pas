{*
  Classes métier de jeu
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit PlayUtils;

interface

uses
  Windows, Forms, Classes, Controls, Graphics, Dialogs, StdCtrls, Math, ScUtils,
  FunLabyUtils, SdDialogs;

const {don't localize}
  attrViewSize = 'ViewSize'; /// Nom d'attribut pour la taille de la vue

type
  {*
    Thread de déplacement du pion
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TMoveThread = class(TThread, IPlayerController)
  private
    Player : TPlayer; /// Joueur concerné
    Dir : TDirection; /// Direction dans laquelle faire bouger le joueur
  protected
    procedure Execute; override;
  public
    constructor Create(APlayer : TPlayer; ADir : TDirection;
      AOnTerminate : TNotifyEvent);

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function ShowDialog(const Title, Text : string;
      DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
      DefButton : Byte = 1;
      AddFlags : LongWord = 0) : TDialogResult;

    function ShowDialogRadio(const Title, Text : string; DlgType : TMsgDlgType;
      DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
      const RadioTitles : array of string; var Selected : integer;
      OverButtons : boolean = False) : Word;

    function ChooseNumber(const Title, Prompt : string;
      Default, Min, Max : integer) : integer;

    procedure MapChanged;
  end;

  {*
    Vue du joueur
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlayerView = class
  private
    FMaster : TMaster; /// Maître FunLabyrinthe
    FPlayer : TPlayer; /// Joueur lié

    function GetMinSize : integer;
    function GetMaxSize : integer;

    function GetSize : integer;
    procedure SetSize(Value : integer);
    function GetWidth : integer;
    function GetHeight : integer;
  public
    constructor Create(APlayer : TPlayer);

    procedure Draw(Canvas : TCanvas);

    property Master : TMaster read FMaster;
    property Player : TPlayer read FPlayer;
    property MinSize : integer read GetMinSize;
    property MaxSize : integer read GetMaxSize;
    property Size : integer read GetSize write SetSize;
    property Width : integer read GetWidth;
    property Height : integer read GetHeight;
  end;

implementation

type
  {*
    Classe thread-safe qui affiche une boîte de dialogue
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TShowDialog = class
  public
    Title : string;              /// Titre
    Text : string;               /// Texte
    DlgType : TDialogType;       /// Type de boîte de dialogue
    DlgButtons : TDialogButtons; /// Boutons présents
    DefButton : Byte;            /// Bouton par défaut
    AddFlags : LongWord;         /// Flags additionnels
    Result : TDialogResult;      /// Bouton choisi par l'utilisateur

    procedure Execute;
  end;

  {*
    Classe thread-safe qui affiche une boîte de dialogue avec des boutons radio
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TShowDialogRadio = class
  public
    Title : string;                /// Titre
    Text : string;                 /// Texte
    DlgType : TMsgDlgType;         /// Type de boîte de dialogue
    DlgButtons : TMsgDlgButtons;   /// Boutons présents
    DefButton : TModalResult;      /// Bouton par défaut
    RadioTitles : array of string; /// Texte des boutons radio
    Selected : integer;            /// Index du bouton sélectionné
    OverButtons : boolean;         /// Indique la position des boutons
    Result : Word;                 /// Bouton choisi par l'utilisateur

    procedure Execute;
  end;

  {*
    Classe thread-safe qui demande au joueur de choisir un nombre
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TChooseNumber = class
  public
    Title : string;    /// Titre
    Prompt : string;   /// Invite
    Default : integer; /// Nombre par défaut
    Min : integer;     /// Nombre minimum
    Max : integer;     /// Nombre maximum
    Result : integer;  /// Nombre choisi par l'utilisateur

    procedure Execute;
  end;

{----------------------------------}
{ Classes de dialogues thread-safe }
{----------------------------------}

{*
  Affiche la boîte de dialogue
*}
procedure TShowDialog.Execute;
begin
  Result := ScUtils.ShowDialog(Title, Text, DlgType, DlgButtons,
    DefButton, AddFlags);
end;

{*
  Affiche la boîte de dialogue
*}
procedure TShowDialogRadio.Execute;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Création de la boîte de dialogue
  Form := CreateMessageDialog(Text, DlgType, DlgButtons);

  with Form do
  try
    Caption := Title;
    // On augmente la taille de la boîte de dialogue
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

    // On centre la boîte de dialogue
    Position := poScreenCenter;

    // Affichage de la boîte de dialogue
    Result := ShowModal;

    // Récupération du choix de l'utilisateur
    Selected := -1;
    for I := 0 to ControlCount-1 do
    begin
      if (Controls[I] is TRadioButton) and
         TRadioButton(Controls[I]).Checked then
        Selected := Controls[I].Tag;
    end;
  finally
    Free;
  end;
end;

{*
  Affiche la boîte de dialogue
*}
procedure TChooseNumber.Execute;
begin
  Result := QueryNumber(Title, Prompt, Default, Min, Max);
end;

{--------------------}
{ Classe TMoveThread }
{--------------------}

{*
  Crée une instance de TMoveThread
  @param APlayer        Joueur à déplacer
  @param ADir           Direction dans laquelle déplacer le joueur
  @param AOnTerminate   Gestionnaire d'événement OnTerminate
*}
constructor TMoveThread.Create(APlayer : TPlayer; ADir : TDirection;
  AOnTerminate : TNotifyEvent);
begin
  inherited Create(True);
  Player := APlayer;
  Dir := ADir;
  OnTerminate := AOnTerminate;
  Resume;
end;

{*
  Méthode d'exécution du thread
*}
procedure TMoveThread.Execute;
var Redo : boolean;
begin
  Player.Controller := Self;
  try
    Player.Move(Dir, True, Redo);
    while Redo do
    begin
      Player.Master.Temporize;
      Dir := Player.Direction;
      Player.Move(Dir, False, Redo);
    end;
  finally
    Player.Controller := nil;
  end;
end;

{*
  Renvoie une référence à l'interface spécifiée
  QueryInterface renvoie une référence à l'interface spécifiée, si l'objet
  supporte cette interface.
  @param IID   GUID de l'interface à obtenir
  @param Obj   Référence à l'interface spécifiée
*}
function TMoveThread.QueryInterface(const IID : TGUID; out Obj) : HResult;
begin
  Result := E_NoInterface;
end;

{*
  Incrémente le compteur de référence
  @return Nouvelle valeur du compteur de référence
*}
function TMoveThread._AddRef : integer;
begin
  Result := 1;
end;

{*
  Décrémente le compteur de référence
  @return Nouvelle valeur du compteur de référence
*}
function TMoveThread._Release : integer;
begin
  Result := 1;
end;

{*
  Affiche une boîte de dialogue
  @param Title        Titre de la boîte de dialogue
  @param Text         Texte de la boîte de dialogue
  @param DlgType      Type de boîte de dialogue
  @param DlgButtons   Boutons présents dans la boîte de dialogue
  @param DefButton    Bouton sélectionné par défaut
  @param AddFlags     Flags additionnels pour MessageBox
  @return Code de résultat du bouton cliqué
*}
function TMoveThread.ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
var Dialog : TShowDialog;
begin
  Dialog := TShowDialog.Create;
  try
    Dialog.Title := Title;
    Dialog.Text := Text;
    Dialog.DlgType := DlgType;
    Dialog.DlgButtons := DlgButtons;
    Dialog.DefButton := DefButton;
    Dialog.AddFlags := AddFlags;

    Synchronize(Dialog.Execute);

    Result := Dialog.Result;
  finally
    Dialog.Free;
  end;
end;

{*
  Affiche une boîte de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title         Titre de la boîte de dialogue
  @param Text          Texte de la boîte de dialogue
  @param DlgType       Type de boîte de dialogue
  @param DlgButtons    Boutons présents dans la boîte de dialogue
  @param DefButton     Bouton sélectionné par défaut
  @param RadioTitles   Libellés des différents boutons radio
  @param Selected      Bouton radio sélectionné
  @param OverButtons   Boutons radio placés au-dessus des boutons si True
  @return Code de résultat du bouton cliqué
*}
function TMoveThread.ShowDialogRadio(const Title, Text : string;
  DlgType : TMsgDlgType; DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;
var Dialog : TShowDialogRadio;
    I : integer;
begin
  Dialog := TShowDialogRadio.Create;
  try
    Dialog.Title := Title;
    Dialog.Text := Text;
    Dialog.DlgType := DlgType;
    Dialog.DlgButtons := DlgButtons;
    Dialog.DefButton := DefButton;

    SetLength(Dialog.RadioTitles, Length(RadioTitles));
    for I := 0 to Length(RadioTitles)-1 do
      Dialog.RadioTitles[I] := RadioTitles[I];

    Dialog.Selected := Selected;
    Dialog.OverButtons := OverButtons;

    Synchronize(Dialog.Execute);

    Selected := Dialog.Selected;
    Result := Dialog.Result;
  finally
    Dialog.Free;
  end;
end;

{*
  Affiche une invite au joueur lui demandant de choisir un nombre
  @param Title     Titre de la boîte de dialogue
  @param Prompt    Invite
  @param Default   Valeur par défaut affichée
  @param Min       Valeur minimale que peut choisir le joueur
  @param Max       Valeur maximale que peut choisir le joueur
  @return La valeur qu'a choisie le joueur
*}
function TMoveThread.ChooseNumber(const Title, Prompt : string;
  Default, Min, Max : integer) : integer;
var Dialog : TChooseNumber;
begin
  Dialog := TChooseNumber.Create;
  try
    Dialog.Title := Title;
    Dialog.Prompt := Prompt;
    Dialog.Default := Default;
    Dialog.Min := Min;
    Dialog.Max := Max;

    Synchronize(Dialog.Execute);

    Result := Dialog.Result;
  finally
    Dialog.Free;
  end;
end;

{*
  Le joueur a changé de carte
*}
procedure TMoveThread.MapChanged;
begin
end;

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

  Size := Size; // Mettre Size dans les bornes [MinSize ; MaxSize]
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
var Map : TMap;
    Size, Width, Height : integer;
    OrigX, OrigY : integer;
    X, Y, Z, I : integer;
    QPos : TQualifiedPos;
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

  // Dessin des cases
  QPos.Map := Map;
  Z := Player.Position.Z;
  for X := 0 to Width-1 do for Y := 0 to Height-1 do
  begin
    QPos.Position := Point3D(OrigX+X, OrigY+Y, Z);
    Map[QPos.Position].DoDraw(QPos, Canvas, X*ScrewSize, Y*ScrewSize);
  end;

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
  begin
    if (Map = Player.Map) and (Position.Z = Z) then
    begin
      DrawInPlace(Canvas, (Position.X-OrigX)*ScrewSize,
        (Position.Y-OrigY)*ScrewSize);
    end;
  end;
end;

end.

