unit PlayUtils;

interface

uses
  Windows, Forms, Classes, Controls, Graphics, Dialogs, StdCtrls, Math, ScUtils,
  FunLabyUtils, NumberDialog;

const {don't localize}
  attrViewSize = 'ViewSize'; /// Nom d'attribut pour la taille de la vue

type
  {*
    Thread de d�placement du pion
  *}
  TMoveThread = class(TThread, IDialoger)
  private
    Player : TPlayer;
    Dir : TDirection;
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
  end;

  {*
    Vue du joueur
  *}
  TPlayerView = class
  private
    FMaster : TMaster; /// Ma�tre FunLabyrinthe
    FPlayer : TPlayer; /// Joueur li�

    function GetMinSize : integer;
    function GetMaxSize : integer;

    function GetSize : integer;
    procedure SetSize(Value : integer);
    function GetTotalSize : integer;
  public
    constructor Create(APlayer : TPlayer);

    procedure Draw(Canvas : TCanvas);

    property Master : TMaster read FMaster;
    property Player : TPlayer read FPlayer;
    property MinSize : integer read GetMinSize;
    property MaxSize : integer read GetMaxSize;
    property Size : integer read GetSize write SetSize;
    property TotalSize : integer read GetTotalSize;
  end;

implementation

type
  TShowDialog = class
  public
    Title : string;
    Text : string;
    DlgType : TDialogType;
    DlgButtons : TDialogButtons;
    DefButton : Byte;
    AddFlags : LongWord;
    Result : TDialogResult;

    procedure Execute;
  end;

  TShowDialogRadio = class
  public
    Title : string;
    Text : string;
    DlgType : TMsgDlgType;
    DlgButtons : TMsgDlgButtons;
    DefButton : TModalResult;
    RadioTitles : array of string;
    Selected : integer;
    OverButtons : boolean;
    Result : Word;

    procedure Execute;
  end;

  TChooseNumber = class
  public
    Title : string;
    Prompt : string;
    Default : integer;
    Min : integer;
    Max : integer;
    Result : integer;

    procedure Execute;
  end;

////////////////////////////////////////
/// Classes de dialogues thread-safe ///
////////////////////////////////////////

procedure TShowDialog.Execute;
begin
  Result := ScUtils.ShowDialog(Title, Text, DlgType, DlgButtons,
    DefButton, AddFlags);
end;

procedure TShowDialogRadio.Execute;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Cr�ation de la boite de dialogue
  Form := CreateMessageDialog(Text, DlgType, DlgButtons);

  with Form do
  try
    Caption := Title;
    // On augmente la taille de la boite de dialogue
    Height := Height + Length(RadioTitles) * 25;

    // Cr�ation des boutons radio et d�termination de la largeur minimale
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

    // Il faut aussi v�rifier que la fiche peut afficher les textes des RadioBox
    // en entier
    OldWidth := 0;
    if (MaxWidth + 40) > Width then
    begin
      OldWidth := Width;
      Width := MaxWidth +40;
    end;

    for I := 0 to ComponentCount-1 do
    begin
      // On r�cup�re chaque bouton
      if Components[I] is TButton then
      begin
        Button := TButton(Components[I]);

        // On met le bon bouton par d�faut et on le s�lectionne
        Button.Default := Button.ModalResult = DefButton;
        if Button.Default then ActiveControl := Button;

        // S'il le faut, d�caler tous les boutons vers le bas
        if OverButtons then
          Button.Top := Button.Top + Length(RadioTitles) * 25;

        // S'il le faut, d�caler tous les boutons vers la droite
        if OldWidth > 0 then
          Button.Left := Button.Left + (Width - OldWidth) div 2;
      end;
    end;

    // On centre la boite de dialogue
    Position := poScreenCenter;

    // Affichage de la bo�te de dialogue
    Result := ShowModal;

    // R�cup�ration du choix de l'utilisateur
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

procedure TChooseNumber.Execute;
begin
  Result := TFormNumber.ChooseNumber(Title, Prompt, Default, Min, Max);
end;

//////////////////////////
/// Classe TMoveThread ///
//////////////////////////

{*
  Cr�e une instance de TMoveThread
  @param APlayer        Joueur � d�placer
  @param ADir           Direction dans laquelle d�placer le joueur
  @param AOnTerminate   Gestionnaire d'�v�nement OnTerminate
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
  M�thode d'ex�cution du thread
*}
procedure TMoveThread.Execute;
var Redo : boolean;
begin
  Player.Dialoger := Self;
  try
    Player.Move(Dir, True, Redo);
    while Redo do
    begin
      Sleep(500);
      Dir := Player.Direction;
      Player.Move(Dir, False, Redo);
    end;
  finally
    Player.Dialoger := nil;
  end;
end;

function TMoveThread.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

function TMoveThread._AddRef : integer;
begin
  Result := 1;
end;

function TMoveThread._Release : integer;
begin
  Result := 1;
end;

{*
  Affiche une bo�te de dialogue
  @param Title        Titre de la bo�te de dialogue
  @param Text         Texte de la bo�te de dialogue
  @param DlgType      Type de bo�te de dialogue
  @param DlgButtons   Boutons pr�sents dans la bo�te de dialogue
  @param DefButton    Bouton s�lectionn� par d�faut
  @param AddFlags     Flags additionnels pour MessageBox
  @return Code de r�sultat du bouton cliqu�
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
  Affiche une bo�te de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title         Titre de la bo�te de dialogue
  @param Text          Texte de la bo�te de dialogue
  @param DlgType       Type de bo�te de dialogue
  @param DlgButtons    Boutons pr�sents dans la bo�te de dialogue
  @param DefButton     Bouton s�lectionn� par d�faut
  @param RadioTitles   Libell�s des diff�rents boutons radio
  @param Selected      Bouton radio s�lectionn�
  @param OverButtons   Boutons radio plac�s au-dessus des boutons si True
  @return Code de r�sultat du bouton cliqu�
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
  @param Title     Titre de la bo�te de dialogue
  @param Prompt    Invite
  @param Default   Valeur par d�faut affich�e
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

//////////////////////////
/// Classe TPlayerView ///
//////////////////////////

{*
  Cr�e une instance de TPlayerView
  @param APlayer   Joueur li�
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
  Taille totale affich�e par la vue
  @return Taille totale affich�e par la vue
*}
function TPlayerView.GetTotalSize : integer;
begin
  Result := Player.Map.ZoneSize + 2*Size;
end;

{*
  Dessine la vue sur un canevas
  @param Canvas   Canevas sur lequel dessiner la vue
*}
procedure TPlayerView.Draw(Canvas : TCanvas);
var Map : TMap;
    Size, TotalSize : integer;
    OrigX, OrigY : integer;
    X, Y, Z, I : integer;
begin
  // Simplifier et acc�l�rer les acc�s aux informations
  Map := Player.Map;
  Size := GetSize;
  TotalSize := GetTotalSize;

  // Origine � la position du joueur
  OrigX := Player.Position.X;
  OrigY := Player.Position.Y;
  { Si le joueur a fini de jouer et qu'on est en bordure de carte, on affiche la
    la zone la plus proche dans la carte. }
  if Player.PlayState <> psPlaying then
  begin
    if OrigX = -1 then OrigX := 0 else
    if OrigX = Map.Dimensions.X then dec(OrigX);
    if OrigY = -1 then OrigY := 0 else
    if OrigY = Map.Dimensions.Y then dec(OrigY);
  end;
  // Origine au niveau de la zone
  dec(OrigX, IntMod(OrigX, Map.ZoneSize));
  dec(OrigY, IntMod(OrigY, Map.ZoneSize));
  // Origine au niveau de la vue
  dec(OrigX, Size);
  dec(OrigY, Size);

  // Dessin des cases
  Z := Player.Position.Z;
  for X := 0 to TotalSize-1 do for Y := 0 to TotalSize-1 do
    Map[Point3D(OrigX+X, OrigY+Y, Z)].Draw(Canvas, X*ScrewSize, Y*ScrewSize);

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
    if Position.Z = Z then
      Draw(Canvas, (Position.X-OrigX)*ScrewSize, (Position.Y-OrigY)*ScrewSize);
end;

end.
