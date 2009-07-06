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
    Contr�leur de joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerController = class(TThread)
  private
    FPlayer: TPlayer; /// Joueur contr�l�

    FNextKey: Word;               /// Prochaine touche � presser
    FNextShift: TShiftState;      /// Prochain �tat des touches sp�ciales
    FNextNotificationMsgID: Word; /// Prochain ID de message de notification

    FDialogInfos: TStdDialogInfos; /// Infos de la bo�te de dialogue � afficher
  
    FExceptionToShow: Exception; /// Exception � afficher

    procedure ExecuteDialog;
    function ShowDialogCommand(const Params: string): string;
    function ShowDialogRadioCommand(const Params: string): string;
    function ChooseNumberCommand(const Params: string): string;

    function PlayerCommand(Sender: TPlayer;
      const Command, Params: string): string;

    procedure ShowException;

    function GetViewWidth: Integer;
    function GetViewHeight: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(APlayer: TPlayer);
    destructor Destroy; override;

    procedure DrawView(Canvas: TCanvas);
    procedure PressKey(Key: Word; Shift: TShiftState);
    procedure PostNotificationMessage(MsgID: Word);

    property Player: TPlayer read FPlayer;

    property ViewWidth: Integer read GetViewWidth;
    property ViewHeight: Integer read GetViewHeight;
  end;

implementation

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
  Affiche une bo�te de dialogue montrant l'exception FExceptionToShow
*}
procedure TPlayerController.ShowException;
begin
  ShowDialog(FExceptionToShow.ClassName, FExceptionToShow.Message, dtError);
end;

{*
  Largeur de la vue
  @return Largeur de la vue, en pixels
*}
function TPlayerController.GetViewWidth: Integer;
begin
  Result := Player.Mode.Width;
end;

{*
  Hauteur de la vue
  @return Hauteur de la vue, en pixels
*}
function TPlayerController.GetViewHeight: Integer;
begin
  Result := Player.Mode.Height;
end;

{*
  M�thode d'ex�cution du thread
*}
procedure TPlayerController.Execute;
var
  Msg: TPlayerMessage;
  Key: Word;
  Shift: TShiftState;
begin
  while not Terminated do
  begin
    try
      if FNextNotificationMsgID <> 0 then
      begin
        // Send message
        Msg.MsgID := FNextNotificationMsgID;
        FNextNotificationMsgID := 0;
        Player.SendMessage(Msg);
      end else if FNextKey <> 0 then
      begin
        // Press key
        Key := FNextKey;
        Shift := FNextShift;
        FNextKey := 0;
        Player.PressKey(Key, Shift);
      end else
      begin
        // Nothing to do
        Sleep(50);
      end;
    except
      on Error: Exception do
      begin
        FExceptionToShow := Error;
        Synchronize(ShowException);
        FExceptionToShow := nil;
      end;
    end;
  end;
end;

{*
  Dessine la vue du joueur
  @param Canvas   Canevas cible
*}
procedure TPlayerController.DrawView(Canvas: TCanvas);
begin
  Player.DrawView(Canvas);
end;

{*
  Presse une touche
  @param Key   Code de la touche press�e
*}
procedure TPlayerController.PressKey(Key: Word; Shift: TShiftState);
begin
  if FNextKey <> 0 then
    Exit;

  FNextShift := Shift;
  FNextKey := Key;
end;

{*
  Poste un message de notification au joueur
  @param MsgID   ID du message de notification � poster
*}
procedure TPlayerController.PostNotificationMessage(MsgID: Word);
begin
  while FNextNotificationMsgID <> 0 do
    Sleep(50);

  FNextNotificationMsgID := MsgID;
end;

end.

