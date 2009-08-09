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
  FunLabyUtils, FunLabyCoreConsts, GR32;

type
  {*
    Contrôleur de joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerController = class(TThread)
  private
    FPlayer: TPlayer; /// Joueur contrôlé

    FNextKey: Word;               /// Prochaine touche à presser
    FNextShift: TShiftState;      /// Prochain état des touches spéciales
    FNextNotificationMsgID: Word; /// Prochain ID de message de notification

    FExceptionToShow: Exception; /// Exception à afficher

    procedure ShowException;

    function GetViewWidth: Integer;
    function GetViewHeight: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(APlayer: TPlayer);

    procedure DrawView(Bitmap: TBitmap32);
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
  Crée une instance de TPlayerController
  @param APlayer   Joueur à contrôler
*}
constructor TPlayerController.Create(APlayer: TPlayer);
begin
  inherited Create(False);

  FPlayer := APlayer;
end;

{*
  Affiche une boîte de dialogue montrant l'exception FExceptionToShow
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
  Méthode d'exécution du thread
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
  @param Bitmap   Bitmap cible
*}
procedure TPlayerController.DrawView(Bitmap: TBitmap32);
begin
  Player.DrawView(Bitmap);
end;

{*
  Presse une touche
  @param Key   Code de la touche pressée
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
  @param MsgID   ID du message de notification à poster
*}
procedure TPlayerController.PostNotificationMessage(MsgID: Word);
begin
  while FNextNotificationMsgID <> 0 do
    Sleep(50);

  FNextNotificationMsgID := MsgID;
end;

end.

