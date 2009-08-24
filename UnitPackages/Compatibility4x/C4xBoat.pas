{*
  Barque de compatibilit� 4.x
  @author sjrd
  @version 5.0
*}
unit C4xBoat;

interface

uses
  SysUtils, Classes, ScUtils, SdDialogs, FunLabyUtils, MapTools, Generics,
  FLBFields, FLBBoat;

resourcestring
  sNumberedBoat = 'Barque n�%d'; /// Nom d'une barque num�rot�e

const
  idOldBoatPlugin = 'OldBoatPlugin'; /// ID du plug-in barque de compatibilit�
  fmtidBoat = 'Boat%d';              /// Format d'ID d'une barque

  fBoat = FLBBoat.fBoat; /// Nom du fichier image de la barque

  /// Attribut num�ro de la barque
  attrBoatNumber = 'BoatNumber';

type
  {*
    Plug-in barque de compatibilit�
    @author sjrd
    @version 5.0
  *}
  TOldBoatPlugin = class(TBoatPlugin)
  public
    procedure Moved(Context: TMoveContext); override;
  end;

  {*
    Barque de compatibilit�
    @author sjrd
    @version 5.0
  *}
  TOldBoat = class(TGround)
  private
    FNumber: Integer; /// Num�ro de la barque
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    constructor CreateNumbered(AMaster: TMaster; const AID: TComponentID;
      ANumber: Integer);

    procedure Entered(Context: TMoveContext); override;

    property Number: Integer read FNumber;
  end;

implementation

{----------------------}
{ TOldBoatPlugin class }
{----------------------}

procedure TOldBoatPlugin.Moved(Context: TMoveContext);
begin
  with Context do
  begin
    if not (DestSquare.Field is TWater) then
    begin
      SrcSquare := ChangeField(SrcSquare, Format(fmtidBoat,
        [Player.Attribute[attrBoatNumber]]));

      Player.Attribute[attrBoatNumber] := 0;
      Player.RemovePlugin(Self);
    end;
  end;
end;

{----------------}
{ TOldBoat class }
{----------------}

{*
  [@inheritDoc]
*}
constructor TOldBoat.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := sBoat;
  Painter.AddImage(fBoat);
end;

{*
  Cr�e une barque num�rot�e
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de la barque
  @param ANumber   Num�ro de la barque
*}
constructor TOldBoat.CreateNumbered(AMaster: TMaster; const AID: TComponentID;
  ANumber: Integer);
begin
  Create(AMaster, AID);

  FNumber := ANumber;

  Name := Format(sNumberedBoat, [ANumber]);
  EditVisualTag := IntToStr(Number);
end;

{*
  [@inheritDoc]
*}
procedure TOldBoat.Entered(Context: TMoveContext);
begin
  with Context do
  begin
    Player.Attribute[attrBoatNumber] := Number;
    Player.AddPlugin(Master.Plugin[idOldBoatPlugin]);

    Square := ChangeField(Square, idWater);
  end;
end;

end.

