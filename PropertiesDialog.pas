unit PropertiesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FunLabyUtils;

resourcestring
  sPlayerProperties = 'Propri�t�s du joueur';
  sPlayerObjects = 'Objets poss�d�s par le joueur :';
  sMapProperties = 'Propri�t�s de la carte';

type
  TFormProperties = class(TForm)
    ListBox: TListBox;
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    procedure ShowPlayerProps(Player : TPlayer);
    procedure ShowMapProps(Map : TMap);
  end;

var
  FormProperties: TFormProperties;

implementation

{$R *.DFM}

procedure TFormProperties.ShowPlayerProps(Player : TPlayer);
var I : integer;
begin
  with ListBox.Items do
  begin
    Caption := sPlayerProperties;
    Clear;

    Add(sPlayerObjects);
    Add('');

    for I := 0 to Player.Master.ObjectDefCount-1 do
      Player.Master.ObjectDefs[I].ShownInfos[Player];

    ShowModal;
  end;
end;

procedure TFormProperties.ShowMapProps(Map : TMap);
begin
  with ListBox.Items do
  begin
    Caption := sMapProperties;
    Clear;

    { TODO 1 : Afficher les propri�t�s de la carte }

    ShowModal;
  end;
end;

end.
