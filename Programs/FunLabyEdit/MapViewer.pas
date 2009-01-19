unit MapViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FunLabyUtils, FunLabyEditOTA, BaseMapViewer;

type
  {*
    Visualisateur de cartes
    @author sjrd
    @version 5.0
  *}
  TFormMapViewer = class(TForm, IOTAMapViewer50)
    FrameBaseMapViewer: TFrameBaseMapViewer;
  private
    function GetMaster: TMaster;
    procedure SetMaster(Value: TMaster);
  public
    property Master: TMaster read GetMaster write SetMaster;
  end;

implementation

{$R *.dfm}

{-----------------------}
{ Classe TFormMapViewer }
{-----------------------}

{*
  Ma�tre FunLabyrinthe
  @return Ma�tre FunLabyrinthe
*}
function TFormMapViewer.GetMaster: TMaster;
begin
  Result := FrameBaseMapViewer.Master;
end;

{*
  Change de ma�tre
  @param Value   Nouveau ma�tre
*}
procedure TFormMapViewer.SetMaster(Value: TMaster);
begin
  FrameBaseMapViewer.Master := Value;
end;

end.

