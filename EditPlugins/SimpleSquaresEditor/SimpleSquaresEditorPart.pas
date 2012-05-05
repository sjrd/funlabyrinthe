unit SimpleSquaresEditorPart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyEditOTA, SimpleSquaresUtils;

type
  {*
    Classe de base pour les cadres qui font partie d'éditeur SimpleSquares
    @author sjrd
    @version 5.0
  *}
  TFrameSimpleSquaresEditorPart = class(TFrame, ISimpleSquaresEditor)
  protected
    CustomVisibility: Boolean;

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified; virtual;
  end;

implementation

{$R *.dfm}

{-------------------------------------}
{ TFrameSimpleSquaresEditorPart class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameSimpleSquaresEditorPart.Create(AOwner: TComponent);
begin
  inherited;

  if not CustomVisibility then
  begin
    Visible := False;
    Align := alClient;
  end;
end;

{*
  [@inheritDoc]
*}
function TFrameSimpleSquaresEditorPart.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleSquaresEditorPart.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

end.

