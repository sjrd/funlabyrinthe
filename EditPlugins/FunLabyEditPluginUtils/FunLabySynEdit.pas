unit FunLabySynEdit;

interface

uses
  SysUtils, Classes, Controls, Graphics, SynEdit, SepiCompilerErrors;

type
  {*
    TSynEdit sp�cialis� pour FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabySynEdit = class(TSynEdit)
  private
    FErrorLine: Integer; /// Ligne d'erreur mise en �vidence (0 si aucune)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function DoOnSpecialLineColors(Line: Integer;
      var Foreground, Background: TColor): Boolean; override;

    procedure SetErrorLine(Value: Integer); virtual;
  public
    procedure ShowError(Error: TSepiCompilerError);

    property ErrorLine: Integer read FErrorLine write SetErrorLine;
  end;

implementation

{-----------------------}
{ TFunLabySynEdit class }
{-----------------------}

{*
  [@inheritDoc]
*}
procedure TFunLabySynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  ErrorLine := 0;
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabySynEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  ErrorLine := 0;
  inherited;
end;

{*
  [@inheritDoc]
*}
function TFunLabySynEdit.DoOnSpecialLineColors(Line: Integer;
  var Foreground, Background: TColor): Boolean;
begin
  if Line = ErrorLine then
  begin
    Foreground := clWhite;
    Background := clMaroon;
    Result := True;
  end else
    Result := inherited DoOnSpecialLineColors(Line, Foreground, Background);
end;

{*
  Modifie la ligne d'erreur mise en �vidence
  @param Value   Nouvelle ligne d'erreur � mettre en �vidence (0 pour aucune)
*}
procedure TFunLabySynEdit.SetErrorLine(Value: Integer);
begin
  if Value = FErrorLine then
    Exit;

  if FErrorLine <> 0 then
    InvalidateLine(FErrorLine);
  FErrorLine := Value;
  if FErrorLine <> 0 then
    InvalidateLine(FErrorLine);
end;

{*
  Met en �vidence une erreur de compilation Sepi
  @param Error   Erreur � mettre en �vidence
*}
procedure TFunLabySynEdit.ShowError(Error: TSepiCompilerError);
begin
  CaretX := Error.Col;
  CaretY := Error.Line;
  EnsureCursorPosVisible;
  ErrorLine := Error.Line;
  SetFocus;
end;

end.

