unit SimpleSquaresPlayerColorActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresActionEditor, GR32;

type
  {*
    Cadre d'édition d'une action Changer la couleur du pion
    @author sjrd
    @version 5.0
  *}
  TFramePlayerColorActionEditor = class(TFrameActionEditor)
    LabelColor: TStaticText;
    ListBoxColor: TColorBox;
    procedure ListBoxColorChange(Sender: TObject);
    procedure ListBoxColorGetColors(Sender: TCustomColorBox; Items: TStrings);
  private
    FCurrentAction: TPlayerColorAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TPlayerColorAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

uses
  GR32_Blend;

{$R *.dfm}

function TweakWinColor(Color32: TColor32): TColor;
begin
  if Color32 = clTransparent32 then
    Result := clNone
  else
    Result := WinColor(Color32);
end;

function TweakColor32(WinColor: TColor): TColor32;
begin
  if WinColor = clNone then
    Result := clTransparent32
  else
    Result := Color32(WinColor);
end;

{-------------------------------------}
{ TFramePlayerColorActionEditor class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFramePlayerColorActionEditor.ActivateAction;
begin
  ListBoxColor.HandleNeeded; // work around bug of SetSelected
  ListBoxColor.Selected := TweakWinColor(CurrentAction.Color);

  ListBoxColor.OnChange := ListBoxColorChange;
end;

{*
  [@inheritDoc]
*}
procedure TFramePlayerColorActionEditor.DeactivateAction;
begin
  ListBoxColor.OnChange := nil;
end;

{*
  Gestionnaire d'événement OnClick de la color box
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFramePlayerColorActionEditor.ListBoxColorChange(Sender: TObject);
begin
  CurrentAction.Color := TweakColor32(ListBoxColor.Selected);

  MarkModified;
end;

{*
  Gestionnaire d'événement OnGetColors de la color box
  @param Sender   Objet qui a déclenché l'événement
  @param Items    Liste de chaînes dans laquelle stocker les couleurs
*}
procedure TFramePlayerColorActionEditor.ListBoxColorGetColors(
  Sender: TCustomColorBox; Items: TStrings);

  procedure AddColor(Color: TColor32);
  begin
    Items.AddObject(Color32ToString(Color), TObject(TweakWinColor(Color)));
  end;

begin
  AddColor(clTransparent32);
  AddColor(clBlack32);
  AddColor(clDimGray32);
  AddColor(clGray32);
  AddColor(clLightGray32);
  AddColor(clWhite32);
  AddColor(clMaroon32);
  AddColor(clGreen32);
  AddColor(clOlive32);
  AddColor(clNavy32);
  AddColor(clPurple32);
  AddColor(clTeal32);
  AddColor(clRed32);
  AddColor(clLime32);
  AddColor(clYellow32);
  AddColor(clBlue32);
  AddColor(clFuchsia32);
  AddColor(clAqua32);
end;

end.

