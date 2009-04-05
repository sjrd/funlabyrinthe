unit SimpleSquaresDeactivateEffectActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ScUtils, FunLabyUtils, FilesUtils,
  FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresActions, SimpleSquaresConsts;

type
  {*
    Cadre d'édition d'une action Désactiver un effet
    @author sjrd
    @version 5.0
  *}
  TFrameDeactivateEffectActionEditor = class(TFrame)
    LabelEffectID: TLabel;
    SquaresImages: TImageList;
    EditEffectID: TComboBoxEx;
    procedure EditEffectIDChange(Sender: TObject);
  private
    MasterFile: TMasterFile; /// Fichier maître

    FCurrentAction: TDeactivateEffectAction; /// Action courante

    procedure RegisterSingleComponent(Component: TSquareComponent); stdcall;
    procedure RegisterComponentSet(Template: TSquareComponent;
      const Components: array of TSquareComponent; BaseIndex: Integer;
      const DialogTitle, DialogPrompt: string); stdcall;

    procedure FillIDEdits;

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentAction(Value: TDeactivateEffectAction);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentAction: TDeactivateEffectAction
      read FCurrentAction write SetCurrentAction;
  end;

implementation

{$R *.dfm}

{------------------------------------------}
{ TFrameDeactivateEffectActionEditor class }
{------------------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameDeactivateEffectActionEditor.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
end;

{*
  Enregistre un unique composant
  @param Component   Le composant à enregistrer
*}
procedure TFrameDeactivateEffectActionEditor.RegisterSingleComponent(
  Component: TSquareComponent);
var
  SquareBmp: TSquareBitmap;
  ImageIndex: Integer;
  Category: TComboExItems;
  Item: TComboExItem;
begin
  // Choix de la catégorie
  if Component is TEffect then
    Category := EditEffectID.ItemsEx
  else
    Exit; // ignore

  // Ajout de l'image du composant dans la liste d'images
  SquareBmp := TSquareBitmap.Create;
  try
    Component.Draw(NoQPos, SquareBmp.Canvas);
    ImageIndex := SquaresImages.AddMasked(SquareBmp, clTransparent);
  finally
    SquareBmp.Free;
  end;

  // Ajout du bouton
  Item := Category.Add;
  Item.Caption := Component.ID;
  Item.ImageIndex := ImageIndex;
end;

{*
  Enregistre un ensemble de composants
  @param Template       Composant modèle pour l'image et le nom à afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
  @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
*}
procedure TFrameDeactivateEffectActionEditor.RegisterComponentSet(
  Template: TSquareComponent; const Components: array of TSquareComponent;
  BaseIndex: Integer; const DialogTitle, DialogPrompt: string);
begin
  // No support for component sets
end;

{*
  Renseigne les éditeurs ID de composant d'après l'action courante
*}
procedure TFrameDeactivateEffectActionEditor.FillIDEdits;
begin
  with CurrentAction do
  begin
    EditEffectID.Text := IIF(EffectID = '', SNone, EffectID);
    EditEffectID.ItemIndex := EditEffectID.Items.IndexOf(EditEffectID.Text);
  end;
end;

{*
  [@inheritDoc]
*}
function TFrameDeactivateEffectActionEditor.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie l'action à éditer
  @param Value   Nouvelle action
*}
procedure TFrameDeactivateEffectActionEditor.SetCurrentAction(
  Value: TDeactivateEffectAction);
begin
  if CurrentAction <> nil then
  begin
    Visible := False;

    EditEffectID.OnChange := nil;
  end;

  FCurrentAction := Value;

  if CurrentAction <> nil then
  begin
    if MasterFile = nil then // first time
    begin
      EditEffectID.ItemsEx.Add.Caption := SNone;

      MasterFile := GetFunLabyEditMainForm.MasterFile;
      MasterFile.RegisterComponents(RegisterSingleComponent,
        RegisterComponentSet);
    end;

    FillIDEdits;

    EditEffectID.OnChange := EditEffectIDChange;

    Visible := True;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameDeactivateEffectActionEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

{*
  Gestionnaire d'événement OnChange d'un des edit d'ID de composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameDeactivateEffectActionEditor.EditEffectIDChange(
  Sender: TObject);
var
  NewID: TComponentID;
begin
  NewID := (Sender as TComboBoxEx).Text;
  if NewID = SNone then
    NewID := '';

  CurrentAction.EffectID := NewID;

  MarkModified;
end;

end.

